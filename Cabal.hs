module Cabal where

import qualified Codec.Archive.Tar as T
import qualified Data.ByteString.Lazy as BS
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import qualified Distribution.PackageDescription.Parse as P
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Distribution.PackageDescription as PD
import qualified Distribution.Package as Package
import qualified Distribution.Version as Version
import           System.FilePath (takeExtension, splitDirectories, normalise)
import Distribution.Text (simpleParse)
import           Solve (M, P, V, D)

allowedDepends :: M P (M V [D]) -> M P (M V (M P [V]))
allowedDepends m = Map.map (\m' ->
                   Map.map (\deps' ->
                   Map.fromList (
                   map (\dep ->
                         let pn = depPackageName dep
                             vsAllowed = depVersionRange dep
                             vsAvailable = case Map.lookup pn m
                                           of Just mvs -> Map.keys mvs
                                              Nothing  -> []
                             in (pn, filter (withinRange vsAllowed) vsAvailable)) deps')) m') m
  
closureF :: Ord a => (a -> Set.Set a) -> Set.Set a -> Set.Set a
closureF f names = if names == theseAndAllDirectDepends
                   then names
                   else closureF f theseAndAllDirectDepends
  where theseAndAllDirectDepends = allDirectDepends `Set.union` names
        allDirectDepends = names `bind` f

-- | A monadic bind for 'Set'
bind :: (Ord a, Ord b) => Set.Set a -> (a -> Set.Set b) -> Set.Set b
bind s f = Set.unions (Set.toList (Set.map f s))

depPackageName :: D -> P
depPackageName (Package.Dependency p _) = p

depVersionRange :: D -> Version.VersionRange
depVersionRange (Package.Dependency _ v) = v

withinRange :: Version.VersionRange -> V -> Bool
withinRange = flip Version.withinRange

-- It's parsing that makes this take so long!
parse :: C8.ByteString -> [D]
parse = dependenciesOfParse
        . P.parsePackageDescription
        . C8.unpack

dependenciesOfParse :: P.ParseResult PD.GenericPackageDescription
                    -> [D]
dependenciesOfParse x = case x of
  P.ParseOk _ gpd -> (maybe [] id
                      . fmap PD.condTreeConstraints
                      . PD.condLibrary)
                     gpd
  -- vv FIXME: This is not right!  We should actually forget about the
  -- package if this happens.  Return Nothing and propagate
  -- appropriately.
  _               -> []

toListWithKey :: (k -> v -> a) -> M k [v] -> [a]
toListWithKey f = concat . Map.elems . Map.mapWithKey (map . f)

relevantDependencies :: Set.Set (P, V) -> M P (M V [D]) -> M P (M V (M P [V]))
relevantDependencies initial m =
  (Map.filter (not . Map.null)
  . Map.mapWithKey (\p vmpv ->
     Map.filterWithKey (\v _ ->
       (p, v) `Set.member` transitiveClosure) vmpv))
  a
  where transitiveClosure :: Set.Set (P, V)
        transitiveClosure = closureF lookup' initial 

        a :: M P (M V (M P [V]))
        a = allowedDepends m

        lookup' (p, v) = Set.fromList (toListWithKey (,) (maybe Map.empty id (Map.lookup v =<< Map.lookup p a)))

dependencies :: M P (M V BS.ByteString)
             -> [P]
             -> M P (M V [D])
             -> M P (M V [D])
dependencies _ [] soFar = soFar
dependencies cabalFiles' (packageName:packageNames) soFar =
  if packageName `elem` Map.keys soFar
  then ignore
  else case Map.lookup packageName cabalFiles' of
    Nothing -> ignore -- Not sure what to do here.  A dependency's cabal file is missing!
    Just vs -> let versionDeps = Map.map parse vs
                   directDeps  = map depPackageName (concat (Map.elems versionDeps))
               in dependencies cabalFiles' (packageNames ++ directDeps) (Map.insert packageName versionDeps soFar)
    where ignore = dependencies cabalFiles' packageNames soFar


               
cabalFiles :: T.Entry -> M P (M V BS.ByteString) -> M P (M V BS.ByteString)
cabalFiles entry m = case T.entryContent entry of
  T.NormalFile contents _
    | takeExtension fileName == ".cabal"
      -> case splitDirectories (normalise fileName) of
        [pkgname, vers, _] -> case simpleParse vers of
          Just ver -> Map.alter (let k = ver
                                     v = contents
                                 in \x -> Just $ case x of
                                   Nothing -> Map.singleton k v
                                   Just m' -> Map.insert k v m')
                      (Package.PackageName pkgname) m
          _ -> m
        _ -> m
  _ -> m
  where fileName = T.entryPath entry

-- NB foldEntries is a foldr so we should probably build the list lazily
readCabalFiles :: String -> IO (M P (M V BS.ByteString))
readCabalFiles filename = do
  fileContents <- BS.readFile filename
  return (T.foldEntries cabalFiles Map.empty (const Map.empty) (T.read fileContents))

-- | Reads a cabal index tarball to produce a map containing all
-- dependency information relevant to the specified package and
-- version.
deps :: (P, V) -> IO (M P (M V (M P [V])))
deps pv = do
  foo <- readCabalFiles "/home/tom/.cabal/packages/hackage.haskell.org/00-index.tar"
  let m       = Map.map (Map.map parse) foo
      initial = Set.fromList [pv]
      baz     = relevantDependencies initial m

  return baz

saveDeps :: String -> (P, V) -> IO ()
saveDeps filename pv = do
  baz <- deps pv
  writeFile filename (show baz)

loadDeps :: String -> IO (M P (M V (M P [V])))
loadDeps = fmap read . readFile
