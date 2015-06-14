# A proof-of-concept cabal solver example using SAT

1. Install the `minisat` binary.
[http://www.minisat.se/MiniSat.html](http://www.minisat.se/MiniSat.html).

2. Go to the `deps` function in `Cabal.hs` and change the location of
the cabal index tarball to the one on your system.

3. Run `saveOpaleyeDeps` to load the cabal index tarball, parse all
the cabal files within, and save the dependencies that pertain to the
`opaleye` package.  This step is very slow (around 100 seconds on my
machine) because I parse all cabal files at the start, rather than
parsing them on demand.  A fairly basic fix could speed up this stage
a lot.

4. Run `main` to find the install plan for opaleye which requires the
fewest dependent packages to be installed.  This takes around 15
seconds on my machine.  Speeding this up is work in progress.

    ghci Main.hs
    *Main> saveOpaleyeDeps
    ...
    *Main> main
    Looking for 0 <= num packages < 100
    Looking for 0 <= num packages < 49
    ...
