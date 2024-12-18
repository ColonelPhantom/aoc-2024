run_day_ghci () {
    cd Day$1
    /usr/bin/time -f "Day$1: %e seconds" runghc Day$1.hs < day$1.input > /dev/null
    cd ..
}

run_day_ghc () {
    cd Day$1
    ghc -O2 -o Day$1.exe Day$1.hs -prof -fprof-auto > /dev/null
    /usr/bin/time -f "Day$1: %e seconds" ./Day$1.exe +RTS -p < day$1.input > /dev/null
    cd ..
}

for i in {01..18}; do
    run_day_ghc $i;
done