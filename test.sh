run_day_ghci () {
    cd Day$1
    /usr/bin/time -f "Day$1: %e seconds" runghc Day$1.hs < day$1.input > /dev/null
    cd ..
}

run_day_ghc () {
    cd Day$1
    ghc -O2 -o Day$1.exe Day$1.hs -threaded > /dev/null
    /usr/bin/time -f "Day$1: %e seconds (%P CPU)" ./Day$1.exe +RTS -N -ls < day$1.input > /dev/null
    cd ..
}

for i in {01..25}; do
    run_day_ghc $i;
done