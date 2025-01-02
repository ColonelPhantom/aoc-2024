hyperfine -m 2 -M 25 -w 1 --export-csv perf.csv \
    -L day 01,02,03,04,05,06,07,08,09,10,11,12,13,14,15,16,17,18,19,20,21,22,23,24,25 \
    -s "ghc -O -o 'Day{day}/Day{day}.exe' 'Day{day}/Day{day}.hs'" \
    "Day{day}/Day{day}.exe < Day{day}/day{day}.input" \