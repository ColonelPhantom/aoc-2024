set datafile separator ","
set boxwidth 0.5
set style fill solid

plot "perf.csv" using 9:2:xtic(9) with boxes notitle