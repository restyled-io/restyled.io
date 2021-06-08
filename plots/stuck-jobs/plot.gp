reset

set title "Stuck Job % (30 days)"
set datafile separator ","

set xdata time
set timefmt "%Y-%m-%d"
set format x "%m/%d"

set term png
set output "/dev/stdout"
plot "/dev/stdin" using 1:4 with lines notitle
