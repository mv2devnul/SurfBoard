if [ $# -ne 1 ]; then
   echo "Must supply an input file"
   exit 1
fi

gnuplot <<EOD
reset
set terminal svg size 2048,768 dynamic background "#ffffff"

set xdata time
set timefmt "%b %d %Y %H:%M:%S"
set format x "%m/%d:%H:%M"
set xlabel "Time"

set ylabel "Power Levels"

set autoscale
set grid

set style data linespoints
set datafile separator ","

plot "$1" using 1:2 title "Downstream Power", \
"" using 1:3 title "Upstream Power"

EOD