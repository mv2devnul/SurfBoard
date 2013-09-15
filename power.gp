if [ $# -ne 1 ]; then
    echo "Must supply one argument---an input file."
    exit 1
fi

SAMPLES=$(wc -l <"$1")
OUTAGES=$(grep '0,0,0,0,0' "$1" | wc -l)
RATIO=$(dc -e "2 k $OUTAGES $SAMPLES / p")
gnuplot <<EOD
reset
set terminal svg size 2048,768 dynamic background "#ffffff"
#set terminal png size 1024,768 background "#ffffff"

set xdata time
#Keep this one around for old data sets: set timefmt "%b %d %Y %H:%M:%S"
set timefmt "%Y-%m-%d-%H-%M-%S"
set format x "%d:%H:%M"

set xlabel "Time ($SAMPLES samples, $OUTAGES outages, ${RATIO}%)"
set ylabel "Power Levels (dBmV)"

set autoscale
set grid

set style data lines
set linetype 1 lc rgb "red" lw 2 pt 0
set linetype 2 lc rgb "green" lw 2 pt 0
set linetype 3 lc rgb "blue" lw 2 pt 0

set datafile separator ","

plot "$1" \
using 1:16 title "Downstream Power (should be -15 -- +15)", \
"$1" using 1:21 title "Upstream Power (should be 30 -- 55 with QAM256)", \
"$1" using 1:14title "Downstream SNR (should be >30)"

EOD
