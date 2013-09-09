if [ $# -ne 2 ]; then
    echo "usage $0 input-file output-file"
fi
if [ -f "$2" ]; then
    echo "output file $2 exists, please delete it or use another file name for output"
    exit 1
fi
if [ -f "$2.svg" ]; then
    echo "SVG output file $2.svg exists, please delete it or use another file name for output"
    exit 2
fi

ccl64 -e '(quicklisp:quickload "surfboard")' -e "(report-levels \"$1\" \"$2\")" -e "(quit)"
./power.gp "$2" > "$2.svg"
xdg-open "$2.svg"

