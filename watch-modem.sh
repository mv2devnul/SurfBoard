if [ $# -ne 2 ]; then
	echo "usage: watch-modem csv-file interval-in-seconds"
	exit 1
fi

ccl64 -e '(quicklisp:quickload "surfboard")' -e "(watch-modem \"$1\" $2)"

