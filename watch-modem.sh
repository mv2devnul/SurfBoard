case $# in
0) cmd="(watch-modem)"
   ;;
1) cmd="(watch-modem \"$1\")" 
   ;;
2) cmd="(watch-modem \"$1\" $2)"
   ;;
*) echo "Usage: watch-modem [output-file] [interval-in-seconds]"
   exit 1
   ;;
esac

ccl64 -e '(quicklisp:quickload "surfboard")' -e "$cmd"

