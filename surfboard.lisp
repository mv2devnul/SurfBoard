;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: CL-USER; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.
(in-package #:surfboard)

(eval-when (:load-toplevel :compile-toplevel :execute)
  (pushnew :CONCURRENT-PLOT *features*)

  (defun mkstr (&rest args)
    "Given ARGS, create a string. Case of symbols in ARGS is determined by *PRINT-CASE*.
Strings' case are as passed in
eg:
    (dolist (*print-case* '(:upcase :downcase :capitalize :studly))
      (format t \"~a -->> ~a~%\" *print-case* (mkstr 'this '-is- \"SPARTA\")))
yields:
    UPCASE -->> THIS-IS-SPARTA
    downcase -->> this-is-SPARTA
    Capitalize -->> This-Is-SPARTA
    STUDlY -->> ThiS-IS-SPARTA"
    (format nil "~{~a~}" args))

  (defun mksym (&rest args)
    "Intern a symbol in current package.  NB: we coerce to all upcase."
    (intern (string-upcase (apply #'mkstr args))))

  (defun mkkeyword (&rest args)
    "Intern a keyword."
    (intern (string-upcase (apply #'mkstr args)) :keyword))
  )

(defparameter *modem-status-uri* "http://192.168.100.1/indexData.htm")
(defparameter *modem-stats-uri*  "http://192.168.100.1/cmSignalData.htm")
(defparameter *modem-logs-uri*   "http://192.168.100.1/cmLogsData.htm")

#+:CONCURRENT-PLOT
(progn
  (defparameter *chanl* nil)
  (defmacro c-send (d) `(chanl:send *chanl* ,d))
  (defmacro c-recv ()  `(chanl:recv *chanl*)))

(defparameter *modem-status-headers*
  '("DOCSIS Downstream Channel Acquisition"
    "DOCSIS Ranging"
    "Establish IP Connectivity using DHCP"
    "Establish Time Of Day"
    "Transfer Operational Parameters through TFTP"
    "Register Connection"
    "Cable Modem Status"
    "Initialize Baseline Privacy"
    "Current Time and Date"
    "System Up Time"))

(defparameter *modem-stats-headers*
  '("Downstream Channel ID"
    "Downstream Frequency"
    "Downstream Signal to Noise Ratio"
    "Downstream Downstream Modulation"
    "Downstream Power Level"
    "Upstream Channel ID"
    "Upstream Frequency"
    "Upstream Ranging Service ID"
    "Upstream Symbol Rate"
    "Upstream Power Level"
    "Upstream Ranging Status"
    "Signal Stats Channel ID"
    "Signal Stats Total Unerrored Codewords"
    "Signal Stats Total Correctable Codewords"
    "Signal Stats Total Uncorrectable Codewords"))

(defun upto-char (string c)
  "return the part of STRING up until we find the char C"
  (subseq string 0 (position c string)))

(defun ensure-strings (in-lst)
  "Given a list, make sure that all members are strings"
  (loop for e in in-lst collecting (if (stringp e) e "BAD STRING")))

(defmacro trim-string (s)
  "Given a string S, get rid of spaces, nulls, and newlines from the end."
  `(string-trim '(#\Null #\Space #\Newline) ,s))

(defun get-timestamp-string ()
  "returns a string with year-month-day-hour-minute-second"
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (format nil "~4d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-~2,'0d" year month date hour minute second)))

(defmacro defstruct+ (name &rest slots)
  "macro to create a structure and a function to populate that structure from a list.
The list is expected to match the order of the slots as given to this macro.
Example: (destruct my-struct slot-1 slot-2 slot3)
creates my-struct and also a function mk-my-struct that takes a list."
  `(progn
     (defstruct ,name ,@(mapcar (lambda (s) s) slots))
     (defun ,(mksym "mk-" name) (lst)
       (funcall ',(mksym "make-" name)
                ,@(loop for i = 0 then (incf i)
                        for c in slots
                        collecting (mkkeyword c)
                        collecting `(nth ,i lst))))))

(defstruct+ modem-status-row name value)                ; for the modem status table
(defstruct+ modem-log-row time priority code message)   ; for the modem log table

(defstruct+ modem-stats-row                             ; for the modem stats tables
  timestamp
  docsis-downstream-channel-acquisition
  docsis-ranging
  establish-ip-connectivity-using-dhcp
  establish-time-of-day
  transfer-operational-parameters-through-tftp
  register-connection
  cable-modem-status
  initialize-baseline-privacy
  current-time-and-date
  system-up-time
  downstream-channel-id
  downstream-frequency
  downstream-signal-to-noise-ratio
  downstream-downstream-modulation
  downstream-power-level
  upstream-channel-id
  upstream-frequency
  upstream-ranging-service-id
  upstream-symbol-rate
  upstream-power-level
  upstream-ranging-status
  signal-stats-channel-id
  signal-stats-total-unerrored-codewords
  signal-stats-total-correctable-codewords
  signal-stats-total-uncorrectable-codewords)


(defun write-csv-line (lst stream)
  "Write a CSV line to STREAM constructed from LST."
  (fare-csv:with-strict-rfc4180-csv-syntax ()
    (fare-csv:write-csv-line lst stream)))

(defun get-modem-status ()
  "Get the modem status from the web page."
  (let (this-run)
    (labels ((recurse (x)
               (optima:match x
                 (`(:tr nil (:td nil ,name) (:td nil ,value))
                   (push (mk-modem-status-row (ensure-strings (list name value))) this-run))
                 ((list* (satisfies keywordp) _ children)
                  (map nil #'recurse children)))))
      (recurse (closure-html:parse (drakma:http-request *modem-status-uri* :connection-timeout 5) (closure-html:make-lhtml-builder))))
    (nreverse this-run)))

;;; warning: hideous hack to follow. The SB6121 embeds a table inside a table, which makes using OPTIMA
;;; messy.  I really should figure out how to do this all with OPTIMA, but this is just easier...
(defparameter *pattern* "<TR><TD>Power Level<TABLE border=0 cellPadding=0 cellSpacing=0 width=300>        <TBODY><TR>          <TD align=left><SMALL>The Downstream Power Level reading is a             snapshot taken at the time this page was requested. Please             Reload/Refresh this Page for a new reading           </SMALL></TD></TR></TBODY></TABLE></TD>")

(defun get-modem-stats ()
  "Parse the modem signal stats page of the SB6121"
  (let ((this-run)
        (html))
    (labels ((recurse (x)
               (optima:match x
                 (`(:tr nil (:td nil ,name) (:td nil ,value))
                   (progn
                     (setf value (trim-string (upto-char value #\No-Break_Space)))
                     (push (mk-modem-stats-row (ensure-strings (list name value))) this-run)))

                 ((list* (satisfies keywordp) _ children)
                  (map nil #'recurse children)))))

      (setf html (cl-ppcre:regex-replace (cl-ppcre:create-scanner *pattern*) (drakma:http-request *modem-stats-uri* :connection-timeout 5) "<TR><TD>Power Level</TD>"))
      (setf html (cl-ppcre:regex-replace (cl-ppcre:create-scanner "<BR>") html ""))
      (setf html (cl-ppcre:regex-replace (cl-ppcre:create-scanner "&nbsp;") html ""))
      (recurse (closure-html:parse html (closure-html:make-lhtml-builder))))
    (nreverse this-run)))

(defun get-modem-log ()
  "Parse the modem log page of the SB6121"
  (let (this-run)
    (labels ((recurse (x)
               (optima:match x
                 (`(:tr nil (:td nil ,col1) (:td nil ,col2) (:td nil ,col3) (:td nil ,col4))
                   (push (mk-modem-log-row (ensure-strings (list col1 col2 col3 col4))) this-run))
                 ((list* (satisfies keywordp) _ children)
                  (map nil #'recurse children)))))
      (recurse (closure-html:parse (drakma:http-request *modem-logs-uri* :connection-timeout 5) (closure-html:make-lhtml-builder)))

      (nreverse this-run))))

(defun write-header-line (stream)
  "Write the header line for the CSV file."
  (write-csv-line (append (list "Timestamp") *modem-status-headers* *modem-stats-headers*) stream))

(defun write-table-line (stream &optional modem-status modem-stats)
  "Write modem status, then stats to STREAM"
  (let (s)
    (push (get-timestamp-string) s)

    (if modem-status
        (dolist (e modem-status)
          (push (modem-status-row-value e) s))
        (dotimes (i (length *modem-status-headers*))
          (push 0 s)))

    (if modem-stats
        (dolist (e modem-stats)
          (push (modem-status-row-value e) s))
        (dotimes (i (length *modem-stats-headers*))
          (push 0 s)))

    (write-csv-line (nreverse s) stream)))

;; modem entries read from the modem are in reverse chronological order
;; e.g., most recent error is (first *last-log*)
(defparameter *last-log* nil "list of last log entries  read from http/modem.")

(defun modem-log-row-equal (a b)
  "Compare two modem log entries, ignoring the timestamp we add"
  (and (string= (modem-log-row-time a)     (modem-log-row-time b))
       (string= (modem-log-row-priority a) (modem-log-row-priority b))
       (string= (modem-log-row-code a)     (modem-log-row-code b))
       (string= (modem-log-row-message a)  (modem-log-row-message b))))

(defun read-modem-log (f &optional (no-header t))
  "Read in the modem log and convert it back to a list of structs"
  (let* ((ret (mapcar (lambda (l) (mk-modem-log-row l)) (fare-csv:read-csv-file f))))
    (when no-header (pop ret))
    ret))

(defmacro while (test &body body)
  `(do ()
       ((not ,test))
     ,@body))

(defun process-modem-log (log f)
  "Tries to keep a running log of the modem log.
NB: the SB 6121 only displays the last 20 error messages, so it is entirely
possible that we miss capturing errors (e.g. modem gets >20 errors in
the timeframe from the last time we sampled."
  (let ((ts (get-timestamp-string)))
    (when (null *last-log*)
      ;; first time called
      (write-csv-line '("Timestamp" "Real-time" "Time" "Priority" "Code" "Message") f)
      (dolist (l (nreverse log))
        (write-csv-line (list ts
                              (modem-log-row-time l)
                              (modem-log-row-priority l)
                              (modem-log-row-code l)
                              (modem-log-row-message l)) f))
      (setf *last-log* log)
      (return-from process-modem-log t))

    (let (new)
      (loop for row in log do
        (when (modem-log-row-equal row (first *last-log*))
          (return))
        (push row new))
      (dolist (row (nreverse new))
        (write-csv-line (list ts
                              (modem-log-row-time row)
                              (modem-log-row-priority row)
                              (modem-log-row-code row)
                              (modem-log-row-message row))
                        f)))
    (setf *last-log* log)))

(defmacro modem-stats-name  (f) `(format nil "~a.csv" ,f))
(defmacro modem-error-log-name  (f) `(format nil "~a-log.csv" ,f))
(defmacro socket-error-name (f) `(format nil "~a.errs" ,f))

(defun watch-modem (log-name &optional (interval 60))
  "Sit in an infinite loop, reading the modem status/stats/log at INTERVAL seconds and writing to file-based logs"
  (setf *last-log* nil)
  (setf *chanl* (make-instance 'chanl:bounded-channel))
  (format t "Writing to ~a at ~:d second interval~%" log-name interval)

  (let* ((count                       0)
         (modem-stats-file-exists     (probe-file (modem-stats-name log-name)))
         (modem-error-log-file-exists (probe-file (modem-error-log-name log-name)))
         (modem-error-log-file        (open (modem-error-log-name log-name) :direction :output :if-does-not-exist :create :if-exists :append))
         (socket-error-file           (open (socket-error-name log-name) :direction :output :if-does-not-exist :create :if-exists :append))
         (modem-stats-file            (open (modem-stats-name  log-name) :direction :output :if-does-not-exist :create :if-exists :append)))

    #+:CONCURRENT-PLOT
    (progn
      (chanl:pcall #'chanl-worker-thread)
      (c-send "start")
      (c-send (modem-stats-name log-name)))

    (when modem-error-log-file-exists
      ;; since we store the modem log in chronological order (e.g. the last line of file is
      ;; the most-recent error) we need to reverse what we read
      (setf *last-log* (nreverse (last (read-modem-log modem-error-log-file) 20))))

    (unwind-protect
         (unless modem-stats-file-exists (write-header-line modem-stats-file))

        (loop
          (format t "Starting iteration ~:d~%" (incf count))

          (handler-case
              (let* ((modem-log (get-modem-log))
                     (modem-stats (get-modem-stats))
                     (modem-status (get-modem-status)))
                (write-table-line modem-stats-file modem-status modem-stats)
                (process-modem-log modem-log modem-error-log-file))
            (usocket:socket-error (c)
              (write-table-line modem-stats-file)
              (let ((line (format nil "At ~a, got condition: <~a>" (get-timestamp-string) c)))
                (format *error-output* "~a~%" line)
                (format socket-error-file "~a~%" line))))

          (finish-output modem-error-log-file)
          (finish-output socket-error-file)
          (finish-output modem-stats-file)

          #+:CONCURRENT-PLOT
          (c-send "plot")

          (sleep interval)))

      ;; unwind-protect cleanup.
      #+:CONNCURRENT-PLOT
      (c-send "quit")

      (when modem-stats-file (finish-output modem-stats-file) (close modem-stats-file))
      (when socket-error-file (finish-output socket-error-file) (close socket-error-file))
      (when modem-error-log-file (finish-output modem-error-log-file) (close modem-error-log-file))))

#+:CONCURRENT-PLOT
(let* ((proc)
       (file)
       (stream)
       (plot `("reset"
               "set term wxt noraise"
               "set xdata time"
               "set timefmt \"%Y-%m-%d-%H-%M-%S\""
               "set format x \"%d:%H:%M\""
               "set ylabel \"Power Levels (dBmV)\""
               "set autoscale"
               "set grid"
               "set style data lines"
               "set linetype 1 lc rgb \"red\" lw 1 pt 0"
               "set linetype 2 lc rgb \"green\" lw 1 pt 0"
               "set linetype 3 lc rgb \"blue\" lw 1 pt 0"
               "set datafile separator \",\"")))

  (defun start-gnuplot ()
    "Kick off gnuplot as a sub-process"
    (setf proc (ccl:run-program "gnuplot" '() :input :stream :output :stream :wait nil))
    (unless proc
      (error "Cannot create process."))
    (setf stream (make-two-way-stream
                  (ccl:external-process-output-stream proc)
                  (ccl:external-process-input-stream proc))))

  (defun end-gnuplot ()
    "Tell gnuplot to quit"
    (when stream
      (send-command "quit")
      (close stream)))

  (defun read-no-hang ()
    "Read input from gnuplot w/o hanging."
    (with-output-to-string (str)
      (do ((c (read-char-no-hang stream)
              (read-char-no-hang stream)))
          ((null c))
        (write-char c str))))

  (defun send-command (c)
    "Send a gnuplot command, ensure that newline/flush is sent/done."
    (format stream c)
    (fresh-line stream)
    (finish-output stream))

  (defun do-plot ()
    "Send plot commands to gnuplot"
    ;(format *error-output* "File = ~a~%" file)
    (dolist (p plot)
      ;(format *error-output* "Sending ~a, gnuploat sez: ~a~%" p (read-no-hang))
      (send-command p))

    (let ((count 0)
          (outages 0))
      (with-open-file (f file)
        (read-line f nil) ; throw away header
        (do ((line (read-line f nil) (read-line f nil)))
            ((null line))
          (incf count)
          (when (search "0,0,0,0,0" line) (incf outages))))
      (send-command (format nil "plot \"~a\" using 1:16 title \"Downstream Power (should be -15 -- +15)\", \\" file))
      (send-command "\"\" using 1:21 title \"Upstream Power (should be 30 -- 55 with QAM256)\", \\")
      (send-command "\"\" using 1:14title \"Downstream SNR (should be >30)\"")
      (send-command (format nil "set xlabel \"Time (~d samples, ~d outages, ~f%)\""
                            count outages (round (/ outages count))))
      (send-command "replot")))

  (defun chanl-worker-thread ()
    "A thread that sits and waits on *CHANL* and when it receives a msg on *CHANL*, performs the appropriate action."
    (loop
      (let ((msg (c-recv)))
        ;(format *error-output* "Recv: ~a~%" msg)
        (cond ((string= "start" msg)
               (start-gnuplot))
              ((string= "quit" msg)
               (end-gnuplot)
               (return-from chanl-worker-thread nil))
              ((string="plot" msg)
               (do-plot))
              (t (setf file msg))))))) ; if not start/quit/plot, assume it's the name of the plot file to use

(defparameter *up-time-pat*      "^(\\d+) days (\\d+)h:(\\d+)m:(\\d+)s")
(defun uptime-seconds (v)
  (optima:match v ((optima.ppcre:ppcre *up-time-pat* days hours mins secs)
                   (+ (* (parse-integer days) 24 60 60)
                      (* (parse-integer hours) 60 60)
                      (* (parse-integer mins) 60)
                      (parse-integer secs)))))
(defun seconds-to-uptime (seconds)
  (let* ((days (floor seconds (* 60 60 24)))
         (seconds (rem seconds (* 60 60 24)))
         (hours (floor seconds (* 60 60)))
         (seconds (rem seconds (* 60 60)))
         (minutes (floor seconds 60))
         (seconds (rem seconds 60)))
    (format nil "~d days ~dh:~dm:~ds" days hours minutes seconds)))

(defun max-uptime (file)
  (let ((times (mapcar (lambda (l) (mk-modem-stats-row l)) (fare-csv:read-csv-file file)))
        (max 0)
        (this-time))
    (pop times)
    (dolist (r times)
      (setf this-time (uptime-seconds (modem-stats-row-system-up-time r)))
      (if (and this-time (> this-time max))
          (setf max this-time)))
    (format t "Max uptime was ~a~%" (seconds-to-uptime max))))
