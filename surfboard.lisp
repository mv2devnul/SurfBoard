;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: CL-USER; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(defparameter *modem-status-uri* "http://192.168.100.1/indexData.htm")
(defparameter *modem-stats-uri*  "http://192.168.100.1/cmSignalData.htm")
(defparameter *modem-logs-uri*   "http://192.168.100.1/cmLogsData.htm")

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

(defstruct table-row name value)

(defun get-modem-status ()
  (let (this-run)
    (labels ((recurse (x)
               (optima:match x
                 (`(:tr nil (:td nil ,name) (:td nil ,value))
                   (push (make-table-row :name name :value value) this-run))
                 ((list* (satisfies keywordp) _ children)
                  (map nil #'recurse children)))))
      (recurse (closure-html:parse (drakma:http-request *modem-status-uri* :connection-timeout 5) (closure-html:make-lhtml-builder))))
    (nreverse this-run)))

(defun upto-char (string c)
  (subseq string 0 (position c string)))

;;; warning: hideous hack to follow. The SB6121 embeds a table inside a taoble, which makes using OPTIMA
;;; messy.  I really should figure out how to do this all with OPTIMA, but this is just easier...
(defparameter *pattern* "<TR><TD>Power Level<TABLE border=0 cellPadding=0 cellSpacing=0 width=300>        <TBODY><TR>          <TD align=left><SMALL>The Downstream Power Level reading is a             snapshot taken at the time this page was requested. Please             Reload/Refresh this Page for a new reading           </SMALL></TD></TR></TBODY></TABLE></TD>")
(defmacro trim-string (s) `(string-trim '(#\Null #\Space #\Newline) ,s))
(defun get-timestamp-string ()
  "returns a string with year-month-day-hour-minute-second"
  (multiple-value-bind (second minute hour date month year) (get-decoded-time)
    (format nil "~4d-~2,'0d-~2,'0d-~2,'0d-~2,'0d-~2,'0d" year month date hour minute second)))

(defun get-modem-stats ()
  "Parse the modem signal stats page of the SB6121"
  (let ((this-run)
        (html))
    (labels ((recurse (x)
               (optima:match x
                 (`(:tr nil (:td nil ,name) (:td nil ,value))
                   (progn
                     (setf value (trim-string (upto-char value #\No-Break_Space)))
                     (push (make-table-row :name name :value value) this-run)))

                 ((list* (satisfies keywordp) _ children)
                  (map nil #'recurse children)))))

      (setf html (cl-ppcre:regex-replace (cl-ppcre:create-scanner *pattern*) (drakma:http-request *modem-stats-uri* :connection-timeout 5) "<TR><TD>Power Level</TD>"))
      (setf html (cl-ppcre:regex-replace (cl-ppcre:create-scanner "<BR>") html ""))
      (setf html (cl-ppcre:regex-replace (cl-ppcre:create-scanner "&nbsp;") html ""))
      (recurse (closure-html:parse html (closure-html:make-lhtml-builder))))
    (nreverse this-run)))

(defstruct log-row time priority code message)

(defun get-modem-log ()
  "Parse the modem log page of the SB6121"
  (let (this-run)
    (labels ((recurse (x)
               (optima:match x
                 (`(:tr nil (:td nil ,col1) (:td nil ,col2) (:td nil ,col3) (:td nil ,col4))
                   (push (make-log-row :time col1 :priority col2 :code col3 :message col4) this-run))
                 ((list* (satisfies keywordp) _ children)
                  (map nil #'recurse children)))))
      (recurse (closure-html:parse (drakma:http-request *modem-logs-uri* :connection-timeout 5) (closure-html:make-lhtml-builder)))
      (nreverse this-run))))


(defun write-header-line (stream)
  "Write the header line for the CSV file."
  (fare-csv:write-csv-line (append (list "Timestamp") *modem-status-headers* *modem-stats-headers*) stream))

(defun write-table-line (stream &optional modem-status modem-stats)
  "Get the modem status, then stats, and create/write a line to the CSV file."
  (let ((s))
    (push (get-timestamp-string) s)
    (if modem-status
        (dolist (e modem-status)
          (push (table-row-value e) s))
        (dotimes (i (length *modem-status-headers*))
          (push 0 s)))
    (if modem-stats
        (dolist (e modem-stats)
          (push (table-row-value e) s))
        (dotimes (i (length *modem-stats-headers*))
          (push 0 s)))

    (fare-csv:write-csv-line (nreverse s) stream)))

(defparameter *last-log* nil)

(defun log-row-equal (a b)
  "Compare two modem log entries."
  (and (string= (log-row-time a) (log-row-time b))
       (string= (log-row-priority a) (log-row-priority b))
       (string= (log-row-code a) (log-row-code b))
       (string= (log-row-message a) (log-row-message b))))

(defun process-modem-log (log f)
  "Tries to keep a running log of the modem log; NOT totally correct!!!!"
  (let ((ts (get-timestamp-string)))
    (when (null *last-log*)
      ;; first time called
      (fare-csv:write-csv-line '(Real-time Time Priority Code Message) f)
      (dolist (l log)
        (fare-csv:write-csv-line (list ts
                                       (log-row-time l)
                                       (log-row-priority l)
                                       (log-row-code l)
                                       (log-row-message l)) f))
      (setf *last-log* log)
      (return-from process-modem-log t))

    (when (not (log-row-equal (first log) (first *last-log*)))
      ;; the log we just read has a different entry for the first record. try to splice together old/new
      (block move-old
        (dolist (l log)
          (if (not (log-row-equal l (first *last-log*)))
              (fare-csv:write-csv-line (list ts
                                             (log-row-time l)
                                             (log-row-priority l)
                                             (log-row-code l)
                                             (log-row-message l)) f)
              (return-from move-old t))))
      (setf *last-log* log))))

(defun watch-modem (&optional (log-name "modem") (interval 60))
  "Sit in an infinite loop, reading the modem status/stats/log at INTERVAL seconds."
  (setf *last-log* nil)
  (format t "Writing to ~a at ~:d second interval~%" log-name interval)
  (let ((count 0)
        (ml  (open (format nil "~a-log.csv" log-name) :direction :output :if-does-not-exist :create :if-exists :rename))
        (err (open (format nil "~a.errs" log-name)    :direction :output :if-does-not-exist :create :if-exists :rename))
        (f   (open (format nil "~a.csv" log-name)     :direction :output :if-does-not-exist :create :if-exists :rename)))
    (unwind-protect
      (fare-csv:with-strict-rfc4180-csv-syntax ()
        (write-header-line f)
        (loop
          (format t "Starting iteration ~:d~%" (incf count))
          (handler-case
              (let* ((modem-log (get-modem-log))
                     (modem-stats (get-modem-stats))
                     (modem-status (get-modem-status)))
                (write-table-line f modem-status modem-stats)
                (process-modem-log modem-log ml))
            (condition (c)
              ;; most likely cause for error is modem is down, so write a fake entry
              ;; (ie, with power-levels 0, etc)
              (write-table-line f)
              (let ((line (format nil "At ~a, got condition: <~a>~%" (get-timestamp-string) c)))
                (format *error-output* "~a~%" line)
                (format err "~a~%" line))))
          (finish-output ml)
          (finish-output err)
          (finish-output f)
          (sleep interval)))

      ;; unwind-protect cleanup.
      (when f (finish-output f) (close f))
      (when err (finish-output err) (close err))
      (when ml (finish-output ml) (close ml)))))
