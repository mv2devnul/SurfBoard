(defparameter *modem-status* "http://192.168.100.1/indexData.htm")
(defparameter *modem-stats*  "http://192.168.100.1/cmSignalData.htm")
(defparameter *modem-logs*   "http://192.168.100.1/cmLogsData.htm")

(defstruct table-row name value)

(defun get-modem-status ()
  (let (this-run)
    (labels ((recurse (x)
               (optima:match x
                 (`(:tr nil (:td nil ,name) (:td nil ,value))
                   (push (make-table-row :name name :value value) this-run))
                 ((list* (satisfies keywordp) _ children)
                  (map nil #'recurse children)))))
      (recurse (closure-html:parse (drakma:http-request *modem-status*) (closure-html:make-lhtml-builder))))
    (nreverse this-run)))

(defun upto-char (string c)
  (subseq string 0 (position c string)))

(defparameter *pattern* "<TR><TD>Power Level<TABLE border=0 cellPadding=0 cellSpacing=0 width=300>        <TBODY><TR>          <TD align=left><SMALL>The Downstream Power Level reading is a             snapshot taken at the time this page was requested. Please             Reload/Refresh this Page for a new reading           </SMALL></TD></TR></TBODY></TABLE></TD>")
(defmacro trim-string (s) `(string-trim '(#\Null #\Space #\Newline) ,s))

(defun get-modem-stats ()
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

      (setf html (cl-ppcre:regex-replace (cl-ppcre:create-scanner *pattern*) (drakma:http-request *modem-stats*) "<TR><TD>Power Level</TD>"))
      (setf html (cl-ppcre:regex-replace (cl-ppcre:create-scanner "<BR>") html ""))
      (setf html (cl-ppcre:regex-replace (cl-ppcre:create-scanner "&nbsp;") html ""))
      (recurse (closure-html:parse html (closure-html:make-lhtml-builder))))
    (nreverse this-run)))

(defstruct log-row time priority code message)

(defun get-modem-log ()
  (let (this-run)
    (labels ((recurse (x)
               (optima:match x
                 (`(:tr nil (:td nil ,col1) (:td nil ,col2) (:td nil ,col3) (:td nil ,col4))
                   (push (make-log-row :time col1 :priority col2 :code col3 :message col4) this-run))
                 ((list* (satisfies keywordp) _ children)
                  (map nil #'recurse children)))))
      (recurse (closure-html:parse (drakma:http-request *modem-logs*) (closure-html:make-lhtml-builder)))
      (nreverse this-run))))

(defun get-timestamp-string ()
  "returns a string with year-month-day-hour-minute-second"
  (multiple-value-bind (second minute hour date month year day daylight-p zone) (get-decoded-time)
    (declare (ignore zone daylight-p date))
    (format nil "~a-~a-~a-~a-~a-~a" year month day hour minute second)))

(defun write-header-line (stream)
  (fare-csv:write-csv-line '("Timestamp"
                             "DOCSIS Downstream Channel Acquisition"
                             "DOCSIS Ranging"
                             "Establish IP Connectivity using DHCP"
                             "Establish Time Of Day"
                             "Transfer Operational Parameters through TFTP"
                             "Register Connection"
                             "Cable Modem Status"
                             "Initialize Baseline Privacy"
                             "Current Time and Date"
                             "System Up Time"
                             "Downstream Channel ID"
                             "Downstream Frequency"
                             "Downstream Signal to Noise Ratio"
                             "Downstream Downstream Modulation"
                             "Downstream Power Level"
                             "Upstream Channel ID"
                             "Upstream Frequency"
                             "Upstream Ranging Service ID"
                             "Upstream Symbol Rate"
                             "Upstream Power Level"
                             "Upstream Ranging Status "
                             "Signal Stats Channel ID"
                             "Signal Stats Total Unerrored Codewords"
                             "Signal Stats Total Correctable Codewords"
                             "Signal Stats Total Uncorrectable Codewords")
                           stream))

(defun write-table-line (stream modem-status modem-stats)
  (let ((s))
    (push (get-timestamp-string) s)
    (dolist (e modem-status)
      (push (table-row-value e) s))
    (dolist (e modem-stats)
      (push (table-row-value e) s))
    (fare-csv:write-csv-line (nreverse s) stream)))

(defun watch-modem (log-name &optional (interval (* 5 60)))
  (format t "Writing to ~a at ~:d interval~%" log-name interval)
  (let ((count 0)
        (f (open log-name :direction :output :if-does-not-exist :create :if-exists :rename)))
    (unwind-protect
         (progn
           (write-header-line f)
           (loop
             (format t "Starting iteration ~:d~%" (incf count))
             (handler-case
                 (let* (
                        ;;(modem-log (get-modem-log))
                        (modem-stats (get-modem-stats))
                        (modem-status (get-modem-status)))
                                        ;(format t "Log: ~a~%" modem-log)
                                        ;(format t "Stats: ~a~%" modem-stats)
                                        ;(format t "Status: ~a~%" modem-status)
                   (write-table-line f modem-status modem-stats)
                   (finish-output f))
               (condition (c)
                 (warn "At ~a, got condition: <~a>" (get-timestamp-string) c)))
             (sleep interval)))
      (when f (finish-output f) (close f)))))
