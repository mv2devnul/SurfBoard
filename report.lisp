;;; -*- Mode: Lisp;  show-trailing-whitespace: t; Base: 10; indent-tabs: nil; Syntax: ANSI-Common-Lisp; Package: CL-USER; -*-
;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

;;; this could just as easily be done in awk/perl, but what the heck.

(defstruct modem-log-record
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

(defun make-modem-record (line)
  (make-modem-log-record
   :timestamp (nth 0 line)
   :docsis-downstream-channel-acquisition (nth 1 line)
   :docsis-ranging (nth 2 line)
   :establish-ip-connectivity-using-dhcp (nth 3 line)
   :establish-time-of-day (nth 4 line)
   :transfer-operational-parameters-through-tftp (nth 5 line)
   :register-connection (nth 6 line)
   :cable-modem-status (nth 7 line)
   :initialize-baseline-privacy (nth 8 line)
   :current-time-and-date (nth 9 line)
   :system-up-time (nth 10 line)
   :downstream-channel-id (nth 11 line)
   :downstream-frequency (nth 12 line)
   :downstream-signal-to-noise-ratio (nth 13 line)
   :downstream-downstream-modulation (nth 14 line)
   :downstream-power-level (nth 15 line)
   :upstream-channel-id (nth 16 line)
   :upstream-frequency (nth 17 line)
   :upstream-ranging-service-id (nth 18 line)
   :upstream-symbol-rate (nth 19 line)
   :upstream-power-level (nth 20 line)
   :upstream-ranging-status (nth 21 line)
   :signal-stats-channel-id (nth 22 line)
   :signal-stats-total-unerrored-codewords (nth 23 line)
   :signal-stats-total-correctable-codewords (nth 24 line)
   :signal-stats-total-uncorrectable-codewords (nth 25 line)))

(defun map-log-file (name &optional (func #'print))
  (with-open-file (f name)
    (fare-csv:read-csv-line f) ; throw away header
    (do ((line (fare-csv:read-csv-line f) (fare-csv:read-csv-line f)))
        ((null line))
      (fare-csv:with-strict-rfc4180-csv-syntax ()
        (funcall func (make-modem-record line))))))

(defun report-levels (in-name out-name)
  (let ((pattern (cl-ppcre:create-scanner " dB.*$")))
    (with-open-file (out out-name :direction :output)
      (write-line "##Time,Downstream Power,Upstream Power,Downstream SNR" out)
      (map-log-file in-name
                    (lambda (l)
                      (fare-csv:write-csv-line (list ;(modem-log-record-current-time-and-date l)
                                                (modem-log-record-timestamp l)
                                                (cl-ppcre:regex-replace pattern (modem-log-record-downstream-power-level l) "")
                                                (cl-ppcre:regex-replace pattern (modem-log-record-upstream-power-level l) "")
                                                (cl-ppcre:regex-replace pattern (modem-log-record-downstream-signal-to-noise-ratio l) ""))
                                               out))))))
