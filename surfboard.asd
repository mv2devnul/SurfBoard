;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(asdf:defsystem #:surfboard
  :description "Code to watch my cable modem and collect stats."
  :author "Mark VandenBrink"
  :license "Public Domain"
  :depends-on (#:optima #:optima.ppcre #:drakma #:closure-html #:closure-common #:fare-csv #:cl-ppcre #:chanl)
  :components ((:file "surfboard" :depends-on ("packages"))
			   (:file "packages")))
