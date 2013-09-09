;;; Copyright (c) 2013, Mark VandenBrink. All rights reserved.

(asdf:defsystem #:surfboard
  :description "Code to watch my cable modem and collect stats."
  :author "Mark VandenBrink"
  :license "Public Domain"
  :depends-on (#:optima #:drakma #:closure-html #:closure-common #:fare-csv)
  :components ((:file "surfboard")
			   (:file "profile" :depends-on ("surfboard"))));;; taglib.asd
