;;; -*- Lisp -*-
(defpackage :cliki-system (:use #:cl #:asdf))
(in-package :cliki-system)

(defsystem cliki
  :depends-on (ARANEIDA net-telent-date)
  :version "0.3.3"
  :components ((:file "defpackage")               
	       (:file "cliki-instance-class" :depends-on ("defpackage"))
	       (:file "cliki-instance" :depends-on ("cliki-instance-class"))
	       (:file "cliki-page-class" :depends-on ("defpackage"))
	       (:file "cliki-page" :depends-on ("cliki-page-class"))
	       (:file "cliki-request-class" :depends-on ("defpackage"))
	       (:file "cliki-request" :depends-on ("cliki-request-class"))
	       #+nil
               (:file "indexing"
		      :depends-on ("cliki-instance" "cliki-request"))
	       (:file "index" :depends-on ("cliki-instance" "cliki-request"))
	       (:file "hyperspec" :depends-on ("defpackage"))
               ;(:file "link-checker") :depends-on ("indexing"))
               (:file "view" :depends-on
		      ("hyperspec"  "cliki-request"))
               (:file "recent-changes"
		      :depends-on ("cliki-instance"  "cliki-request"))
               (:file "view-source"
		      :depends-on ("cliki-instance" "cliki-request"))
               (:file "edit" :depends-on ("recent-changes"))
               (:file "search" :depends-on ("index"))
               (:file "handlers" :depends-on
                      ("view" "index" "view-source" "edit" "search"))
	       (:static-file "TODO")
	       (:static-file "make-TODO" :pathname "make-TODO.pl")
	       (:static-file "NEWS")
	       (:static-file "README")))


