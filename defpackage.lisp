(in-package :user)
(defpackage "CLIKI"
  (:export cliki-view-handler cliki-edit-handler)
  (:use "MAKE" "COMMON-LISP" "ARANEIDA"))

;;; language for inline searches and stuff
(defpackage "CLIKI-PAGES" (:use #| nil |# ))

;;; used for collection of markup routines
(defpackage "CLIKI-HTML")



