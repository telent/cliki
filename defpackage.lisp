(in-package :user)
(defpackage "CLIKI"
  (:export cliki-view-handler cliki-edit-handler)
  (:use "MAKE" "COMMON-LISP" "ARANEIDA"))

;;; used for interning page titles in, and not a lot else.  Working
;;; EQUAL hash table would be nice.
(defpackage "CLIKI-PAGES")

;;; used for collection of markup routines
(defpackage "CLIKI-HTML")
