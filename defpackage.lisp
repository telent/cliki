(in-package :cl-user)
(defpackage "CLIKI"
  (:export cliki-view-handler cliki-edit-handler cliki-instance cliki-url-root
	   cliki-css-text cliki-page-header cliki-user-name
	   request-cliki cliki-data-directory
	   html-for-keyword
	   compute-index-for-page add-to-index-for-page make-index-for-page
	   search-term-relevance
	   edit-handler)
  (:use "NET.TELENT.DATE" "COMMON-LISP" "ARANEIDA" "SOCKETS"))

;;; language for inline searches and stuff
(defpackage "CLIKI-PAGES" (:use #| nil |# ))

;;; used for collection of markup routines
(defpackage "CLIKI-HTML")



