(in-package :CL-USER)
(defpackage "CLIKI"
  (:export cliki-view-handler cliki-edit-handler cliki-instance cliki-url-root
	   cliki-css-text cliki-page-header cliki-page-footer cliki-user-name
	   write-a-href request-cliki cliki-data-directory
	   html-for-keyword form-element-for-keyword
	   parse-form-element-for-keyword
	   request-cliki cliki-default-page-name
	   cliki-page find-page page-pathname
	   compute-index-for-page add-to-index-for-page make-index-for-page
	   search-term-relevance search-term-summary
	   edit-handler
	   
	   elided-stream)
	   
  (:import-from #+sbcl "SB-GRAY" #+cmu "EXT"
                "FUNDAMENTAL-CHARACTER-OUTPUT-STREAM")
  (:use "NET.TELENT.DATE" "COMMON-LISP" "SB-GRAY" "ARANEIDA" "SOCKETS"))

;;; language for inline searches and stuff
(defpackage "CLIKI-PAGES" (:use #| nil |# ))

;;; used for collection of markup routines
(defpackage "CLIKI-HTML")



