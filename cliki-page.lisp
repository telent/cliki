(in-package :cliki)

(defmethod page-pathname ((page cliki-page))
  (merge-pathnames (page-filename page)
		   (cliki-data-directory (page-cliki page))))

(defmethod page-tfidf ((page cliki-page))
  (let ((cliki (page-cliki page)))
    (loop for (term . frequency) in (page-tf page)
	  collect (cons term
			(* frequency (log (/ (cliki-number-of-documents cliki)
					     (cliki-idf cliki term))))))))

(defmethod page-url ((page cliki-page))
  (merge-url (urlstring-escape (page-title page))
	     (cliki-url-root (page-cliki page))))
	     

(defmethod print-object ((page cliki-page) stream)
  (print-unreadable-object (page stream :type t :identity t)
			   (princ (page-title page) stream)))