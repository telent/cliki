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
  (merge-url (cliki-url-root (page-cliki page))
	     (urlstring-escape (page-title page))))

(defmethod print-object ((page cliki-page) stream)
  (print-unreadable-object (page stream :type t :identity t)
			   (princ (page-title page) stream)))

;;; it would be good to find a more appropriate file for this one
(defun scan-stream (chars stream dispatch)
  (let ((c1 (read-char stream)))
    (dolist (possible chars)
      (when (eql c1 (car possible))
	(if (listp (cadr possible))
	    (let ((r (scan-stream (cadr possible) stream dispatch)))
	      (return-from scan-stream (if (stringp (car r)) r (cons c1 r))))
	    (let ((c2 (peek-char nil stream)))
	      (return-from scan-stream 
		(if (eql c2 #\( )
		    (list
		     (funcall dispatch (cadr possible) 
			      (read-matched-parens stream)))
		    (list c1)))))))
    (list c1)))

