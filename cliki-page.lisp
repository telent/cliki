(in-package :cliki)

(defmethod page-index ((page cliki-page) index)
  (cdr (assoc index (page-indices page))))


(defmethod (setf page-index) (new-value (page cliki-page) index)
  (setf (cdr (assoc index (page-indices page))) new-value))

(defmethod page-tfidf ((page cliki-page))
  (let ((cliki (page-cliki page)))
    (loop for (term . frequency) in (page-index page :tf)
	  collect (cons term
			(* frequency (log (/ (cliki-number-of-documents cliki)
					     (cliki-idf cliki term))))))))

(defmethod page-url ((cliki cliki-view) (page cliki-page))
  (merge-url (cliki-url-root cliki)
	     (urlstring-escape (page-title page))))

(defmethod page-summary ((cliki cliki-view) (page cliki-page) search-term)
  (loop for i in (apply #'search-term-summary cliki page search-term)
	append `(,i (br))))

(defmethod print-object ((page cliki-page) stream)
  (print-unreadable-object (page stream :type t :identity t)
			   (princ (page-title page) stream)))

(defun escape-for-filename (title)
  (with-output-to-string (o)
    (labels ((maybe-escape (char)
	       (cond ((alphanumericp char) (princ char o))
		     ((member char '(#\, #\! #\-)) (princ char o))
		     (t (format o "=~X" (char-code char))))))
      (map 'nil #'maybe-escape title))))

(defun scan-stream (chars stream output dispatch)
  (labels ((find-token (chars stream dispatch)
	     (let* ((c1 (read-char stream)))
	       (cons c1
		     (dolist (possible chars)
		       (when (eql c1 (car possible))
			 (return
			   (if (listp (cadr possible))
			       (find-token
				(cadr possible) stream dispatch)
			       (let ((c2 (peek-char nil stream nil)))
				 (when (eql c2 #\( )
				   (funcall dispatch (cadr possible) 
					    (read-matched-parens stream))
				   (throw 'matched nil)))))))))))
    (handler-case
	(loop
	 (catch 'matched
	   (dolist (i (find-token chars stream dispatch))
	     (funcall output i))))
      (end-of-file (c) (declare (ignore c)) nil))))



