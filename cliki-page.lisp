(in-package :cliki)

(defmethod page-index ((page cliki-page) index)
  (cdr (assoc index (page-indices page))))

(defmethod page-pathname ((page cliki-page) &key (version :newest))
  (let ((pathname (slot-value page 'pathname)))
    (if pathname
	(let ((newest (car (page-versions page))))
	  (when (eql version :newest) (setf version newest))
	  (if (zerop version)
	      pathname
	      (make-pathname :type (princ-to-string version)
			     :defaults pathname)))
	nil)))

(defmethod (setf page-pathname) (value (page cliki-page))
  (setf (slot-value page 'pathname) value))

;;; page last-modified times
;;; (1) a slot in cliki-page stores the last time that it was updated
;;; from within cliki 
;;; - P is edited => touch P
;;; - P links to P' and P' is created => touch P
;;; - P formerly linked to P' and now doesn't => touch P'
;;; (2) if the page slot is out of date wrt the filesystem, update it

(defmethod page-last-modified ((page cliki-page))
  (let ((lm (slot-value page 'last-modified))
	(d (aif (page-pathname page) (file-write-date it) 0)))
    (max lm d)))

(defmethod touch-page ((page cliki-page))
  (setf (slot-value page 'last-modified) (get-universal-time)))


    
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
