(in-package :cliki)

(defmethod find-page-or-redirect ((request cliki-request))
  (let* ((string (urlstring-unescape (request-path-info request)))
         (pos (position #\/ string :from-end t))
         (search-string (subseq string (if pos (1+ pos) 0)))
	 (cliki (request-cliki request))
         (actual (find-page cliki search-string)))
    (cond
      ((not actual)			; no page found
       (values nil search-string))
      ((string= search-string (page-title actual)) ;same name
       (values actual (page-title actual)))
      (t				;other name, redirect the browser
       (request-redirect request (page-url actual))
       (values :redirected (page-title actual))))))

