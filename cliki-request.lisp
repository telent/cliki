(in-package :cliki)

(defmethod find-page-or-redirect ((cliki cliki-instance)
				  (request request))
  (let* ((string (urlstring-unescape (request-path-info request)))
         (pos (position #\/ string :from-end t))
         (search-string (subseq string (if pos (1+ pos) 0)))
	 (query (url-query (request-url request)))
         (actual (find-page cliki
			    (if (> (length search-string) 0)
				search-string
				"index"))))
    (cond
      ((not actual)			; no page found
       (values nil search-string))
      ((string= search-string (page-title actual)) ;same name
       (values actual (page-title actual)))
      (t				;other name, redirect the browser
       (request-redirect request
			 (merge-url
			  (page-url actual)
			  (if query (format nil "?~A" query) "")))
       (signal 'response-sent)))))

;; user is some kind of object which represents a user.  If your cliki-instance
;; subclass makes it anything other than a string, it needs to provide 
;; methods for these
(defgeneric cliki-user-name (cliki user))
(defmethod cliki-user-name ((cliki cliki-instance) user)
  user)

(defgeneric cliki-user-cookie (cliki user))
(defmethod cliki-user-cookie ((cliki cliki-instance) user)
  (format nil "username=~A; path=~A; expires=~A; domain=~A"
	  (urlstring-escape (user-name user))
	  (url-path (cliki-url-root cliki))
	  "Sun, 01-Jun-2036 00:00:01 GMT"
	  (url-host (cliki-url-root cliki))))

(defmethod cliki-user-cookie ((cliki cliki-instance) (user (eql nil)))
  (format nil "username=; path=~A; expires=~A; domain=~A"
	  (url-path (cliki-url-root cliki))
	  "Mon, 32-Jul-2001 00:00:01 GMT"
	  (url-host (cliki-url-root cliki))))
