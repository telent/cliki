(in-package :cliki)

(defmethod form-element-for-keyword ((cliki cliki-view) n keyword &rest args)
  (declare (ignore args))
  nil)

(defmethod parse-form-element-for-keyword
    ((cliki cliki-view) request keyword prefix)
  (format *trace-output* "~A => ~A~%"
	  keyword (body-params (format nil "Eb~A" prefix)
			       (request-body request)))
  "dummy")

(defmethod parse-form-element-for-keyword
    ((cliki cliki-view) request (keyword (eql :body)) prefix)
  (body-param (format nil "E~A" prefix) (request-body request)))

(defun write-page-form-to-stream (cliki in-stream stream)
  "Read the file for PAGE and write to STREAM, substituting weird markup language elements as we go. "  
  (let ((element-num 0)
	(acc-stream (make-string-output-stream)))
    (labels ((dispatch (token arg)
	       (let* ((l (long-form-for token arg))
		      (s (apply #'form-element-for-keyword
				cliki  element-num l)))
		 (cond
		   (s (end-text)
		      (format stream "<INPUT type=hidden name=T~A value=~A>~%"
			      element-num (car l))
		      (write-sequence (apply #'form-element-for-keyword
					     cliki  element-num l)
				      stream)
		      (incf element-num))
		   (t (let* ((*package* (find-package :keyword))
			     (short (find token (cliki-short-forms cliki)
					  :key #'cadr)))
			(if short
			    (format acc-stream "~A(~A)"
				    (car short) (strip-outer-parens arg))
			    (format acc-stream ":(~A ~S)"
				    token (strip-outer-parens arg))))))))
	     (end-text ()
	       (let* ((buf (get-output-stream-string acc-stream))
		      (cr (count #\Newline buf)))
		 (when (> (count-if #'graphic-char-p buf) 0)
		   (format stream "<INPUT type=hidden name=T~A value=BODY>~%"
			   element-num)
		   (format stream "<TEXTAREA rows=~A cols=80 name=E~A>~%"
			   (min 15 (floor (* cr 1.5))) element-num)
		   (write-sequence buf stream)
		   (format stream "</TEXTAREA>~%")
		   (incf element-num) )))
	     (output (c) (write-char c acc-stream)))
      (scan-stream (cliki-short-forms cliki)
		   in-stream
		   #'output #'dispatch)
      (end-text)
      (close acc-stream))))

(defmethod handle-request-response ((handler edit-handler)
				    (method (eql :get))
				    request )
  (let* ((cliki (handler-cliki handler))
	 (auth-username (cliki-user-name cliki (request-user request)))
	 (unauth-username (urlstring-unescape (request-cookie request "username"))))
    (multiple-value-bind (page title) (find-page-or-redirect cliki request)
      (if (< (length unauth-username) 1) (setf unauth-username nil))
      (request-send-headers request
			    :expires (get-universal-time)
			    :cache-control "no-cache")
      (with-page-surround (cliki request (format nil "Edit ``~A''" title)
				 '(((meta :name "ROBOTS"
				     :content "noindex,nofollow"))))
	(format cliki::out "
 <form method=post>
 <!-- textarea wrap=virtual name=text rows=20 cols=80 -->~%")
	(let ((default (format nil "<input type=hidden name=T0 value=BODY>
<textarea wrap=virtual name=E0 rows=20 cols=80>
Describe _(~A) here
</textarea>" title)))
	  (if page
	      (with-open-file (in-stream (page-pathname page) :direction :input)
		(if (zerop (file-length in-stream))
		    (write-sequence default out)
		    (write-page-form-to-stream cliki in-stream out)))
	      (write-sequence default out)))
	(format out "<!-- /textarea-->
 <br>Please supply ~Aa summary of changes for the Recent Changes page.  If you are making a minor alteration to a page you recently edited, you can avoid making another Recent Changes entry by leaving the Summary box blank
 <br><b>Summary of changes:</b>
  <input type=text size=60 name=summary value=\"\">"
		(if (request-user request) "" "your name and " ))
	(if (request-user request)
	    (format out "<br><b>Your name:</b> <tt>~A</tt>
 <br><input type=submit value=Save name=Save></form></body></html>"
		    auth-username)
	    (format out
		    "<br><b>Your name:</b>
  <input type=text size=30 name=name value=~S>
 <br><input type=checkbox ~A name=rememberme > <b>Check this box to fill in your name automatically next time</b> (uses a cookie)
  <br><input type=submit value=Save name=Save></form></body></html>"
		    (or unauth-username "A N Other")
		    (if unauth-username "checked=checked" ""))))
      t)))


(defun save-stream (cliki request pathname)
  (with-open-file (out-file pathname :direction :output)
    (let ((body (request-body request)))
      (loop for (name value) in body
	    for el =
	    (and (eql (elt name 0) #\T) (digit-char-p (elt name 1))
		 (parse-form-element-for-keyword
		  cliki request (intern value :keyword)
		  (parse-integer name :start 1 :junk-allowed t)))
	    when (typep el 'cons)
	    do (with-standard-io-syntax  (format out-file "~&:~S" el))
	    end
	    when (typep el 'string)
	    do (write-sequence el out-file)))))

(defmethod save-page ((cliki cliki-instance) request &optional title)
  (multiple-value-bind (page titl)
      (find-page-or-redirect cliki request)
    (labels ((title-file (n) (merge-pathnames
			      (make-pathname :type "titles") n)))
      (let* ((title (or title titl))
	     (*default-pathname-defaults* (cliki-data-directory cliki))
	     (filename (merge-pathnames (escape-for-filename title)))
	     (page
	      (or page
		  (make-instance 'cliki-page
				 :title title :names (list title)
				 :pathname filename
				 :cliki cliki)))
	     (old-filename (merge-pathnames (page-pathname page)))
	     (body (request-body request))
	     (username (cliki-user-name
			cliki (or (request-user request)
				  (body-param "name" body))))
	     (title-filename (title-file filename)))
	#+nil
	(when (string= username "A N Other")
	  (request-send-error request 403 "Anonymous posting is disabled: please use a real name (yours, for preference)")
	  (return-from save-page nil))
	(save-stream cliki request filename)
	(with-open-file (out-file title-filename :direction :output)
	  (with-standard-io-syntax
	    (write (page-names page) :stream out-file)))
	;; in cliki 0.4.1 we changed the mapping from title => pathname
	;; to cope with characters like # . / % etc.  Old filenames
	;; continue to work, but when pages are saved they'll use the
	;; new name.  To avoid needing to migrate all repositories at
	;; once, we do it lazily on save-page
	(unless (string= (namestring filename) (namestring old-filename))
	  (let ((names (list old-filename (title-file old-filename))))
	    (setf (page-pathname page) filename)
	    (dolist (f names)
	      (when (probe-file f)
		(format *terminal-io* "Deleting ~S ~%" f)
		(delete-file f))) ))
	(add-recent-change cliki (get-universal-time) title
			   username
			   (body-param "summary" body))
	(setf (gethash (canonise-title title) (cliki-pages cliki)) page)
	(update-page-indices cliki page)
	(update-idf cliki)
	title))))

(defmethod handle-request-response ((handler edit-handler)
				    (method (eql :post))
				    request )
  (let* ((cliki (handler-cliki handler))
	 title
	 (out (request-stream request))
	 (body (request-body request))
	 (cookie nil))
    (unless (request-user request)
      (if (body-param "rememberme" body)
	  (setf cookie (cliki-user-cookie
			cliki (body-param "name" body))))
      (if (and (not (body-param "rememberme" body))
	       (request-cookie request "username"))
	  ;; cookie previously set; seems reasonable that unticking the box
	  ;; should be interpreted as a request to clear it
	  (setf cookie (cliki-user-cookie cliki nil))))
    (handler-case
	(setf title (save-page cliki request))
      (error (c)
	(request-send-error request 500 "Unable to save file: ~A" c))
      (:no-error (c)
	(declare (ignorable c))
	(request-send-headers request :set-cookie cookie)
	(format out "Thanks for editing ~A.  You probably need to `reload' or `refresh' to see your changes take effect"
		(format nil "<a href=\"~A\">~A</a>"
			(urlstring (merge-url
				    (cliki-url-root cliki)
				    (request-path-info request)))
			title))))
    t))
