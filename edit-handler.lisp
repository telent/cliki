(in-package :cliki)

(defmethod handle-request-response ((handler edit-handler)
				    (method (eql :get))
				    request )
  (let* ((out (request-stream request))
	 (cliki (handler-cliki handler))
	 (username (cliki-user-name cliki (request-user request))))
    (multiple-value-bind (page title) (find-page-or-redirect cliki request)
      (if (< (length username) 1) (setf username nil))
      (request-send-headers request
			    :expires (get-universal-time)
			    :cache-control "no-cache")
      (cliki-page-header cliki request (format nil "Edit ``~A''" title)
			 '(((meta :name "ROBOTS"
			     :content "noindex,nofollow"))))
      (format out "
<form method=post>
<textarea wrap=virtual name=text rows=20 cols=80>~%")
      (if page
	  (with-open-file (in (page-pathname page) :direction :input)
	    ;; XXX copy-stream isn't good enough: we have to do something
	    ;; clever with bits of HTML (like, form elements) embedded in
	    ;; Lisp code, otherwise we leave the form forthwith
	    (araneida::copy-stream in out))
	  (format out "Describe _(~A) here~%" title))
      (format out "</textarea>
<br>Please supply ~Aa summary of changes for the Recent Changes page.  If you are making a minor alteration to a page you recently edited, you can avoid making another Recent Changes entry by leaving the Summary box blank
<br><b>Summary of changes:</b>
 <input type=text size=60 name=summary value=\"\">"
	      (if (request-user request) "" "your name and " ))
      (if (request-user request)
	  (format out "<br><b>Your name:</b> <tt>~A</tt>
<br><input type=submit value=Save name=Save></form></body></html>"
		  (cliki-user-name cliki (request-user request)))
	  (format out
		  "<br><b>Your name:</b>
 <input type=text size=30 name=name value=~S>
<br><input type=checkbox ~A name=rememberme > <b>Check this box to fill in your name automatically next time</b> (uses a cookie)
 <br><input type=submit value=Save name=Save></form></body></html>"
		  (or username "A N Other")
		  (if username "checked=checked" "")))
      t)))

 (defmethod handle-request-response ((handler edit-handler)
				     (method (eql :post))
				     request )
   (multiple-value-bind (page title)
       (find-page-or-redirect (handler-cliki handler) request)
     (unless page
       (setf page (make-instance 'cliki-page
				 :title title
				 :names (list title)
				 :filename
				 (filename-for-title (request-cliki request)
						     title)
				 :cliki (handler-cliki handler))))
     (let* ((filename (page-pathname page))
	    (title-filename (merge-pathnames
			     (make-pathname :type "titles")
			     (page-pathname page)))
	    (cliki (handler-cliki handler))
	    (out (request-stream request))
	    (body (request-body request))
	    (cookie nil)
	    (view-href (format nil "<a href=\"~A\">~A</a>"
			       (urlstring (merge-url
					   (cliki-url-root cliki)
					   (request-path-info request)))
			       title)))
       (unless (request-user request)
	 (if (body-param "rememberme" body)
	     (setf cookie (cliki-user-cookie cliki (request-user request))))
	 (if (and (not (body-param "rememberme" body))
		  (request-header request :Cookie))
	     ;; cookie previously set; seems reasonable that unticking the box
	     ;; should be interpreted as a request to clear it
	     (setf cookie (cliki-user-cookie cliki nil))))
       (handler-case
	   (progn
	    (with-open-file (out-file filename :direction :output)
	      (write-sequence (body-param "text" body) out-file))
	    (with-open-file (out-file title-filename :direction :output)
	      (with-standard-io-syntax
		(write (page-names page) :stream out-file))))
	(error (c) (request-send-error request 500 "Unable to save file: ~A" c))
	(:no-error (c)
	  (declare (ignorable c))
	  (add-recent-change cliki (get-universal-time) title
			     (cliki-user-name
			      cliki (request-user request))
			     (body-param "name" body)
			     (body-param "summary" body))
	  (request-send-headers request :set-cookie cookie)
	  (format out "Thanks for editing ~A.  You probably need to `reload' or `refresh' to see your changes take effect" view-href)))
      ;; XXX should do aliases here too
      ;; we could put this in an after handler, you know
      (setf (gethash (canonise-title title) (cliki-pages cliki)) page)
      (update-page-indices cliki page)
      (update-idf cliki)
      t)))
