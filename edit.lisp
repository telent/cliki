(in-package :cliki)

;;; need to put in check for simultaneous edits

(defun edit-page (request title root)
  (declare (optimize (debug 3)))
  (let* ((out (request-stream request))
	 (cookie (car (request-header request :cookie)))
	 (username (and cookie
			(let ((p (position #\= cookie)))
			  (and p
			       (string-trim "\"" (subseq cookie (1+ p))))))))
    (if (< (length username) 1) (setf username nil))
    (request-send-headers request :expires (get-universal-time))
    (send-cliki-page-preamble
     request (format nil "Edit ``~A''" title)
     "<meta name=\"ROBOTS\" content=\"noindex,nofollow\">")
    (format (request-stream request) "
<form method=post>
<textarea wrap=virtual name=text rows=20 cols=80>~%")
    (with-open-file (in (merge-pathnames title root) :direction :input
			:if-does-not-exist nil)
	;; XXX copy-stream isn't good enough: we have to do something
	;; clever with bits of HTML (like, form elements) embedded in
	;; Lisp code, otherwise we leave the form forthwith
	(if in (araneida::copy-stream in out)
	    (format out "Describe _(~A) here~%" title)))
    (format out "</textarea>
<br>Please supply your name and a summary of changes for the Recent Changes page.  If you are making a minor alteration to a page you recently edited, you can avoid making another Recent Changes entry by leaving the Summary box blank
<br><b>Summary of changes:</b>
 <input type=text size=60 name=summary value=\"\">
<br><b>Your name:</b>
 <input type=text size=30 name=name value=~S>
<br><input type=checkbox ~A name=rememberme > <b>Check this box to fill in your name automatically next time</b> (uses a cookie)
<br><input type=submit value=Save name=Save></form></body></html>"
	    (or username "A N Other")
	    (if username "checked=checked" ""))))



(defun save-page (request title root)
  (let* ((file (merge-pathnames title root))
         (out (request-stream request))
         (body (request-body request))
	 (cookie nil)
         (view-href (format nil "<a href=\"~A\">~A</a>"
                            (request-path-info request) title)))
    (if (body-param "rememberme" body)
	(setf cookie
	      (format nil "username=~s; path=~A; expires=~A; domain=~A"
		      (body-param "name" body)
		      (url-path (cliki-request-url-root request))
		      "Sun, 01-Jun-2036 00:00:01 GMT"
		      (url-host (cliki-request-url-root request)))))
    
    (if (and (not (body-param "rememberme" body))
	     (request-header request :Cookie))
	;; cookie previously set; seems reasonable that unticking the box
	;; should be interpreted as a request to clear it
	(setf cookie
	      (format nil "username=\"\"; path=~A; expires=~A; domain=~A"
		      (url-path (cliki-request-url-root request))
		      "Mon, 32-Jul-2001 00:00:01 GMT"
		      (url-host (cliki-request-url-root request)))))
    (handler-case 
	(with-open-file (out-file file :direction :output)
	    (write-sequence (body-param "text" body) out-file))
      (error (c) (request-send-error 500 "Unable to save file: ~A" c))
      (:no-error (c)
	(declare (ignorable c))
	(add-recent-change root (get-universal-time) title
			   (body-param "name" body)
			   (body-param "summary" body))
	(request-send-headers request :set-cookie cookie)
	(format out "Thanks for editing ~A.  You probably need to `reload' or `refresh' to see your changes take effect" view-href)))
    (update-indexes-for-page title root)))

