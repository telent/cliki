(in-package :cliki)

;;; need to put in check for simultaneous edits

(defun edit-page (request page title)
  (let* ((out (request-stream request))
	 (cookie (request-cookie request "username"))
	 (username (and cookie
			(urlstring-unescape cookie))))
    (if (< (length username) 1) (setf username nil))
    (request-send-headers request
			  :expires (get-universal-time)
			  :cache-control "no-cache")
    (cliki-page-header
     (request-cliki request) request (format nil "Edit ``~A''" title)
     '(((meta :name "ROBOTS" :content "noindex,nofollow"))))
    (format (request-stream request) "
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
<br>Please supply your name and a summary of changes for the Recent Changes page.  If you are making a minor alteration to a page you recently edited, you can avoid making another Recent Changes entry by leaving the Summary box blank
<br><b>Summary of changes:</b>
 <input type=text size=60 name=summary value=\"\">
<br><b>Your name:</b>
 <input type=text size=30 name=name value=~S>
<br><input type=checkbox ~A name=rememberme > <b>Check this box to fill in your name automatically next time</b> (uses a cookie)
<br><input type=submit value=Save name=Save></form></body></html>"
	    (or username "A N Other")
	    (if username "checked=checked" ""))))

(defun filename-for-title (cliki title)
  ;; XXX should generate a guaranteed unique filename (and still
  ;; preferably one that is vaguely like the page title)
  (remove #\. title))




