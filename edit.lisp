(in-package :cliki)

;;; need to put in check for simultaneous edits

(defun edit-page (request title root)
  (let ((out (request-stream request))
        (view-href (format nil "<a href=\"~A\">~A</a>"
                           (request-path-info request) title)))
    (request-send-headers request :expires (get-universal-time))
    (format (request-stream request)
            "<html><head><title>Cliki : Edit ``~A''</title>
<meta name=\"ROBOTS\" content=\"noindex,nofollow\"></head>
<body><h1>Edit ``~A''</h1>
<form method=post>
<textarea wrap=virtual name=text rows=20 cols=80>~%"
            title view-href)
    (handler-case
     (with-open-file (in (merge-pathnames title root) :direction :input)
       ;; XXX copy-stream isn't good enough: we have to do something
       ;; clever with bits of HTML (like, form elements) embedded in
       ;; Lisp code, otherwise we leave the form forthwith
       (araneida::copy-stream in out))
     (file-error (e) ;; probably it just doesn't exist: not actually an error
                 (declare (ignore e))
                 (format out "Describe _(~A) here~%" title ))
     (error (e) (format out "Describe _(~A) here~%~A~%" title e)))
    (format out "</textarea>
<br><b>Summary of changes: <input type=text size=60 name=summary value=\"?\">
<br><b>Your name: <input type=text size=30 name=name value=\"A N Other\">
<br><input type=submit value=Save name=Save></form></body></html>")))

(defun save-page (request title root)
  (let* ((file (merge-pathnames title root))
         ;;(ndx-file (merge-pathnames (make-pathname :type "ndx") file))
         (out (request-stream request))
         (body (request-body request))
         (view-href (format nil "<a href=\"~A\">~A</a>"
                            (request-path-info request) title)))
    (handler-case 
     (with-open-file (out-file file :direction :output)
       (write-sequence (body-param "text" body) out-file))
     (error (c) (request-send-error 500 "Unable to save file: ~A" c))
     (:no-error (c)
                (declare (ignorable c))
                (add-recent-change root (get-universal-time) title
                                   (body-param "name" body)
                                   (body-param "summary" body))
                (request-send-headers request)
                (format out "Thanks for editing ~A.  You probably need to `reload' or `refresh' to see your changes take effect" view-href)))
    (update-indexes-for-page title root)))

