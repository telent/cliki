(in-package :cliki)

;;; copy-stream isn't good enough: we have to do something clever with
;;; bits of HTML (like, form elements) embedded in Lisp code, otherwise
;;; we leave the form forthwith

;;; need to put in check for simultaneous edits, too.


(defun cliki-edit-handler (request arg-string root)
  (let* ((file (merge-pathnames arg-string root))
         (out (request-stream request))
         (view-href (format nil "<a href=\"../~A\">~A</a>"
                            arg-string arg-string))
         (method (request-method request)))
    (case method
      ((:head :get)
       (request-send-headers request)
       (format (request-stream request)
               "<html><head><title>Cliki : Edit ~A</title></head>
<body><h1>Edit ~A</h1>
<form method=post>
<textarea name=text rows=10 cols=60>" arg-string view-href arg-string)
       (if (probe-file file)
           (with-open-file (in file :direction :input)
             (if (ignore-errors (peek-char nil in nil))
                 (araneida::copy-stream in out))))
       (format out "</textarea><br><input type=submit value=Save name=Save></form></body></html>"))
      ((:post)
       (with-open-file (out-file file :direction :output)
         (write-sequence (body-param "text" (request-body request)) out-file))
       (request-send-headers request)
       (format out "Thanks for editing ~A" view-href)))))
