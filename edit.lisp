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
    (format out "</textarea><br><input type=submit value=Save name=Save></form></body></html>")))

(defun save-page (request title root)
  (let* ((file (merge-pathnames title root))
         ;;(ndx-file (merge-pathnames (make-pathname :type "ndx") file))
         (out (request-stream request))
         (view-href (format nil "<a href=\"~A\">~A</a>"
                            (request-path-info request) title)))
    (handler-case 
     (with-open-file (out-file file :direction :output)
       (write-sequence (body-param "text" (request-body request)) out-file))
     ;;(with-open-file (out-ndx ndx-file :direction :output)
     ;; (prin1 (index-for (body-param "text" (request-body request))) out-ndx))
     (error (c) (request-send-error 500 "Unable to save file: ~A" c))
     (:no-error (c)
                (declare (ignorable c))
                (request-send-headers request)
                (format out "Thanks for editing ~A.  Don't forget to update the <a href=\"Recent+Changes\">Recent Changes</a> page to describe what you did.  Oh, and you probably need to `reload' or `refresh' to see your changes take effect" view-href)))
    (update-indexes-for-page title root)))

;;; This is fairly intolerant about the format of the file it reads.
(defun update-recent-changes (title root)
  (let* ((pathname (merge-pathnames "Recent Changes" root))
         (now-decoded (multiple-value-list (get-decoded-time)))
         (this-morning (encode-universal-time 0 0 0
                                              (elt now-decoded 3)
                                              (elt now-decoded 4)
                                              (elt now-decoded 5)
                                              (elt now-decoded 6)))
         (document
          (with-open-file (in pathname :direction :input)
            (copy-tree (net.html.parser:parse-html in))))
         (last-change (ext:parse-time
                       (second
                        (find-if (lambda (x) (and (consp x) (eql (car x) :h3)))
                                 document)))))
    (unless (>= last-change this-morning)
      (setf document
            `(,(car document)
              (:h3 ,(with-date this-morning
                      (format nil "~/cliki:dayname/, ~A ~/cliki:monthname/ ~A"
                              day date month year)))
              #.(format nil "~%")
              (:ul)
              #.(format nil "~%")
              ,@(cdr document))))
    (let ((ul (find-if (lambda (x) (and (consp x) (eql (car x) :ul))) document)))
      (setf (cdr ul)
            (cons (list :li (format nil "_(~A): something changed" title))
                  (cdr ul))))
    
    (with-open-file (out pathname :direction :output)
      (dolist (tree document)
        (araneida::html-stream out tree net.html.parser::*in-line*)))))

                           

