(in-package :cliki)

;;; view-page does everything including http header output (just in case
;;; it turns out to be a graphic or something)

(defmethod view-page ((page page-object) request)
  (let ((out (request-stream request))
        (file (page-pathname page))
        (title (page-title page)))
    (request-send-headers request)
    (format out
            "<html><head><title>Cliki : ~A</title></head>
<body><h1>~A</h1>~%" title title)
    (if (probe-file file)
        (with-open-file (in file :direction :input)
          (if (ignore-errors (peek-char nil in nil))
              (write-stream-to-stream (request-base-url request) in out))
          (format out "<hr><a href=\"~A\">Edit this page</a>"
                  (urlstring (page-edit-url page))))
      (format out
              "internal error: can't find file ~A"
              file))))

(defmethod make-edit-form ((page page-object) request)
  (let* ((file (page-pathname page))
         (title (page-title page))
         (out (request-stream request))
         (view-href (format nil "<a href=\"~A\">~A</a>"
                            (urlstring (page-view-url page)) title)))
    (request-send-headers request :expires (get-universal-time))
    (format (request-stream request)
            "<html><head><title>Cliki : Edit ``~A''</title></head>
<body><h1>Edit ``~A''</h1>
<form method=post>
<textarea name=text wrap=virtual rows=20 cols=80>" title view-href title)
    (if (probe-file file)
        (with-open-file (in file :direction :input)
          (if (ignore-errors (peek-char nil in nil))
              (araneida::copy-stream in out))))
    (format out "</textarea><br><input type=submit value=Save name=Save></form></body></html>")))

(defmethod make-edit-form ((page (eql 'phtml-page-object)) request)
  (let* ((title
          (pathname-name (parse-namestring
                          (urlstring-unescape (request-path-info request)))))
         (out (request-stream request)))
    (format (request-stream request)
            "<html><head><title>Cliki : Create ``~A''</title></head>
<body><h1>Create ``~A''</h1>
<form method=post>
<textarea name=text wrap=virtual rows=20 cols=80>" title title title)
    (format out "Describe _(~A) here</textarea><br><input type=submit value=Save name=Save></form></body></html>" title)))

(defmethod process-post-request ((page phtml-page-object) request)
  (let* ((file (page-pathname page))
         (title (page-title page))
         (out (request-stream request))
         (view-href (format nil "<a href=\"~A\">~A</a>"
                            (urlstring (page-view-url page)) title)))

(defmethod process-post-request ((page (eql 'phtml-page-object)) request)
  (let* ((title
          (pathname-name (parse-namestring
                          (urlstring-unescape (request-path-info request)))))
         (out (request-stream request)))
    (with-open-file
      (out-file (merge-pathnames
                 (make-pathname :name title
                                :type (type-for-class 'phtml-page-object))
                 (page-pathname-defaults (find-page "index")))
                :direction :output)
      (write-sequence (body-param "text" (request-body request)) out-file))
    (let ((page (make-page title
                           (merge-url (request-base-url request) "../")
                           (page-pathname-defaults (find-page "index")))))
      (request-send-headers request)
      (format out "Thanks for creating <a href=\"~A\">~A</a>"
              (urlstring (page-view-url page)) title))))




(defmethod search-relevance ((page phtml-page-object) search-criterion)
  ;; XXX implement
  1)

(defmethod delete-page ((page phtml-page-object))
  ;; XXX delete the file
  (call-next-method))

(defmethod (setf page-object-title) (title (page phtml-page-object))
  ;; XXX rename the file
  (call-next-method))
