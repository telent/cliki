(in-package :cliki)

(defclass phtml-page-object (page-object))

;;; lots of stuff that this code depends on is defined in view.lisp

;;; view-page does everything including http header output (just in case
;;; it turns out to be a graphic or something)

(defmethod view-page ((page phtml-page-object) request)
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

(defmethod make-edit-form ((page phtml-page-object) request)
  (let* ((file (page-pathname page))
         (title (page-title page))
         (out (request-stream request))
         (view-href (format nil "<a href=\"~A\">~A</a>"
                            (urlstring (page-view-url page)) title)))
    (request-send-headers request)
    (format (request-stream request)
            "<html><head><title>Cliki : Edit ~A</title></head>
<body><h1>Edit ~A</h1>
<form method=post>
<textarea name=text rows=10 cols=60>" title view-href title)
    (if (probe-file file)
        (with-open-file (in file :direction :input)
          (if (ignore-errors (peek-char nil in nil))
              (araneida::copy-stream in out))))
    (format out "</textarea><br><input type=submit value=Save name=Save></form></body></html>")))

(defmethod process-post-request ((page phtml-page-object) request)
  (let* ((file (page-pathname page))
         (title (page-title page))
         (out (request-stream request))
         (view-href (format nil "<a href=\"~A\">~A</a>"
                            (urlstring (page-view-url page)) title)))
    (with-open-file (out-file file :direction :output)
      (write-sequence (body-param "text" (request-body request)) out-file))
    ;; XXX and should reparse the file to get categories
    (request-send-headers request)
    (format out "Thanks for editing ~A" view-href)))))

(defmethod search-relevance ((page phtml-page-object) search-criterion)
  ;; XXX implement
  1)

(defmethod delete-page ((page phtml-page-object))
  ;; XXX delete the file
  (call-next-method))

(defmethod (setf page-object-title) (title (page phtml-page-object))
  ;; XXX rename the file
  (call-next-method))
