(in-package :cliki)

(defun find-page-name (title root)
  (labels ((match-p (a b)
                    (string-equal (substitute #\_ #\Space a)
                                  (substitute #\_ #\Space b))))
    (let* ((candidates (mapcar #'pathname-name (directory root))))
      (find title candidates :test #'match-p))))

(defun request-title (request root)
  (let* ((string (urlstring-unescape (request-path-info request)))
         (pos (position #\/ string :from-end t))
         (search-string (subseq string (if pos (1+ pos) 0)))
         (actual (find-page-name search-string root)))
    (when (string= search-string actual)
      (return-from request-title actual))
    (request-redirect request
                      (merge-url (request-url request) (urlstring-escape actual)))
    nil))

(defun cliki-get-handler (request arg-string root)
  (let ((title (request-title request root)))
    (unless title (return-from cliki-get-handler))
    (let ((action (url-query (request-url request)))
          (file (merge-pathnames title root))
          (out (request-stream request)))
      (cond
       ((not action)
        (view-page request title root))
       ((string-equal action "source")
        (view-page-source request title root))
       ((string-equal action "edit")
        (edit-page request title root))
       ;; can add in other ops like delete etc
       (t
        (request-send-error request 500 "Eh?"))))))

(defun cliki-head-handler (request arg-string root)
  (let ((title (request-title request root)))
    (unless title (return-from cliki-head-handler))
    (let* ((file (merge-pathnames title root))
           (date (file-write-date file)))
      (if date
          (request-send-headers request :last-modified date)
        (request-send-headers request :response-code 404
                              :response-text "Not found")))))

(defun cliki-post-handler (request arg-string root)
  (let ((title  (request-title request root)))
    (if title (save-page request title root))))

(defun cliki-list-all-pages-handler (request arg-string url-root fs-root)
  (let ((pages (mapcar #'pathname-name (directory fs-root :sort t))))
    (request-send-headers request)
    (write-sequence
     (html
      `(html
        (head (title "Cliki: All pages"))
        (body
         (h1 "All pages")
         (ul
          ,@(mapcar
             (lambda (x)
               `(li ((a :href
                        ,(urlstring (merge-url url-root (urlstring-escape x))))
                     ,x)))
                pages)))))
     (request-stream request))))


(defun export-handlers (base-url directory)
  (create-indexes directory)
  (export-handler base-url (list 'cliki-get-handler directory)
                  :method :get)
  (export-handler base-url (list 'cliki-head-handler directory)
                  :method :head)
  (export-handler base-url (list 'cliki-post-handler directory)
                  :method :post)
  (export-handler (merge-url base-url "admin/all-pages")
                  (list 'cliki-list-all-pages-handler base-url directory))
  (export-handler base-url (lambda (r rest) (request-redirect r "index"))
                  :method :get :match :exact))

