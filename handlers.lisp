(in-package :cliki)

(defun request-title (request)
  (let* ((string (request-path-info request))
         (pos (position #\/ string :from-end t)))
    (urlstring-unescape
     (subseq string (if pos (1+ pos) 0)))))

(defun cliki-get-handler (request arg-string root)
  (let* ((action (url-query (request-url request)))
         (title (request-title request))
         (file (merge-pathnames title root))
         (out (request-stream request)))
    (cond
     ((not action)
      (view-page request title root))
     ((string-equal action "edit")
      (edit-page request title root))
     ;; can add in other ops like delete etc
     (t
      (request-send-error request 500 "Eh?")))))

(defun cliki-post-handler (request arg-string root)
  (let ((title  (request-title request)))
    (save-page request title root)))

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
  (export-handler base-url (list 'cliki-get-handler directory)
                  :method :get)
  (export-handler base-url (list 'cliki-get-handler directory)
                  :method :head)
  (export-handler base-url (list 'cliki-post-handler directory)
                  :method :post)
  (export-handler (merge-url base-url "admin/all-pages")
                  (list 'cliki-list-all-pages-handler base-url directory))
  (export-handler base-url (lambda (r rest) (request-redirect r "index"))
                  :method :get :match :exact))
