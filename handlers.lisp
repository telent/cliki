(in-package :cliki)

(defun cliki-get-handler (request arg-string root)
  (let* ((action (url-query (request-url request)))
         (title (urlstring-unescape (request-path-info request)))
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
  (let ((title (urlstring-unescape (request-path-info request))))
    (save-page request title root)))

(defun export-handlers (base-url directory)
  (export-handler base-url (list 'cliki-get-handler directory)
                  :method :get)
  (export-handler base-url (list 'cliki-get-handler directory)
                  :method :head)
  (export-handler base-url (list 'cliki-post-handler directory)
                  :method :post)
  (export-handler base-url (lambda (r rest) (request-redirect r "index"))
                  :method :get :match :exact))
