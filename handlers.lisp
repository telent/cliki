(in-package :cliki)

(defun cliki-edit-get-handler (request arg-string)
  (let ((page (find-page arg-string)))
    (make-edit-form page request)))

(defun cliki-edit-post-handler (request arg-string)
  (let ((page (find-page arg-string)))
    (process-post-request page request)))

(defun cliki-get-handler (request arg-string)
  (let ((page (find-page arg-string)))
    (if page
        (view-page page request)
      (view-create-page-menu request arg-string))))

(defun export-handlers (base-url directory)
  (export-handler base-url 'cliki-get-handler)
  (export-handler (merge-url base-url "edit/") 'cliki-edit-get-handler
                  :method :get)
  (export-handler (merge-url base-url "edit/") 'cliki-edit-get-handler
                  :method :head)
  (export-handler (merge-url base-url "edit/") 'cliki-edit-post-handler
                  :method :post)
  (mapc
   (lambda (filename)
     (make-page (pathname-name filename) base-url directory))
   (directory directory)))

