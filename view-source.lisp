(in-package :cliki)

(defun view-page-source (request page title)
  (let ((out (request-stream request)))
    (handler-case
     (with-open-file (in (page-pathname page) :direction :input)
       (request-send-headers request
                             :content-type "text/plain"
                             :expires (get-universal-time))
       (araneida::copy-stream in out)
       t)
      (error (e) ;; probably it just doesn't exist: not actually an error
	(request-send-error request 404 "Can't send file: ~A" e)))))
