(in-package :cliki)

(defun canonise-title (title)
  "Return the key for the pages hash for the document with title TITLE"
  (let* ((dot-html-p (string-equal title ".html" :start1
				   (max 0 (- (length title) 5))))
	 (title (if dot-html-p (subseq title 0 (- (length title) 5))
		    title)))
    (string-downcase  
     (substitute #\Space #\_
		 (urlstring-unescape (remove #\% title))))))

(defmethod find-page ((cliki cliki-instance) title)
  (gethash (canonise-title title) (cliki-pages cliki)))

(defmethod cliki-idf ((cliki cliki-instance) term)
  (gethash term (slot-value cliki 'idf)))

;;; XXX this doesnt give an entirely correct answer because it counts
;;; aliases twice
(defmethod cliki-number-of-documents ((cliki cliki-instance))
  (hash-table-count (cliki-pages cliki)))

(defun update-idf (cliki)
  "Update idf, using page-tf for each page and summing stuff.  page-tf
is set by update-page-indices (at startup and after edits).  "
  (let ((idf (make-hash-table :test 'equal)))
    (loop for document being the hash-values of (cliki-pages cliki)
	  do
	  (let* ((frequencies (page-index document :tf))
		 (terms (mapcar #'car frequencies)))
	    (dolist (term terms)
	      (let ((x (gethash term idf)))
		(setf (gethash term idf) (1+ (or x 0)))))))
    (setf (slot-value cliki 'idf) idf)))

(defmethod shared-initialize
    :after ((cliki cliki-instance) slot-names &rest initargs)
    (declare (ignorable initargs))
  (setf (cliki-data-directory cliki) (pathname (cliki-data-directory cliki)))
  (let ((files (remove-if-not
		#'pathname-name
		(directory
		 (merge-pathnames "*.titles"
				  (cliki-data-directory cliki))))))
    (dolist (f files) ;(subseq files 0 14))
      (let* ((titles (with-open-file (i f :direction :input) (read i)))
	     (*default-pathname-defaults* (cliki-data-directory cliki))
	     (p (make-instance 'cliki-page
			       :title (car titles)
			       :names titles
			       :filename
			       (make-pathname :name (pathname-name f)) ;ew
			       :cliki cliki)))
	(format t "Adding page ~D~%" p) 
	(dolist (title titles)
	  (setf (gethash (canonise-title title) (cliki-pages cliki))
		p))))
    (loop for page being the hash-values of (cliki-pages cliki)
	  do (update-page-indices cliki page)) 
    (update-idf cliki)
    (restore-recent-changes cliki)
    (let ((edit-handler (make-instance 'edit-handler :cliki cliki)))
      (install-handler cliki edit-handler "edit/" nil))

    (install-handler cliki 'cliki-list-all-pages-handler "admin/all-pages" t)
    (install-handler cliki
		     (lambda (request)
		       (request-send-headers request
					     :content-type "text/css")
		       (cliki-css-text cliki (request-stream request))
		       t)
		     "admin/cliki.css" t)


    (install-handler cliki 'cliki-search-handler "admin/search" nil)
    (install-handler cliki `(view-recent-changes) "Recent%20Changes" nil)
    (install-handler cliki `(rdf-recent-changes) "recent-changes.rdf" t)
    (install-handler cliki `(sexp-recent-changes) "recent-changes.sexp" t)
    (install-handler cliki `(view-recent-changes) "Recent+Changes" nil)
  ))

(defmethod render-html (request
			(cliki cliki-instance) (handler araneida:handler)
			html-tree )
  (html-stream (request-stream request) html-tree))

(defmethod handle-request-authentication ((handler cliki-instance)
					  method request)
  (setf (request-user request) (request-cookie request "auth-username")))

(defmethod request-cliki ((request request))
  (labels ((find-handler (handlers)
	     (cond ((null handlers) nil)
		   ((typep (caar handlers) 'cliki-instance)
		    (caar handlers))
		   (t (find-handler (cdr handlers))))))
    (find-handler (request-handled-by request))))
		    

(defmethod handle-request-response ((handler cliki-instance)
				    (method (eql :head))
				    request )
  (or
   (call-next-method)
   (multiple-value-bind (page title) (find-page-or-redirect handler request)
     (cond
       (page
	(request-send-headers request :last-modified 
			      (file-write-date (page-pathname page)))
	t)
       (t nil)))))
   
(defmethod handle-request-response ((handler cliki-instance)
				    (method (eql :get))
				    request )
  (or
   (call-next-method)
   (multiple-value-bind (page title) (find-page-or-redirect handler request)
     (let ((action (url-query (request-url request))))
       (cond
	 ((not action)
	  (view-page handler request page title))
	 ((string-equal action "source")
	  (view-page-source request page title))
	 ((string-equal action "download")
	  (request-redirect
	   request (merge-url (request-url request)
			      (caar (page-index page  :download)))))
	 (t
	  (request-send-error request 500 "Eh?")))))))
