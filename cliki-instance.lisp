(in-package :cliki)

(defun canonise-title (title)
  "Return the key for the pages hash for the document with title TITLE"
  (string-downcase  
   (substitute #\Space #\_
	       (urlstring-unescape (remove #\% title)))))

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
  (setf (cliki-data-directory cliki) (pathname (cliki-data-directory cliki)))
  (let ((handlers (cliki-handlers cliki))
	(base-url (cliki-url-root cliki))
	(files (remove-if-not
		#'pathname-name
		(directory
		 (merge-pathnames "*.titles"
				  (cliki-data-directory cliki))))))
    (dolist (f files)
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
    (export-handler base-url 'cliki-get-handler
		    :method :get :stage handlers)
    (export-handler base-url
		    (lambda (r rest)
		      (declare (ignore rest))
		      (request-redirect
		       r (merge-url (request-url r) "index")))
		    :match :exact :method :get :stage handlers)
    (export-handler base-url 'cliki-head-handler
		    :method :head :stage handlers)
    (export-handler base-url 'cliki-post-handler 
		    :method :post :stage handlers)
    (export-handler (merge-url base-url "admin/all-pages")
		    'cliki-list-all-pages-handler :stage handlers)
    (export-handler (merge-url base-url "admin/cliki.css")
		    'css-file-handler  :stage handlers)
    (export-handler (merge-url base-url "admin/search")
		    'cliki-search-handler :stage handlers)
    (export-handler (merge-url base-url "Recent%20Changes")
		    `(view-recent-changes) :stage handlers)
    (export-handler (merge-url base-url "recent-changes.rdf")
		    `(rdf-recent-changes) :stage handlers)
    (export-handler (merge-url base-url "recent-changes.sexp")
		    `(sexp-recent-changes) :stage handlers)
    (export-handler (merge-url base-url "Recent+Changes")
		    `(view-recent-changes) :stage handlers)
    (setf (cliki-handlers cliki) handlers)))

