(in-package :cliki)

(defun canonise-title (title)
  "Return the key for the pages hash for the document with title TITLE"
  (if (zerop (length title))
      nil
      (nstring-downcase (substitute #\Space #\_ title))))

(defmethod find-page ((cliki cliki-view) title)
  (gethash (or (canonise-title title)
	       (string-downcase (cliki-default-page-name cliki)))
	   (cliki-pages cliki)))

(defun name-for-url (url)
  (let* ((path (url-path url))
	 (slash (position #\/ path :from-end t))
	 (dothtml (search ".html" path :from-end t)))
    (urlstring-unescape (subseq path (if slash (1+ slash) 0) dothtml))))

#||
* (name-for-url (parse-urlstring "http://www.foo.com/blah/bAng%20banG.html"))
"bAng banG"
* (name-for-url (parse-urlstring "http://www.foo.com/blah/bAng%20banGhtml"))
"bAng banGhtml"
* (name-for-url (parse-urlstring "http://www.foo.com/blah/bAn.g%20banGhtml"))
"bAn.g banGhtml"
* (name-for-url (parse-urlstring "http://www.foo.com/blah/bAn.g%20banG.html"))
"bAn.g banG"
||# 

(defmethod cliki-idf ((cliki cliki-instance) term)
  (gethash term (slot-value cliki 'idf)))

;;; XXX this doesnt give an entirely correct answer because it counts
;;; aliases twice
(defmethod cliki-number-of-documents ((cliki cliki-view))
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
			       ;; f is foo.titles: lose pathname-type
			       :pathname 
			       (merge-pathnames (make-pathname
						 :name (pathname-name f)))
			       :cliki cliki)))
	(dolist (title titles)
	  (setf (gethash (canonise-title title) (cliki-pages cliki))
		p))))
    (loop for page being the hash-values of (cliki-pages cliki)
	  do
	  (progn
	    (format t "Indexing page ~D~%" page)
	    (update-page-indices cliki page)))
    (update-idf cliki)
    (restore-recent-changes cliki)))

(defmethod shared-initialize
    :after ((cliki cliki-view) slot-names &rest initargs)
  (declare (ignorable initargs))
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

  (install-handler cliki
		   (lambda (request)
		     (request-send-error request 404 "not found"))
		   "favicon.ico" nil)
  (install-handler cliki 'cliki-search-handler "admin/search" nil)
  (install-handler cliki `(view-recent-changes) "Recent%20Changes" nil)
  (install-handler cliki `(rdf-recent-changes) "recent-changes.rdf" t)
  (install-handler cliki `(sexp-recent-changes) "recent-changes.sexp" t)
  (install-handler cliki `(view-recent-changes) "Recent+Changes" nil))
  

#+nil
(defmethod render-html (request
			(cliki cliki-instance) (handler araneida:handler)
			html-tree )
  (html-stream (request-stream request) html-tree))

;; XXX is this reasonable?  I don't think so, really
(defmethod handle-request-authentication ((handler cliki-view)
					  method request)
  (setf (request-user request) (request-cookie request "auth-username")))

(defmethod request-cliki ((request request))
  (labels ((find-handler (handlers)
	     (cond ((null handlers) nil)
		   ((typep (caar handlers) 'cliki-view)
		    (caar handlers))
		   (t (find-handler (cdr handlers))))))
    (find-handler (request-handled-by request))))
		    

(defmethod handle-request-response ((handler cliki-view)
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
   
(defmethod handle-request-response ((handler cliki-view)
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
	  (let ((d (and page (page-index page :package))))
	    (when d
	      (request-redirect
	       request (merge-url (parse-urlstring "http://ww.telent.net/cclan/")
				  (caar d)))
	      t)))
	 (t
	  (request-send-error request 500 "Eh?")))))))
