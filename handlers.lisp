(in-package :cliki)

(defclass cliki-request (request)
  ((url-root :accessor cliki-request-url-root :initarg :url-root)
   (data-directory :accessor cliki-request-data-directory
		   :initarg :data-directory)))
   
(defvar *cliki-handlers* (list nil))
(defvar *cliki-initialized-p* nil)

(defun find-page-name (title root)
  (labels ((match-p (a b)
                    (string-equal (substitute #\_ #\Space a)
                                  (substitute #\_ #\Space b))))
    (let* ((candidates (mapcar #'pathname-name (files-in-directory root))))
      (or (find title candidates :test #'match-p)
          title))))

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

(defun cliki-get-handler (request arg-string)
  (let* ((root (cliki-request-data-directory request))
	 (title (request-title request root)))
    (unless title (return-from cliki-get-handler))
    (let ((action (url-query (request-url request))))
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

(defun cliki-head-handler (request arg-string)
  (let* ((root (cliki-request-data-directory request))
	 (title (request-title request root)))
    (unless title (return-from cliki-head-handler))
    (let* ((file (merge-pathnames title root))
           (date (file-write-date file)))
      (if date
          (request-send-headers request :last-modified date)
        (request-send-headers request :response-code 404
                              :response-text "Not found")))))

(defun cliki-post-handler (request arg-string)
  (let* ((root (cliki-request-data-directory request))
	 (title  (request-title request root)))
    (if title (save-page request title root))))

(defun cliki-list-all-pages-handler (request arg-string)
  (let* ((fs-root (cliki-request-data-directory request))
	 (url-root (cliki-request-url-root request))
	 (pages (mapcar #'pathname-name (files-in-directory fs-root))))
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

(defun cliki-search-handler (request rest-of-url)
  (let* ((url (request-url request))
	 (words (car (url-query-param url  "words")))
	 (results (search-for-string (urlstring-unescape words)))
	 (start (parse-integer
		 (or (car (url-query-param url "start"))
		     "0") :junk-allowed t))
	 (end (min (length results) (+ start 10)))
	 (out (request-stream request)))
    (request-send-headers request)
    (send-cliki-page-preamble request "Search results")

    (format out "<form action=\"~Aadmin/search\"> Search again: <input name=words size=20 value=~S></form>"
	    (urlstring (cliki-request-url-root request)) words)
    (cond (results
	   (format out "<table>")
	   (loop for (name . rel) in (subseq results start end)
		 for j from start to end
		 do (format out "~&<tr><td>~A</td><td><a href=\"../~A\">~A</a> (~,01F% relevant)</td></tr>"
			    (1+ j)
			    (urlstring-escape (pathname-name (parse-namestring name)))
			    (pathname-name (parse-namestring name))
			    (* rel 100)))
	   (format out "~&</table>")
	   (print-page-selector
	    (request-stream request) start 10 (length results)
	    (format nil "~A?words=~A&start="
		    (url-path url)
		    (urlstring-escape words))))
	  (t (format out "Sorry, no pages match your search term.  Whack fol o diddle i ay")))))
		     

#|
(setf *cliki-initialized-p* nil)
|#
(defun cliki-handler (request exports discriminator arg-string fs-root)
  (declare (ignore exports arg-string))
  (unless *cliki-initialized-p*
    (initialize (request-base-url request) fs-root))
  (change-class request 'cliki-request)
  (setf (cliki-request-url-root request) (request-base-url request)
	(cliki-request-data-directory request) fs-root)
  (dispatch-request request *cliki-handlers* discriminator))

;;; these are bound in cliki-handler

(defun initialize (base-url directory)
  (setf *last-directory-time* 0)
  (create-indexes directory)
  (setf  *cliki-handlers* (list nil))
  (setf *tfidf-index* (reindex-text directory))
  (restore-recent-changes directory)
  (export-handler base-url 'cliki-get-handler
		  :method :get :stage *cliki-handlers*)
  (export-handler base-url
		  (lambda (r rest)
		    (declare (ignore rest))
		    (request-redirect
		     r (merge-url (request-url r) "index")))
		  :match :exact :method :get :stage *cliki-handlers*)
  (export-handler base-url 'cliki-head-handler
		  :method :head :stage *cliki-handlers*)
  (export-handler base-url 'cliki-post-handler 
		  :method :post :stage *cliki-handlers*)
  (export-handler (merge-url base-url "admin/all-pages")
                  'cliki-list-all-pages-handler :stage *cliki-handlers*)
  (export-handler (merge-url base-url "admin/cliki.css")
		  'css-file-handler  :stage *cliki-handlers*)
  (export-handler (merge-url base-url "admin/search")
		  'cliki-search-handler :stage *cliki-handlers*)
  (export-handler (merge-url base-url "Recent%20Changes")
		  `(view-recent-changes) :stage *cliki-handlers*)
  (export-handler (merge-url base-url "Recent+Changes")
		  `(view-recent-changes) :stage *cliki-handlers*)
  (setf *cliki-initialized-p* t))


(defun test ()
  (let ((base-url (parse-urlstring "http://ww.noetbook.telent.net/")))
    (export-server (make-instance 'server :name "ww.noetbook.telent.net" :base-url base-url :port 8000))
    (export-handler (merge-url base-url "/cliki/")
		    '(cliki-handler #p"/var/www/cliki/")
		    :needs-discriminator t)
    (with-open-file (conf "/tmp/cliki.cf"
			  :direction :output
			  :if-does-not-exist :create)
	(output-apache-conf conf))
    (install-serve-event-handlers)))


