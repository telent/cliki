(in-package :cliki)

(defmethod cliki-css-text ((cliki cliki-instance) stream)
  (write-sequence
   "HTML { font-family: times,serif; } 
BODY {  background-color: White }
H1,H2,H3,H4 { font-family: Helvetica,Arial }
H1 {  color: DarkGreen }
H2 { font-size: 100% }
DIV { margin-left: 5%; margin-right: 5% }
A.internal { color: #0077bb }
A.hyperspec { color: #442266 }
" stream))

(defun css-file-handler (request rest-of-url)
  (request-send-headers request :content-type "text/plain")
  (cliki-css-text (request-cliki request) (request-stream request)))

(defun cliki-get-handler (request arg-string)
  (multiple-value-bind (page title) (find-page-or-redirect request)
    (when (eql page :redirected) (return-from cliki-get-handler))
    (let ((action (url-query (request-url request))))
      (cond
       ((not action)
	(view-page request page title))
       ((string-equal action "source")
        (view-page-source request page title))
       ((string-equal action "download")
	(request-redirect
	 request (merge-url (request-url request) (download-url page))))
       ((string-equal action "edit")
        (edit-page request page title))
       ;; can add in other ops like delete etc
       (t
        (request-send-error request 500 "Eh?"))))))

(defun cliki-head-handler (request arg-string)
  (multiple-value-bind (page title) (find-page-or-redirect request)
    (when (eql page :redirected)
      ;; no point answering the request, -or-redirect already did
      (return-from cliki-head-handler))
    (if page
      (request-send-headers request :last-modified 
			    (file-write-date (page-pathname page)))
      (request-send-headers request :response-code 404
			    :response-text "Not found"))))

(defun cliki-post-handler (request arg-string)
  (multiple-value-bind (page title) (find-page-or-redirect request)
    (when (eql page :redirected) (return-from cliki-post-handler))
    (save-page request page title)))

(defun cliki-list-all-pages-handler (request arg-string)
  (let* ((pages (loop for p being the hash-values of
		      (cliki-pages (request-cliki request))
		      collect p)))
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
		      ,(urlstring (page-url x)))
                     ,(page-title x))))
	     pages)))))
     (request-stream request))))

(defun cliki-search-handler (request rest-of-url)
  (let* ((url (request-url request))
	 (cliki (request-cliki request))
	 (words (car (url-query-param url  "words")))
	 (results (search-for-string cliki (urlstring-unescape words)))
	 (start (parse-integer
		 (or (car (url-query-param url "start"))
		     "0") :junk-allowed t))
	 (end (min (length results) (+ start 10)))
	 (out (request-stream request)))
    (request-send-headers request)
    (cliki-page-header cliki request "Search results")

    (format out "<form action=\"~Aadmin/search\"> Search again: <input name=words size=20 value=~S></form>"
	    (urlstring (cliki-url-root cliki)) words)
    (cond (results
	   (format out "<table>")
	   (loop for (name . rel) in (subseq results start end)
		 for j from start to end
		 do (format out "~&<tr><td>~A</td><td><a href=\"~A\">~A</a> (~,01F% relevant)</td></tr>"
			    (1+ j)
			    (urlstring (page-url name))
			    (page-title name)
			    (* rel 100)))
	   (format out "~&</table>")
	   (print-page-selector
	    (request-stream request) start 10 (length results)
	    (format nil "~A?words=~A&start="
		    (url-path url)
		    (urlstring-escape words))))
	  (t (format out "Sorry, no pages match your search term.  Whack fol o diddle i ay")))))
		     

(defun cliki-handler (request exports discriminator arg-string cliki-instance)
  (declare (ignore exports arg-string))
  (change-class request 'cliki-request)
  (setf (request-cliki request) cliki-instance)
  (dispatch-request request (cliki-handlers cliki-instance) discriminator))

(defvar   *cliki-instance*)

(defun test ()
  (let ((base-url (parse-urlstring "http://ww.noetbook.telent.net/")))
    (setf *cliki-instance*
	  (make-instance 'cliki-instance
                         :data-directory "/var/www/cliki/"
                         :url-root (merge-url base-url "/cliki/")))    
    (export-server (make-instance 'server :name "ww.noetbook.telent.net" :base-url base-url :port 8000))
    (export-handler (merge-url base-url "/cliki/")
		    `(cliki-handler ,*cliki-instance*)
		    :needs-discriminator t)
    (export-handler (merge-url base-url "/cliki/")
		    (lambda (r re)
		      (request-send-error r 500
					  (princ-to-string (request-condition r)))
		      (close (request-stream r))
		      (break))
		    :stage :error)
    (with-open-file (conf "/tmp/cliki.cf"
			  :direction :output
			  :if-does-not-exist :create)
	(output-apache-conf conf))
    (install-serve-event-handlers)))


