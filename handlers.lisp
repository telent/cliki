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


(defun cliki-list-all-pages-handler (request)
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

(defun search-pages (cliki term)
  (sort (loop for page being the hash-values of (cliki-pages cliki)
	      for relevance = (apply #'search-term-relevance cliki page term)
	      if (> relevance 0)
	      collect (cons page relevance))
	#'>
	:key #'cdr))

(defun complex-search-term (term)
  (cond ((eql (elt term 0) #\()
	 (let ((*read-eval* nil)
	       (*package* (find-package :keyword)))
	   (ignore-errors
	     (values (read-from-string
		      (urlstring-unescape term))))))
	(t `(:body ,term))))

(defmethod format-search-relevance ((cliki cliki-instance) relevance)
  (format nil "(~,01F% relevant)" (* relevance 100)))

(defun cliki-search-handler (request)
  (let* ((url (request-url request))
	 (cliki (request-cliki request))
	 (term (car (url-query-param url  "words"))))
    (multiple-value-bind (c-term error) (complex-search-term term)
      (let* ((results (unless error (search-pages cliki c-term)))
	     (start (parse-integer
		     (or (car (url-query-param url "start"))
			 "0") :junk-allowed t))
	     (end (min (length results) (+ start 10)))
	     (out (request-stream request)))
	(request-send-headers request)
	(cliki-page-header cliki request "Search results")
	
	(format out "<form action=\"~Aadmin/search\"> Search again: <input name=words size=60 value=~S></form>"
		(urlstring (cliki-url-root cliki))
		(html-escape (if error term (prin1-to-string c-term))))
	(cond
	  (error
	   (format out "Sorry, your search term could not be read<pre>~A</pre>" (html-escape (princ-to-string error))))
	  (results
	   (loop for (name . rel) in (subseq results start end)
		 for j from start to end
		 do (format out "~&<p>~A <b><a href=\"~A\">~A</a></b> ~A<br>~A"
			    (1+ j)
			    (urlstring (page-url name))
			    (page-title name)
			    (format-search-relevance cliki rel)
			    (let ((s (page-summary cliki name)))
			      (if s
				  (format nil "<div style=\"margin-top: -5px; margin-left: 5%; font-size: 80%\">~A</div>~%"
					  s)
				  ""))
			    ))
	   (print-page-selector
	    (request-stream request) start 10 (length results)
	    (format nil "~A?words=~A&start="
		    (url-path url)
		    (urlstring-escape (princ-to-string c-term)))))
	  (t (format out "Sorry, no pages match your search term.")))
	t))))

		     

(defvar   *cliki-instance*)

#|
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


|#
