(in-package :cliki)

(defun restore-recent-changes (cliki &optional max-entries)
  (let ((path (merge-pathnames "admin/recent-changes.dat"
			       (cliki-data-directory cliki))))
    (ensure-directories-exist path)
    (with-open-file (in path
			:if-does-not-exist :create
			:direction :input)
      (loop for entry = (read in nil nil)
	    while (and entry (or (not max-entries) (> max-entries 0)))
	    if max-entries do (decf max-entries)
	    do (push entry (cliki-recent-changes cliki))))))

(defun add-recent-change (cliki date title user &optional description)
  (let* ((entry (list date title user description))
	 (changes (cliki-recent-changes cliki))
	 (preceding (find-if (lambda (x) (string= (second x) title))
			     changes)))
    ;; if the description is empty and the preceding recent changes
    ;; entry for the same title has the same user name, don't add this one
    (unless (and (string= description "")
		 (string= (third preceding) user))
      (push entry (cliki-recent-changes cliki))
      (with-open-file (out (merge-pathnames #p"admin/recent-changes.dat"
					    (cliki-data-directory cliki))
			   :direction :output :if-exists :append
			   :if-does-not-exist :create)
	  (with-standard-io-syntax (print entry out))))))

(defun same-day-p (date1 date2)
  (= (floor date1 86400) (floor date2 86400)))

(defun view-recent-changes (request)
  (declare (ignore rest-of-url))
  (let* ((out  (request-stream request))
	 (cliki (request-cliki request))
	 (changes (cliki-recent-changes cliki))
	 (start
	  (parse-integer
	   (or (car (url-query-param (request-url request) "start")) "0")
	   :junk-allowed t))
	 (number 30))
    (request-send-headers request :last-modified (caar changes))
    (with-page-surround (cliki request "Recent Changes")
      (if (= start 0)
	  (format out
		  "<blockquote>This page is updated automatically.  There's also an <a href=\"recent-changes.rdf\">RSS 0.91</a> RDF feed"
		  (urlstring
		   (merge-url (request-url request) "recent-changes.rdf")))
	  (format out "<p>Older entries (starting at ~D)</p>~%" start))
      (loop for (this-date title user . description)
	    in (subseq changes start
		       (min (+ start number) (length changes)))
	    and old-date = 0 then this-date
	    if (and title description user)
	    unless (same-day-p this-date old-date)
	    do (with-date this-date 0
			  (format out
				  "</blockquote>
<a name=~D><h3>~/cliki:dayname/ ~A ~/cliki:monthname/ ~A</h3></a>
<blockquote>"
				  this-date day-of-week day-of-month month year))
	    if (and title description user)
	    do (with-date this-date 0
			  (format out "<br> ~D:~2,'0D <b>~A</b> : ~A -- ~A ~%"
				  hour minute
				  (if title (write-a-href cliki title nil) "?")
				  (car description)
				  (if user (write-a-href cliki user nil) ""))))
      (princ "</blockquote><p>" out)
      (print-page-selector out start number (length changes)
			   (format nil "~A?start="
				   (url-path (request-url request))))
      )))

(defun rss-recent-changes-stream (cliki stream)
  (let ((seen-titles nil)
	(changes (cliki-recent-changes cliki)))
    (labels ((datefmt (date)
	       (with-date date 0
			  (format nil "~/cliki:dayname/ ~A ~/cliki:monthname/" 
				  day-of-week day-of-month month))))
      (loop for (date title user . description)
	    in changes
	    if (> (length seen-titles) 15) return nil end
	    if (assoc title seen-titles :test #'string=)
	    do (incf (fourth (assoc title seen-titles :test #'string=)))
	    else
	    do (push (list title date description 1) seen-titles)
	    end)
      (let ((items
	     (loop for (title date description edits)
		   in (nreverse seen-titles)
		   for f-date = (datefmt date)
		   for descr = (if (> edits 1)
				   (format nil "~A edits" edits)
				   (car description))
		   for url = (urlstring (merge-url (cliki-url-root cliki)
						   (urlstring-escape title)))
		   collect `("item" () 
				    ("title" () ,f-date " : " ,title" : " ,descr)
				    ("link" ()  ,url)))))
	(xmls:write-xml
	 `("rss" (("version" "0.92"))
		 ("channel"
		  ()
		  ("title" 
		   ()
		   ,(cliki-title cliki) " Recent Changes" 
		   ("link" () ,(urlstring (cliki-url-root cliki)) "Recent%20Changes")
		   ("description" () ,(cliki-title cliki) " Recent Changes" ))
		  ,@items
		  ("textinput" 
		   () 
		   ("title" () ,(cliki-title cliki) " Search")
		   ("description" () "Search all pages")
		   ("name" () "words")
		   ("link" () ,(urlstring (cliki-url-root cliki))
			   "admin/search"))))
       	 stream)
	t))))

(defun rdf-recent-changes (request)
  (let* ((out  (request-stream request))
	 (cliki (request-cliki request))
	 (changes (cliki-recent-changes cliki)))
    (request-send-headers request :content-type "text/xml"
			  ;:conditional t
			  :last-modified (caar changes))
    (rss-recent-changes-stream cliki out)))

(defun sexp-recent-changes (request)
  (let* ((out  (request-stream request))
	 (cliki (request-cliki request))
	 (changes (cliki-recent-changes cliki)))
    (request-send-headers request :content-type "text/plain"
			  :conditional t
			  :last-modified (caar changes))
    (print (subseq changes 0 200) out)
    t))
