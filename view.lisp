(in-package :cliki)

;;; this becomes a method so that we can change the look and feel for
;;; different clikis in the same image
(defmethod cliki-page-header ((cliki cliki-instance) request title &optional head)
  (let* ((stream (request-stream request))
         (home (cliki-url-root cliki)))
    (labels ((ahref (l) (urlstring (araneida:merge-url home l)))) 
      (let ((out
	     (html
	      `(html
		(head (title ,(format nil "CLiki : ~A" title))
		 ,@head
		 ((link :rel "alternate"
			:type "application/rss+xml"
			:title "Recent Changes"
			:href
			,(urlstring (merge-url (cliki-url-root cliki)
					       "recent-changes.rdf"))))
		 ((link :rel "stylesheet" :href
			,(urlstring (merge-url (cliki-url-root cliki)
					       "admin/cliki.css")))))
		(body
		 ((table :width "100%")
		  (tr
		   (td ((a :href ,(urlstring home))
			((img :border 0 :src "/cliki.png" :alt "[ Home ]"))))
		   ((td :colspan 3) "CLiki pages can be edited by anybody at any time.  Imagine a <i>scarily comprehensive legal disclaimer</i>.  Double it.  Add two.  <!-- Now shut your eyes.  Dark, isn't it? -->")))
		 (center
		  (table
		   (tr
		    (td ((a :href ,(ahref "#")) "[ Home ]"))
		    (td ((a :href ,(ahref "Recent%20Changes")) "[ Recent Changes ]"))
		    (td ((a :href ,(ahref "CLiki")) "[ About CLiki ]"))
		    (td ((a :href ,(ahref "Text%20Formatting")) "[ Text Formatting ]")))
		   ))
		 (hr)
		 (h1 ,title)
		 (deleteme))))))
	(write-sequence
	 (subseq out 0 (search "<DELETEME>" out))
	 stream)))))

(defmethod cliki-page-footer
    ((cliki cliki-instance) request title)
  (let ((page (find-page cliki title)))
    (format (request-stream request)
	    "<hr><form action=\"~Aadmin/search\"><a href=\"edit/~A\">Edit page</a> | <a href=\"~A?source\">View source</a> |  Last edit: ~A | <a href=\"CLiki+Search\"> Search:</a> <input name=words size=20></form>"
	    (urlstring (cliki-url-root cliki))
	    (urlstring-escape title) (urlstring-escape title)
	    (if page
		(universal-time-to-rfc-date
		 (file-write-date (page-pathname page)))
		"(none)"))))

    
(defun print-page-selector
    (stream start-of-page number-on-page total-length urlstring-stub)
  "Print result page selector with `previous', `next', and numbered links to each result page. Form links by glomming offset to URLSTRING-STUB"
  (labels ((url (name offset)
	     (format stream "~&<td><a href=\"~A~A\">~A</a></td>"
		     urlstring-stub offset name)))
    (princ "<center><table><tr><td>Result page:  </td><td> </td>" stream)
    (let ((first-on-screen
	   (* (floor start-of-page number-on-page) number-on-page)))
      (if (> first-on-screen 0)
	  (url "Previous"
	       (- first-on-screen number-on-page)))
      (loop for i from 0 to total-length by number-on-page
	    for j = 1 then (1+ j)
	    if (<= i start-of-page (+ i number-on-page -1))
	    do (format stream "~&<td>~A</td>" j)
	    else do (url j i))
      (if (< (+ first-on-screen number-on-page) total-length)
	  (url "Next" (+ first-on-screen number-on-page)))
      (princ "</tr></table></center>" stream))))


(defun long-form-for (token arg)
  (let ((*read-eval* nil)
	(*package* (find-package "KEYWORD")))
    (if (eq token :long-form)
	(multiple-value-bind (r e)
	    (ignore-errors (values (read-from-string arg)))
	  (if e (list :error (format nil "~A" e) arg)  r))
	(list (intern (string-upcase token) :keyword)
	      (strip-outer-parens arg)))))

(defun write-page-contents-to-stream (cliki page out-stream)
  "Read the file for PAGE and write to OUT-STREAM, substituting weird markup language elements as we go. "  
  (let ((newlines 0)
        (returns 0))
    (labels ((dispatch (token arg)
	       (apply #'html-for-keyword
		      cliki out-stream (long-form-for token arg)))
	     (output (c)
	       (cond
		 ((and (or (> newlines 1) (> returns 1))
		       (member c '(#\Newline #\Return)))
		  (write-sequence "<p>" out-stream)
		  (setf newlines 0 returns 0))
		 ((eql c #\Newline)
		  (incf newlines)
		  (write-char c out-stream))
		 ((eql c #\Return)
		  (incf returns)
		  (write-char c out-stream))
		 (t
		  (setf newlines 0 returns 0)
		  (write-char c out-stream)))))
      (with-open-file (in-stream (page-pathname page) :direction :input)
	(scan-stream (cliki-short-forms cliki)
		     in-stream
		     #'output #'dispatch)))))

(defun view-page (cliki request page title)
  (let ((out (request-stream request)))
    (request-send-headers request)
    (cliki-page-header cliki request title)
    (if page
	(progn
	  ;; we could remove the current page from the topic/links
	  ;; here.  that might be a good idea
	  (let* ((topics (page-topics page))
		 (backlinks
		  (sort (set-difference (page-backlinks page) topics)
			#'string-lessp :key #'page-title)))
	    (write-page-contents-to-stream cliki page out)
	    (when topics
	      (format out "<hr><b>Page~p in this topic: </b> " (length topics))
	      (dolist (c topics)
		(format out "~A &nbsp; "
			(write-a-href cliki (page-title c) nil))))
	    (when backlinks
	      (format out "<hr><b>~A linked from: </b> "
		      (if topics "Also" "This page is")
		      (length backlinks))
	      (dolist (c backlinks)
		(format out "~A &nbsp; "
			(write-a-href cliki (page-title c) nil))))))
	(format out
		"This page doesn't exist yet.  Please create it if you want to"))
    (cliki-page-footer cliki request title)
    t))


(defgeneric html-for-keyword (cliki stream keyword &rest rest &key &allow-other-keys))
(defmethod html-for-keyword ((cliki cliki-instance)
			     stream (keyword t) &rest args)
  (format stream "<b> [unrecognised ~A keyword occurred here: args ~S] </b>"
	  keyword args))

(defmethod html-for-keyword ((cliki cliki-instance)
			     stream (keyword (eql :error)) &rest args)
  (destructuring-bind (error form) args
    (format stream "<b> [Syntax error in tag: </b><br>~A<pre>~S</pre><b>] </b>"
	    form (html-escape error))))

(defmethod html-for-keyword ((cliki cliki-instance) stream
			     (keyword (eql :topic))
			     &rest args &aux (arg (car args)))
  (write-a-href cliki arg stream))

(defmethod html-for-keyword ((cliki cliki-instance) stream
			     (keyword (eql :link))
			     &rest args &aux (arg (car args)))
  (write-a-href cliki arg stream))

(defmethod html-for-keyword ((cliki cliki-instance) stream
			     (keyword (eql :legacy-search))
			     &rest args &aux (arg (car args)))
  (legacy-search-result cliki arg stream))

(defmethod html-for-keyword ((cliki cliki-instance) stream
			     (keyword (eql :search))
			     &rest args)
  (destructuring-bind
	(&key title
	      (no-results-message "No results from search")
	      term show-relevance-p &allow-other-keys) args
    (let ((pages
	   (sort 
	    (loop for page being the hash-values of (cliki::cliki-pages cliki)
		  for r = (apply #'search-term-relevance cliki page term)
		  if (> r 0)
		  collect (list r page))
	    #'>
	    :key #'car))) 
      (cond (pages
	     (format stream "~A<ul>" title)
	     (dolist (p pages)
	       (format stream
		       (if show-relevance-p "<li>~A (~A)</li>"
			   "<li>~A</li>")
		       (write-a-href cliki (page-title (cadr p)) nil)
		       (car p)))
	     (format stream "</ul>"))
	    (t (princ no-results-message stream))))))

(defmethod html-for-keyword ((cliki cliki-instance) stream
			     (keyword (eql :clhs))
			     &rest args &aux (arg (car args)))
  (let* ((url (hyperspec-url arg)))
    (if url
	(format stream
		"<a class=\"hyperspec\" href = \"~a\"><b>~a</b></a>"
		url arg)
	(princ arg stream))))

(defmethod html-for-keyword ((cliki cliki-instance) stream
			     (keyword (eql :download))
			     &rest args &aux (arg (car args)))
  (format stream
	  "<a class=\"download\" href=\"~A\"><b>Download from ~A</b></a>"
	  arg arg))


(defun legacy-search-result (cliki string stream)
  ;; this business with read-from-string is a hangover from Ye Olde
  ;; search syntax
  (let* ((*read-eval* nil)
         (form (read-from-string (format nil "( ~A )" string) nil nil)))
    (destructuring-bind (term &key attribute match case-sensitive) form
      (let ((titles
             (search-pages cliki term :attribute attribute :match match  
                           :case-sensitive case-sensitive)))
	(princ
	 (html
	  `(ul
	    ,@(mapcar (lambda (x) `(li ((a :class "internal"
					 :href ,(urlstring-escape x)) ,x)))
		      titles)))
	 stream)))))

(defun strip-outer-parens (string)
  (and (eql (elt string 0) #\()
       (subseq string 1 (- (length string) 1))))

;; caller should unescape STRING if it needed it

(defmethod write-a-href ((cliki cliki-instance) title stream)
  "Write an A HREF element for the CLiki page TITLE.  STREAM may be an open stream or T or NIL, a la FORMAT"
  (let ((escaped (urlstring-escape title)))
    (if (find-page cliki title)
        (format stream "<a class=\"internal\" href=\"~A\" >~A</a>" escaped title)
      (format stream "~A<a class=\"internal\" href=\"edit/~A\" >?</a>" title escaped))))

(defun read-matched-parens (stream)
  "Read from STREAM until we have seen as many #\) as #\(, returning
the string read.  Characters may be escaped by a preceding backslash;
this is left in the output but not counted by the bracket matcher"
  (let ((eof (gensym))
        (nesting 0))
    (with-output-to-string (out)
      (loop
       (let ((c (read-char stream nil eof)))
         (if (eql c eof) (return out))
         (if (eql c #\\)
             (progn
               (write-char c out)
               (setf c (read-char stream nil eof)))
           (progn
             (if (eql c #\() (incf nesting))
             (if (eql c #\)) (decf nesting))))
         (write-char c out)
         (if (eql nesting 0) (return out))
         )))))

(rt:deftest read-matched-parens
            (with-input-from-string (i "(foo(bar(bax)bo\\)ol))fluff")
                                    (read-matched-parens i))
            "(foo(bar(bax)bo\\)ol))")

