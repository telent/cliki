(in-package :cliki)

;;; this becomes a method so that we can change the look and feel for
;;; different clikis in the same image
(defmethod cliki-page-header ((cliki cliki-instance) request title &optional head)
  (let* ((stream (request-stream request))
         (home (cliki-url-root (request-cliki request))))
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

(defun view-page (request page title)
  (let* ((out (request-stream request))
	 (cliki (request-cliki request)))
    (request-send-headers request)
    (cliki-page-header cliki request title)
    (if page
	(handler-case
	    (let* ((categories (page-categories page))
		   (backlinks
		    (sort (set-difference (page-backlinks page) categories)
			  #'string-lessp :key #'page-title)))
	      (with-open-file (in (page-pathname page) :direction :input)
		  (write-stream-to-stream cliki in out))
	      (when categories
		(format out "<hr><b>Page~p in this topic: </b> " (length categories))
		(dolist (c categories)
		  (format out "~A &nbsp; "
			  (write-a-href cliki (page-title c) nil))))
	      (when backlinks
		(format out "<hr><b>~A linked from: </b> "
			(if categories "Also" "This page is")
			(length backlinks))
		(dolist (c backlinks)
		  (format out "~A &nbsp; "
			  (write-a-href cliki (page-title c) nil)))))
	  (error (e)
	    (format out
		    "Some error occured: <pre>~A</pre>" e)))
	(format out
		"This page doesn't exist yet.  Please create it if you want to"))
    (format out "<hr><form action=\"~Aadmin/search\"><a href=\"~A?edit\">Edit page</a> | <a href=\"~A?source\">View source</a> |  Last edit: ~A | <a href=\"CLiki+Search\"> Search:</a> <input name=words size=20></form>"
	    (urlstring (cliki-url-root cliki))
            (urlstring-escape title) (urlstring-escape title)
            (if page
		(universal-time-to-rfc-date
		 (file-write-date (page-pathname page)))
		"(none)")
            )))


(defun write-stream-to-stream (cliki in-stream out-stream)
  "Read from IN-STREAM and write to OUT-STREAM, substituting weird markup language elements as we go. "  
  (let ((newlines 0)
        (returns 0)
	(eof (gensym "EOF"))
	(buffer))
    (labels ((dispatch (token arg)
	       (let ((*read-eval* nil)
		     (*package* (find-package "KEYWORD")))
		 (if (eq token :long-form) 
		     (apply #'html-for-keyword cliki (read-from-string arg))
		     (html-for-keyword 
		      cliki (intern (string-upcase token) :keyword)
		      (strip-outer-parens arg)))))
	     (scan ()
	       (handler-case
		   (scan-stream (cliki-short-forms cliki)
				in-stream #'dispatch)
		 (end-of-file (eof-c) (list eof))))       	       
	     (more-buffer ()
	       (setf buffer (nconc buffer (scan))))
	     (peekc ()
	       (unless buffer (more-buffer))
	       (car buffer))
	     (readc ()
	       (unless buffer (more-buffer))
	       (prog1 (car buffer)
		 (setf buffer (cdr buffer)))))
      (do ((c (readc)  (readc))
	   (c1 (peekc) (peekc)))
	  ((eq c eof) nil)
	(cond
	  ((stringp c) (write-sequence c out-stream))
	  ((and (member c '(#\Newline #\Return))
		(not (member c1 '(#\Newline #\Return))))
	   (if (or (> newlines 1) (> returns 1))
	       (unless (eql c1 #\<)
		 (write-sequence "<p>" out-stream)))
	   (setf newlines 0 returns 0))
	  ((eql c #\Newline)
	   (incf newlines)
	   (write-char c out-stream))
	  ((eql c #\Return)
	   (incf returns)
	   (write-char c out-stream))
	  (t (write-char c out-stream)))))))

(defgeneric html-for-keyword (cliki keyword &rest rest &key &allow-other-keys))
(defmethod html-for-keyword ((cliki cliki-instance)
			     (keyword t) &rest args)
  (format nil "<b> [unrecognised ~A keyword occurred here: args ~S] </b>"
	  keyword args))

(defmethod html-for-keyword ((cliki cliki-instance) (keyword (eql :topic))
			     &rest args &aux (arg (car args)))
  (write-a-href cliki arg nil))

(defmethod html-for-keyword ((cliki cliki-instance) (keyword (eql :link))
			     &rest args &aux (arg (car args)))
  (write-a-href cliki arg nil))

(defmethod html-for-keyword ((cliki cliki-instance) (keyword (eql :search))
			     &rest args &aux (arg (car args)))
  (legacy-search-result cliki arg nil))

(defmethod html-for-keyword ((cliki cliki-instance) (keyword (eql :clhs))
			     &rest args &aux (arg (car args)))
  (let* ((url (hyperspec-url arg)))
    (if url
	(format nil
		"<a class=\"hyperspec\" href = \"~a\"><b>~a</b></a>"
		url arg)
	arg)))

(defmethod html-for-keyword ((cliki cliki-instance) (keyword (eql :download))
			     &rest args &aux (arg (car args)))
  (format nil "<a class=\"download\" href=\"~A\"><b>Download from ~A</b></a>"
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
	(html
	 `(ul
	   ,@(mapcar (lambda (x) `(li ((a :class "internal"
					:href ,(urlstring-escape x)) ,x)))
		     titles)))))))

(defun strip-outer-parens (string)
  (and (eql (elt string 0) #\()
       (subseq string 1 (- (length string) 1))))

;; caller should unescape STRING if it needed it

(defmethod write-a-href ((cliki cliki-instance) title stream)
  "Write an A HREF element for the CLiki page TITLE.  STREAM may be an open stream or T or NIL, a la FORMAT"
  (let ((escaped (urlstring-escape title)))
    (if (find-page cliki title)
        (format stream "<a class=\"internal\" href=\"~A\" >~A</a>" escaped title)
      (format stream "~A<a class=\"internal\" href=\"~A?edit\" >?</a>" title escaped))))

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

