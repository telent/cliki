(in-package :cliki)

(defun cliki-html::titlebar (stream format-arg colon-p at-p &rest params)
  "Print a string of HTML that gets inserted at the top of each page.
You could put your company logo here, or something like that.  This is
intended for use as a FORMAT Tilde-slash function"
  (declare (ignore colon-p at-p params))
  (let* ((here (request-url format-arg))
         (home (cliki-url-root (request-cliki format-arg))))
    (labels ((ahref (l) (urlstring (araneida:merge-url home l)))) 
      (write-sequence
       (html
	`(p
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
	  (hr)))
       stream))))

(defun send-cliki-page-preamble (request title &optional (head ""))
  (let ((out (request-stream request)))
    (format out "<html><head><title>Cliki : ~A</title>~A</head>
<link rel=\"stylesheet\" href=\"~Aadmin/cliki.css\">
<body>
~/cliki-html:titlebar/
<h1>~A</h1>~%"
	    title head (urlstring (cliki-url-root (request-cliki request)))
	    request title)))

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
    (send-cliki-page-preamble request title)
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
    (format out "<hr><form action=\"/cliki/admin/search\"><a href=\"~A?edit\">Edit this page</a> | <a href=\"~A?source\">View page source</a> |  Last edit: ~A | <a href=\"CLiki+Search\"> Search CLiki</a> <input name=words size=20></form>"
            (urlstring-escape title) (urlstring-escape title)
            (if page
		(file-write-date (page-pathname page))
		"(none)")
            )))


(defun write-stream-to-stream (cliki in-stream out-stream)
  "Read from IN-STREAM and write to OUT-STREAM, substituting weird markup language elements as we go. "  
  (let ((eof (gensym))
        (newlines 0)
        (returns 0))
    (do ((c (read-char in-stream nil eof) (read-char in-stream nil eof))
         (c1 (peek-char nil in-stream nil) (peek-char nil in-stream nil)))
        ((eq c eof) nil)
      (cond
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
       ;; Before this method gets much longer, the following stuff wants to
       ;; be turned into dispatches to a helper function per special char
       ((and (eql c #\/) (eql c1 #\())  ; search
        (write-search-result
         cliki (read-matched-parens in-stream) out-stream))
       ((and (member c '(#\* #\_)) (eql c1 #\())  ; link
        (write-a-href
	 cliki (strip-outer-parens (read-matched-parens in-stream)) out-stream))
       ((and (eql c #\#) (eql (char-upcase c1) #\H))
        (let* ((h (read-char in-stream))
	       (term (read-matched-parens in-stream))
	       (stripped-term (strip-outer-parens term))
               (url (and stripped-term (hyperspec-url stripped-term ))))
          (cond (url
		 (format out-stream "<a class=\"hyperspec\" href = \"~a\"><b>~a</b></a>" url stripped-term))
		(stripped-term
		 (write-string term out-stream))
		(t
		 (format out-stream "~C~C~A" #\# h term)))))
       (t (write-char c out-stream)) ))))

(defun write-search-result (cliki string stream)
  (let* ((*read-eval* nil)
         (form (read-from-string string nil nil)))
    (destructuring-bind (term &key attribute match case-sensitive) form
      (let ((titles
             (search-pages cliki term :attribute attribute :match match  
                           :case-sensitive case-sensitive)))
        (write-sequence
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
      (format stream "~A<a class=\"internal\" href=\"~A?edit\" >?</a>" title escaped))))


(defun write-code (in-string out)
  ;; should test (car form) for language and inline/display distinction
  ;; should do substitution on (cadr form) to turn < into &lt;
  (let* ((*read-eval* nil)
         (form (read-from-string in-string nil nil)))
    (destructuring-bind (language &key (case :downcase)
                                  mode right-margin) (car form)
      (let* ((*print-case* case)
             (*print-right-margin* right-margin)
             (text (with-output-to-string (o)
                     (dolist (f (cdr form))
                       (pprint f o)))))
        (format out (if (eql mode :display) "<pre>" "<tt>"))
        (with-input-from-string (i text)
          (do ((c (read-char i nil nil) (read-char i nil nil)))
              ((not c) (return))
            (if (eql c #\<)
                (princ "&lt;" out)
              (write-char c out))))
        (format out (if (eql mode :display) "</pre>" "</tt>"))))))

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

