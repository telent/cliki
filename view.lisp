(in-package :cliki)

(defun cliki-html::titlebar (stream format-arg colon-p at-p &rest params)
  "Print a string of HTML that gets inserted at the top of each page.
You could put your company logo here, or something like that.  This is
intended for use as a FORMAT Tilde-slash function"
  (declare (ignore colon-p at-p params))
  (let* ((here (request-url format-arg))
         (home (merge-url here "index")))
  (write-sequence
   (html
    `((table :width "100%")
      (tr
       (td ((a :href ,(urlstring home))
            ((img :src "/cliki.png" :alt "[ Home ]"))))
       (td "CLiki pages can be edited by anybody at any time.  Imagine the <i>most fearsomely comprehensive legal disclaimer you have ever seen</i>, and double it"))
      (tr ((td :colspan 4) (hr)))))
   stream)))

;;; this is ugly.  It's  a cheap (in programmer-time terms, anyway) hack
;;; for CMUCL's lack of EQUAL hash tables
(defvar *pages-for-category* (make-hash-table))
(defun add-page-to-category (category title)
  (let ((sym (intern category :cliki-pages)))
    (pushnew title (gethash sym *pages-for-category*) :test #'string=)))
(defun pages-for-category (category)
  (let ((sym (intern category :cliki-pages)))
    (gethash sym *pages-for-category*)))

             
(defun view-page (request title root)
  (let ((out  (request-stream request)))
    (request-send-headers request)
    (format out
            "<html><head><title>Cliki : ~A</title></head>
<body>
~/cliki-html:titlebar/
<h1>~A</h1>~%" title request title)
    (let ((categories
           (handler-case
            (with-open-file (in (merge-pathnames title root) :direction :input)
              (write-stream-to-stream in out))
            (file-error (e)                    ;probably just doesn't exist
                        (declare (ignore e))
                        (format out
                                "This page doesn't exist yet.  Please create it if you want to") '())
            (error (e)
                   (format out
                           "Some error occured: <pre>~A</pre>" e)
                   '()))))
      (when categories
        (format out "<hr><b>Topic~p:</b> " (length categories))
        (dolist (c categories)
          (add-page-to-category c title)
          (format out "~A &nbsp; "
                  (write-a-href c root nil)))))
    (format out "<hr><a href=\"~A?edit\">Edit this page</a>"
            (urlstring-escape title))))

(defun directory-for (pathname)
  (merge-pathnames (make-pathname :name :wild) pathname))

(defun write-stream-to-stream (in-stream out-stream)
  "Read from IN-STREAM and write to OUT-STREAM, substituting weird markup language elements as we go.  IN-STREAM must be a FILE-STREAM or SYNONYM-STREAM associeted with same if internal links are to work.  Returns a list of categories compiled from the *(foo) elements read from IN-STREAM"
  (let ((eof (gensym))
        (newlines 0)
        (categories nil)                
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
         (directory-for in-stream)
         (read-matched-parens in-stream) out-stream))
       ((and (eql c #\_) (eql c1 #\())  ; link
        (write-a-href
         (strip-outer-parens (read-matched-parens in-stream))
         (directory-for in-stream) out-stream))
       ((and (eql c #\*) (eql c1 #\())  ; link and category
        (let ((title (strip-outer-parens (read-matched-parens in-stream))))
          (pushnew title categories :test #'string-equal)
          (write-a-href title (directory-for in-stream) out-stream)))
       ((and (eql c #\') (eql c1 #\())  ; lisp code
        (write-code
         (read-matched-parens in-stream) out-stream))
       (t (write-char c out-stream))
       ))
    categories))


(defun write-search-result (directory string stream)
  (let* ((*read-eval* nil)
         (form (read-from-string string nil nil)))
    (destructuring-bind (term &key attribute match case-sensitive) form
      (let ((titles
             (search-pages term directory :attribute attribute :match match  
                           :case-sensitive case-sensitive)))
        (write-sequence
         (html
          `(ul
            ,@(mapcar (lambda (x) `(li ((a :href ,(urlstring-escape x)) ,x)))
                      titles)))
         stream)))))

(defun strip-outer-parens (string)
  (subseq string 1 (- (length string) 1)))

;; caller should unescape STRING if it needed it

(defun write-a-href (title root stream)
  "Write an A HREF element for the CLiki page TITLE found in the directory ROOT.  STREAM may be an open stream or T or NIL, a la FORMAT"
  (let ((escaped (urlstring-escape title)))
    (if (probe-file (merge-pathnames title root))
        (format stream "<a href=\"~A\" >~A</a>" escaped title)
      (format stream "~A<a href=\"~A?edit\" >?</a>" title escaped))))


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

