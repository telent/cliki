(in-package :cliki)

(defun phtml-view-handler (request arg-string root)
  (let* ((file (merge-pathnames arg-string root))
         (out (request-stream request)))
    (request-send-headers request)
    (format out
            "<html><head><title>Cliki : ~A</title></head>
<body><h1>~A</h1>~%" arg-string arg-string)
    (if (probe-file file)
        (with-open-file (in file :direction :input)
          (if (ignore-errors (peek-char nil in nil))
              (write-stream-to-stream (request-base-url request) in out)))
      (format out
              "This page doesn't exist yet, but you can create it if you like"))
    (format out "<hr><a href=\"edit/~A\">Edit this page</a>"
            arg-string))))


(defun write-stream-to-stream (base-url in-stream out-stream)
  (let ((eof (gensym)))
    (do ((c (read-char in-stream nil eof) (read-char in-stream nil eof))
         (c1 (peek-char nil in-stream nil) (peek-char nil in-stream nil)))
        ((eq c eof) nil)
      (cond
       ((and (eql c #\/) (eql c1 #\())
        (write-search-result (read-matched-parens in-stream) out-stream))
       ((and (eql c #\_) (eql c1 #\())
        (write-a-href
         base-url (strip-outer-parens (read-matched-parens in-stream))
         out-stream))
       ((and (eql c #\') (eql c1 #\())
        (write-code
         (read-matched-parens in-stream) out-stream))
       (t (write-char c out-stream))
       ))))

(defun write-search-result (string stream)
  (format stream "<b>Sorry, searching is not yet implemented</b>"))

(defun strip-outer-parens (string)
  (subseq string 1 (- (length string) 1)))

(defun write-a-href (base string stream)
  (format stream "<a href=\"~A\" >~A</a>"
          (urlstring (merge-url base string))
          string)))

(rt:deftest
 write-a-href 
 (with-output-to-string (o)
   (write-a-href "http://ww.telent.net/foo?" "bar baz" o))
 "<a href=\"http://ww.telent.net/foo?bar%20baz\" >bar baz</a>")

(defun write-code (in-string out)
  ;; should test (car form) for language and inline/display distinction
  ;; should do substitution on (cadr form) to turn < into &lt;
  (let* ((form (read-from-string in-string nil nil))
         (*print-case* :downcase)
         (text (with-output-to-string (o)
                 (pprint (cadr form) o))))
    (format out "<pre>")
    (with-input-from-string (i text)
      (do ((c (read-char i nil nil) (read-char i nil nil)))
          ((not c) (return))
        (if (eql c #\<)
            (princ "&lt;" out)
          (write-char c out))))
    (format out "</pre>")))

(rt:deftest
 write-stream
 (with-input-from-string
   (i "hello _(a link) '(cl (defun foo(x) (* 2 x))) ")
   (with-output-to-string (o)
     (write-stream-to-stream
      (parse-urlstring "http://ww.telent.net/cliki?") i o)))
 #.(format nil "hello <a href=\"http://ww.telent.net/a link\" >a link</a> <pre>~%(defun foo (x) (* 2 x))</pre> "))


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


;;; I really hate this kind of stuff.  Should we unescape?  Should we only
;;; unescape occurences of CHAR?  Who knows ...
;;; (actually this isn't used anyway)

(defun read-until-char (stream char)
  "Read text from STREAM until the first unescaped occurrence of CHAR,
or end of file.  Returns as multiple values (1) the text read,
excluding CHAR, and (2) CHAR if it was read (i.e. not end of file).
Escape a character by preceding it with a backslash character, which
will be removed in the output"
  (let ((eof (gensym)))
    (with-output-to-string (out)
      (loop
       (let ((c (read-char stream nil eof)))
         (if (eql c eof) (return (values out nil)))
         (if (eql c #\\)
             (setf c (read-char stream nil eof))
           (if (eql c char) (return (values out c))))
         (write-char c)
         (write-char c out))))))

