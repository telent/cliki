(in-package :cliki)

(defparameter *stop-words*
  (list "http" "href" "that" "quot" "with" "may" "the" "don" "can" "non"
	"but" "not" "will" "use" "from" "there" "for" "and"))

(defun word-char-p (c)
  (or (alpha-char-p c)
      (member c '(#\_  #\. #\* #\( #\)))))

(defun read-word (stream)
  "Read a word (sequence of characters satisfying WORD-CHAR-P) from STREAM.  Discards all leading and up to one following non-word character"
  (let* ((c
	  (loop for i = (read-char stream nil nil)
		until (or (not i) (word-char-p i))
		finally (return i)))
	 (word
	  (and c
	       (loop for i = c then (read-char stream nil nil)
		     while (and i (word-char-p i))
		     collect i))))
    (if word (coerce word 'string))))

(defun weight-for-word (word)
  (if
   (or (< (length word) 3)		;stopwords
       (member (string-downcase word)
	       *stop-words* :test #'equal))
   0
   (let ((elt0 (elt word 0)))
     (cond ((eq elt0 #\_) 2)			; links count extra
	   ((eq elt0 #\*) 4)			; topics even more so
	   (t 1)))))

(defun string-trim-if-not (predicate string)
  (let ((start (position-if predicate string))
	(end  (position-if predicate string :from-end t)))
    (if start
	(subseq string start (and end (1+ end)))
	"")))

(defun stem-for-word (word)
  "Return the stem of WORD.  Note: doesn't presently do it very well"
  (let ((word (string-trim-if-not #'alpha-char-p word)))
    (string-downcase (string-right-trim "s." word))))

(defmethod term-frequencies ((document cliki-page))
  "Read all words in the page DOCUMENT, discard stopwords, and return a hash table word -> frequency"
  (let ((table (make-hash-table :test 'equal))
	ret)
    (labels ((add-word (word &optional weight)
	       (let* ((stem (stem-for-word word))
		      (n (gethash stem  table))
		      (w (or weight (weight-for-word word))))
		 (if (> w 0)
		     (if (numberp n) (setf (gethash stem table) (+ w n))
			 (setf (gethash stem table) w))))))
      (dolist (w (araneida::split (page-title document)))
	(add-word w 6))
      (with-open-file (i (page-pathname document) :direction :input)
	  (loop for word = (read-word i)
	   while word
	   do (add-word word))))
    (with-hash-table-iterator (generator-fn table)
	(loop     
         (multiple-value-bind (more? k v) (generator-fn)
           (unless more? (return))
	   (push (cons k v) ret))))
    ret))

;;; if we kept out document vectors as _real_ vectors they'd probably
;;; be incredibly sparse for the most part.  So we have lists of
;;; ((term . freq) (term . freq) ...)

(defun document-vector-+ (x y)
  "Add document vectors X and Y"
  ;; ensure that x is the longer vector
  (if (< (length x) (length y)) (rotatef x y))
  (let ((ret (copy-list x)))
    (dolist (dimension y)
      (let ((place (assoc (car dimension) ret :test #'equal)))
	(if place
	    (incf (cdr place) (cdr dimension))
	    (push dimension ret))))
    ret))

(defun document-vector-cosine (x y)
  "Return the cosine of the angle between vectors X and Y"
  ;; we don't ever need the dot product directly, so we might as
  ;; well normalise the length while we're here given that we have
  ;; to iterate over at least one of the vectors anyway
  ;;(declare (optimize (speed 3)))
  (if (< (length x) (length y)) (rotatef x y))
  (let ((mag2-x 0) (mag2-y 0) (dot 0))
    (dolist (term y)
      (incf dot (* (cdr term)
		   (or (cdr (assoc (car term) x :test #'equal)) 0)))
      (incf mag2-y (* (cdr term) (cdr term))))
    (dolist (term x)
      (incf mag2-x (* (cdr term) (cdr term))))
    (/ dot (* (sqrt mag2-x) (sqrt mag2-y)))))

#|
idf(term) is log(number of documents/number of documents it occurs in)

each document is an n-dimensional vector where each term is a dimension, and
the size in that dimension is (tf document term) (idf tgerm)

then we turn our search term into a similar vector with 1 on each axis for a
supplied term

then we do dot product between search term and each other document to find
the docs with closest angle (biggest cos theta)	
|#

(defun hash-table-values (table)
  (loop for d being the hash-values of table collect d))


;;; was reindex-text
(defun update-idf (cliki)
  "Update idf, using page-tf for each page and summing stuff.  page-tf
is set by update-indexes-for-page (at startup and after edits).  "
  (let ((idf (make-hash-table :test 'equal)))
    (loop for document being the hash-values of (cliki-pages cliki)
	  do
	  (let* ((frequencies (page-tf document))
		 (terms (mapcar #'car frequencies)))
	    (dolist (term terms)
	      (let ((x (gethash term idf)))
		(setf (gethash term idf) (1+ (or x 0)))))))
    (setf (slot-value cliki 'idf) idf)))
    
(defun search-for-string (cliki string)
  (let ((terms (mapcar
		(lambda (x) (cons (stem-for-word x) 1))
		(remove-if-not (lambda (w) (> (weight-for-word w) 0))
			       (araneida::split string)))))
    (sort (loop for document being the hash-values of (cliki-pages cliki)
		for doc-terms = (page-tfidf document)
		for cos = (document-vector-cosine terms doc-terms)
		if (>  cos 0)
		collect (cons document cos))
	  #'> :key #'cdr)))


#|


(let ((h (make-hash-table :test 'eql)))
  (setf (gethash "doo" h) 3)
  (setf (gethash "boo" h) 4)
  (hashtable-as-alist h))

(let ((h (make-hash-table :test 'eql)))
  (setf (gethash "doo" h) 3)
  (setf (gethash "boo" h) 4)
  (loop for k being each hash-key of h
	collect (list k (gethash k h))))


|#



      