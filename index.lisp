(in-package :cliki)

;;;; we do all indices for a page in a single pass here.  The protocol
;;;; is extensible to new indices: look at 
;;;; make-index-for-page
;;;; add-to-index-for-page
;;;; compute-index-for-page

;;; This gets called once per page and calls the gfs for the methods
;;; below

;;; XXX need some way to add contents of special tags to the tf
;;; indexing, ideally in a way that each tag can define its importance.
;;; This would be most useful if it could be called from
;;; add-to-index-for-page

;;; the problem with the preceding is that the indices we want to
;;; update are not accessible from the page itself yet as we're still
;;; creating them at this time.  So, how can add-to-index-for-page get
;;; hold of the tf index?

(defmethod update-page-indices ((cliki cliki-instance) (page cliki-page))
  (let ((indices nil)
	(word-chars nil))
    (labels ((index-for (c)
	       (unless (assoc c indices)
		 (push (cons c (make-index-for-page cliki page c)) indices))
	       (assoc c indices))
	     (update (token &rest args)
	       (let ((i (index-for token)))
		 (setf (cdr i)
		       (add-to-index-for-page cliki page token (cdr i)
					      (lambda (x w)
						(update :tf x :weight w))
					      args))))
	     (dispatch (token arg)
	       (destructuring-bind (token &rest args) (long-form-for token arg)
		 (apply #'update token args)))
	     (process-word (chars)
	       (let ((word (coerce (nreverse chars) 'string)))
		 (update :tf word)))
	     (do-body-words (c)
	       (cond ((and (not word-chars) (word-char-p c))
		      (push c word-chars))
		     ((and word-chars  (not (word-char-p c)))
		      (process-word word-chars)
		      (setf word-chars nil))
		     (word-chars
		      (push c word-chars)))))
      (dolist (w (araneida::split (page-title page)))
	(update :tf  w :weight 6))
      (with-open-file (in-stream (page-pathname page))
	(scan-stream (cliki-short-forms cliki)
		     in-stream #'do-body-words #'dispatch))
      (setf (page-indices page) indices)
      (loop for i in indices
	    for (n . v) = i
	    do (setf (cdr i) (compute-index-for-page cliki page n v))))))


;;; default methods are tuned to an index which is basically a list of all
;;; the arguments from each time the term occurs,  so

(defgeneric make-index-for-page (cliki page index-name))
(defgeneric add-to-index-for-page
    (cliki page index-name index fulltext-fn arguments))
(defgeneric compute-index-for-page (cliki page index-name index))


(defmethod make-index-for-page ((cliki cliki-instance) (page cliki-page)
				(index-name t))
  nil)

(defmethod add-to-index-for-page ((cliki cliki-instance) (page cliki-page)
				  (index-name t) index fulltext-fn arguments)
  (cons arguments index))

(defmethod add-to-index-for-page ((cliki cliki-instance) (page cliki-page)
				  (index-name (eql :clhs)) index
				  fulltext-fn arguments)
  (dolist (w arguments)
    (funcall fulltext-fn w 3))
  (call-next-method))

;;; this is a crap name.  should be something like 'finalize', but not
;;; that word exactly because it has been stolen from general use by GC

(defmethod compute-index-for-page ((cliki cliki-instance) (page cliki-page)
				  (index-name t) index)
  index)

;;; the backlinks and topic indices are special, because they're
;;; updated on the "other" page: i.e. if Foo contains the marker
;;; _(Bar), it is Bar which will be updated.  So we use this mechanism
;;; to collect the pages to index, then when we have the full list of
;;; referenced pages we loop over the collection and update all the
;;; relevant links

;;; these could both be made a lot faster if we saved the previous set
;;; of links from the source page, and used it to delete the previous
;;; page-{categories,topics} contents of the target pages
(defmethod compute-index-for-page ((cliki cliki-instance)
				   (source-page cliki-page)
				   (index-name (eql :topic)) index)
  (loop for target-page being the hash-values of (cliki-pages cliki)
	if (member
	    target-page index :key (lambda (x) (find-page cliki (car x))))
	do (pushnew source-page (page-topics target-page))
	else do (setf (page-topics target-page)
		      (remove source-page (page-topics target-page))))
  index)

(defmethod compute-index-for-page ((cliki cliki-instance)
				   (source-page cliki-page)
				   (index-name (eql :link)) index)
  (loop for target-page being the hash-values of (cliki-pages cliki)
	if (member
	    target-page index :key (lambda (x) (find-page cliki (car x))))
	do (pushnew source-page (page-backlinks target-page))
	else do (setf (page-backlinks target-page)
		      (remove source-page (page-backlinks target-page))))
  index)



;;; Term Frequency indexing

;;; Stop words.  Note that it ignores all words < 3 characters long,
;;; so this list is considerably shorter than it might have to be

(defparameter *stop-words*
  (list "http" "href" "that" "quot" "with" "may" "the" "don" "can" "non"
	"but" "not" "will" "use" "from" "there" "for" "and"
	"any" "are" "which" "etc" "them")) 

;;; Is this the best selection of word constituents?
(defun word-char-p (c) (or (alpha-char-p c) (digit-char-p c)))

(defun interesting-word-p (word)
  (not (or (< (length word) 3)
	   (member (string-downcase word)
		   *stop-words* :test #'equal))))

(defun stem-for-word (word)
  "Return the stem of WORD.  Note: doesn't presently do it very well"
  (string-right-trim "s." (string-downcase word)))


(defmethod  make-index-for-page ((cliki cliki-instance) (page cliki-page)
				(index-name (eql :tf)))
  (make-hash-table :test #'equal))

(defmethod add-to-index-for-page ((cliki cliki-instance) (page cliki-page)
				  (index-name (eql :tf)) table
				  fulltext-fn arguments)
  (destructuring-bind (word &key (weight 1) &allow-other-keys) arguments
    (let ((stem (stem-for-word word)))
      (when (interesting-word-p stem)
	(let ((n (gethash stem  table)))
	  (if (numberp n) (setf (gethash stem table) (+ weight n))
	      (setf (gethash stem table) weight)))))
    table))

(defmethod compute-index-for-page ((cliki cliki-instance) (page cliki-page)
				  (index-name (eql :tf)) table)
  ;; we actually want a list of words rather than a hash table, to work
  ;; with the search routines.  XXX would it help to sort the list?
  (let ((ret nil))
    (with-hash-table-iterator (generator-fn table)
      (loop     
       (multiple-value-bind (more? k v) (generator-fn)
	 (unless more? (return))
	 (push (cons k v) ret))))
    ret))



;;; if we kept our document vectors as _real_ vectors they'd probably
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



(defun search-for-string (cliki string)
  (let ((terms (mapcar
		(lambda (x) (cons (stem-for-word x) 1))
		(remove-if-not #'interesting-word-p
			       (araneida::split string)))))
    (sort (loop for document being the hash-values of (cliki-pages cliki)
		for doc-terms = (page-tfidf document)
		for cos = (document-vector-cosine terms doc-terms)
		if (>  cos 0)
		collect (cons document cos))
	  #'> :key #'cdr)))
