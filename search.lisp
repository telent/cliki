(in-package :cliki)

;;; search for stuff

;;; XXX this shows all the signs of wanting to be a
;;; multiply-dispatched method on (attribute match)

;;; XXX is there a standard term for :case-sensitive in CLHS?

;;; TODO syntax errors and so forth should throw something
;;; standardised which write-stream-to-stream can catch (so fix search-error)

(defun search-pages (cliki term &key (attribute :body) (match :substring)
                          (case-sensitive nil))
  "Search pages in PATHNAME for TERM according to the criteria in the keyword
arguments.  Returns a list of page titles.
ATTRIBUTE is (or :title :topic :body)
MATCH is (or :exact :substring :regular-expression)
CASE-SENSITIVE is (or t nil)"
  (sort 
   (case attribute
     (:body (search-page-bodies cliki term match case-sensitive))
     (:title (search-page-titles cliki term match case-sensitive))
     (:topic (search-page-topics cliki term match case-sensitive))
     (t (search-error "Unknown search attribute")))
   #'string-lessp))


(defun search-error (&rest args) (apply #'error args))

(defun search-predicate (match case-p)
  (cond
   ((and (eql match :substring) case-p)
    (lambda (term x) (search term x)))
   ((eql match :substring) 
    (lambda (term x) (search term x :test #'char-equal)))
   ((and (eql match :exact) case-p)
    #'string=)
   ((eql match :exact)
    #'string-equal)
   (t (search-error "Unknown search match criterion"))))

(defun search-page-titles (cliki term match case-sensitive)
  (let ((pred (search-predicate match case-sensitive)))
    (loop for page being the hash-values of (cliki-pages cliki)
	  if (funcall pred term (page-title page))
	  collect (page-title page))))

(defun search-page-bodies (cliki term match case-sensitive)
  (declare (ignorable term pathname match case-sensitive))
  ;; need to do this based on the tfidf stuff
  (search-error "Full body searching not yet implemented.  Sorry."))

(defun search-page-topics (cliki term match case-sensitive)
  (let ((pred (search-predicate match case-sensitive)))
    ;; find all pages that have "term" in their name
    ;; display their combined topics lists
    (mapcar #'page-title
	    (remove-duplicates
	     (sort 
	      (loop for page being the hash-values of (cliki-pages cliki)
		    if (member term (page-names page) :test pred)
		    append (page-categories page))
	      #'string-lessp
	      :key #'page-title)))))


     