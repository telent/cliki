(in-package :cliki)

;;; search for stuff

;;; XXX this shows all the signs of wanting to be a
;;; multiply-dispatched method on (attribute match)

;;; XXX is there a standard term for :case-sensitive in CLHS?

;;; TODO syntax errors and so forth should throw something
;;; standardised which write-stream-to-stream can catch (so fix search-error)

(defun search-pages (term pathname &key (attribute :body) (match :substring)
                          (case-sensitive nil))
  "Search pages in PATHNAME for TERM according to the criteria in the keyword
arguments.  Returns a list of page titles.
ATTRIBUTE is (or :title :topic :body)
MATCH is (or :exact :substring :regular-expression)
CASE-SENSITIVE is (or t nil)"
  (case attribute
    (:body (search-page-bodies term pathname match case-sensitive))
    (:title (search-page-titles term pathname match case-sensitive))
    (:topic (search-page-topics term pathname match case-sensitive))
    (t (search-error "Unknown search attribute"))))

(defun search-error (&rest args) (apply #'error args))

(defun inverted-search-predicate (match case-p)
  (cond
   ((and (eql match :substring) case-p)
    (lambda (term x) (search term x :test #'char/=)))
   ((eql match :substring) 
    (lambda (term x) (search term x :test #'char-not-equal)))
   ((and (eql match :exact) case-p)
    #'string/=)
   ((eql match :exact)
    #'string-not-equal)
   (t (search-error "Unknown search match criterion"))))
                             
(defun search-page-titles (term pathname match case-sensitive)
  (let ((all-titles (mapcar #'pathname-name (directory pathname))))
    (remove term all-titles
            :test (inverted-search-predicate match case-sensitive))))

;;; XXX full text search over bodies is going to be slow.  Suggested
;;; approach: for each file we create a set of (word.frequency) pairs
;;; (ignore stopwords) and save it in filename.idx.  We load all the
;;; file.idx files into memory when the server starts; we create a
;;; new one whenever someone saves a file

(defun search-page-bodies (term pathname match case-sensitive)
  (declare (ignorable term pathname match case-sensitive))
  (search-error "Full body searching not yet implemented.  Sorry."))


(defun search-page-topics (term pathname match case-sensitive)
  (declare (ignorable term pathname match case-sensitive))
  (pages-for-category term))

     