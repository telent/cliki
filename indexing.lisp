(in-package :cliki)

(defun create-indexes (cliki)
  (loop for page being the hash-values of (cliki-pages cliki)
	do (update-indexes-for-page page)))

(defun grep-stream-for-links (in-stream link-introducers cliki)
  (let ((eof (gensym))
        (links (mapcar #'list link-introducers)))
    (do ((c (read-char in-stream nil eof) (read-char in-stream nil eof))
         (c1 (peek-char nil in-stream nil) (peek-char nil in-stream nil)))
        ((eq c eof) links)
      (if (and (eql c1 #\( )
               (member c link-introducers))
          (let ((l (assoc c links)))
            (setf (cdr l)
                  (adjoin (find-page cliki (strip-outer-parens
					    (read-matched-parens in-stream)))
                          (cdr l))))))))

(defun update-indexes-for-page (source-page)
  (let* ((cliki (page-cliki source-page))
	 (links
          (with-open-file (in (page-pathname source-page) :direction :input)
	      (grep-stream-for-links in '(#\_ #\*) cliki)))
         (page-links-on-updated-page (cdr (assoc #\_ links)))
         (category-links-on-updated-page (cdr (assoc #\* links))))
    ;; full-text indexing
    (setf (page-tf source-page) (term-frequencies source-page))
    (loop for target-page being the hash-values of (cliki-pages cliki)
	  for k = (canonise-title (page-title target-page))

          if (member target-page category-links-on-updated-page)
          do (pushnew source-page (page-categories target-page))
          else do (setf (page-categories target-page)
			(remove source-page (page-categories target-page)))

          if (member target-page page-links-on-updated-page)
          do (pushnew source-page (page-backlinks target-page))
          else do (setf (page-backlinks target-page)
			(remove source-page (page-backlinks target-page))))))
