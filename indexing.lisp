(in-package :cliki)

;; hashtable of page -> list of all pages which refer to it
(defvar *backlinks* (make-hash-table))
(defvar *categories* (make-hash-table))

(defun directory-for (pathname)
  (merge-pathnames (make-pathname :name :wild) pathname))

(defun create-indexes (root)
  (setf *backlinks* (make-hash-table)
        *categories* (make-hash-table))
  (dolist (fn (directory root))
    (unless (wild-pathname-p fn)
      (update-indexes-for-page (pathname-name fn) (directory-for fn)))))

(defun add-link-to-hashtable (table from to)
  (let* ((key (intern from #.*package*))
         (entry (gethash key table)))
    (unless (or (equal from to)
                (member to entry :test #'equal))
      (setf (gethash key table)
            (merge 'list entry (list to) #'string-lessp)))))

(defun remove-link-from-hashtable (table from to)
  (let* ((key (intern from #.*package*))
         (entry (gethash key table)))
    (when (member to entry :test #'equal)
      (setf (gethash key table)
            (delete to entry :test #'equal)))))

(defun grep-stream-for-links (in-stream link-introducers root)
  (let ((eof (gensym))
        (links (mapcar #'list link-introducers)))
    (do ((c (read-char in-stream nil eof) (read-char in-stream nil eof))
         (c1 (peek-char nil in-stream nil) (peek-char nil in-stream nil)))
        ((eq c eof) links)
      (if (and (eql c1 #\( )
               (member c link-introducers))
          (let ((l (assoc c links)))
            (setf (cdr l)
                  (adjoin (find-page-name (strip-outer-parens
                                           (read-matched-parens in-stream))
                                          root)
                          (cdr l))))))))

(defun update-indexes-for-page (title root
                                      &aux
                                      (pathname (merge-pathnames title root)))
  (unless (eql
           (unix:unix-file-kind (namestring pathname))
           :file)
      (return-from update-indexes-for-page nil))
  (let* ((links
          (with-open-file (in pathname :direction :input)
            (grep-stream-for-links in '(#\_ #\*) root)))
         (page-links-on-updated-page (cdr (assoc #\_ links)))
         (category-links-on-updated-page (cdr (assoc #\* links))))
    (loop for k in (mapcar #'pathname-name (directory root))
          if (member k category-links-on-updated-page :test #'equal)
          do (add-link-to-hashtable *categories* k title)
          else do (remove-link-from-hashtable *categories* k title)
          if (member k page-links-on-updated-page :test #'equal)
          do (add-link-to-hashtable *backlinks* k title)
          else do (remove-link-from-hashtable *backlinks* k title))))

    
#|
(with-input-from-string (i " one of _(these) and *(two) of *(those) ")
  (grep-stream-for-links i '(#\_ #\*)))

(create-indexes "/var/www/cliki/")
|#
