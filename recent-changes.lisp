(in-package :cliki)

(defvar *recent-changes-list* (list))
;;; date title user description

(defun restore-recent-changes (root &optional max-entries)
  (with-open-file (in (merge-pathnames "admin/recent-changes.dat" root)
		      :direction :input)
    (loop for entry = (read in nil nil)
	  do (format t "~A~%" entry)
	  while (and entry (or (not max-entries) (> max-entries 0)))
          if max-entries do (decf max-entries)
          do (push entry *recent-changes-list*))))


(defun add-recent-change (root date title user &optional description)
  (let ((entry (list date title user description)))
    ;; if the description is "?" and the preceding recent changes
    ;; entry for the same title has the same user name, don't add this one
    (let ((preceding (find-if (lambda (x) (string= (second x) title))
			      *recent-changes-list*)))
      (unless (and (string= description "")
		   (string= (third preceding) user))
	(push entry *recent-changes-list*)
	(with-open-file (out (merge-pathnames #p"admin/recent-changes.dat" root)
			     :direction :output :if-exists :append
			     :if-does-not-exist :create)
	    (print entry out))))))

(defun most-recent-change (title)
  (find title *recent-changes-list* :key #'second :test #'equal))

(defun same-day-p (date1 date2)
  (and date1 date2 
       (let ((start-of-date1
	      (with-date date1 nil
		(encode-universal-time 0 0 0 day-of-month month year))))
	 (<= start-of-date1 date2 (+ start-of-date1 86400)))))

(defun view-recent-changes (request rest-of-url)
  (declare (ignore rest-of-url))
  (let ((out  (request-stream request))
	(root (cliki-request-data-directory request))
        (start
         (parse-integer
	  (or (car (url-query-param (request-url request) "start")) "0")
	  :junk-allowed t))
        (number 30))
    (request-send-headers request)
    (send-cliki-page-preamble request "Recent Changes")
    (if (= start 0)
	(format out
		"<blockquote>This page is now updated automatically.  Look out for RSS exports in the near future too. -- ~A"
		(write-a-href "Daniel Barlow" root nil))
	(format out "<p>Older entries (starting at ~D)</p>~%" start))
    (loop for (this-date title user . description)
             in (subseq *recent-changes-list* start
                        (min (+ start number) (length *recent-changes-list*)))
          and old-date = 0 then this-date
          if (and title description user)
          unless (same-day-p this-date old-date)
          do (with-date this-date nil
               (format out
                       "</blockquote>
<a name=~D><h3>~/cliki:dayname/ ~A ~/cliki:monthname/ ~A</h3></a>
<blockquote>"
                       this-date day-of-week day-of-month month year))
          if (and title description user)
          do (with-date this-date nil
               (format out "<br> ~D:~2,'0D <b>~A</b> : ~A -- ~A ~%"
                       hour minute
		       (if title (write-a-href title root nil) "?")
                       (car description)
                       (write-a-href user root nil))))
    (princ "<p>" out)
    (print-page-selector out start number (length *recent-changes-list*)
			 (format nil "~A?start="
				 (url-path (request-url request))))
    ))

