(in-package :cliki)

(defun restore-recent-changes (cliki &optional max-entries)
  (with-open-file (in (merge-pathnames "admin/recent-changes.dat"
				       (cliki-data-directory cliki))
		      :direction :input)
      (loop for entry = (read in nil nil)
       while (and entry (or (not max-entries) (> max-entries 0)))
       if max-entries do (decf max-entries)
       do (push entry (cliki-recent-changes cliki)))))

(defun add-recent-change (cliki date title user &optional description)
  (let* ((entry (list date title user description))
	 (changes (cliki-recent-changes cliki))
	 (preceding (find-if (lambda (x) (string= (second x) title))
			     changes)))
    ;; if the description is empty and the preceding recent changes
    ;; entry for the same title has the same user name, don't add this one
    (unless (and (string= description "")
		 (string= (third preceding) user))
      (push entry (cliki-recent-changes cliki))
      (with-open-file (out (merge-pathnames #p"admin/recent-changes.dat"
					    (cliki-data-directory cliki))
			   :direction :output :if-exists :append
			   :if-does-not-exist :create)
	  (with-standard-io-syntax (print entry out))))))

(defun same-day-p (date1 date2)
  (and date1 date2 
       (let ((start-of-date1
	      (with-date date1 nil
		(encode-universal-time 0 0 0 day-of-month month year))))
	 (<= start-of-date1 date2 (+ start-of-date1 86400)))))

(defun view-recent-changes (request rest-of-url)
  (declare (ignore rest-of-url))
  (let* ((out  (request-stream request))
	 (cliki (request-cliki request))
	 (changes (cliki-recent-changes cliki))
	 (start
	  (parse-integer
	   (or (car (url-query-param (request-url request) "start")) "0")
	   :junk-allowed t))
	 (number 30))
    (request-send-headers request)
    (cliki-page-header cliki request "Recent Changes")
    (if (= start 0)
	(format out
		"<blockquote>This page is now updated automatically.  Look out for RSS exports in the near future too. -- ~A"
		(write-a-href cliki "Daniel Barlow" nil))
	(format out "<p>Older entries (starting at ~D)</p>~%" start))
    (loop for (this-date title user . description)
	  in (subseq changes start
		     (min (+ start number) (length changes)))
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
		       (if title (write-a-href cliki title nil) "?")
                       (car description)
                       (if user (write-a-href cliki user nil)))))
    (princ "<p>" out)
    (print-page-selector out start number (length changes)
			 (format nil "~A?start="
				 (url-path (request-url request))))
    ))

