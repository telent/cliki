(in-package :cliki)

(defvar *recent-changes-list* (list))
;;; date title user description

#|
(setf *recent-changes-list*
      (list
       (list (- (get-universal-time) 100) "A Page" "anon" "some change")
       (list (- (get-universal-time) 200) "A Pa2ge" "anon" "some change")
       (list (- (get-universal-time) 300) "A Pa3ge" "anon" "some change")
       (list (- (get-universal-time) 400) "A Pa4ge" "anon" "some change")
       (list (- (get-universal-time) 500) "A Pa5ge" "anon" "some change")
       (list (- (get-universal-time) 500) "A Pa6ge" "anon" "some change")
       (list (- (get-universal-time) 600) "A Pa7ge" "anon" "some change")
       (list (- (get-universal-time) 700) "A Pa8ge" "anon" "some change")
       (list (- (get-universal-time) 100000) "A Pa8ge" "anon" "some change")
       (list (- (get-universal-time) 102000) "A Pa8ge" "anon" "some change")
       (list (- (get-universal-time) 105000) "A Pa8ge" "anon" "some change")))
|#       

(defun restore-recent-changes (root &optional max-entries)
  (with-open-file (in (merge-pathnames "admin/recent-changes.dat" root)
		      :direction :input)
    (loop for entry = (read in nil nil)
	  while (and entry (or (not max-entries) (> max-entries 0)))
          if max-entries do (decf max-entries)
          do (cons entry *recent-changes-list*))))


(defun add-recent-change (root date title user &optional description)
  (let ((entry (list date title user description)))
    (push entry *recent-changes-list*)
    (with-open-file (out (merge-pathnames #p"admin/recent-changes.dat" root)
                         :direction :output :if-exists :append
                         :if-does-not-exist :create)
      (print entry out))))

(defun most-recent-change (title)
  (find title *recent-changes-list* :key #'second :test #'equal))

(defun same-day-p (date1 date2)
  (let ((start-of-date1
         (with-date date1
           (encode-universal-time 0 0 0 date month year))))
    (<= start-of-date1 date2 (+ start-of-date1 86400))))

(defun view-recent-changes (request title root)
  (let ((out  (request-stream request))
        (start
         (or (parse-integer
              (or (url-query-param (request-url request) "start") "")
              :junk-allowed t) 0))
        (number
         (or (parse-integer
              (or (url-query-param (request-url request) "number") "")
              :junk-allowed t) 30)))
    (request-send-headers request)
    (format out
            "<html><head><title>Cliki : Recent Changes</title></head>
<link rel=\"stylesheet\" href=\"/dan.css\">
<body>
~/cliki-html:titlebar/
<h1>Recent Changes</h1>~%<blockquote>This page is now updated automatically.  Look out for RSS exports in the near future too. -- ~A"
            request
            (write-a-href "Daniel Barlow" root nil))
    (loop for (this-date title user . description)
             in (subseq *recent-changes-list* start
                        (min (+ start number) (length *recent-changes-list*)))
          and old-date = 0 then this-date
          if description
          unless (same-day-p this-date old-date)
          do (with-date this-date
               (format out
                       "</blockquote>
<a name=~D><h3>~/cliki:dayname/ ~A ~/cliki:monthname/ ~A</h3></a>
<blockquote>"
                       this-date day date month year))
          do (with-date this-date
               (format out "<br> ~D:~2,'0D <b>~A</b> : ~A -- ~A ~%"
                       hour minute (write-a-href title root nil)
                       (car description)
                       (write-a-href user root nil))))
    ;;; should add links to older/newer changes
    ))

