(in-package :cliki)

(defclass page-object ()
  ((title :initarg :title :reader page-title)))

(defmethod page-pathname ((page page-object))
  (merge-pathnames
   (make-pathname :name (page-title page))
   *page-pathname-defaults*))

(defparameter *all-pages* (make-hash-table)
  "Hash table of all pages, keyed on page name as interned into CLIKI-PAGES")
(defmethod add-page ((page page-object))
  (setf (gethash (intern (page-title page) "CLIKI-PAGES") *all-pages*) page))
(defun find-page (title)
  (gethash (intern title  "CLIKI-PAGES") *all-pages*))

(defun make-page (title)
  (or (find-page title)
      (add-page (make-instance 'page-object :title title))))

(defmethod view-page ((page page-object) request)
  (let ((out (request-stream request))
        (file (page-pathname page))
        (title (page-title page)))
    (request-send-headers request)
    (format out
            "<html><head><title>Cliki : ~A</title></head>
<body><h1>~A</h1>~%" title title)
    (if (probe-file file)
        (with-open-file (in file :direction :input)
          (if (ignore-errors (peek-char nil in nil))
              (write-stream-to-stream (request-base-url request) in out))
          (format out "<hr><a href=\"~A?edit\">Edit this page</a>"
                  (urlstring (page-url page))))
      (format out
              "internal error: can't find file ~A"
              file))))






(defgeneric view-page ((page page-object) request))
(defgeneric make-edit-form ((page page-object) request))
(defmethod make-edit-form ((page (eql 'page-object)) request)
  (let* ((title
          (pathname-name (parse-namestring
                          (urlstring-unescape (request-path-info request)))))
         (out (request-stream request)))
    (format out
            "<html><head><title>Cliki : Create ``~A''</title></head>
<body><h1>Create ``~A''</h1>
<p>You may choose the type of page to create, from <ul>"
            title title )
    (dolist (ty *classes-for-types*)
      (format out "<li> <a href=\"~A.~A\">~A</a></li>~%"
              (request-path-info request) (car ty) (cdr ty)))
    (format out "</ul></body>~%")))
    
                    
(defgeneric process-post-request ((page page-object) request))
(defgeneric search-relevance ((page page-object) search-criterion))

(defgeneric delete-page ((page page-object)))
(defmethod delete-page ((page page-object))
  (remhash (intern (page-title page) "CLIKI-PAGES") *all-pages*)
  (ignore-errors (delete-file (page-pathname page))))

(defgeneric (setf page-title) (title (page page-object)))

(defmethod (setf page-title) (title (page page-object))
  ;; really should lock around this to provide some measure of
  ;; atomicity
  (handler-case
   (let ((old-url (page-view-url page))
         (old-dir (page-pathname-defaults page)))
     (rename-file (page-pathname page) (make-pathname :name title))
     (delete-page page)
     ;; old-url ends in a non-directory component; we ignore that bit
     ;; cos make-page only wants the directory part
     (make-page title old-url old-dir))
   (file-error (c) (declare (ignorable c)) nil)))

(defun view-create-page-menu (request title)
  (request-send-headers request)
  (format (request-stream request)
          "<h1>Unimplemented</h1>
Sorry.  You can't create new pages yet."))

