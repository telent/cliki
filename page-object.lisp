(in-package :cliki)

(defclass page-object ()
  ((pathname :initarg :pathname :accessor page-pathname)
   (view-url :initarg :view-url :accessor page-view-url)
   (edit-url :initarg :edit-url :accessor page-edit-url)
   (delete-url :initarg :delete-url :accessor page-delete-url)         
   (title :initarg :title :reader page-title)))

(defvar *all-pages* (make-hash-table)
  "Hash table of all pages, keyed on page name as interned into CLIKI-PAGES")
(defmethod add-page ((page page-object))
  (setf (gethash (intern (page-title page) "CLIKI-PAGES") *all-pages*) page))
(defun find-page (title)
  (gethash (intern title  "CLIKI-PAGES") *all-pages*))

;;; now all of a sudden this class has to know about all of its subclasses.
;;; at some stage we'll include a way for new subclasses to register their
;;; filetype here
(defun class-for-type (type)
  (cond ((string-equal type "phtml") 'phtml-page-object)
        (t 'page-object)))
  
(defun make-page (title base-url directory)
  (or
   (find-page title)
   (let* ((wildpath (merge-pathnames
                     (make-pathname :type :wild)
                     (merge-pathnames title directory)))
          (view-url (merge-url base-url (urlstring-escape title)))
          (edit-url (merge-url (merge-url base-url "edit/")
                               (urlstring-escape title)))
          (delete-url (merge-url (merge-url base-url "delete/")
                                 (urlstring-escape title)))
          (real-name (elt (directory wildpath) 0)))
     (add-page (make-instance
                (class-for-type (pathname-type real-name))
                :title title :pathname real-name :view-url view-url
                :edit-url edit-url :delete-url delete-url)))))

(defgeneric view-page ((page page-object) request))
(defgeneric make-edit-form ((page page-object) request))
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
   (let* ((old-path (page-pathname page))
          (old-url (page-view-url page))
          (old-dir
           (merge-pathnames
            (make-pathname :type :wild :version :wild :name :wild) old-path)))
     (rename-file (page-pathname page) (make-pathname :name title))
     (delete-page page)
     (make-page title old-url old-dir))
   (file-error (c) (declare (ignorable c)) nil)))

(defun view-create-page-menu (request title)
  (request-send-headers request)
  (format (request-stream request)
          "<h1>Unimplemented</h1>
Sorry.  You can't create new pages yet."))

