(in-package :cliki)

(defclass cliki-instance ()
  ;; caller should initialize data-directory and url-root; the rest will
  ;; be sorted out internally
  ((data-directory :accessor cliki-data-directory :initarg :data-directory)
   (title :reader cliki-title :initarg :title :initform "CLiki")
   (url-root :accessor cliki-url-root :initarg :url-root)
   (recent-changes :accessor cliki-recent-changes :initform (list))
   (idf)
   (handlers  :accessor cliki-handlers :initform (list nil))
   (pages :accessor cliki-pages :initform (make-hash-table :test 'equal))))


