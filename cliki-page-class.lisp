(in-package :cliki)

;;; XXX should initialize some of these to e.g. (list) or (make-hash-table)


(defclass cliki-page ()
  ((title :accessor page-title :initarg :title)
   (names :accessor page-names :initarg :names)
   (cliki :accessor page-cliki :initarg :cliki)
   (indices :accessor page-indices :initarg :indices :initform nil)
   (pathname :accessor page-pathname :initarg :pathname)
   (backlinks :accessor page-backlinks :initform nil)
   (topics :accessor page-topics :initform nil)
   ))
