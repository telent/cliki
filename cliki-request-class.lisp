(in-package :cliki)

(defclass cliki-request (request)
  ((cliki :accessor request-cliki :initarg :cliki)))
