(in-package :cliki)

(defclass cliki-instance (araneida::dispatching-handler)
  ;; caller should initialize data-directory, title, url-root; the
  ;; rest will be sorted out internally
  ((data-directory :accessor cliki-data-directory :initarg :data-directory)
   (title :reader cliki-title :initarg :title :initform "CLiki")
   ;; XXX is this still needed?
   (url-root :accessor cliki-url-root :initarg :url-root)
   (recent-changes :accessor cliki-recent-changes :initform (list))
   (idf)
   (pages :accessor cliki-pages :initform (make-hash-table :test 'equal))
   (short-forms :accessor cliki-short-forms
		:initarg :short-forms
		:initform '((#\_ :link)
			    (#\* :topic)
			    (#\/ :legacy-search)
			    (#\# ((#\H :clhs)))
			    (#\> :download)
			    (#\: :long-form)))))


