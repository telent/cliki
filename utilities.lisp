(in-package :cliki)

(defun integer-for (thing &key (default 0) (start 0) end)
  (or (parse-integer (or thing "") :start start :end end :junk-allowed t) 
      default))

(defun string-prefix-p (short long)
  (let ((m (mismatch short long)))
    (or (not m) (= m (length short)))))