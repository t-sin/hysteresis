(defpackage #:hysteresis
  (:use #:cl)
  (:export))
(in-package #:hysteresis)

(defstruct entry
  id value)

(defstruct historized-symbol
  (name :type symbol)
  (history :type vector))

(defun add-history-entry (hsym value)
  (if (null (historized-symbol-history hsym))
      (setf (historized-symbol-history hsym)
            (make-array 1 :fill-pointer 0 :adjustable t
                        :initial-contents (list (make-entry :id 0 :value value))))
      (vector-push-extend (make-entry :id 0 :value value) (historized-symbol-history hsym))))
      

(defparameter *history-length* 10)
(defparameter *history* (make-hash-table))

(defun set-value (name value &optional (history *history*))
  )

(defmacro hdefun (&rest &key args)
  )
