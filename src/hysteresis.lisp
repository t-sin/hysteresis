(defpackage #:hysteresis
  (:use #:cl
        #:hysteresis.history)
  (:export))
(in-package #:hysteresis)

(defparameter *entry-count* 0)
(defparameter *history-length* 10)

(defstruct entry
  id value)

(defstruct historized-symbol
  (name nil :type symbol)
  (history nil :type history))

(defun make-entry* (value)
  (prog1 (make-entry :id *entry-count* :value value)
    (incf *entry-count*)))

(defun add-history-entry (hsym value)
  (if (null (historized-symbol-history hsym))
      (setf (historized-symbol-history hsym)
            (make-array 1 :fill-pointer 0 :adjustable t
                        :initial-contents (list (make-entry* value))))
      (vector-push-extend (make-entry* value) (historized-symbol-history hsym))))
      


(defun set-value (name value &optional (history *history*))
  )

(defmacro hdefun (&rest args)
  )
