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
      (let ((history (make-history* *history-length*)))
        (setf (historized-symbol-history hsym) history)
        (add-entry (make-entry* value) history))
      (add-entry (make-entry* value) (historized-symbol-history hsym))))

(defun revert-history (n hsym)
  )

(defparameter *symbols* (make-hash-table))

(defun set-value (name value &optional (history *history*))
  )

(defmacro hdefun (&rest args)
  )
