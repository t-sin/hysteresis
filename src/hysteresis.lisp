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

(defparameter *symbols* (make-hash-table))

;;; exposed API

(defun set-value (name value)
  (multiple-value-bind (hsym exists?)
      (gethash name *symbols*)
    (if (null exists?)
        (let ((hsym (make-historized-symbol :name name :history (make-history*))))
          (setf (gethash name *symbols*) hsym)
          (prog1 value
            (add-history-entry hsym value)))
        (prog1 value
            (add-history-entry hsym value)))))

(defun get-value (name)
  (let ((hsym (gethash name *symbols*)))
    (if (null hsym)
        nil
        (let ((present-value (entry-at-present (historized-symbol-history hsym))))
          (entry-value present-value)))))

(defun present (&optional name)
  (if (null name)
      (maphash (lambda (_ hsym)
                 (declare (ignore _))
                 (move-to-present (historized-symbol-history hsym)))
               *symbols*)
      (move-to-present (historized-symbol-history (gethash name *symbols*)))))

(defun revert (n &optional name)
  (if (null name)
      (maphash (lambda (_ hsym)
                 (declare (ignore _))
                 (move-to n (historized-symbol-history hsym)))
               *symbols*)
      (move-to n (historized-symbol-history (gethash name *symbols*)))))

;;; interface macros for user

(defmacro hdefun (&rest args)
  )
