(defpackage #:hysteresis
  (:use #:cl
        #:hysteresis.history)
  (:export #:set-value
           #:get-value
           #:present
           #:revert
           #:hdefun
           #:hsetq))
(in-package #:hysteresis)

(defparameter *entry-count* 0)
(defparameter *history-length* 30)

(defstruct entry
  id value function)

(defstruct historized-symbol
  (name nil :type symbol)
  (history nil :type history))

(defun make-entry* (obj &optional (type :value))
  (prog1 (make-entry :id *entry-count* type obj)
    (incf *entry-count*)))

(defun add-history-entry (hsym type value)
  (if (null (historized-symbol-history hsym))
      (let ((history (make-history* *history-length*)))
        (setf (historized-symbol-history hsym) history)
        (add-entry (make-entry* value type) history))
      (add-entry (make-entry* value type) (historized-symbol-history hsym))))

(defparameter *symbols* (make-hash-table))

;;; exposed API

(defun set-value (name value &optional (type :value))
  (multiple-value-bind (hsym exists?)
      (gethash name *symbols*)
    (if (null exists?)
        (let ((hsym (make-historized-symbol :name name :history (make-history*))))
          (setf (gethash name *symbols*) hsym)
          (progn
            (add-history-entry hsym type value)
            (values value hsym)))
        (progn
          (add-history-entry hsym type value)
          (values value hsym)))))

(defun get-value (name &optional (type :value))
  (let ((hsym (gethash name *symbols*)))
    (if (null hsym)
        nil
        (let ((present-entry (entry-at-present (historized-symbol-history hsym))))
          (ecase type
            (:value (entry-value present-entry))
            (:function (entry-function present-entry)))))))

(defun (setf get-value) (value name)
  (prog1 (set-value name value)))

(defun present (&optional name)
  (if (null name)
      (maphash (lambda (_ hsym)
                 (declare (ignore _))
                 (move-to-present (historized-symbol-history hsym)))
               *symbols*)
      (move-to-present (historized-symbol-history (gethash name *symbols*)))))

(defun revert (n &optional name)
  (let ((n* (- n)))
    (if (null name)
        (maphash (lambda (_ hsym)
                   (declare (ignore _))
                   (move-to n* (historized-symbol-history hsym)))
                 *symbols*)
        (move-to n* (historized-symbol-history (gethash name *symbols*))))))

;;; interface macros for user

(defmacro hdefun (name lambda-list &body body)
  (let (($value (gensym))
        ($hsym (gensym))
        ($entry (gensym)))
    `(multiple-value-bind (,$value ,$hsym)
         (set-value ',name #'(lambda ,lambda-list ,@body) :function)
       (declare (ignore ,$value))
       (setf (symbol-function ',name)
             (lambda ,lambda-list
               (let ((,$entry (entry-at-present (historized-symbol-history ,$hsym))))
                 (apply (entry-function ,$entry) (list ,@lambda-list))))))))

(defmacro hsetq (name value)
  `(progn
    (define-symbol-macro ,name (get-value ',name))
    (setf (get-value ',name) ,value)))
