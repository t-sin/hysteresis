(defpackage #:hysteresis
  (:use #:cl)
  (:export))
(in-package #:hysteresis)

(defparameter *maximum-history-length* 10)

(defstruct history
  (head 0 :type number)
  (tail 0 :type number)
  (vector nil :type simple-vector))

(defun make-history* (&optional vec (len *maximum-history-length*))
  (if (null vec)
      (make-history :vector (make-array len :initial-element nil))
      (let ((initvec (concatenate 'vector vec (make-array (- len (length vec))
                                                          :initial-element nil))))
        (make-history :head (length vec) :tail 0
                      :vector (make-array len :initial-contents initvec)))))

(defun push-entry (entry history)
  (let ((len (length (history-vector history))))
    (if (= (mod (1+ (history-head history)) len)
           (history-tail history))
        (setf (aref (history-vector history) (history-head history)) entry
              (history-head history) (mod (1+ (history-head history)) len)
              (history-tail history) (mod (1+ (history-tail history)) len))
        (setf (aref (history-vector history) (history-head history)) entry
              (history-head history) (mod (1+ (history-head history)) len)))))

(defun pop-entry (history)
  (let ((len (length (history-vector history))))
    (if (= (history-head history) (history-tail history))
        (if (null (aref (history-vector history) (history-head history)))
            (error "history is empty.")
            (setf (aref (history-vector history) (history-tail history)) nil))
        (progn
          (setf (aref (history-vector history) (history-tail history)) nil
                (history-tail history) (mod (1+ (history-tail history)) len))))))

(defun entry-at (n history)
  (when (not (= (history-head history) (history-tail history)))
    (aref (history-vector history)
          (mod (- (history-head history) (1+ n)) (length (history-vector history))))))

(defstruct entry
  id value)

(defstruct hystorized-symbol
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
      

(defparameter *history-length* 10)
(defparameter *history* (make-hash-table))

(defun set-value (name value &optional (history *history*))
  )

(defmacro hdefun (&rest &key args)
  )
