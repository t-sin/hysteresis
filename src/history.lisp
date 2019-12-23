(defpackage #:hysteresis.history
  (:use #:cl)
  (:export #:history
           #:make-history*
           #:history-head
           #:history-tail
           #:history-vector
           #:add-entry
           #:drop-entry
           #:entry-at
           #:entry-at-present
           #:move-to-present
           #:move-to))
(in-package #:hysteresis.history)

(defparameter *maximum-history-length* 10)

(defstruct history
  (head 0 :type number)
  (tail 0 :type number)
  (now 0 :type number)
  (now-modified? nil)
  (vector nil :type simple-vector))

(defun make-history* (&optional vec (len *maximum-history-length*))
  (if (null vec)
      (make-history :vector (make-array len :initial-element nil))
      (let ((initvec (concatenate 'vector vec (make-array (- len (length vec))
                                                          :initial-element nil))))
        (make-history :head (length vec) :tail 0 :now (length vec)
                      :vector (make-array len :initial-contents initvec)))))

(defun add-entry (entry history)
  (let ((len (length (history-vector history))))
    ;; if head catches up with tail in ring buffer
    (if (= (mod (1+ (history-head history)) len)
           (history-tail history))
        (progn
          (unless (history-now-modified? history)
            (setf (history-now history) (history-head history)))
          (setf (aref (history-vector history) (history-head history)) entry)
          (setf (history-head history) (mod (1+ (history-head history)) len))
          (setf (history-tail history) (mod (1+ (history-tail history)) len)))
        (progn
          (unless (history-now-modified? history)
            (setf (history-now history) (history-head history)))
          (setf (aref (history-vector history) (history-head history)) entry)
          (setf (history-head history) (mod (1+ (history-head history)) len))))))

(defun drop-entry (history)
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

(defun entry-at-present (history)
  (elt (history-vector history) (history-now history)))

(defun move-to-present (history)
  (setf (history-now-modified? history) nil)
  (let ((len (length (history-vector history))))
    (setf (history-now history) (mod (1- (history-head history)) len))))

(defun move-to (n history)
  (setf (history-now-modified? history) t)
  (let ((now (history-now history)))
    (cond ((plusp n)
           (when (> (+ now n) (history-head history))
             (error "too large n (~a); it's future." n)))
          ((minusp n)
           (when (< (+ now n) (history-tail history))
             (error "too small n (~a); it's gone in the past." n))))
    (setf (history-now history) (+ now n))))
