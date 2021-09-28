(in-package #:inravina)

(defclass queue ()
  ((head-cons
     :initform nil
     :accessor queue-head-cons)
   (tail-cons
     :initform nil
     :accessor queue-tail-cons)))

(defun head (queue &aux (head-cons (queue-head-cons queue)))
  (values (car head-cons) (and head-cons t)))

(defun tail (queue &aux (tail-cons (queue-tail-cons queue)))
  (values (car tail-cons) (and tail-cons t)))

(defun push-tail (queue item)
  (with-slots (head-cons tail-cons) queue
    (let ((old-tail-cons tail-cons))
      (setf tail-cons (list item))
      (if old-tail-cons
        (setf (cdr old-tail-cons) tail-cons)
        (setf head-cons tail-cons))))
  item)

(defun pop-head (queue)
  (with-slots (head-cons tail-cons) queue
    (prog1
      (pop head-cons)
      (unless head-cons
        (setf tail-cons nil)))))

(defun queue-empty-p (queue)
  (null (queue-head-cons queue)))

