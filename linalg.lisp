;;
;; linalg
;;


(defpackage #:linalg
  (:use :cl)
  (:export #:our-matrix
	   #:matrix-at
	   #:|setf matrix-at|
	   #:copy-matrix
	   #:transposed
	   #:matrix-print))
  

(in-package #:linalg)

(defclass our-matrix ()
  ((rows
    :initarg :rows
    :initform (error ":rows must be specified.")
    :reader matrix-rows)
   (cols
    :initarg :cols
    :initform (error ":columns must be specified.")
    :reader matrix-cols)
   (data
    :initarg :data
    :accessor matrix-data)))

(defmethod initialize-instance :after ((m our-matrix) &key generator)
  (assert (< 0 (matrix-rows m))
	  nil
	  ":rows must be greater than 0.")
  (assert (< 0 (matrix-cols m))
	  nil
	  ":cols must be greater than 0.")
  (if (slot-boundp m 'data)
      (progn
	(assert (= (length (matrix-data m)) (* (matrix-rows m) (matrix-cols m)))
		nil
		":data dimension should be ~d."
		(* (matrix-rows m) (matrix-cols m)))
	(assert (not generator)
		nil
		":data and :generator may not be specified at the same time."))
      (if (functionp generator)
	  (progn
	    (setf (matrix-data m)
		  (make-array (* (matrix-rows m) (matrix-cols m)) :element-type 'single-float))
	    (dotimes (i (matrix-rows m) m)
	      (dotimes (j (matrix-cols m))
		(setf (matrix-at m i j)
		      (funcall generator i j)))))
	  (progn
	    (setf (matrix-data m)
		  (make-array (* (matrix-rows m) (matrix-cols m))
			      :element-type 'single-float
			      :initial-element 0.0))
	    m))))

(defun matrix-at (m i j)
  (aref (matrix-data m) (+ (* i (matrix-cols m)) j)))

(defun (setf matrix-at) (value m i j)
  (setf (aref (matrix-data m) (+ (* i (matrix-cols m)) j)) value))

(defun copy-matrix (m)
  (make-instance 'our-matrix
		 :rows (matrix-rows m)
		 :cols (matrix-cols m)
		 :data (copy-seq (matrix-data m))))

(defun tranposed (m)
  (make-instance 'our-matrix
		 :rows (matrix-cols m)
		 :cols (matrix-rows m)
		 :generator #'(lambda (i j) (matrix-at m j i))))

(defun matrix-print (m)
  (dotimes (i (matrix-rows m) nil)
    (dotimes (j (matrix-cols m))
      (format t "~7,2f " (matrix-at m i j)))
    (terpri)))

(defmacro domatrix ((m i j &optional elt) &body body)
  `(dotimes (,i (matrix-rows ,m) ,m)
     (dotimes (,j (matrix-cols ,m))
       ,@(if elt
	     `((symbol-macrolet ((,elt (matrix-data ,m ,i ,j)))
		 ,@body))
	     body))))
       


(defmethod m* ((a our-matrix) (b our-matrix))
  (assert (= (matrix-cols a) (matrix-rows b)))
  (let ((result (make-instance 'our-matrix
			       :rows (matrix-rows a)
			       :cols (matrix-cols b))))
    (domatrix (result i j elt)
	      (dotimes (k (matrix-cols a))
		(incf elt (* (matrix-at a i k) (matrix-at b k j)))))))

(defmethod m* ((m our-matrix) (r real))
  (make-instance 'our-matrix
		 :rows (matrix-rows m)
		 :cols (matrix-cols m)
		 :data (map 'vector #'(lambda (i) (i r)) (matrix-data m))))

(defmethod m* ((r real) (m our-matrix))
  (m* m s))

	
    
  
