(defun main ()
  (let* ((N 1000)
	 (a (make-array `(,N ,N) :initial-element 1.0d0
			:element-type 'double-float))
	 (b (make-array `(,N) :initial-element 1000.0d0
			:element-type 'double-float))
	 (errmax 0.0d0))
    (dotimes (i N)
      (setf (aref a i i) 1001.0d0))
    (solve a b)
    (dotimes (i N)
      (if (not (<= (abs (- (aref b i) 0.5d0)) (abs errmax)))
	(setq errmax (- (aref b i) 0.5d0))))
    (format t "~,6g~%" errmax)
    (if (not (<= (abs errmax) 1.0d-8))
      (princ "Large error"))))

(main)
(quit)
