(include "./lin.scm")
(define n 1000)
(define a (make-vector n))
(foreach 0 n
	 (lambda (i)
	   (let ((ai (make-vector n 1.0)))
	     (vector-set! ai i 1001.0)
	     (vector-set! a i ai))))
(define b (make-vector n 1000.0))
(solve a b)
(display
  (foreach-fold 0 n 0.0
		(lambda (i errmax)
		  (let ((bi-error (- (vector-ref b i) 0.5)))
		    (if (not (<= (abs bi-error) (abs errmax))) bi-error errmax)))))
(newline)
