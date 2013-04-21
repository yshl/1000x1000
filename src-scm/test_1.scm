(module test
	(import (lin "./lin.scm")))

(define n 1000)
(define a
  (vector-map
    (lambda(i)
      (let ((ai (make-f64vector n 1.0)))
	(f64vector-set! ai i 1001.0)
	ai))
    (list->vector (iota n))))
(define b (make-f64vector n 1000.0))

(solve a b)

(define (absmax a b)
  (if (not (<= (abs a) (abs b)))
    a b))
(display
  (let loop ((errmax 0.0) (i 0))
    (if (< i n)
      (loop (absmax (- (f64vector-ref b i) 0.5) errmax) (+ i 1))
      errmax)))
(newline)
