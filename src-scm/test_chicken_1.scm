(use srfi-4)
(declare (uses lin_chicken))

(define n 1000)
(define a (make-vector n))
(let loop ((i 0))
  (when (< i n)
    (let ((ai (make-f64vector n 1.0)))
      (f64vector-set! ai i 1001.0)
      (vector-set! a i ai))
    (loop (+ i 1))))

(define b (make-f64vector n 1000.0))

(solve a b)

(define (amax a b)
  (if (not (<= (abs a) (abs b)))
    a b))
(display
  (let loop ((errmax 0.0) (i 0))
    (if (< i n)
      (loop (amax (- (f64vector-ref b i) 0.5) errmax) (+ i 1))
      errmax)))
(newline)
