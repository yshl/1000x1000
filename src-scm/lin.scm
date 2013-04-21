(module lin
	(export solve))

(define v@ f64vector-ref)
(define v! f64vector-set!)
(define (v-! vec i factor)
  (f64vector-set! vec i (- (v@ vec i) factor)))
(define (v*! vec i factor)
  (f64vector-set! vec i (* (v@ vec i) factor)))
(define (vswap! vec i j)
  (let ((tmp (v@ vec i)))
    (v! vec i (v@ vec j))
    (v! vec j tmp)))

(define av@ vector-ref)
(define av! vector-set!)
(define (a@ a i j)
  (f64vector-ref (av@ a i) j))
(define (a! a i j value)
  (f64vector-set! (av@ a i) j value))
(define (a-! a i j value)
  (v-! (av@ a i) j value))
(define (a*! a i j value)
  (v*! (av@ a i) j value))
(define (aswap! a i j)
  (let ((tmp (av@ a i)))
    (av! a i (av@ a j))
    (av! a j tmp)))

(define (vlen vec)
  (cond ((f64vector? vec) (f64vector-length vec))
	((vector? vec) (vector-length vec))))

(define (min-length vec1 . vecs)
  (define (min-length-1 accum vecs)
    (if (eq? vecs '())
      accum
      (min-length-1 (min accum (vlen (car vecs)))
		    (cdr vecs))))
  (min-length-1 (vlen vec1) vecs))

(define (foreach-from f from . vecs)
  (let ((n (apply min-length vecs)))
    (let loop ((i from))
      (when (< i n)
	(f i)
	(loop  (+ i 1))))))

(define (fold-from f init from . vecs)
  (let ((n (apply min-length vecs)))
    (let loop ((accum init)(i from))
      (if (< i n)
	(loop (f i accum) (+ i 1))
	accum))))

(define (foreach-right f . vecs)
  (let loop ((i (- (apply min-length vecs) 1)))
    (when (>= i 0)
      (f i)
      (loop (- i 1)))))

(define (abs-max ai)
  (fold-from
    (lambda(j ajmax)
      (max ajmax (abs (v@ ai j))))
    0.0 0 ai))

(define (scale-array a b)
  (foreach-from
    (lambda(i)
      (let* ((ai (av@ a i))
	     (factor (/ 1.0 (abs-max ai))))
	(foreach-from
	  (lambda(j)
	    (v*! ai j factor))
	  0 ai)
	(v*! b i factor)))
    0 a))

(define (search-pivot a i)
  (car (fold-from
	 (lambda(j ajimax)
	   (let ((aji (abs (a@ a j i))))
	     (if (> aji (cdr ajimax))
	       (cons j aji)
	       ajimax)))
	 (cons -1 0.0) i a)))

(define (pivot a b i)
  (let ((maxj (search-pivot a i)))
    (unless (= i maxj)
      (aswap! a i maxj)
      (vswap! b i maxj))))

(define (scale-pivot ai b i)
  (let ((factor (/ 1.0 (v@ ai i))))
    (foreach-from
      (lambda(j)
	(v*! ai j factor))
      (+ i 1) ai)
    (v*! b i factor)))

(define (elim-col a b i)
  (let ((ai (av@ a i))
	(bi (v@ b i)))
    (foreach-from
      (lambda(j)
	(let* ((aj (av@ a j))
	       (aji (a@ a j i)))
	  (foreach-from
	    (lambda(k)
	      (v-! aj k (* aji (v@ ai k))))
	    (+ i 1) aj ai)
	  (v-! b j (* aji bi))))
      (+ i 1) a b)))

(define (forward-elimination a b)
  (foreach-from
    (lambda(i)
      (pivot a b i)
      (scale-pivot (av@ a i) b i)
      (elim-col a b i))
    0 a b))

(define (back-substitution a b)
  (foreach-right
    (lambda(i)
      (foreach-from
	(lambda(j)
	  (v-! b i (* (a@ a i j) (v@ b j))))
	(+ i 1) (av@ a i) b))
    a b))

(define (solve a b)
  (scale-array a b)
  (forward-elimination a b)
  (back-substitution a b))
