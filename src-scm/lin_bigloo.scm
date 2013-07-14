(module lin_bigloo (export solve))

(define (f64vector-fold proc init vec)
  (let ((n (f64vector-length vec)))
    (let loop ((accum init) (i 0))
      (if (<fx i n)
	(loop (proc (f64vector-ref vec i) accum) (+ i 1))
	accum))))
(define (vector-swap! vec i j)
  (let ((vi (vector-ref vec i))
	(vj (vector-ref vec j)))
    (vector-set! vec i vj)
    (vector-set! vec j vi)))
(define (vector-map! proc vec)
  (let ((n (vector-length vec)))
    (let loop ((i 0))
      (when (<fx i n)
	(vector-set! vec i (proc i (vector-ref vec i)))
	(loop (+fx i 1))))
    vec))

(define (v@ vec i) (f64vector-ref vec i))
(define (v! vec i val) (f64vector-set! vec i val))
(define (v-! vec i factor)
  (f64vector-set! vec i (-fl (v@ vec i) factor)))
(define (v*! vec i factor)
  (f64vector-set! vec i (*fl (v@ vec i) factor)))
(define (vswap! vec i j)
  (let ((tmp (v@ vec i)))
    (v! vec i (v@ vec j))
    (v! vec j tmp)))

(define (av@ arr i) (vector-ref arr i))
(define (av! arr i ai) (vector-set! arr i ai))
(define (a@ a i j)
  (f64vector-ref (av@ a i) j))
(define (a! a i j value)
  (f64vector-set! (av@ a i) j value))
(define (a-! a i j value)
  (v-! (av@ a i) j value))
(define (a*! a i j value)
  (v*! (av@ a i) j value))

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
      (when (<fx i n)
	(f i)
	(loop  (+fx i 1))))))

(define (fold-from f init from . vecs)
  (let ((n (apply min-length vecs)))
    (let loop ((accum init)(i from))
      (if (<fx i n)
	(loop (f i accum) (+fx i 1))
	accum))))

(define (foreach-right f . vecs)
  (let loop ((i (- (apply min-length vecs) 1)))
    (when (>=fx i 0)
      (f i)
      (loop (-fx i 1)))))

(define (f64vector-map! proc vec)
  (let ((n (f64vector-length vec)))
    (let loop ((i 0))
      (when (<fx i n)
	(f64vector-set! vec i (proc (f64vector-ref vec i)))
	(loop (+fx i 1))))
    vec))

(define (abs-max ai)
  (f64vector-fold (lambda(aij ajmax) (max ajmax (abs aij))) 0.0 ai))

(define (scale-array a b)
  (vector-map!
    (lambda(i ai)
      (let ((factor (/fl 1.0 (abs-max ai))))
	(v*! b i factor)
	(f64vector-map! (lambda(aij) (*fl aij factor)) ai)))
    a))

(define (search-pivot a i)
  (car (fold-from
	 (lambda(j ajimax)
	   (let ((aji (abs (a@ a j i))))
	     (if (>fl aji (cdr ajimax))
	       (cons j aji)
	       ajimax)))
	 (cons -1 0.0) i a)))

(define (pivot a b i)
  (let ((maxj (search-pivot a i)))
    (unless (= i maxj)
      (vector-swap! a i maxj)
      (vswap! b i maxj))))

(define (scale-pivot ai b i)
  (let ((factor (/fl 1.0 (v@ ai i))))
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
	      (v-! aj k (*fl aji (v@ ai k))))
	    (+ i 1) aj ai)
	  (v-! b j (*fl aji bi))))
      (+ i 1) a b)))

(define (forward-elimination a b)
  (let ((n (vector-length a)))
    (let loop ((i 0))
      (when (< i n)
	(pivot a b i)
	(scale-pivot (av@ a i) b i)
	(elim-col a b i)
	(loop (+ i 1))))))

(define (back-substitution a b)
  (foreach-right
    (lambda(i)
      (foreach-from
	(lambda(j)
	  (v-! b i (*fl (a@ a i j) (v@ b j))))
	(+ i 1) (av@ a i) b))
    a b))

(define (solve a b)
  (scale-array a b)
  (forward-elimination a b)
  (back-substitution a b))
