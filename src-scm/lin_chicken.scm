(declare (unit lin_chicken)
	 (export solve))

(use srfi-1)
(use srfi-4)
(use srfi-4-utils)
(use vector-lib)

(define (v-! vec i factor)
  (f64vector-set! vec i (fp- (f64vector-ref vec i) factor)))
(define (v*! vec i factor)
  (f64vector-set! vec i (fp* (f64vector-ref vec i) factor)))
(define (vswap! vec i j)
  (let ((tmp (f64vector-ref vec i)))
    (f64vector-set! vec i (f64vector-ref vec j))
    (f64vector-set! vec j tmp)))

(define (a@ a i j)
  (f64vector-ref (vector-ref a i) j))

(define (vlen vec)
  (cond ((f64vector? vec) (f64vector-length vec))
	((vector? vec) (vector-length vec))
	(else #f)))

(define (min-length . vecs)
  (apply min (map vlen vecs)))

(define (abs-max ai)
  (let ((n (f64vector-length ai)))
    (let loop ((ajmax 0.0) (j 0))
      (if (fx< j n)
	(loop (max ajmax (abs (f64vector-ref ai j))) (fx+ j 1))
	ajmax))))

(define (scale-row factor ai from to)
  (do ((j from (fx+ j 1))) ((fx>= j to))
    (v*! ai j factor)))

(define (scale-array a b)
  (let ((n (min-length a b)))
    (do ((i 0 (fx+ i 1))) ((fx>= i n))
      (let* ((ai (vector-ref a i))
	     (factor (fp/ 1.0 (abs-max ai))))
	(scale-row factor ai 0 n)
	(v*! b i factor)))))

(define (search-pivot a i)
  (let ((n (vector-length a)))
    (let loop ((ajimax (abs (a@ a i i))) (jmax i) (j (fx+ i 1)))
      (if (fx< j n)
	(let ((aji (abs (a@ a j i))))
	  (if (fp> aji ajimax)
	    (loop aji j (fx+ j 1))
	    (loop ajimax jmax (fx+ j 1))))
	jmax))))

(define (pivot a b i)
  (let ((maxj (search-pivot a i)))
    (unless (= i maxj)
      (vector-swap! a i maxj)
      (vswap! b i maxj))))

(define (scale-pivot ai b i iend)
  (let ((factor (fp/ 1.0 (f64vector-ref ai i))))
    (scale-row factor ai (fx+ i 1) iend)
    (v*! b i factor)))

(define (sub-y-ax y a x from to)
  (do ((i from (fx+ i 1))) ((fx>= i to))
    (v-! y i (fp* a (f64vector-ref x i)))))

(define (update-lower-col a b i blockend)
  (let ((n (min-length a b)))
    (do ((i1 i (fx+ i1 1))) ((fx>= i1 blockend))
      (pivot a b i1)
      (let ((ai (vector-ref a i1)))
	(scale-pivot ai b i1 blockend)
	(do ((j (fx+ i1 1) (fx+ j 1))) ((fx>= j n))
	  (let* ((aj (vector-ref a j))
		 (aji (f64vector-ref aj i1)))
	    (sub-y-ax aj aji ai (fx+ i1 1) blockend)
	    (v-! b j (fp* aji (f64vector-ref b i1)))))))))

(define (update-upper-row a i blockend)
  (let ((n (vector-length a)))
    (do ((i1 i (fx+ i1 1))) ((fx>= i1 blockend))
      (let ((ai (vector-ref a i1)))
	(scale-row (fp/ 1.0 (f64vector-ref ai i1)) ai blockend n)
	(do ((j (fx+ i1 1) (fx+ j 1))) ((fx>= j blockend))
	  (let* ((aj (vector-ref a j))
		 (aji (f64vector-ref aj i1)))
	    (sub-y-ax aj aji ai blockend n)))))))

(define (update-a a i blockend)
  (let ((n (vector-length a))
	(blocksize 8))
    (do ((i1 blockend (fx+ i1 1))) ((fx>= i1 n))
      (let ((ai (vector-ref a i1)))
	(do ((j i (fx+ j blocksize))) ((fx> (fx+ j blocksize) blockend))
	  (let ((aij0 (f64vector-ref ai (fx+ j 0)))
		(aij1 (f64vector-ref ai (fx+ j 1)))
		(aij2 (f64vector-ref ai (fx+ j 2)))
		(aij3 (f64vector-ref ai (fx+ j 3)))
		(aij4 (f64vector-ref ai (fx+ j 4)))
		(aij5 (f64vector-ref ai (fx+ j 5)))
		(aij6 (f64vector-ref ai (fx+ j 6)))
		(aij7 (f64vector-ref ai (fx+ j 7)))
		)
	    (do ((k blockend (fx+ k 1))) ((fx>= k n))
	      (v-! ai k (+ (fp* aij0 (a@ a (fx+ j 0) k))
			   (fp* aij1 (a@ a (fx+ j 1) k))
			   (fp* aij2 (a@ a (fx+ j 2) k))
			   (fp* aij3 (a@ a (fx+ j 3) k))
			   (fp* aij4 (a@ a (fx+ j 4) k))
			   (fp* aij5 (a@ a (fx+ j 5) k))
			   (fp* aij6 (a@ a (fx+ j 6) k))
			   (fp* aij7 (a@ a (fx+ j 7) k))
			   )))))
	(let ((jbegin (fx- blockend (modulo (fx- blockend i) blocksize))))
	  (do ((j jbegin (fx+ j 1))) ((fx>= j blockend))
	    (let ((aij (f64vector-ref ai j))
		  (aj (vector-ref a j)))
	      (sub-y-ax ai aij aj blockend n))))))))

(define (forward-elimination a b)
  (let ((n (vector-length a))
	(blocksize 8))
    (do ((i 0 (fx+ i blocksize))) ((fx>= i n))
      (let ((blockend (min n (fx+ i blocksize))))
	(update-lower-col a b i blockend)
	(update-upper-row a i blockend)
	(update-a a i blockend)))))

(define (dot-prod a b from to)
  (let loop ((sum 0.0) (i from))
    (if (fx< i to)
      (loop (fp+ sum (fp* (f64vector-ref a i) (f64vector-ref b i)))
	    (fx+ i 1))
      sum)))

(define (back-substitution a b)
  (let ((n (min-length a b)))
    (do ((i (fx- n 1) (fx- i 1))) ((fx< i 0))
      (v-! b i (dot-prod (vector-ref a i) b (fx+ i 1) n)))))

(define (solve a b)
  (scale-array a b)
  (forward-elimination a b)
  (back-substitution a b))
