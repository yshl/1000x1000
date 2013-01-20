(define (foreach from to proc)
  (if (< from to)
    (begin (proc from)
	   (foreach (+ from 1) to proc))))

(define (foreach-rev from to proc)
  (if (> from to)
    (begin (proc from)
	   (foreach-rev (- from 1) to proc))))

(define (foreach-fold from to accum proc)
  (if (< from to)
    (foreach-fold (+ from 1) to (proc from accum) proc)
    accum))

(define (mat-ref a i j)
  (vector-ref (vector-ref a i) j))

(define (swap vec i j)
  (let ((tmp (vector-ref vec i)))
    (vector-set! vec i (vector-ref vec j))
    (vector-set! vec j tmp)))

(define (absmax ai j)
  (let ((n (vector-length ai))
	(ajimax (abs (vector-ref ai j))))
    (foreach-fold (+ j 1) n (abs (vector-ref ai j))
		  (lambda (k ajimax)
		    (max ajimax (abs (vector-ref ai k)))))))

(define (search-pivot a i)
  (let ((n (vector-length a)))
    (car (foreach-fold (+ i 1) n (list i (abs (mat-ref a i i)))
		       (lambda (j ajimax)
			 (let ((aji (abs (mat-ref a j i))))
			   (if (> aji (cadr ajimax))
			     (list j aji)
			     ajimax)))))))

(define (pivot a b i)
  (let ((maxj (search-pivot a i)))
    (if (not (= i maxj))
      (begin (swap a i maxj)
	     (swap b i maxj)))))

(define (mul-set! vec i factor)
  (vector-set! vec i (* (vector-ref vec i) factor)))

(define (vector-mul-set! vec from to factor)
  (foreach from to
	   (lambda (i)
	     (mul-set! vec i factor))))

(define (scale-pivot ai b i)
  (let ((n (vector-length ai))
	(factor (/ 1.0 (vector-ref ai i))))
    (vector-mul-set! ai (+ i 1) n factor)
    (mul-set! b i factor)))

(define (scale-array a b)
  (let ((n (vector-length a)))
    (foreach 0 n
	     (lambda(i)
	       (let* ((ai (vector-ref a i))
		      (factor (/ 1.0 (absmax ai 0))))
		 (vector-mul-set! ai 0 n factor)
		 (mul-set! b i factor))))))

(define (sub-setf! vec i fac)
  (vector-set! vec i (- (vector-ref vec i) fac)))

(define (vector-sub-setf! vec1 vec2 fac from to)
  (foreach from to
	   (lambda(i)
	     (sub-setf! vec1 i (* (vector-ref vec2 i) fac)))))

(define (elim-col a b i)
  (let ((n (vector-length a))
	(ai (vector-ref a i)))
    (foreach (+ i 1) n
	     (lambda(j)
	       (let* ((aj (vector-ref a j))
		      (aji (vector-ref aj i)))
		 (vector-sub-setf! aj ai aji (+ i 1) n)
		 (sub-setf! b j (* aji (vector-ref b i))))))))

(define (forward-elimination a b)
  (let ((n (vector-length a)))
    (foreach 0 n
	     (lambda(i) (pivot a b i)
			(scale-pivot (vector-ref a i) b i)
			(elim-col a b i)))))

(define (dot vec1 vec2 from to)
  (foreach-fold from to 0.0
		(lambda(i sum)
		  (+ sum (* (vector-ref vec1 i)
			    (vector-ref vec2 i))))))

(define (back-substitution a b)
  (let ((n (vector-length a)))
    (foreach-rev (- n 1) -1
		 (lambda(i)
		   (let ((ai (vector-ref a i)))
		     (sub-setf! b i (dot ai b (+ i 1) n)))))))

(define (solve a b)
  (scale-array a b)
  (forward-elimination a b)
  (back-substitution a b))
