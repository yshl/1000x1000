(defmacro swap (a b)
  `(let ((tmp ,a))
     (setf ,a ,b)
     (setf ,b tmp)))

(defun absmax (a i j)
  (declare (type (simple-array double-float (* *)) a)
	   (type fixnum i j))
  (let ((N (array-dimension a 1)))
    (loop for k fixnum from j below N maximize
	  (abs (aref a i k)))))

(defun pivot (a b i)
  (declare (type (simple-array double-float(* *)) a)
	   (type (simple-array double-float(*)) b)
	   (type fixnum i))
  (let ((N (array-dimension a 1))
	(ajimax (abs (aref a i i)))
	(maxj i))
    (loop for j fixnum from (1+ i) below N do
	  (let ((aji (abs (aref a j i))))
	    (if (> aji ajimax)
	      (setq ajimax aji
		    maxj j))))
    (if (/= i maxj)
      (progn
	(loop for j fixnum from i below N do
	      (swap (aref a i j) (aref a maxj j)))
	(swap (aref b i) (aref b maxj))))))

(defmacro mul-setf (a b)
  `(setf ,a (* ,a ,b)))

(defun scale-pivot (a b i blockend)
  (declare (type (simple-array double-float (* *)) a)
	   (type (simple-array double-float (*)) b)
	   (type fixnum i blockend))
  (let ((factor (/ 1.0d0 (aref a i i))))
    (loop for j fixnum from (1+ i) below blockend do
	  (mul-setf (aref a i j) factor))
    (mul-setf (aref b i) factor)))

(defun scale-array (a b)
  (declare (type (simple-array double-float (* *)) a)
	   (type (simple-array double-float (*)) b))
  (let ((N (array-dimension a 1)))
    (loop for i fixnum from 0 below N do
	  (let ((factor (/ 1.0d0 (absmax a i 0))))
	    (loop for j fixnum from 0 to (1- N) do
		  (mul-setf (aref a i j) factor))
	    (mul-setf (aref b i) factor)))))

(defmacro sub-setf (a b)
  `(setf ,a (- ,a ,b)))

(defun update-lower-col (a b i blockend)
  (declare (type (simple-array double-float (* *)) a)
	   (type (simple-array double-float (*)) b)
	   (type fixnum i blockend))
  (let ((n (array-dimension a 0)))
    (loop for i1 fixnum from i below blockend do
	  (pivot a b i1)
	  (scale-pivot a b i1 blockend)
	  (loop for j fixnum from (1+ i1) below n do
		(let ((aji (aref a j i1)))
		  (loop for k fixnum from (1+ i1) below blockend do
			(sub-setf (aref a j k) (* aji (aref a i1 k))))
		  (sub-setf (aref b j) (* aji (aref b i1))))))))

(defun update-upper-row (a i blockend)
  (declare (type (simple-array double-float (* *)) a)
	   (type fixnum i blockend))
  (let ((n (array-dimension a 0)))
    (loop for i1 fixnum from i below blockend do
	  (let ((factor (/ 1.0d0 (aref a i1 i1))))
	    (loop for k fixnum from blockend below n do
		  (mul-setf (aref a i1 k) factor)))
	  (loop for j fixnum from (1+ i1) below blockend do
		(let ((aji (aref a j i1)))
		  (loop for k fixnum from blockend below n do
			(sub-setf (aref a j k) (* aji (aref a i1 k)))))))))

(defun update-a (a i blockend)
  (declare (type (simple-array double-float (* *)) a)
	   (type fixnum i blockend))
  (let ((n (array-dimension a 0))
	(blocksize 8))
    (loop for i1 fixnum from blockend below n do
	  (loop for j fixnum from i below blockend by blocksize do
		(let ((aij0 (aref a i1 (+ j 0)))
		      (aij1 (aref a i1 (+ j 1)))
		      (aij2 (aref a i1 (+ j 2)))
		      (aij3 (aref a i1 (+ j 3)))
		      (aij4 (aref a i1 (+ j 4)))
		      (aij5 (aref a i1 (+ j 5)))
		      (aij6 (aref a i1 (+ j 6)))
		      (aij7 (aref a i1 (+ j 7)))
		      )
		  (loop for k fixnum from blockend below n do
			(sub-setf (aref a i1 k)
				  (+ (* aij0 (aref a (+ j 0) k))
				     (* aij1 (aref a (+ j 1) k))
				     (* aij2 (aref a (+ j 2) k))
				     (* aij3 (aref a (+ j 3) k))
				     (* aij4 (aref a (+ j 4) k))
				     (* aij5 (aref a (+ j 5) k))
				     (* aij6 (aref a (+ j 6) k))
				     (* aij7 (aref a (+ j 7) k))
				     )))))
	  (let ((jbegin (- blockend (mod (- blockend i) blocksize))))
	    (loop for j fixnum from jbegin below blockend do
		  (let ((aij (aref a i1 j)))
		    (loop for k fixnum from blockend below n do
			  (sub-setf (aref a i1 k) (* aij (aref a j k))))))))))

(defun forward-elimination (a b)
  (declare (type (simple-array double-float (* *)) a)
	   (type (simple-array double-float (*)) b))
  (let ((n (array-dimension a 0))
	(blocksize 24))
    (loop for i fixnum from 0 below n by blocksize do
	  (let ((blockend (min n (+ i blocksize))))
	    (update-lower-col a b i blockend)
	    (update-upper-row a i blockend)
	    (update-a a i blockend)))))

(defun back-substitution (a b)
  (declare (type (simple-array double-float (* *)) a)
	   (type (simple-array double-float (*)) b))
  (let ((n (array-dimension a 0)))
    (loop for i fixnum from (1- n) downto 0 do
	  (sub-setf (aref b i)
		    (loop for j fixnum from (1+ i) below n sum
			  (* (aref a i j) (aref b j)))))))

(defun solve (a b)
  (declare (type (simple-array double-float (* *)) a)
	   (type (simple-array double-float (*)) b))
  (scale-array a b)
  (forward-elimination a b)
  (back-substitution a b))
