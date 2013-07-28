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
	      (setq ajimax aji)
	      (setq maxj j))))
    (if (/= i maxj)
      (progn
	(loop for j fixnum from i below N do
	      (swap (aref a i j) (aref a maxj j)))
	(swap (aref b i) (aref b maxj))))))

(defmacro mul-setf (a b)
  `(setf ,a (* ,a ,b)))

(defun scale-pivot (a b i)
  (declare (type (simple-array double-float (* *)) a)
	   (type (simple-array double-float (*)) b)
	   (type fixnum i))
  (let ((n (array-dimension a 1))
	(factor (/ 1.0d0 (aref a i i))))
    (loop for j fixnum from (1+ i) below n do
	  (mul-setf (aref a i j) factor))
    (mul-setf (aref b i) factor)))

(defun scale-array (a b)
  (declare (type (simple-array double-float (* *)) a)
	   (type (simple-array double-float (*)) b))
  (let ((N (array-dimension a 1)))
    (loop for i fixnum from 0 below N do
	  (let ((factor (/ 1.0d0 (absmax a i 0))))
	    (loop for j fixnum from 0 to (1- N) do
		  (mul-setf (aref a i j) factor ))
	    (mul-setf (aref b i) factor)))))

(defmacro sub-setf (a b)
  `(setf ,a (- ,a ,b)))

(defun elim-col (a b i)
  (declare (type (simple-array double-float (* *)) a)
	   (type (simple-array double-float (*)) b)
	   (type fixnum i))
  (let ((n (array-dimension a 0)))
    (loop for j fixnum from (1+ i) below n do
	  (let ((aji (aref a j i)))
	    (loop for k fixnum from (1+ i) below n do
		  (sub-setf (aref a j k) (* aji (aref a i k))))
	    (sub-setf (aref b j) (* aji (aref b i)))))))

(defun forward-elimination (a b)
  (declare (type (simple-array double-float (* *)) a)
	   (type (simple-array double-float (*)) b))
  (let ((n (array-dimension a 0)))
    (dotimes (i n)
      (pivot a b i)
      (scale-pivot a b i)
      (elim-col a b i))))

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
