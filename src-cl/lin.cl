(defmacro swap (a b)
  `(let ((tmp ,a))
     (setf ,a ,b)
     (setf ,b tmp)))

(defun absmax (a i j)
  (let ((N (array-dimension a 1))
	(ajimax (abs (aref a i j))))
    (loop for k from (1+ j) to (1- N) do
	  (setq ajimax (max ajimax (abs (aref a i k)))))
    ajimax))

(defun pivot (a b i)
  (let ((N (array-dimension a 1))
	(ajimax (abs(aref a i i)))
	(maxj i))
    (loop for j from (1+ i) to (1- N) do
	  (let ((aji (abs (aref a j i))))
	    (if (> aji ajimax)
	      (setq ajimax aji)
	      (setq maxj j))))
    (if (/= i maxj)
      (progn
	(loop for j from i to (1- N) do
	      (swap (aref a i j) (aref a maxj j)))
	(swap (aref b i) (aref b maxj))))))

(defmacro mul-setf (a b)
  `(setf ,a (* ,a ,b)))

(defun scale-row (a b i)
  (let ((n (array-dimension a 1))
	(factor (/ 1.0 (aref a i i))))
    (loop for j from (1+ i) to (1- n) do
	  (mul-setf (aref a i j) factor))
    (mul-setf (aref b i) factor)))

(defun scale (a b)
  (let ((N (array-dimension a 1)))
    (loop for i from 0 to (1- N) do
	  (setq factor (/ 1.0d0 (absmax a i 0)))
	  (loop for j from 0 to (1- N) do
		(mul-setf (aref a i j) factor ))
	  (mul-setf (aref b i) factor))))

(defmacro sub-setf (a b)
  `(setf ,a (- ,a ,b)))

(defun elim-col (a b i)
  (let ((n (array-dimension a 0)))
    (loop for j from (1+ i) to (1- n) do
	  (let ((aji (aref a j i)))
	    (loop for k from (1+ i) to (1- n) do
		  (sub-setf (aref a j k) (* aji (aref a i k))))
	    (sub-setf (aref b j) (* aji (aref b i)))))))

(defun forward-elimination (a b)
  (let ((n (array-dimension a 0)))
    (dotimes (i n)
      (progn
	(pivot a b i)
	(scale-row a b i)
	(elim-col a b i)))))

(defun back-substitution (a b)
  (let ((n (array-dimension a 0)))
    (loop for i from (1- n) downto 0 do
	  (loop for j from (1+ i) to (1- n) do
		(sub-setf (aref b i) (* (aref a i j) (aref b j)))))))

(defun solve (a b)
  (progn
    (scale a b)
    (forward-elimination a b)
    (back-substitution a b)))

(defun main ()
  (let* ((N 1000)
	 (a (make-array `(,N ,N) :initial-element 1.0d0
			:element-type 'double-float))
	 (b (make-array `(,N) :initial-element 1000.0d0
			:element-type 'double-float)))
    (dotimes (i N)
      (setf (aref a i i) 1001.0d0))
    (solve a b)
    (dotimes (i N)
      (format t "~,6g~%" (aref b i)))))

(main)
(quit)
