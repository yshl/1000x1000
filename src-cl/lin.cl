(defmacro swap (a b)
  `(let ((tmp ,a))
     (setf ,a ,b)
     (setf ,b tmp)))

(defun pivot (a b i)
  (let ((N (array-dimension a 1))
	(ajimax (abs(aref a i i)))
	(maxj i))
    (dotimes (j N)
      (let ((aji (abs (aref a j i))))
	(if (> aji ajimax)
	  (setq ajimax aji)
	  (setq maxj j))))
    (if (/= i maxj)
      (progn
	(dotimes (j N)
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
    (forward-elimination a b)
    (back-substitution a b)))

(defun main ()
  (let* ((N 1000)
	 (a (make-array `(,N ,N) :initial-element 1.0d0))
	 (b (make-array `(,N) :initial-element 1000.0d0)))
    (dotimes (i N)
      (setf (aref a i i) 1001.0d0))
    (solve a b)
    (dotimes (i N)
      (format t "~g~%" (aref b i)))))

(main)
(quit)
