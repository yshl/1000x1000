(compile-file "lin.lisp" :system-p t)
(compile-file "test_1.lisp" :system-p t)

(c:build-program "test_1_ecl" :lisp-files '("lin.o" "test_1.o")
		 :epilogue-code '(main))
(quit)
