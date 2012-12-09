EXES=lin_f lin_c linp_c lin_cc lin_d linp_d lin_ml lin_cl
SOL=solution.txt
SOL_F90=solution_f90.txt
all: $(EXES)
.PHONY: test clean

lin_f: src-f/lin.f90
	gfortran -O3 -Wall -o $@ $^
lin_c: src-c/lin.c
	gcc -O3 -Wall -o $@ $^
linp_c: src-c/linp.c
	gcc -O3 -Wall -o $@ $^
lin_cc: src-cc/lin.cc
	g++ -O3 -Wall -o $@ $^
lin_d: src-d/lin.d
	dmd -O -release -inline -of$@ $^
linp_d: src-d/linp.d
	dmd -O -release -inline -of$@ $^
lin_ml: src-ml/lin.ml
	ocamlopt -o $@ $^
lin_cl: src-cl/lin.cl
	sbcl --noinform --eval "(compile-file \"$<\")" --eval "(quit)"
	cp -p src-cl/lin.fasl $@

test_f: lin_f
	./$< | diff -u - $(SOL_F90)
	time ./$< > /dev/null
test_c: lin_c
	./$< | diff -u - $(SOL)
	time ./$< > /dev/null
testp_c: linp_c
	./$< | diff -u - $(SOL)
	time ./$< > /dev/null
test_cc: lin_cc
	./$< | diff -u - $(SOL)
	time ./$< > /dev/null
test_d: lin_d
	./$< | diff -u - $(SOL)
	time ./$< > /dev/null
testp_d: linp_d
	./$< | diff -u - $(SOL)
	time ./$< > /dev/null
test_ml: lin_ml
	./$< | diff -u - $(SOL)
	time ./$< > /dev/null
test_cl: lin_cl
	sbcl --noinform --load $< | diff -ub - $(SOL)
	time sbcl --noinform --load $< > /dev/null
test:
	make test_f
	make test_c
	make testp_c
	make test_cc
	make test_d
	make testp_d
	make test_ml
	make test_cl

clean:
	rm -f $(EXES) *.o
