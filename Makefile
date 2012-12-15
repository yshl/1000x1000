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

test_c: lin_c
	./$< | diff -u - $(SOL)

testp_c: linp_c
	./$< | diff -u - $(SOL)

test_cc: lin_cc
	./$< | diff -u - $(SOL)

test_d: lin_d
	./$< | diff -u - $(SOL)

testp_d: linp_d
	./$< | diff -u - $(SOL)

test_ml: lin_ml
	./$< | diff -u - $(SOL)

test_cl: lin_cl
	sbcl --noinform --load $< | diff -ub - $(SOL)

time_f: lin_f
	time ./$< > /dev/null
	time ./$< > /dev/null

time_c: lin_c
	time ./$< > /dev/null
	time ./$< > /dev/null

timep_c: linp_c
	time ./$< > /dev/null
	time ./$< > /dev/null

time_cc: lin_cc
	time ./$< > /dev/null
	time ./$< > /dev/null

time_d: lin_d
	time ./$< > /dev/null
	time ./$< > /dev/null

timep_d: linp_d
	time ./$< > /dev/null
	time ./$< > /dev/null

time_ml: lin_ml
	time ./$< > /dev/null
	time ./$< > /dev/null

time_cl: lin_cl
	time sbcl --noinform --load $< > /dev/null
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

time:
	make time_f
	make time_c
	make timep_c
	make time_cc
	make time_d
	make timep_d
	make time_ml
	make time_cl

clean:
	rm -f $(EXES) *.o
