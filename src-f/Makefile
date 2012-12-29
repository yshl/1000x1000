EXES=test_1_f test_lapack_1_f
SOL=../solution_f90.txt
all: $(EXES)
.PHONY: test clean

test_1_f: test_1.f90 lin.f90
	gfortran -O3 -Wall -o $@ $^

test_lapack_1_f: test_lapack_1.f90
	gfortran -O3 -Wall -o $@ $^ -lsatlas

test_f: test_1_f
	./$<

test_lapack_f: test_lapack_1_f
	./$<

time_f: test_1_f
	time ./$< > /dev/null
	time ./$< > /dev/null

time_lapack_f: test_lapack_1_f
	time ./$< > /dev/null
	time ./$< > /dev/null

test:
	make test_f
	make test_lapack_f

time:
	make time_f
	make time_lapack_f

clean:
	rm -f $(EXES) *.o *.mod
