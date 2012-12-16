EXES=test_1_f
SOL=../solution_f90.txt
all: $(EXES)
.PHONY: test clean

test_1_f: test_1.f90 lin.f90
	gfortran -O3 -Wall -o $@ $^

test_f: test_1_f
	./$<

time_f: test_1_f
	time ./$< > /dev/null
	time ./$< > /dev/null

test:
	make test_f

time:
	make time_f

clean:
	rm -f $(EXES) *.o *.mod
