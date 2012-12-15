EXES=lin_f
SOL=../solution_f90.txt
all: $(EXES)
.PHONY: test clean

lin_f: lin.f90
	gfortran -O3 -Wall -o $@ $^

test_f: lin_f
	./$< | diff -u - $(SOL)

time_f: lin_f
	time ./$< > /dev/null
	time ./$< > /dev/null

test:
	make test_f

time:
	make time_f

clean:
	rm -f $(EXES) *.o
