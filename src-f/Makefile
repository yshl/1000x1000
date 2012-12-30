EXES=test_1_f test_lapack_1_f
FC=gfortran
FFLAGS=-O3 -Wall
LAPACKLIB=-lsatlas

.SUFFIXES: .f90
.PHONY: test clean

all: $(EXES)

.f90.o:
	$(FC) $(FFLAGS) -c $<

lin.o lin.mod: lin.f90
test_1.o: test_1.f90 lin.f90
test_lapack_1.o: test_lapack_1.f90

test_1_f: test_1.o lin.o
	$(FC) $(FFLAGS) -o $@ $^

test_lapack_1_f: test_lapack_1.o
	$(FC) $(FFLAGS) -o $@ $^ $(LAPACKLIB)

test: $(EXES)
	@for prog in $(EXES); do \
	    echo $$prog; \
	    ./$$prog; \
	done

time: $(EXRS)
	@for prog in $(EXES); do \
	    echo $$prog; \
	    time ./$$prog; \
	    time ./$$prog; \
	done

clean:
	rm -f $(EXES) *.o *.mod
