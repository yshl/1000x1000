EXES=test_1_f test_lapack_1_f test_2_f test_lapack_2_f
FC=gfortran
F77=gfortran
FFLAGS=-O3 -Wall
LAPACKLIB=-llapack -lblas

.SUFFIXES: .f90
.PHONY: test clean

all: $(EXES)

.f90.o:
	$(FC) $(FFLAGS) -c $<
.f90.mod:
	$(FC) $(FFLAGS) -c $<
.f.o:
	$(F77) $(FFLAGS) -c $<

lin.o lin.mod: lin.f90
lin_complete.o lin_complete.mod: lin_complete.f90
test_1.o: test_1.f90 lin.mod
test_2.o: test_2.f90 lin_complete.mod
test_lapack_1.o: test_lapack_1.f90
test_lapack_2.o: test_lapack_2.f90
dgetc2.o: dgetc2.f

test_1_f: test_1.o lin.o
	$(FC) $(FFLAGS) -o $@ $^

test_lapack_1_f: test_lapack_1.o
	$(FC) $(FFLAGS) -o $@ $^ $(LAPACKLIB)

test_2_f: test_2.o lin_complete.o
	$(FC) $(FFLAGS) -o $@ $^

test_lapack_2_f: test_lapack_2.o dgetc2.o
	$(FC) $(FFLAGS) -o $@ $^ $(LAPACKLIB)

test: $(EXES)
	@for prog in $(EXES); do \
	    echo $$prog; \
	    ./$$prog; \
	done

time: $(EXES)
	@for prog in $(EXES); do \
	    echo $$prog; \
	    time ./$$prog; \
	    time ./$$prog; \
	done

clean:
	rm -f $(EXES) *.o *.mod
