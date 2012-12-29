EXES=test_array_1_c test_pointer_1_c test_lapack_1_c
CC=gcc
CFLAGS=-O3 -Wall
LAPACKLIB=-lsatlas
LAPACKINC=

.PHONY: test clean

all: $(EXES)

.c.o:
	$(CC) $(CFLAGS) -c $<

lin_array.o: lin_array.c lin_array.h
lin_pointer.o: lin_pointer.c lin_pointer.h
test_array_1.o: test_array_1.c lin_array.h
test_pointer_1.o: test_pointer_1.c lin_pointer.h
test_lapack_1.o: test_lapack_1.c
	$(CC) $(CFLAGS) -o $@ $< $(LAPACKINC)

test_array_1_c: test_array_1.o lin_array.o
	$(CC) $(CFLAGS) -o $@ $^

test_pointer_1_c: test_pointer_1.o lin_pointer.o
	$(CC) $(CFLAGS) -o $@ $^

test_lapack_1_c: test_lapack_1.c
	$(CC) $(CFLAGS) -o $@ $^ $(LAPACKLIB)

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
	rm -f $(EXES) *.o
