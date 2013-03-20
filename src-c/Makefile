EXES=test_array_1_c test_pointer_1_c test_lapack_1_c \
     test_array_2_c test_pointer_2_c test_lapack_2_c \
     test_array_rand_c test_pointer_rand_c
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
lin_array_complete.o: lin_array_complete.c lin_array.h
lin_pointer_complete.o: lin_pointer_complete.c lin_pointer.h
test_array_1.o: test_array_1.c lin_array.h
test_pointer_1.o: test_pointer_1.c lin_pointer.h
test_array_2.o: test_array_2.c lin_array.h
test_pointer_2.o: test_pointer_2.c lin_pointer.h
test_array_rand.o: test_array_rand.c lin_array.h
test_pointer_rand.o: test_pointer_rand.c lin_pointer.h
test_lapack_1.o: test_lapack_1.c
	$(CC) $(CFLAGS) -c $< $(LAPACKINC)
test_lapack_2.o: test_lapack_2.c
	$(CC) $(CFLAGS) -c $< $(LAPACKINC)
../src-f/dgetc2.o:
	$(MAKE) -C ../src-f $@

test_array_1_c: test_array_1.o lin_array.o
	$(CC) $(CFLAGS) -o $@ $^

test_pointer_1_c: test_pointer_1.o lin_pointer.o
	$(CC) $(CFLAGS) -o $@ $^

test_lapack_1_c: test_lapack_1.o
	$(CC) $(CFLAGS) -o $@ $^ $(LAPACKLIB)

test_array_2_c: test_array_2.o lin_array_complete.o
	$(CC) $(CFLAGS) -o $@ $^ -lm

test_pointer_2_c: test_pointer_2.o lin_pointer_complete.o
	$(CC) $(CFLAGS) -o $@ $^ -lm

test_lapack_2_c: test_lapack_2.o ../src-f/dgetc2.o
	$(CC) $(CFLAGS) -o $@ $^ $(LAPACKLIB)

test_array_rand_c: test_array_rand.o lin_array_complete.o
	$(CC) $(CFLAGS) -o $@ $^ -lm

test_pointer_rand_c: test_pointer_rand.o lin_pointer_complete.o
	$(CC) $(CFLAGS) -o $@ $^ -lm

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
