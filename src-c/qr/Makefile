EXES=test_ahouse_1_c test_agivens_1_c test_qr_1_c test_lq_1_c \
     test_ahouse_2_c test_agivens_2_c test_qr_2_c test_lq_2_c
CC=gcc
CFLAGS=-O3 -Wall

.PHONY: test clean

all: $(EXES)

.c.o:
	$(CC) $(CFLAGS) -c $<

lin_array_householder.o: lin_array_householder.c lin_array.h
lin_array_qr.o: lin_array_qr.c lin_array.h
lin_array_lq.o: lin_array_lq.c lin_array.h
lin_array_givens.o: lin_array_givens.c lin_array.h

test_ahouse_1_c: ../test_array_1.o lin_array_householder.o
	$(CC) $(CFLAGS) -o $@ $^ -lm

test_qr_1_c: ../test_array_1.o lin_array_qr.o
	$(CC) $(CFLAGS) -o $@ $^ -lm

test_lq_1_c: ../test_array_1.o lin_array_lq.o
	$(CC) $(CFLAGS) -o $@ $^ -lm

test_agivens_1_c: ../test_array_1.o lin_array_givens.o
	$(CC) $(CFLAGS) -o $@ $^ -lm

test_ahouse_2_c: ../test_array_2.o lin_array_householder.o
	$(CC) $(CFLAGS) -o $@ $^ -lm

test_qr_2_c: ../test_array_2.o lin_array_qr.o
	$(CC) $(CFLAGS) -o $@ $^ -lm

test_lq_2_c: ../test_array_2.o lin_array_lq.o
	$(CC) $(CFLAGS) -o $@ $^ -lm

test_agivens_2_c: ../test_array_2.o lin_array_givens.o
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
