EXES=test_array_1_c test_pointer_1_c test_lapack_1_c test_array_2_c test_pointer_2_c
SOL=../solution.txt
all: $(EXES)
.PHONY: test clean

test_array_1_c: test_array_1.c lin_array.c
	gcc -O3 -Wall -o $@ $^

test_pointer_1_c: test_pointer_1.c lin_pointer.c
	gcc -O3 -Wall -o $@ $^

test_lapack_1_c: test_lapack_1.c
	gcc -O3 -Wall -o $@ $^ -lsatlas

test_array_2_c: test_array_2.c lin_array_complete.c
	gcc -O3 -Wall -o $@ $^ -lm

test_pointer_2_c: test_pointer_2.c lin_pointer_complete.c
	gcc -O3 -Wall -o $@ $^ -lm

test_array_c: test_array_1_c test_array_2_c
	@for prog in $^ ; do \
	    echo $$prog; \
	    ./$$prog ; \
	done

test_pointer_c: test_pointer_1_c test_pointer_2_c
	@for prog in $^ ; do \
	    echo $$prog; \
	    ./$$prog; \
	done

test_lapack_c: test_lapack_1_c
	./$<

time_array_c: test_array_1_c test_array_2_c
	@for prog in $^ ; do \
	    echo $$prog; \
	    time ./$$prog > /dev/null; \
	    time ./$$prog > /dev/null; \
	done

time_pointer_c: test_pointer_1_c test_pointer_2_c
	@for prog in $^ ; do \
	    echo $$prog; \
	    time ./$$prog > /dev/null ; \
	    time ./$$prog > /dev/null ; \
	done

time_lapack_c: test_lapack_1_c
	time ./$< > /dev/null
	time ./$< > /dev/null

test:
	make test_array_c
	make test_pointer_c
	make test_lapack_c

time:
	make time_array_c
	make time_pointer_c
	make time_lapack_c

clean:
	rm -f $(EXES) *.o
