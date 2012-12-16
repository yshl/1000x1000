EXES=lin_array_c lin_pointer_c
SOL=../solution.txt
all: $(EXES)
.PHONY: test clean

lin_array_c: lin_array.c
	gcc -O3 -Wall -o $@ $^

lin_pointer_c: lin_pointer.c
	gcc -O3 -Wall -o $@ $^

test_array_c: lin_array_c
	./$< | diff -u - $(SOL)

test_pointer_c: lin_pointer_c
	./$< | diff -u - $(SOL)

time_array_c: lin_array_c
	time ./$< > /dev/null
	time ./$< > /dev/null

time_pointer_c: lin_pointer_c
	time ./$< > /dev/null
	time ./$< > /dev/null

test:
	make test_array_c
	make test_pointer_c

time:
	make time_array_c
	make time_pointer_c

clean:
	rm -f $(EXES) *.o
