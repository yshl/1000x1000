EXES=test_array_1_c test_pointer_1_c
SOL=../solution.txt
all: $(EXES)
.PHONY: test clean

test_array_1_c: test_array_1.c lin_array.c
	gcc -O3 -Wall -o $@ $^ -I.

test_pointer_1_c: test_pointer_1.c lin_pointer.c
	gcc -O3 -Wall -o $@ $^

test_array_c: test_array_1_c
	./$<

test_pointer_c: test_pointer_1_c
	./$<

time_array_c: test_array_1_c
	time ./$< > /dev/null
	time ./$< > /dev/null

time_pointer_c: test_pointer_1_c
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
