EXES=lin_cc
SOL=../solution.txt
all: $(EXES)
.PHONY: test clean

lin_cc: lin.cc
	g++ -O3 -Wall -o $@ $^

test_cc: lin_cc
	./$< | diff -u - $(SOL)

time_cc: lin_cc
	time ./$< > /dev/null
	time ./$< > /dev/null

test:
	make test_cc

time:
	make time_cc

clean:
	rm -f $(EXES) *.o
