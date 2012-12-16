EXES=test_1_cc
SOL=../solution.txt
all: $(EXES)
.PHONY: test clean

test_1_cc: test_1.cc lin.cc
	g++ -O3 -Wall -o $@ $^

test_cc: test_1_cc
	./$<

time_cc: test_1_cc
	time ./$< > /dev/null
	time ./$< > /dev/null

test:
	make test_cc

time:
	make time_cc

clean:
	rm -f $(EXES) *.o
