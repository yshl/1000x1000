EXES=test_static_1_d test_dynamic_1_d
SOL=../solution.txt
all: $(EXES)
.PHONY: test clean

test_static_1_d: test_static_1.d lin_static.d
	dmd -O -release -inline -of$@ $^
test_dynamic_1_d: test_dynamic_1.d lin_dynamic.d
	dmd -O -release -inline -of$@ $^

test_static_d: test_static_1_d
	./$<

test_dynamic_d: test_dynamic_1_d
	./$<

time_static_d: test_static_1_d
	time ./$< > /dev/null
	time ./$< > /dev/null

time_dynamic_d: test_dynamic_1_d
	time ./$< > /dev/null
	time ./$< > /dev/null

test:
	make test_static_d
	make test_dynamic_d

time:
	make time_static_d
	make time_dynamic_d

clean:
	rm -f $(EXES) *.o
