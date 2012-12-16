EXES=lin_static_d lin_dynamic_d
SOL=../solution.txt
all: $(EXES)
.PHONY: test clean

lin_static_d: lin_static.d
	dmd -O -release -inline -of$@ $^
lin_dynamic_d: lin_dynamic.d
	dmd -O -release -inline -of$@ $^

test_static_d: lin_static_d
	./$< | diff -u - $(SOL)

test_dynamic_d: lin_dynamic_d
	./$< | diff -u - $(SOL)

time_static_d: lin_static_d
	time ./$< > /dev/null
	time ./$< > /dev/null

time_dynamic_d: lin_dynamic_d
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
