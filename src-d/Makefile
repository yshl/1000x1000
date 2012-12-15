EXES=lin_d linp_d
SOL=../solution.txt
all: $(EXES)
.PHONY: test clean

lin_d: lin.d
	dmd -O -release -inline -of$@ $^
linp_d: linp.d
	dmd -O -release -inline -of$@ $^

test_d: lin_d
	./$< | diff -u - $(SOL)

testp_d: linp_d
	./$< | diff -u - $(SOL)

time_d: lin_d
	time ./$< > /dev/null
	time ./$< > /dev/null

timep_d: linp_d
	time ./$< > /dev/null
	time ./$< > /dev/null

test:
	make test_d
	make testp_d

time:
	make time_d
	make timep_d

clean:
	rm -f $(EXES) *.o
