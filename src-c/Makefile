EXES=lin_c linp_c
SOL=../solution.txt
all: $(EXES)
.PHONY: test clean

lin_c: lin.c
	gcc -O3 -Wall -o $@ $^

linp_c: linp.c
	gcc -O3 -Wall -o $@ $^

test_c: lin_c
	./$< | diff -u - $(SOL)

testp_c: linp_c
	./$< | diff -u - $(SOL)

time_c: lin_c
	time ./$< > /dev/null
	time ./$< > /dev/null

timep_c: linp_c
	time ./$< > /dev/null
	time ./$< > /dev/null

test:
	make test_c
	make testp_c

time:
	make time_c
	make timep_c

clean:
	rm -f $(EXES) *.o
