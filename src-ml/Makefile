EXES=lin_ml
SOL=../solution.txt
all: $(EXES)
.PHONY: test clean

lin_ml: lin.ml
	ocamlopt -o $@ $^

test_ml: lin_ml
	./$< | diff -u - $(SOL)

time_ml: lin_ml
	time ./$< > /dev/null
	time ./$< > /dev/null

test:
	make test_ml

time:
	make time_ml

clean:
	rm -f $(EXES) *.o
