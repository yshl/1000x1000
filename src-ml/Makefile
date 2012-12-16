EXES=test_1_ml
SOL=../solution.txt
all: $(EXES)
.PHONY: test clean

test_1_ml: lin.ml test_1.ml
	ocamlopt -o $@ $^

test_ml: test_1_ml
	./$<

time_ml: test_1_ml
	time ./$< > /dev/null
	time ./$< > /dev/null

test:
	make test_ml

time:
	make time_ml

clean:
	rm -f $(EXES) *.o *.cmi *.cmx *.cmo
