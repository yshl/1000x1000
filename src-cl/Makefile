EXES=lin.fasl
SOL=../solution_f90.txt
all: $(EXES)
.PHONY: test clean

lin.fasl: lin.cl
	sbcl --noinform --eval "(compile-file \"$<\")" --eval "(quit)"

test_cl: test_1.cl lin.fasl
	sbcl --noinform --load $<

time_cl: test_1.cl lin.fasl
	time sbcl --noinform --load $< > /dev/null
	time sbcl --noinform --load $< > /dev/null

test:
	make test_cl

time:
	make time_cl

clean:
	rm -f $(EXES)
