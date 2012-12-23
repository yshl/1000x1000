EXES=lin.fasl lin.fas
SOL=../solution_f90.txt
all: $(EXES)
.PHONY: test clean

lin.fasl: lin.cl
	sbcl --noinform --eval "(compile-file \"$<\")" --eval "(quit)"

lin.fas: lin.cl
	ecl -eval "(compile-file \"$<\")" -eval "(quit)"

test_cl: test_1.cl lin.fasl
	sbcl --noinform --load $<

test_ecl: test_ecl_1.cl lin.fas
	ecl -load $<

time_cl: test_1.cl lin.fasl
	time sbcl --noinform --load $< > /dev/null
	time sbcl --noinform --load $< > /dev/null

time_ecl: test_ecl_1.cl lin.fas
	time ecl -load $< > /dev/null
	time ecl -load $< > /dev/null

test:
	make test_cl test_ecl

time:
	make time_cl time_ecl

clean:
	rm -f $(EXES)
