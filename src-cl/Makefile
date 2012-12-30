SBCLFASL=lin.fasl
ECLFAS=lin.fas

.SUFFIXES: .cl .fasl .fas
.PHONY: test clean

all: $(SBCLFASL) $(ECLFAS)

.cl.fasl:
	sbcl --noinform --eval "(compile-file \"$<\")" --eval "(quit)"

.cl.fas:
	ecl -eval "(compile-file \"$<\")" -eval "(quit)"

lin.fasl: lin.cl
lin.fas: lin.cl

test_sbcl: test_1.cl lin.fasl
	sbcl --noinform --load $<

test_ecl: test_ecl_1.cl lin.fas
	ecl -load $<

time_sbcl: test_1.cl lin.fasl
	time sbcl --noinform --load $<
	time sbcl --noinform --load $<

time_ecl: test_ecl_1.cl lin.fas
	time ecl -load $<
	time ecl -load $<

test:
	make test_sbcl test_ecl

time:
	make time_sbcl time_ecl

clean:
	rm -f $(SBCLFASL) $(ECLFAS)
