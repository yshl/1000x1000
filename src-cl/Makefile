SBCLFASL=lin.fasl
ECLFAS=lin.fas

.SUFFIXES: .lisp .fasl .fas
.PHONY: test clean

all: $(SBCLFASL) $(ECLFAS)

.lisp.fasl:
	sbcl --noinform --eval "(compile-file \"$<\")" --eval "(quit)"

.lisp.fas:
	ecl -eval "(compile-file \"$<\")" -eval "(quit)"

lin.fasl: lin.lisp
lin.fas: lin.lisp

test_sbcl: test_1.lisp lin.fasl
	sbcl --noinform --load $(SBCLFASL) --script $<

test_ecl: test_1.lisp lin.fas
	ecl -load $(ECLFAS) -shell $<

time_sbcl: test_1.lisp lin.fasl
	time sbcl --noinform --load $(SBCLFASL) --script $<
	time sbcl --noinform --load $(SBCLFASL) --script $<

time_ecl: test_1.lisp lin.fas
	time ecl -load $(ECLFAS) -shell $<
	time ecl -load $(ECLFAS) -shell $<

test:
	make test_sbcl test_ecl

time:
	make time_sbcl time_ecl

clean:
	rm -f $(SBCLFASL) $(ECLFAS)
