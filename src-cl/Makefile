EXES=test_1_sbcl test_1_ecl

.PHONY: test clean

all: $(EXES)

test_1_sbcl: test_1.lisp lin.lisp
	sbcl --noinform --script build_sbcl.lisp

test_1_ecl: test_1.lisp lin.lisp
	ecl -shell build_ecl.lisp

test: $(EXES)
	@for prog in $(EXES); do \
	    echo $$prog; \
	    ./$$prog; \
	done

time: $(EXES)
	@for prog in $(EXES); do \
	    echo $$prog; \
	    time ./$$prog; \
	    time ./$$prog; \
	done

clean:
	rm -f *.o *.fasl $(EXES)
