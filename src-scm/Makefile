EXES=test_bigloo_1_scm
SCHEME=bigloo
SCMFLAGS=-O6 -copt -O3

.SUFFIXES: .scm
.PHONY: test clean

all: $(EXES)

.scm.o:
	$(SCHEME) $(SCMFLAGS) -c $<

lin_bigloo.o: lin_bigloo.scm
test_bigloo_1.o: test_bigloo_1.scm lin_bigloo.scm

test_bigloo_1_scm: test_bigloo_1.o lin_bigloo.o
	$(SCHEME) $(SCMFLAGS) -o $@ $^

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
	rm -f $(EXES) *.o
