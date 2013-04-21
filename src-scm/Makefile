EXES=test_1_scm
SCHEME=bigloo
SCMFLAGS=-O6 -copt -O3

.SUFFIXES: .scm
.PHONY: test clean

all: $(EXES)

.scm.o:
	$(SCHEME) $(SCMFLAGS) -c $<

lin.o: lin.scm
test_1.o: test_1.scm lin.scm

test_1_scm: test_1.o lin.o
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
