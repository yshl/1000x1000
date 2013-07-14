EXES=test_bigloo_1_scm test_chicken_1_scm
BIGLOO=bigloo
BIGLOOFLAGS=-O6 -copt -O3
CSC=csc
CSCFLAGS=-O5 -C -O3 -heap-size 64M

.SUFFIXES: .scm
.PHONY: test clean

all: $(EXES)

lin_bigloo.o: lin_bigloo.scm
	$(BIGLOO) $(BIGLOOFLAGS) -c $<
test_bigloo_1.o: test_bigloo_1.scm lin_bigloo.scm
	$(BIGLOO) $(BIGLOOFLAGS) -c $<
lin_chicken.o: lin_chicken.scm
	$(CSC) $(CSCFLAGS) -c $<
test_chicken_1.o: test_chicken_1.scm lin_chicken.scm
	$(CSC) $(CSCFLAGS) -c $<

test_bigloo_1_scm: test_bigloo_1.o lin_bigloo.o
	$(BIGLOO) $(BIGLOOFLAGS) -o $@ $^
test_chicken_1_scm: test_chicken_1.o lin_chicken.o
	$(CSC) $(CSCFLAGS) -o $@ $^

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
