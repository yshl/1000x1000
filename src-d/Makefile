EXES=test_static_1_d test_dynamic_1_d
DC=dmd
DFLAGS=-O -release -inline

.SUFFIXES: .d
.PHONY: test clean

all: $(EXES)

.d.o:
	$(DC) $(DFLAGS) -c -of$@ $<

test_static_1.o: test_static_1.d lin_static.d
test_dynamic_1.o: test_dynamic_1.d lin_dynamic.d

test_static_1_d: test_static_1.o lin_static.o
	$(DC) $(DFLAGS) -of$@ $^
test_dynamic_1_d: test_dynamic_1.o lin_dynamic.o
	$(DC) $(DFLAGS) -of$@ $^

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
