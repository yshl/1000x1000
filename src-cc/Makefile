EXES=test_1_cc
CXX=g++
CXXFLAGS=-O3 -Wall

.PHONY: test clean

all: $(EXES)

.cc.o:
	$(CXX) $(CXXFLAGS) -c $<

lin.o: lin.cc lin.h
test_1.o: test_1.cc lin.h
test_1_cc: test_1.o lin.o
	$(CXX) $(CXXFLAGS) -o $@ $^

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
