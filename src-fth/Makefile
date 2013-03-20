EXES=test_1.fth
FORTH=gforth

.PHONY: test clean

all: $(EXES)

test: $(EXES)
	@for prog in $(EXES); do \
	    echo $$prog; \
	    $(FORTH) ./$$prog; \
	done

time: $(EXES)
	@for prog in $(EXES); do \
	    echo $$prog; \
	    time $(FORTH) ./$$prog; \
	    time $(FORTH) ./$$prog; \
	done

clean:
