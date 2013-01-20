EXES=test_1_scm
SCHEME=stalin
SCMFLAGS=-Ob -Om -On -Or -Ot -architecture IA32 -d -copt -O3 -copt -fomit-frame-pointer -copt -freg-struct-return

.PHONY: test clean

all: $(EXES)

test_1_scm: test_1.scm lin.scm
	$(SCHEME) $(SCMFLAGS) $<
	mv test_1 $@

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
	rm -f $(EXES) *.db
