EXES=test_1_ml
OCAMLC=ocamlopt
OCAMLFLAGS=

.PHONY: test clean

all: $(EXES)

test_1_ml: lin.ml test_1.ml
	$(OCAMLC) $(OCAMLFLAGS) -o $@ $^

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
	rm -f $(EXES) *.o *.cmi *.cmx *.cmo
