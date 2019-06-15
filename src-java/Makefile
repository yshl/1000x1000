EXES=Test_1.class
JAVA=java
JAVAC=javac
JAVAFLAGS=

.PHONY: test clean

all: $(EXES)

Lin.class: lin.java
	$(JAVAC) $(JAVAFLAGS) $<

Test_1.class: test_1.java Lin.class
	$(JAVAC) $(JAVAFLAGS) $<

test: $(EXES)
	@for prog in $(EXES); do \
	    echo $$prog; \
	    $(JAVA) `basename $$prog .class`; \
	done

time: $(EXES)
	@for prog in $(EXES); do \
	    echo $$prog; \
	    time $(JAVA) `basename $$prog .class`; \
	    time $(JAVA) `basename $$prog .class`; \
	done

clean:
	rm -f $(EXES) *.class
