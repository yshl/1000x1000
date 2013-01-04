EXES=Test_1.class
JAVA_HOME=/usr/java/jdk
JAVA=$(JAVA_HOME)/bin/java
JAVAC=$(JAVA_HOME)/bin/javac
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
