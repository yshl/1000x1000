SUBDIRS=src-f src-c src-cc src-d src-ml src-cl src-java src-scm src-fth scripts
.PHONY: all test time clean

all:
	@for subdir in $(SUBDIRS); do \
	    make -C $$subdir; \
	done

test:
	@for subdir in $(SUBDIRS); do \
	    make -C $$subdir test; \
	done

time:
	@for subdir in $(SUBDIRS); do \
	    make -C $$subdir time; \
	done

clean:
	rm -f $(EXES) *.o
	@for subdir in $(SUBDIRS); do \
	    make -C $$subdir clean; \
	done
