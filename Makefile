.PHONY: all test time clean

all:
	make -C src-f
	make -C src-c
	make -C src-cc
	make -C src-d
	make -C src-ml
	make -C src-cl

test:
	make -C src-f  test
	make -C src-c  test
	make -C src-cc test
	make -C src-d  test
	make -C src-ml test
	make -C src-cl test

time:
	make -C src-f  time
	make -C src-c  time
	make -C src-cc time
	make -C src-d  time
	make -C src-ml time
	make -C src-cl time

clean:
	rm -f $(EXES) *.o
	make -C src-f  clean
	make -C src-c  clean
	make -C src-cc clean
	make -C src-d  clean
	make -C src-ml clean
	make -C src-cl clean
