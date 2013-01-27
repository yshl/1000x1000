.PHONY: time

all:

test:
	make test_r
	make test_octave
	make test_scilab

test_r: lin.r
	Rscript $<
test_octave: lin.m
	octave -q $<
test_scilab: lin.sce
	scilab -nwni -nb -f $<

time:
	make time_r
	make time_octave
	make time_scilab

time_r: lin.r
	time Rscript $< > /dev/null
	time Rscript $< > /dev/null

time_octave: lin.m
	time octave -q $< > /dev/null
	time octave -q $< > /dev/null

time_scilab: lin.sce
	time scilab -nwni -nb -f $< > /dev/null
	time scilab -nwni -nb -f $< > /dev/null

clean:
