.PHONY: time

all:

test:

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
