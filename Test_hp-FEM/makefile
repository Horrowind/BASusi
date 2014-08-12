

%: %.f
	gfortran -ffree-form -fopenmp -O3 -Wall $^ -o $@

.phony: all clean plot1

all: fem_linear

plot: plotoutput.m
	octave -q plotoutput.m

clean:
	rm -f *.exe;
	rm -f output
