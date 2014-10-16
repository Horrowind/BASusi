FC = gfortran
FOPTS = -ffree-form -O3
FFLAGS = -Wall

## Modules ##########################################################################

UTIL = utilities.o

UTIF = $(addprefix ,$(UTIL))


## Directories ######################################################################


## Rules for Objekt-Files ###########################################################
%.o: %.f
	$(FC) $(FOPTS) $(FFLAGS) -I $(BUILDD) -J $(BUILDD) -c $< -o $@
## Rules for executables ############################################################
%.exe: $(BUILDD)/%.o
	$(FC) $(FOPTS) $(FFLAGS) -I $(BUILDD) -J $(BUILDD) $(BUILDD)/*.o -o $@

## Targets ##########################################################################
.phony: all clean tidy docu plot

all: main.exe 

clean:
	rm -f $(BUILDD)/*.o;
	rm -f $(BUILDD)/*.mod

docu:
	doxygen $(SRCDIR)/doxyfile



multiplot:
	octave -q $(SRCDIR)/multiplot.m

errorplot:
	octave -q $(SRCDIR)/errorplot.m

tidy: clean
	rm -f $(BINDIR)/*.exe;
	rm -rf $(DOCDIR)/*;	
	touch $(DOCDIR)/index
	rm -f output/*;
	touch output/index



main.exe: $(UTIL)


