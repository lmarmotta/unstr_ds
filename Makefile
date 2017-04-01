# Makefile created by mkmf $Id: mkmf,v 1.1.1.1 2008-08-28 01:48:18 carbrevi Exp $ 


# choose fortran compiler
#FC = gfortran
#LD = gfortran

FC = ifort
LD = ifort


# Fortran compiler flags

#intel fortran compiler debug mode: 
FFLAGS  = -g -O0 -fpe:0 -warn declarations -warn unused -warn ignore_loc -warn truncated_source -traceback -check all -implicitnone -openmp
LDFLAGS = -mkl
#intel fortran compiler optimized mode: 
#FFLAGS  = -O3 -fpe:0 -implicitnone -fast -ipo -xHost -parallel -openmp
#LDFLAGS = -mkl

#gfortran compiler debug mode: 
#FFLAGS  = -Wall -g -fbounds-check
#LDFLAGS = 
#gfortran compiler optimized mode: 
#FFLAGS  = -O2 -s -fomit-frame-pointer -fexpensive-optimizations -ffast-math
#LDFLAGS = 


.DEFAULT:
	-touch $@
all: a.out
derived.o: ./derived.f90 shared.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./derived.f90
functions.o: ./functions.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./functions.f90
nlb2d.o: ./nlb2d.f90 shared.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./nlb2d.f90
preproc.o: ./preproc.f90 shared.o functions.o
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./preproc.f90
shared.o: ./shared.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./shared.f90
SRC = ./derived.f90 ./shared.f90 ./nlb2d.f90 ./functions.f90 ./preproc.f90
OBJ = derived.o shared.o nlb2d.o functions.o preproc.o
clean: neat
	-rm -f .cppdefs $(OBJ) *.mod a.out face.dat ighost.dat
neat:
	-rm -f $(TMPFILES)
TAGS: $(SRC)
	etags $(SRC)
tags: $(SRC)
	ctags $(SRC)
a.out: $(OBJ) 
	$(LD) $(OBJ) -o a.out  $(LDFLAGS)
