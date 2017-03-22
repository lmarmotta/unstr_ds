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
unstr_ds.o: ./unstr_ds.f90
	$(FC) $(FFLAGS) $(OTHERFLAGS) -c	./unstr_ds.f90
SRC = ./unstr_ds.f90
OBJ = unstr_ds.o
clean: neat
	-rm -f .cppdefs $(OBJ) *.mod a.out
neat:
	-rm -f $(TMPFILES)
TAGS: $(SRC)
	etags $(SRC)
tags: $(SRC)
	ctags $(SRC)
a.out: $(OBJ) 
	$(LD) $(OBJ) -o a.out  $(LDFLAGS)
