PROG   = interpol.last.01htap

NCLOC  = ${NETCDF}

INCDIR = -I${NCLOC}/include
LIBDIR = -L${NCLOC}/lib 
LIBS   = -lnetcdff -lnetcdf -lm #-lcurl 

FC      = gfortran
FFLAGS  = -O2 -g #-traceback #-fpe0 -check bounds -heaparrays
#FFLAGS  = -g #-O0 -traceback -fpe0
#FFLAGS  = -O3 -ipo -traceback -fpe0 -parallel -openmp -openmp_report1 -opt_report_phase all -par_report1


#FC = gfortran
#FFLAGS = -O2 -ftree-vectorize -funroll-loops  -ffree-form -ffree-line-length-none


.SUFFIXES: .o .f90
.f90.o: $*.f90
	${FC} -c ${FFLAGS} ${INCDIR} $*.f90

MODULES = \
          module_ncroutines.o   \
          module_timeroutines.o \
          module_write_output.o

OBJS    =

${PROG}.exe : ${PROG}.o
	${FC} -o $@ ${FFLAGS} ${OBJS} ${PROG}.o ${MODULES} ${INCDIR} ${LIBDIR} ${LIBS}

clean : 
	rm *.mod *.o *.exe

# DEPENDENCIES

${PROG}.o : ${OBJS} ${MODULES}
