.SUFFIXES: .o .f90

version=0.0

FC=gfortran
LIBS=-lz -lproj
INC= 
FFLAGS= -O0 -ffree-line-length-none -Wunused #-g -pg -fbacktrace -Wall

OBJS=utils.o datetime.o proj.o zlib.o geotiff.o interp.o geo.o uppa.o surf.o main.o 
EXE=CALPREP_v${version}.EXE

.f90.o:
		${FC} ${FFLAGS} -c ${INC} $<

$(EXE): ${OBJS}
		${FC} -o ${EXE} ${OBJS} ${LIBS} 

clean:
		rm -f *.o *.mod $(EXE)
