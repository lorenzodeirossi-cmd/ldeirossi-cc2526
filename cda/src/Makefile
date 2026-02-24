FC=gfortran
FFLAGS =

TARGET_CDA= ../bin/cda
TARGET_TOOLS= ../bin/cube_tools

OBJECTS_CDA= cda.o kinds.o cubes.o
OBJECTS_TOOLS= cube_tools.o kinds.o cubes.o

# Fixed the typo here:
all: $(TARGET_CDA) $(TARGET_TOOLS)

$(TARGET_CDA): $(OBJECTS_CDA)
	$(FC) $(FFLAGS) -o $(TARGET_CDA) $(OBJECTS_CDA)

$(TARGET_TOOLS): $(OBJECTS_TOOLS)
	$(FC) $(FFLAGS) -o $(TARGET_TOOLS) $(OBJECTS_TOOLS)

cube_tools.o: cube_tools.f95 kinds.o cubes.o
cda.o: cda.f95 kinds.o cubes.o
cubes.o: cubes.f95 kinds.o
kinds.o: kinds.f95

.SUFFIXES: .f95 .o
.f95.o:
	$(FC) $(STDF) $(FFLAGS) -c -I. -o $*.o $*.f95

clean:
	$(RM) $(TARGET_CDA) $(TARGET_TOOLS) *.mod *.o
