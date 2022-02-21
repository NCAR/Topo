include machine_settings.make
#include ../machine_settings.make
EXEDIR = .
EXENAME = cube_to_target
RM = rm

.SUFFIXES:
.SUFFIXES: .F90 .o

.F90.o:
	$(FC) $(FFLAGS) $< $(LDFLAGS)

#------------------------------------------------------------------------
# Default rules and macros
#------------------------------------------------------------------------

OBJS := reconstruct.o remap.o shr_kind_mod.o shared_vars.o rot.o smooth_topo_cube.o ridge_ana.o cube_to_target.o f90getopt.o

$(EXEDIR)/$(EXENAME): $(OBJS)
	$(FC) -o $@ $(OBJS) $(LDFLAGS)
	mkdir -p output

clean:
	$(RM) -f $(OBJS)  *.mod $(EXEDIR)/$(EXENAME) output/*.nl

cube_to_target.o: shr_kind_mod.o remap.o reconstruct.o shared_vars.o f90getopt.o
reconstruct.o: remap.o 
remap.o      : shr_kind_mod.o
shared_vars.o: shr_kind_mod.o
