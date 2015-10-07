# Makefile to build definesurf on various platforms
# Note: If netcdf library is not built in the standard location, you must set the environment
# variables INC_NETCDF and LIB_NETCDF

EXEDIR = .
EXENAME = definesurf
RM = rm

.SUFFIXES:
.SUFFIXES: .f90 .o

.f90.o:
	$(FC) $(FFLAGS) $<

#------------------------------------------------------------------------
# Default rules and macros
#------------------------------------------------------------------------

OBJS := ao.o ao_i.o area_ave.o binf2c.o cell_area.o  \
        chkdims.o endrun.o fmain.o handle_error.o inimland.o \
        lininterp.o map_i.o max_ovr.o shr_kind_mod.o sghphis.o sm121.o \
        terrain_filter.o varf2c.o wrap_nf.o interplandm.o map2f.o

$(EXEDIR)/$(EXENAME): $(OBJS)
	$(FC) -o $@ $(OBJS) $(LDFLAGS)

clean:
	$(RM) -f $(OBJS)  *.mod $(EXEDIR)/$(EXENAME)

ao.o: shr_kind_mod.o
ao_i.o: shr_kind_mod.o
area_ave.o: shr_kind_mod.o
binf2c.o: shr_kind_mod.o
cell_area.o: shr_kind_mod.o
chkdims.o:
endrun.o:
fmain.o: shr_kind_mod.o
handle_error.o:
inimland.o: shr_kind_mod.o
lininterp.o: shr_kind_mod.o
map_i.o: shr_kind_mod.o
max_ovr.o: shr_kind_mod.o
shr_kind_mod.o:
sghphis.o: shr_kind_mod.o
sm121.o: shr_kind_mod.o
terrain_filter.o:
map2f.o:
varf2c.o: shr_kind_mod.o
wrap_nf.o:
interplandm.o: 
