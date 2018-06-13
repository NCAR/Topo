include ../../../machine_settings.make
EXEDIR = .
EXENAME = fix_inland_water_elevation
RM = rm

.SUFFIXES:
.SUFFIXES: .F90 .o

# Determine platform 
UNAMES := $(shell uname -s)
UNAMEM := $(findstring CRAY,$(shell uname -m))

.F90.o:
	$(FC) $(FFLAGS) $<

#------------------------------------------------------------------------
# Default rules and macros
#------------------------------------------------------------------------

OBJS := fix_inland_water_elevation.o shr_kind_mod.o

$(EXEDIR)/$(EXENAME): $(OBJS)
	export LD_LIBRARY_PATH=/usr/local/lib
	echo "$(LD_LIBRARY_PATH)"
	$(FC) -o $@ $(OBJS) $(LDFLAGS)

clean:
	$(RM) -f $(OBJS)  *.mod $(EXEDIR)/$(EXENAME)

fix_inland_water_elevation.o: shr_kind_mod.o
shared_vars.o: shr_kind_mod.o
