# Makefile to build definesurf on various platforms
# Note: If netcdf library is not built in the standard location, you must set the environment
# variables INC_NETCDF and LIB_NETCDF

EXEDIR = .
EXENAME = definesurf
RM = rm

.SUFFIXES:
.SUFFIXES: .f90 .o

FC = gfortran

#DEBUG = TRUE
DEBUG = FALSE

# Check for the NetCDF library and include directories 
ifeq ($(LIB_NETCDF),$(null))
LIB_NETCDF := /opt/local/lib
endif

ifeq ($(INC_NETCDF),$(null))
INC_NETCDF := /opt/local/include
endif

# Determine platform 
UNAMES := $(shell uname -s)
UNAMEM := $(findstring CRAY,$(shell uname -m))

#------------------------------------------------------------------------
# GFORTRAN
#------------------------------------------------------------------------
#
ifeq ($(FC),gfortran)
#
# Tramhill
#
#  INC_NETCDF :=/usr/local/netcdf-gcc-g++-gfortran/include
#  LIB_NETCDF :=/usr/local/netcdf-gcc-g++-gfortran/lib

  INC_NETCDF := /opt/local/include
  LIB_NETCDF := /opt/local/lib

  LDFLAGS = -L$(LIB_NETCDF) -lnetcdf -lnetcdff 
  FFLAGS   := -c   -I$(INC_NETCDF)


  ifeq ($(DEBUG),TRUE)
#   FFLAGS += --chk aesu  -Cpp --trace
    FFLAGS += -Wall -fbacktrace -fbounds-check
  else
    FFLAGS += -O
  endif

endif



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
