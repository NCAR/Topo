EXEDIR = .
EXENAME = fix_inland_water_elevation
RM = rm

.SUFFIXES:
.SUFFIXES: .F90 .o

#
# setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:/usr/local/netcdf-4.1.3-gcc-4.4.4-13-lf9581/lib
#
FC = gfortran

#DEBUG=TRUE
DEBUG = FALSE


# Determine platform 
UNAMES := $(shell uname -s)
UNAMEM := $(findstring CRAY,$(shell uname -m))


#------------------------------------------------------------------------
# NAG
#------------------------------------------------------------------------
ifeq ($(FC),nagfor)

  INC_NETCDF :=/usr/local/netcdf-gcc-nag/include
  LIB_NETCDF :=/usr/local/netcdf-pgi/lib

  LDFLAGS = -L$(LIB_NETCDF) -lnetcdf -lnetcdff
  FFLAGS   := -c  -I$(INC_NETCDF)


  ifeq ($(DEBUG),TRUE)
    FFLAGS += -g -C
  else
    FFLAGS += -O
  endif

endif
#------------------------------------------------------------------------
# PGF95
#------------------------------------------------------------------------
#
# setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:/usr/local/netcdf-4.1.3-gcc-4.4.4-13-lf9581/lib
#
ifeq ($(FC),pgf95)
#
# Tramhill
#
  INC_NETCDF :=/usr/local/netcdf-pgi/include
  LIB_NETCDF :=/usr/local/netcdf-pgi/lib

  LDFLAGS = -L$(LIB_NETCDF) -lnetcdf -lnetcdff
  FFLAGS   := -c -Mlarge_arrays -I$(INC_NETCDF)


  ifeq ($(DEBUG),TRUE)
    FFLAGS += -g -Mbounds -traceback -Mchkfpstk
  else
    FFLAGS += -O
  endif

endif


#------------------------------------------------------------------------
# GFORTRAN
#------------------------------------------------------------------------
#
ifeq ($(FC),gfortran)
#
  INC_NETCDF :=/usr/local/netcdf-gcc-g++-gfortran/include
  LIB_NETCDF :=/usr/local/netcdf-gcc-g++-gfortran/lib

  INC_NETCDF := /opt/local/include
  LIB_NETCDF := /opt/local/lib


  LDFLAGS = -L$(LIB_NETCDF) -lnetcdf -lnetcdff 
  FFLAGS   := -c    -I$(INC_NETCDF)



  ifeq ($(DEBUG),TRUE)
    FFLAGS += -Wall -fbacktrace -fbounds-check
  else
    FFLAGS += -O
  endif

endif




.F90.o:
	$(FC) $(FFLAGS) $<

#------------------------------------------------------------------------
# Default rules and macros
#------------------------------------------------------------------------

OBJS := fix_inland_water_elevation.o shr_kind_mod.o

$(EXEDIR)/$(EXENAME): $(OBJS)
	$(FC) -o $@ $(OBJS) $(LDFLAGS)

clean:
	$(RM) -f $(OBJS)  *.mod $(EXEDIR)/$(EXENAME)

fix_inland_water_elevation.o: shr_kind_mod.o
shared_vars.o: shr_kind_mod.o
