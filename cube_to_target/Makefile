EXEDIR = .
EXENAME = cube_to_target
RM = rm

.SUFFIXES:
.SUFFIXES: .F90 .o

#FC = nagfor
#FC = lf95
FC = gfortran
#FC = pgf95

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
# NAG
#------------------------------------------------------------------------
ifeq ($(FC),nagfor)

#  INC_NETCDF :=/usr/local/netcdf-gcc-nag/include
#  LIB_NETCDF :=/usr/local/netcdf-pgi/lib

  INC_NETCDF :=/usr/local/netcdf-gcc-nag/include
  LIB_NETCDF :=/usr/local/netcdf-gcc-nag/lib

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
  INC_NETCDF :=/opt/local/include
  LIB_NETCDF :=/opt/local/lib

  LDFLAGS = -L$(LIB_NETCDF) -lnetcdf -lnetcdff
  FFLAGS   := -c -Mlarge_arrays -I$(INC_NETCDF)


  ifeq ($(DEBUG),TRUE)
    FFLAGS += -g -Mbounds -traceback -Mchkfpstk
  else
    FFLAGS += -O
  endif

endif

#------------------------------------------------------------------------
# LF95
#------------------------------------------------------------------------
#
# setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:/usr/local/netcdf-4.1.3-gcc-4.4.4-13-lf9581/lib
#
ifeq ($(FC),lf95)
#
# Tramhill
#
  INC_NETCDF :=/usr/local/netcdf-4.1.3-gcc-4.4.4-13-lf9581/include
  LIB_NETCDF :=/usr/local/netcdf-4.1.3-gcc-4.4.4-13-lf9581/lib

#  INC_NETCDF :=/usr/local/netcdf-gcc-lf95/include
#  LIB_NETCDF :=/usr/local/netcdf-gcc-lf95/lib

  LDFLAGS = -L$(LIB_NETCDF) -lnetcdf -lnetcdff -lcurl -lhdf5 -lhdf5_hl -mcmodel=medium 
  FFLAGS   := -c --trace --trap --wide -CcdRR8 -I$(INC_NETCDF) 


  ifeq ($(DEBUG),TRUE)
#   FFLAGS += --chk aesu  -Cpp --trace
    FFLAGS += -g --chk a,e,s,u --pca
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
# Tramhill
#
  INC_NETCDF :=/usr/local/netcdf-gcc-g++-gfortran/include
  LIB_NETCDF :=/usr/local/netcdf-gcc-g++-gfortran/lib

  LDFLAGS = -L$(LIB_NETCDF) -lnetcdf -lnetcdff 
  FFLAGS   := -c  -fdollar-ok  -I$(INC_NETCDF)


  ifeq ($(DEBUG),TRUE)
#   FFLAGS += --chk aesu  -Cpp --trace
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

OBJS :=  reconstruct.o remap.o cube_to_target.o shr_kind_mod.o shared_vars.o

$(EXEDIR)/$(EXENAME): $(OBJS)
	$(FC) -o $@ $(OBJS) $(LDFLAGS)
	mkdir -p output

clean:
	$(RM) -f $(OBJS)  *.mod $(EXEDIR)/$(EXENAME)

cube_to_target.o: shr_kind_mod.o remap.o reconstruct.o shared_vars.o
reconstruct.o: remap.o 
remap.o      : shr_kind_mod.o
shared_vars.o: shr_kind_mod.o

#reconstruct.o : shr_kind_mod.o
