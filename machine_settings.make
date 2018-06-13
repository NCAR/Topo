#
# set machine
#
machine=thorodin
#
# set python path
#
export python_path=/usr/local/anaconda-2.4.0/bin/
#
# Fortran settings
#
FC=gfortran
#FC = nagfor
#FC = pgf95
DEBUG=FALSE



# default settings
# LIB_NETCDF := /opt/local/lib
# INC_NETCDF := /opt/local/include
#------------------------------------------------------------------------
# GFORTRAN
#------------------------------------------------------------------------
#
ifeq ($(FC),gfortran)
  ifeq ($(machine),thorodin)
    INC_NETCDF=/usr/local/netcdf-gcc-g++-gfortran/include
    LIB_NETCDF=/usr/local/netcdf-gcc-g++-gfortran/lib
  endif

  LDFLAGS= -L$(LIB_NETCDF) -lnetcdf -lnetcdff
  FFLAGS= -c  -fdollar-ok  -I$(INC_NETCDF)

  ifeq ($(DEBUG),TRUE)
#   FFLAGS += --chk aesu  -Cpp --trace
    FFLAGS += -Wall -fbacktrace -fbounds-check -fno-range-check
  else
    FFLAGS += -O
  endif

endif

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

export INC_NETCDF
export LIB_NETCDF
export LDFLAGS
export FFLAGS
export FC

