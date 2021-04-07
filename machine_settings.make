#
# set machine
#
export machine=cheyenne
#export machine=thorodin
ifeq ($(machine),thorodin)
  export module_gnu=compiler/gnu/8.1.0
  export module_netCDF=tool/netcdf/4.6.1/gcc-8.1.0
#  export module_python="lang/python/3.7.0"
endif
ifeq ($(machine),cheyenne)
  export module_gnu="gnu/9.1.0"
  export module_netCDF="netcdf/4.7.4"
#  module spider gdal
endif
#
# set python path
#
export python_path=/usr/local/anaconda-2.4.0/bin/
#
# Fortran settings
#
ifeq ($(machine),thorodin)
  FC=gfortran
endif
ifeq ($(machine),cheyenne)
  FC=gfortran
endif
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

#  ifeq ($(machine),thorodin)
##    INC_NETCDF=/usr/local/netcdf-gcc-g++-gfortran/include
#    INC_NETCDF=/usr/local/netcdf-c-4.6.1-f-4.4.4-gcc-g++-gfortran-8.1.0/include
###    INC_NETCDF=/usr/local/netcdf-c-4.6.1-f-4.4.4-gcc-g++-gfortran-4.8.5/include
#
##    LIB_NETCDF=/usr/local/netcdf-gcc-g++-gfortran/lib
#    LIB_NETCDF=/usr/local/netcdf-c-4.6.1-f-4.4.4-gcc-g++-gfortran-8.1.0/lib
##    LIB_NETCDF=/usr/local/netcdf-c-4.6.1-f-4.4.4-gcc-g++-gfortran-4.8.5/lib
#  endif
#
  ifeq ($(machine),cheyenne)
    LIB_NETCDF=/glade/u/apps/ch/opt/netcdf/4.6.3/gnu/9.1.0/lib
    INC_NETCDF=/glade/u/apps/ch/opt/netcdf/4.6.3/gnu/9.1.0/include
#    LIB_NETCDF=/glade/u/apps/ch/opt/netcdf/4.7.4/gnu/9.1.0/
#    INC_NETCDF=/glade/u/apps/ch/opt/netcdf/4.7.4/gnu/9.1.0/include/
  endif


#  LDFLAGS= -L$(LIB_NETCDF)  -lnetcdf -lnetcdff
#  FFLAGS= -c  -fdollar-ok  -I$(INC_NETCDF) 

  #
  # module purge
  # module load compiler/gnu/8.1.0
  #
#  LIB_NETCDF = $(NETCDF_PATH)/lib
#  INC_NETCDF = $(NETCDF_PATH)/include
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

  LDFLAGS = -L$(LIB_NETCD)F -lnetcdf -lnetcdff
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
#export LIB_NETCDF
export LDFLAGS
export FFLAGS
export FC
