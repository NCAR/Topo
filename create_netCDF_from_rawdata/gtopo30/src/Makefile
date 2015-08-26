#
# with PGI compiler "-byteswapio" option is needed to read in the binary USGS files correctly
#
EXEDIR = .
EXENAME = create_netCDF
RM = rm

.SUFFIXES:
.SUFFIXES: .F90 .o


FC = gfortran
#FC = pgf95



#------------------------------------------------------------------------                  
# GFORTRAN                                                                                 
#------------------------------------------------------------------------                  
#                                                                                          
ifeq ($(FC),gfortran)
  INC_NETCDF :=/usr/local/netcdf-gcc-g++-gfortran/include
  LIB_NETCDF :=/usr/local/netcdf-gcc-g++-gfortran/lib

  INC_NETCDF := /opt/local/include
  LIB_NETCDF := /opt/local/lib


  LDFLAGS = -L$(LIB_NETCDF) -lnetcdf -lnetcdff
  FFLAGS   := -c    -I$(INC_NETCDF)



  ifeq ($(DEBUG),TRUE)
#   FFLAGS += --chk aesu  -Cpp --trace                                                     
    FFLAGS += -Wall -fbacktrace -fbounds-checki -fno-range-check -fconvert=big-endian
  else
    FFLAGS += -O -fconvert=big-endian
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
    FFLAGS += -g -byteswapio
  else
    FFLAGS += -O -byteswapio
  endif

endif


.F90.o:
	$(FC) $(FFLAGS) $<

#------------------------------------------------------------------------
# Default rules and macros
#------------------------------------------------------------------------

OBJS := create_netCDF_from_USGS.o shr_kind_mod.o

$(EXEDIR)/$(EXENAME): $(OBJS)
	$(FC) -o $@ $(OBJS) $(LDFLAGS)

clean:
	$(RM) -f $(OBJS)  *.mod $(EXEDIR)/$(EXENAME)

create_netCDF_from_USGS.o: shr_kind_mod.o
