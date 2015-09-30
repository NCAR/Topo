#
# with PGI compiler "-byteswapio" option is needed to read in the binary USGS files correctly
#
EXEDIR = .
EXENAME = create_netCDF_gtopo30_raw_data
RM = rm

.SUFFIXES:
.SUFFIXES: .F90 .o

#------------------------------------------------------------------------                  
# GFORTRAN                                                                                 
#------------------------------------------------------------------------                  
#                                                                                          
ifeq ($(FC),gfortran)
    FFLAGS += -fconvert=big-endian
endif


#------------------------------------------------------------------------
# PGF95
#------------------------------------------------------------------------
#
ifeq ($(FC),pgf95)
  FFLAGS   += -Mlarge_arrays -byteswapio
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
