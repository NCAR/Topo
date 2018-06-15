include ../machine_settings.make
EXEDIR = .
EXENAME = make_rll_grid_descriptor_file
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

OBJS :=   make_rll_grid_descriptor_file.o shr_kind_mod.o

$(EXEDIR)/$(EXENAME): $(OBJS)
	$(FC) -o $@ $(OBJS) $(LDFLAGS)
	mkdir -p output

clean:
	$(RM) -f $(OBJS)  *.mod $(EXEDIR)/$(EXENAME)

make_rll_grid_descriptor_file.o: shr_kind_mod.o
