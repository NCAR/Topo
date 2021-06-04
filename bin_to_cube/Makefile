include ../machine_settings.make
EXEDIR = .
EXENAME = bin_to_cube
RM = rm

.SUFFIXES:
.SUFFIXES: .F90 .o

.F90.o:
	$(FC) $(FFLAGS) $< $(LDFLAGS)

#------------------------------------------------------------------------
# Default rules and macros
#------------------------------------------------------------------------

OBJS := bin_to_cube.o shr_kind_mod.o

$(EXEDIR)/$(EXENAME): $(OBJS)
	$(FC) -o $@ $(OBJS) $(LDFLAGS)

clean:
	$(RM) -f $(OBJS)  *.mod $(EXEDIR)/$(EXENAME)

bin_to_cube.o: shr_kind_mod.o
