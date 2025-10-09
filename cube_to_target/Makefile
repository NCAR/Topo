include ./machine_settings.make
EXEDIR = .
EXENAME = cube_to_target
RM = rm

.SUFFIXES:
.SUFFIXES: .F90 .o

.F90.o:
	$(FC) $(FFLAGS) $< $(LDFLAGS) $(LIBS)

#------------------------------------------------------------------------
# Default rules and macros
#------------------------------------------------------------------------

OBJS := reconstruct.o remap.o shr_kind_mod.o shared_vars.o rot.o smooth_topo_cube.o ridge_utils.o ridge_ana.o cube_to_target.o f90getopt.o mask.o

$(EXEDIR)/$(EXENAME): $(OBJS)
	$(FC) -o $@ $(OBJS) $(LDFLAGS) $(LIBS)
	mkdir -p output

clean:
	$(RM) -f $(OBJS)  *.mod $(EXEDIR)/$(EXENAME) output/*.nl git_version.F90

cube_to_target.o: shr_kind_mod.o remap.o reconstruct.o shared_vars.o f90getopt.o mask.o git_version.o
reconstruct.o: remap.o 
remap.o      : shr_kind_mod.o
shared_vars.o: shr_kind_mod.o
ridge_ana.o: ridge_utils.o
git_version.F90:
	@{ \
	  GIT_DESCRIBE="$$(git describe --tags --always --dirty 2>/dev/null || echo unknown)"; \
	  GIT_HASH="$$(git rev-parse --short HEAD 2>/dev/null || echo unknown)"; \
	  GIT_BRANCH="$$(git rev-parse --abbrev-ref HEAD 2>/dev/null || echo unknown)"; \
	  GIT_DATE="$$(git log -1 --format=%cd --date=iso-strict 2>/dev/null || echo unknown)"; \
	  echo "module git_version"; \
	  echo "  implicit none"; \
	  echo "  character(len=*), parameter :: GIT_DESCRIBE = '$$GIT_DESCRIBE'"; \
	  echo "  character(len=*), parameter :: GIT_HASH     = '$$GIT_HASH'"; \
	  echo "  character(len=*), parameter :: GIT_BRANCH   = '$$GIT_BRANCH'"; \
	  echo "  character(len=*), parameter :: GIT_DATE     = '$$GIT_DATE'"; \
	  echo "end module git_version"; \
	} > $@

