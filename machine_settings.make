DEBUG=FALSE

FC ?= $(shell nf-config --fc)
# The ? mark sets the compiler to whatever compiler you've chosen in your module environment.
LDFLAGS = $(shell nf-config --flibs)
LIBS = $(shell nc-config --libs)
FFLAGS = -c $(shell nf-config --fflags)

#------------------------------------------------------------------------
# Gfortran
#------------------------------------------------------------------------
ifeq ($(findstring gfortran, $(FC)),gfortran)
    ifneq ($(findstring pgfortran, $(FC)),pgfortran)
      ifeq ($(DEBUG),TRUE)
        FFLAGS += -Wall -fbacktrace -fbounds-check -fno-range-check
	FFLAGS +=-g -O0 -fcheck=all -Wall -Wextra -Wpedantic -fbacktrace \
         -ffpe-trap=invalid,zero,overflow,underflow,denormal \
         -finit-real=nan -finit-integer=-99999999 \
         -finit-derived -finit-logical=false
      else
        FFLAGS += -O
    endif
  endif
endif

#------------------------------------------------------------------------
# NAG
#------------------------------------------------------------------------
ifeq ($(findstring nagfor, $(FC)),nagfor)
  ifeq ($(DEBUG),TRUE)
    FFLAGS += -g -C
  else
    FFLAGS += -O
  endif

endif

#------------------------------------------------------------------------
# PGF95
#------------------------------------------------------------------------
ifeq ($(findstring pgf95, $(FC)),pgf95)
  FFLAGS += -Mlarge_arrays

  ifeq ($(DEBUG),TRUE)
    FFLAGS += -g -Mbounds -traceback -Mchkfpstk
  else
    FFLAGS += -O
  endif
endif

export LDFLAGS
export FFLAGS
export FC
