SHELL   =/usr/bin/tcsh
#
# ISSUES:
#
# 0. set lfind_ridges automatically based on rdgwin
# 1. do not look for raw data if the intermediate cubed-sphere data exists
#
include experiment_settings.make
include machine_settings.make
#
#######################################################################################################
#
# DO NOT MODIFY BELOW OR YOU MIGHT VIOLATE NAMING CONVENTIONS OR WHAT RAW DATA IS USED
#
#######################################################################################################
#
all: 
#
# low res regression test: smooth on ncube=540 intermediate cubed-sphere grid and map to ne30pg3
#
regr1:
	cd cube_to_target; make; source regr_test1.sh

clean:
	cd bin_to_cube; $(MAKE) clean
	cd cube_to_target; $(MAKE) clean
	rm -f modules_loaded.txt
