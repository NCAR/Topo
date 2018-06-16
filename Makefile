#
# ISSUES:
#
# 0. set lfind_ridges automatically based on rdgwin
# 1. do not look for raw data if the intermediate cubed-sphere data exists
#
include experiment_settings.make
#
#######################################################################################################
#
# DO NOT MODIFY BELOW OR YOU MIGHT VIOLATE NAMING CONVENTIONS OR WHAT RAW DATA IS USED
#
#######################################################################################################
#
raw_data=gmted2010_modis

#ncube=0540
intermediate_cubed_sphere_file=$(PWD)/bin_to_cube/gmted2010_modis-ncube$(ncube)$(stitch).nc
ncube_sph_smooth_fine=001
# MulG: valid options are '_MulG' or ''
MulG=_MulG
# PF  : valid options are '_PF' or ''
PF=_PF
case_name=nc$(ncube)_Co$(ncube_sph_smooth_coarse)_Fi$(ncube_sph_smooth_fine)$(MulG)$(PF)_nullRR
ifeq ($(smooth_topo_file_dir),)
  smooth_topo_file_dir=cube_to_target/inputdata/smooth_topo_cube
endif
smooth_topo_file=$(smooth_topo_file_dir)/topo_smooth_$(case_name)_v02.dat
nl_dir=cube_to_target/inputdata/namelist_defaults
topo_smooth_nl=$(nl_dir)/topo_smooth_$(case_name)_v02.nl
topo_smooth_nl_subdir=inputdata/namelist_defaults/topo_smooth_$(case_name)_v02.nl
topo_file_nl_subdir=inputdata/namelist_defaults/final_$(case_name)_v02$(rdgwin).nl
topo_file_nl=cube_to_target/$(topo_file_nl_subdir)
topo_file=cube_to_target/output/$(output_grid)_$(case_name).nc
#
#********************************
#
# bin ~1km lat-lon data (GMTED2010, MODIS) to ~3km cubed-sphere grid
#
#********************************
#
$(intermediate_cubed_sphere_file): create_netCDF_from_rawdata/$(raw_data)-rawdata.nc
	test -f $(intermediate_cubed_sphere_file) || (cd bin_to_cube; make; chmod +x run.sh; ./run.sh $(raw_data) $(ncube))

#cesm_compliance:
#	(cd cesm_meta_data_compliance; $(python_path) meta.py ../cube_to_target/output/$(model)_$(res)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube)-$(aniso).nc $(model)_$(res)-$(r#aw_data)-$(smoothing)-intermediate_ncube$(ncube)-$(aniso).metadata)	

$(topo_smooth_nl):
	echo 'Namelist file does not exist - creating ...'
	echo $(topo_smooth_nl)

	echo "&topoparams" 								>> $(topo_smooth_nl)
	echo "intermediate_cubed_sphere_fname = '$(intermediate_cubed_sphere_file)'" 	>> $(topo_smooth_nl)
	echo "lsmooth_on_cubed_sphere = .true."						>> $(topo_smooth_nl)
	echo "ncube_sph_smooth_coarse = $(ncube_sph_smooth_coarse)"			>> $(topo_smooth_nl)
	echo "ncube_sph_smooth_fine = $(ncube_sph_smooth_fine)"				>> $(topo_smooth_nl)
	if [ $(MulG),'_MulG' ]; then echo "luse_multigrid = .true." 		 	>> $(topo_smooth_nl); fi
	if [ $(PF),'_PF' ]; then echo "luse_prefilter = .true." 		 	>> $(topo_smooth_nl); fi
	echo "lstop_after_smoothing = .true." 		 				>> $(topo_smooth_nl)
	echo '/' >>  $(topo_smooth_nl)


$(smooth_topo_file): $(topo_smooth_nl) $(intermediate_cubed_sphere_file)
	echo "Here we go ..."
	echo "Creating "$(smooth_topo_file)
	test -f $(smooth_topo_file) || (cd cube_to_target; rm nlmain.nl;  ln -s $(topo_smooth_nl_subdir) nlmain.nl;  make; ./cube_to_target)

$(topo_file_nl):
	echo 'Namelist file does not exist - creating ...'
	echo $(topo_file_nl)

	echo "&topoparams" 								>> $(topo_file_nl)
	echo "intermediate_cubed_sphere_fname = '$(intermediate_cubed_sphere_file)'" 	>> $(topo_file_nl)
	echo "grid_descriptor_fname = '$(grid_descriptor_fname)'"		 	>> $(topo_file_nl)
	echo "output_grid='$(output_grid)'"						>> $(topo_file_nl)
	echo "lsmooth_on_cubed_sphere = .true."						>> $(topo_file_nl)
	echo "ncube_sph_smooth_coarse = $(ncube_sph_smooth_coarse)"			>> $(topo_file_nl)
	echo "ncube_sph_smooth_fine = $(ncube_sph_smooth_fine)"				>> $(topo_file_nl)
	if [ $(MulG),'_MulG' ]; then echo "luse_multigrid = .true." 		 	>> $(topo_file_nl); fi
	if [ $(PF),'_PF' ]; then echo "luse_prefilter = .true." 		 	>> $(topo_file_nl); fi
	echo "lstop_after_smoothing = .false." 		 				>> $(topo_file_nl)
	echo "lread_smooth_topofile = .true." 		 				>> $(topo_file_nl)
#	echo "lfind_ridges=$(lfind_ridges)" 		 				>> $(topo_file_nl)
	echo '/' >>  $(topo_file_nl)

#$(topo_file): $(smooth_topo_file) $(topo_file_nl)
fd: $(smooth_topo_file) $(topo_file_nl)
	echo "Map to target grid"
	(cd cube_to_target; rm nlmain.nl;  ln -s $(topo_file_nl_subdir) nlmain.nl;  make; ./cube_to_target)

test:
	echo $(smooth_topo_file)
	echo $(intermediate_cubed_sphere_file)
	echo $(ncube_sph_smooth_coarse)
	echo $(grid_descriptor_fname)
	echo $(smooth_topo_file_dir)

smooth_topo_cube_only: $(smooth_topo_file)

namelists: $(topo_file_nl) $(topo_smooth_nl)
	echo "Namelist files have been produced or already existed:"
	echo "  "
	echo "  $(topo_file_nl)"
	echo "  $(topo_smooth_nl)"



