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
raw_data=gmted2010_modis

#intermediate_cubed_sphere_file=/project/amp/juliob/topo-data/gmted2010_modis-ncube3000-stitch.nc
#intermediate_cubed_sphere_file=/glade/p/cgd/amp/aherring/grids/topo/gmted2010_modis-ncube3000-stitch.nc
intermediate_cubed_sphere_file=/shared/wmr/mcurry/cesmdata/topo/gmted2010_modis-ncube3000-stitch.nc
intermediate_cubed_sphere_file=/shared/wmr/mcurry/cesmdata/topo/gmted2010_bedmachine-ncube6000.nc

ncube_sph_smooth_fine=001
# MulG: valid options are '_MulG' or ''
#MulG=''
MulG=_MulG
# PF  : valid options are '_PF' or ''
PF=_PF
#PF=''
case_name=nc$(ncube)_Co$(ncube_sph_smooth_coarse)_Fi$(ncube_sph_smooth_fine)$(MulG)$(PF)_nullRR
ifeq ($(smooth_topo_file_dir),)
  smooth_topo_file_dir=cube_to_target/inputdata/smooth_topo_cube
endif
ifeq ($(rdgwin),_NoAniso)
  lfind_ridges=.false.
else
  lfind_ridges=.true.
endif
# smooth topo data on intermediate cubed-sphere grid
smooth_topo_file=$(smooth_topo_file_dir)/topo_smooth_$(case_name)_v02.dat
topo_smooth_nl=cube_to_target/output/topo_smooth_$(case_name)_v02.nl
topo_smooth_nl_subdir=output/topo_smooth_$(case_name)_v02.nl
topo_file_nl_subdir=output/final_$(output_grid)_$(case_name)_v02$(rdgwin).nl
topo_file_nl=cube_to_target/$(topo_file_nl_subdir)
topo_file=cube_to_target/output/$(output_grid)_$(case_name).nc
#
#********************************
#
# bin ~1km lat-lon data (GMTED2010, MODIS) to ~3km cubed-sphere grid
#
#********************************
#
all: $(smooth_topo_file) $(topo_file)

create_netCDF_from_rawdata/$(raw_data)-rawdata.nc:
	test -f $(create_netCDF_from_rawdata/$(raw_data)-rawdata.nc) ||(cd create_netCDF_from_rawdata; make)

$(intermediate_cubed_sphere_file): create_netCDF_from_rawdata/$(raw_data)-rawdata.nc
	test -f $(intermediate_cubed_sphere_file) || (cd bin_to_cube; make; chmod +x run.sh; ./run.sh $(raw_data) $(ncube))

cesm_compliance:
	(cd cesm_meta_data_compliance; $(python_path) meta.py ../cube_to_target/output/$(model)_$(res)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube)-$(aniso).nc $(model)_$(res)-$(r#aw_data)-$(smoothing)-intermediate_ncube$(ncube)-$(aniso).metadata)	


$(smooth_topo_file): $(intermediate_cubed_sphere_file)
	echo 'Namelist file does not exist - creating: '$(topo_smooth_nl)
	echo ' '
	echo 'case_name is '$(case_name)
	echo "&topoparams" 								>> $(topo_smooth_nl)
	echo "intermediate_cubed_sphere_fname = '$(intermediate_cubed_sphere_file)'" 	>> $(topo_smooth_nl)
	echo "lsmooth_on_cubed_sphere = .true."						>> $(topo_smooth_nl)
	echo "grid_descriptor_fname = '$(grid_descriptor_fname)'"		 	>> $(topo_smooth_nl)
	echo "ncube_sph_smooth_coarse = $(ncube_sph_smooth_coarse)"			>> $(topo_smooth_nl)
	echo "ncube_sph_smooth_fine = $(ncube_sph_smooth_fine)"				>> $(topo_smooth_nl)
	echo "lstop_after_smoothing = .true." 		 				>> $(topo_smooth_nl)
	echo "lfind_ridges=.false."        		 				>> $(topo_smooth_nl)
	echo "luse_multigrid = .true." 	                                	 	>> $(topo_smooth_nl)
	echo "luse_prefilter = .true."                          		 	>> $(topo_smooth_nl)
	echo '/' >>  $(topo_smooth_nl)
	test -f $(smooth_topo_file) || (echo "Creating "$(smooth_topo_file); cd cube_to_target; rm -f nlmain.nl;  ln -s $(topo_smooth_nl_subdir) nlmain.nl;  make; ./cube_to_target)

$(topo_file): $(smooth_topo_file)
	echo 'Namelist file does not exist - creating '$(topo_file_nl)
	echo ' '
	echo "&topoparams" 								>> $(topo_file_nl)
	echo "intermediate_cubed_sphere_fname = '$(intermediate_cubed_sphere_file)'" 	>> $(topo_file_nl)
	echo "grid_descriptor_fname = '$(grid_descriptor_fname)'"		 	>> $(topo_file_nl)
	echo "output_grid='$(output_grid)'"						>> $(topo_file_nl)
	echo "lsmooth_on_cubed_sphere = .true."						>> $(topo_file_nl)
	echo "ncube_sph_smooth_coarse = $(ncube_sph_smooth_coarse)"			>> $(topo_file_nl)
	echo "ncube_sph_smooth_fine = $(ncube_sph_smooth_fine)"				>> $(topo_file_nl)
	echo "luse_multigrid = .true." 		 					>> $(topo_file_nl)
	echo "luse_prefilter = .true." 		 					>> $(topo_file_nl)
	echo "lregional_refinement=$(lregional_refinement)"				>> $(topo_file_nl)
	echo "rrfac_max=${rrfac_max}"							>> $(topo_file_nl)
	echo "lstop_after_smoothing = .false." 		 				>> $(topo_file_nl)
	echo "lread_smooth_topofile = $(lread_smooth_topofile)" 			>> $(topo_file_nl)
	echo "lfind_ridges=$(lfind_ridges)" 		 				>> $(topo_file_nl)
	echo "nwindow_halfwidth=$(nwindow_halfwidth)" 		 			>> $(topo_file_nl)
	echo '/' >>  $(topo_file_nl)
	test -f $(topo_file) || (cd cube_to_target; rm -f nlmain.nl; ln -s $(topo_file_nl_subdir) nlmain.nl; make; ./cube_to_target )

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

# generate ~1km GMTED2010 data from source
##
create_netCDF_from_rawdata/gmted2010/mea.nc:
	test -f create_netCDF_from_rawdata/gmted2010_modis-rawdata.nc || (cd create_netCDF_from_rawdata/gmted2010; chmod +x unpack.sh; ./unpack.sh)

clean:
	cd bin_to_cube; $(MAKE) clean
	cd cube_to_target; $(MAKE) clean
	rm -f modules_loaded.txt
