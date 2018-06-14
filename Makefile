include experiment_settings.make
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
	(cd cube_to_target; rm nlmain.nl;  ln -s $(topo_smooth_nl_subdir) nlmain.nl;  make; ./cube_to_target)

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
	echo '/' >>  $(topo_file_nl)

#$(topo_file): $(smooth_topo_file) $(topo_file_nl)
fd: $(smooth_topo_file) $(topo_file_nl)
	echo "Map to target grid"
	(cd cube_to_target; rm nlmain.nl;  ln -s $(topo_file_nl_subdir) nlmain.nl;  make; ./cube_to_target)

test: $(smooth_topo_file)
	echo $(smooth_topo_file)
	echo $(intermediate_cubed_sphere_file)




