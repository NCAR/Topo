include experiment_settings.make
#
# specify raw dataset (netCDF format)
#


#
# Note: NO white spaces after variable assignment
#
# STANDARD CONGIGURATIONS
#
# fv0.9x1.25-gmted2010_modis-cam_fv_smooth-intermediate_ncube3000-no_anisoSGH.nc
#
#model=fv
#res=0.9x1.25
#raw_data=gmted2010_modis
#smoothing=cam_fv_smooth
#ncube=3000
#aniso=no_anisoSGH
#
# fv1.9x2.5-gmted2010_modis-cam_fv_smooth-intermediate_ncube3000-no_anisoSGH.nc
#
model=fv
res=1.9x2.5
raw_data=gmted2010_modis
smoothing=cam_fv_smooth
ncube=540
aniso=no_anisoSGH
#
# fv0.9x1.25-gmted2010_modis-cam_fv_smooth-intermediate_ncube3000-no_anisoSGH.nc
#
#model=fv
#res=0.9x1.25
#raw_data=gmted2010_modis
#smoothing=cam_fv_smooth
#ncube=3000
#aniso=no_anisoSGH

#
# se_ne30np4-gmted2010_modis-cam_se_smooth-intermediate_ncube3000-no_anisoSGH.nc
#
#model=se
#res=ne30np4
#raw_data=gmted2010
#smoothing=cam_se_smooth
#ncube=3000
#aniso=no_anisoSGH

#
# Julio developmental setup
#
# se_ne30np4-gtopo30_modis-cam_se_smooth-intermediate_ncube3000-no_anisoSGH.nc
#
#model=se
#res=ne30np4
#raw_data=gmted2010_modis
#smoothing=julio_smooth
#ncube=3000
#aniso=julio_anisoSGH

#
###########################################################################################################################################
# DONE CASE DEFINITION # DONE CASE DEFINITION # DONE CASE DEFINITION # DONE CASE DEFINITION # DONE CASE DEFINITION # DONE CASE DEFINITION #
###########################################################################################################################################
#
all: cube_to_target plot
bin_to_cube: $(intermediate_cubed_sphere_file)
cube_to_target: $(intermediate_cubed_sphere_file) $(name_list_file)
plot: cube_to_target/ncl/topo-vars-$(model)_$(res)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube)-$(aniso).pdf

#cube_to_target/ncl/topo-vars-$(model)_$(res)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube)-$(aniso).pdf:
#	(cd cube_to_target/ncl; chmod +x plot-topo-vars.sh; ./plot-topo-vars.sh $(model) $(res) $(raw_data) $(smoothing) $(ncube) $(aniso) pdf;\
#	gv topo-vars-$(model)_$(res)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube)-$(aniso).pdf)

$(name_list_file):
	if [ -s $(name_list_file) ];
	then
	  echo "file exists"
	fi
#	test -f $(nl_intermediate_cubed_sphere_file) make_namelist.sh
#
#********************************
#
# bin ~1km lat-lon data (GMTED2010, MODIS) to ~3km cubed-sphere grid
#
#********************************
#
$(intermediate_cubed_sphere_file): create_netCDF_from_rawdata/$(raw_data)-rawdata.nc
	test -f $(intermediate_cubed_sphere_file) || (cd bin_to_cube; make; chmod +x run.sh; ./run.sh $(raw_data) $(ncube))

#
#********************************
# 
# cube_to_target make commands
#
#********************************
#
cube_to_target: cube_to_target/output/$(model)_$(res)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube)-$(aniso).nc
cube_to_target/output/fv_$(res)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube)-$(aniso).nc: bin_to_cube/$(raw_data)-ncube$(ncube).nc cam_fv_topo-smoothing/$(raw_data)-$(model)_$(res)-$(smoothing).nc
	echo cube_to_target/$(model)_$(res)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube)-$(aniso).nc
	echo ./run.sh $(model)_$(res) $(raw_data) $(smoothing) $(ncube) $(aniso)
	(cd cube_to_target; make; chmod +x run.sh; ./run.sh $(model)_$(res) $(raw_data) $(smoothing) $(ncube) $(aniso))


cube_to_target/output/se_$(res)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube)-$(aniso).nc: bin_to_cube/$(raw_data)-ncube$(ncube).nc
	echo cube_to_target/$(model)_$(res)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube)-$(aniso).nc
	echo ./run.sh $(model)_$(res) $(raw_data) $(smoothing) $(ncube) $(aniso)
	(cd cube_to_target; make; chmod +x run.sh; ./run.sh $(model)_$(res) $(raw_data) $(smoothing) $(ncube) $(aniso))


cesm_compliance:
	(cd cesm_meta_data_compliance; $(python_path) meta.py ../cube_to_target/output/$(model)_$(res)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube)-$(aniso).nc $(model)_$(res)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube)-$(aniso).metadata)	



test:
	echo bin_to_cube/$(raw_data)-ncube$(ncube).nc


