include ../machine_settings.make

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
ifeq ($(machine),my_mac)
  python_command:=~pel/anaconda/bin/python
else
  python_command:=python
endif
cr:=create_netCDF_from_rawdata
sm:=cam_fv_topo-smoothing

all: cube_to_target plot
bin_to_cube: bin_to_cube/$(raw_data)-ncube$(ncube).nc 
cube_to_target: bin_to_cube/$(raw_data)-ncube$(ncube).nc cube_to_target/output/$(model)_$(res)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube)-$(aniso).nc
plot: cube_to_target/ncl/topo-vars-$(model)_$(res)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube)-$(aniso).pdf

cube_to_target/ncl/topo-vars-$(model)_$(res)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube)-$(aniso).pdf:
	(cd cube_to_target/ncl; chmod +x plot-topo-vars.sh; ./plot-topo-vars.sh $(model) $(res) $(raw_data) $(smoothing) $(ncube) $(aniso) pdf;\
	gv topo-vars-$(model)_$(res)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube)-$(aniso).pdf)

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

#
#********************************
#
# smooth PHIS the CAM-FV way
#
#********************************
#
cam_fv_smooth:  cam_fv_topo-smoothing/$(raw_data)-$(model)_$(res)-$(smoothing).nc
cam_fv_topo-smoothing/$(raw_data)-$(model)_$(res)-$(smoothing).nc: $(sm)/input/10min-$(raw_data)-phis-raw.nc
	(cd $(sm)/definesurf; make; ./definesurf -t ../input/10min-$(raw_data)-phis-raw.nc  -g ../input/outgrid/$(model)_$(res).nc -l ../input/landm_coslat.nc -remap ../$(raw_data)-$(model)_$(res)-$(smoothing).nc)
cam_fv_topo-smoothing/input/10min-gmted2010_modis-phis-raw.nc: create_netCDF_from_rawdata/gmted2010_modis-rawdata.nc
	(cd $(sm)/input; ncl < make-10min-raw-phis.ncl 'gmted2010_modis=True')
cam_fv_topo-smoothing/input/10min-gtopo30-phis-raw.nc: create_netCDF_from_rawdata/gtopo30-rawdata.nc
	(cd $(sm)/input; ncl < make-10min-raw-phis.ncl 'gmted2010_modis=False')
#
# calculate data for CAM-FV smoothing
#

#********************************
#
# bin ~1km lat-lon data (GMTED2010, MODIS) to ~3km cubed-sphere grid
#
#********************************
#
bin_to_cube/$(raw_data)-ncube$(ncube).nc: create_netCDF_from_rawdata/$(raw_data)-rawdata.nc
	(cd bin_to_cube; make; chmod +x run.sh; ./run.sh $(raw_data) $(ncube))
#bin_to_cube/$(raw_data)-ncube$(ncube).nc: create_netCDF_from_rawdata/$(raw_data)-rawdata.nc raw_netCDF_$(raw_data)
#	(cd bin_to_cube; make; chmod +x run.sh; ./run.sh $(raw_data) $(ncube))
test:
	echo bin_to_cube/$(raw_data)-ncube$(ncube).nc


