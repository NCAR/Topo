#
# user settings
#
# fv0.9x1.25-gmted2010_modis-smooth_cam.nc
#
model=fv0.9x1.25
raw_data=gmted2010_modis
#raw_data=gtopo30
smoothing=smooth_cam
#
# DO NOT EDIT BELOW THIS LINE
#
python_command:=~pel/anaconda/bin/python
cr:=create_netCDF_from_rawdata
sm:=cam_fv_topo-smoothing

init: init_$(raw_data) 
cube_to_target: bin_to_cube/gmted2010_modis-ncube3000.nc cube_to_target/output/$(model)-$(raw_data)-$(smoothing).nc
plot:
	(cd cube_to_target/ncl; chmod +x plot-topo-vars.sh; ./plot-topo-vars.sh $(model) $(raw_data) $(smoothing) pdf;\
	gv topo-vars-$(model)-$(raw_data)-$(smoothing).pdf)
#
# generate intermediate cubed-sphere data from raw data
#
init_gmted2010_modis: raw_netCDF_gmted2010_modis cam_fv_smooth_gmted2010_modis bin_to_cube_gmted2010
init_gtopo30: raw_netCDF_gtopo30 cam_fv_smooth_gtopo30 bin_to_cube_gtopo30

raw_netCDF_gmted2010_modis: create_netCDF_from_rawdata/gmted2010_elevation_and_landfrac_modis_sft_fix_inland_water_elevation.nc
raw_netCDF_gtopo30: create_netCDF_from_rawdata/gtopo30/gtopo30-rawdata.nc
cam_fv_smooth_gmted2010_modis:  cam_fv_topo-smoothing/fv-gmted2010_modis-0.9x1.25.nc
cam_fv_smooth_gtopo30:  cam_fv_topo-smoothing/fv-gtopo30-0.9x1.25.nc
bin_to_cube_gmted2010: bin_to_cube/gmted2010_modis-ncube3000.nc
bin_to_cube_gtopo30: bin_to_cube/gtopo30-ncube3000.nc
#
# merge GMTED2010 elevation data and MODIS land fraction data: raw input data used in bin_to_cube
#
create_netCDF_from_rawdata/gmted2010_elevation_and_landfrac_modis_sft.nc: $(cr)/modis/landwater.nc $(cr)/gmted2010/mea.nc
	$(python_command) $(cr)/create_gmted2010_modis.py $(cr)/modis/landwater.nc $(cr)/gmted2010/mea.nc $(cr)/gmted2010_elevation_and_landfrac_modis.nc
	$(python_command) $(cr)/shift.py $(cr)/gmted2010_elevation_and_landfrac_modis.nc $(cr)/gmted2010_elevation_and_landfrac_modis_sft.nc
	rm create_netCDF_from_rawdata/gmted2010_elevation_and_landfrac_modis.nc

create_netCDF_from_rawdata/gmted2010_elevation_and_landfrac_modis_sft_fix_inland_water_elevation.nc: create_netCDF_from_rawdata/gmted2010_elevation_and_landfrac_modis_sft.nc
	(cd create_netCDF_from_rawdata/gmted2010/fix-inland-water-elevation; make; ./fix_inland_water_elevation)
	rm create_netCDF_from_rawdata/gmted2010_elevation_and_landfrac_modis_sft.nc

#
# generate ~1km land fraction data from MODIS source data
#
create_netCDF_from_rawdata/modis/landwater.nc:
	(cd $(cr)/modis; chmod +x unpack.sh; ./unpack.sh)
#
# generate ~1km GMTED2010 data from source
#
create_netCDF_from_rawdata/gmted2010/mea.nc:
	(cd $(cr)/gmted2010; chmod +x unpack.sh; ./unpack.sh)
#
#
#
create_netCDF_from_rawdata/gtopo30/gtopo30-rawdata.nc:
	(cd $(cr)/gtopo30; chmod +x unpack.sh; ./unpack.sh; cd src; make; ./create_netCDF_of_gtopo30_raw_data)
#
# calculate data for CAM-FV smoothing
#
cam_fv_topo-smoothing/input/10min-gmted2010-modis-phis-raw.nc: create_netCDF_from_rawdata/gmted2010_elevation_and_landfrac_modis_sft.nc
	(cd $(sm)/input; ncl < make-10min-raw-phis.ncl 'gmted2010_modis=True')
cam_fv_topo-smoothing/input/10min-gtopo30-phis-raw.nc: create_netCDF_from_rawdata/gtopo30/gtopo30-rawdata.nc
	(cd $(sm)/input; ncl < make-10min-raw-phis.ncl 'gmted2010_modis=False')
#
# perform CAM-FV smoothing
#
cam_fv_topo-smoothing/fv-gmted2010_modis-0.9x1.25.nc: $(sm)/input/10min-gmted2010-modis-phis-raw.nc
	(cd $(sm); make; ./definesurf -t input/10min-gmted2010-modis-phis-raw.nc  -g input/outgrid/fv_0.9x1.25.nc -l input/landm_coslat.nc -remap  fv-gmted2010_modis-0.9x1.25.nc)

cam_fv_topo-smoothing/fv-gtopo30-0.9x1.25.nc: $(sm)/input/10min-gtopo30-phis-raw.nc
	(cd $(sm); make; ./definesurf -t input/10min-gtopo30-phis-raw.nc  -g input/outgrid/fv_0.9x1.25.nc -l input/landm_coslat.nc -remap  fv-gtopo30-0.9x1.25.nc)
#
# bin ~1km lat-lon data (GMTED2010, MODIS) to ~3km cubed-sphere grid
#
bin_to_cube/gmted2010_modis-ncube3000.nc: create_netCDF_from_rawdata/gmted2010_elevation_and_landfrac_modis_sft.nc
	(cd bin_to_cube; make; chmod +x run.sh; ./run.sh gmted2010)
bin_to_cube/gtopo30-ncube3000.nc: create_netCDF_from_rawdata/gmted2010_elevation_and_landfrac_modis_sft.nc
	(cd bin_to_cube; make; chmod +x run.sh; ./run.sh gtopo30)
#
#
#
cube_to_target/output/fv0.9x1.25-gmted2010_modis-smooth_cam.nc: bin_to_cube/gmted2010_modis-ncube3000.nc cam_fv_topo-smoothing/fv-gmted2010_modis-0.9x1.25.nc
	echo asdfsadf
	echo cube_to_target/$(model)-$(raw_data)-$(smoothing).nc
	echo sadfsadfsadf
	echo ./run.sh $(model) $(raw_data) $(smoothing)
	(cd cube_to_target; make; chmod +x run.sh; ./run.sh $(model) $(raw_data) $(smoothing))
cube_to_target/output/fv0.9x1.25-gtopo30-smooth_cam.nc: bin_to_cube/gtopo30-ncube3000.nc cam_fv_topo-smoothing/fv-gtopo30-0.9x1.25.nc
	echo cube_to_target/$(model)-$(raw_data)-$(smoothing).nc
	(cd cube_to_target; make; chmod +x run.sh; ./run.sh $(model) $(raw_data) $(smoothing))