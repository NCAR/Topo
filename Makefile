EXEDIR:=.
python_command:=~pel/anaconda/bin/python
cr:=create_netCDF_from_rawdata
sm:=cam_fv_topo-smoothing

#all: ($cr)/gmted2010_elevation_and_landfrac_modis_sft.nc definesurf

raw_data: create_netCDF_from_rawdata/gmted2010_elevation_and_landfrac_modis_sft.nc
cam_fv_smooth: definesurf
bin_to_cube: bin_to_cube/gmted2010-modis-ncube3000.nc
cube_to_target: cube_to_target/out.nc
#
# merge GMTED2010 elevation data and MODIS land fraction data: raw input data used in bin_to_cube
#
create_netCDF_from_rawdata/gmted2010_elevation_and_landfrac_modis_sft.nc: $(cr)/modis/landwater.nc $(cr)/gmted2010/mea.nc
	$(python_command) $(cr)/create_gmted2010_modis.py $(cr)/modis/landwater.nc $(cr)/gmted2010/mea.nc $(cr)/gmted2010_elevation_and_landfrac_modis.nc
	$(python_command) $(cr)/shift.py $(cr)/gmted2010_elevation_and_landfrac_modis.nc $(cr)/gmted2010_elevation_and_landfrac_modis_sft.nc
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
# calculate data for CAM-FV smoothing
#
cam_fv_topo-smoothing/input/10min-phis-raw.nc: create_netCDF_from_rawdata/gmted2010_elevation_and_landfrac_modis_sft.nc
	(cd $(sm)/input; ncl < make-10min-raw-phis.ncl)
#
# perform CAM-FV smoothing
#
definesurf: $(sm)/input/10min-phis-raw.nc
	(cd $(sm); make; source run.sh)
#
# bin ~1km lat-lon data (GMTED2010, MODIS) to ~3km cubed-sphere grid
#
bin_to_cube/gmted2010-modis-ncube3000.nc: create_netCDF_from_rawdata/gmted2010_elevation_and_landfrac_modis_sft.nc
	(cd bin_to_cube; make; ./bin_to_cube)
#
#
#
cube_to_target/out.nc:  bin_to_cube/gmted2010-modis-ncube3000.nc
	(cd cube_to_target; make; ./cube_to_target)