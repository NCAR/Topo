EXEDIR:=.
python_command:=~pel/anaconda/bin/python
cr:=create_netCDF_from_rawdata
sm:=cam_fv_topo-smoothing

#all: ($cr)/gmted2010_elevation_and_landfrac_modis_sft.nc definesurf

raw_netCDF_gmted2010_modis: create_netCDF_from_rawdata/gmted2010_elevation_and_landfrac_modis_sft.nc
raw_netCDF_gtopo30: create_netCDF_from_rawdata/gtopo30/gtopo30-rawdata.nc
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
#
#
create_netCDF_from_rawdata/gtopo30/gtopo30-rawdata.nc:
	(cd $(cr)/gtopo30; chmod +x unpack.sh; ./unpack.sh; cd src; make; ./create_netCDF_of_gtopo30_raw_data)
#
# calculate data for CAM-FV smoothing
#
cam_fv_topo-smoothing/input/10min-phis-raw.nc: create_netCDF_from_rawdata/gmted2010_elevation_and_landfrac_modis_sft.nc
	(cd $(sm)/input; ncl < make-10min-raw-phis.ncl 'gmted2010_modis=True')
#
# perform CAM-FV smoothing
#
definesurf: $(sm)/input/10min-phis-raw.nc
	(cd $(sm); make; ./definesurf -t input/10min-phis-raw.nc  -g input/outgrid/fv_0.9x1.25.nc -l input/landm_coslat.nc -remap  fv-gmted2010-0.9x1.25.nc)
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