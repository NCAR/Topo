include ../machine_settings.make
# Specify raw data. Options are:
#
#
# 	gmted2010_modis
#	gtopo30
#
#raw_data=gtopo30
#raw_data=gmted2010_modis
all: raw_netCDF_$(raw_data)
#
#***DO NOT EDIT BELOW THIS LINE*******DO NOT EDIT BELOW THIS LINE*******DO NOT EDIT BELOW THIS LINE*******DO NOT EDIT BELOW THIS LINE****
#
#
#********************************
#
# GMTED2010-MODIS make commands
#
#********************************
#
raw_netCDF_gmted2010_modis: gmted2010_modis-rawdata.nc
#
# the "test -f" commands is to make sure Make does not try to execute when the final rawdata file already exists
#
gmted2010/mea.nc:
	test -f gmted2010_modis-rawdata.nc || (cd gmted2010; make; make clean)

gmted2010/gmted2010_elevation_and_landfrac_modis_sft.nc: modis/landwater.nc gmted2010/mea.nc
	test -f gmted2010_modis-rawdata.nc || (cd gmted2010; make gmted2010_elevation_and_landfrac_modis_sft.nc)

gmted2010_modis-rawdata.nc: gmted2010/gmted2010_elevation_and_landfrac_modis_sft.nc
	test -f gmted2010_modis-rawdata.nc || (cd gmted2010/fix-inland-water-elevation; make; ./fix_inland_water_elevation)
#
# generate ~1km land fraction data from MODIS source data
#
modis/landwater.nc:
	test -f modis/landwater.nc  || (cd modis; make) #chmod +x unpack.sh; ./unpack.sh)
	test -f modis/landwater.nc  || (make clean)
#
#********************************
#
# GTOPO30 make commands
#
#********************************
#
raw_netCDF_gtopo30: gtopo30-rawdata.nc
gtopo30-rawdata.nc:
	(cd gtopo30; chmod +x unpack.sh; ./unpack.sh; cd src; make; ./create_netCDF_gtopo30_raw_data)
#
#
#
test:
	echo "test"

