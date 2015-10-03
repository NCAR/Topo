#
# Format for data files: model-raw_data-smoothing
#
#
# STANDARD CONGIGURATION
#
# fv0.9x1.25-gmted2010_modis-cam_fv_smooth-intermediate_ncube$(ncube).nc
#
model=fv0.9x1.25
raw_data=gmted2010_modis
smoothing=cam_fv_smooth
ncube=3000


#model=fv0.9x1.25
#model=ne30np4
#raw_data=gmted2010_modis
#raw_data=gtopo30
#smoothing=cam_fv_smooth



#
# DO NOT EDIT BELOW THIS LINE
#
python_command:=~pel/anaconda/bin/python
cr:=create_netCDF_from_rawdata
sm:=cam_fv_topo-smoothing

all: cube_to_target plot
rawdata: raw_netCDF_$(raw_data) 
bin_to_cube: bin_to_cube/$(raw_data)-ncube$(ncube).nc
cube_to_target: bin_to_cube/$(raw_data)-ncube$(ncube).nc cube_to_target/output/$(model)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube).nc
plot: cube_to_target/ncl/topo-vars-$(model)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube).pdf

cube_to_target/ncl/topo-vars-$(model)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube).pdf:
	(cd cube_to_target/ncl; chmod +x plot-topo-vars.sh; ./plot-topo-vars.sh $(model) $(raw_data) $(smoothing) $(ncube) pdf;\
	gv topo-vars-$(model)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube).pdf)

#
#********************************
# 
# cube_to_target make commands
#
#********************************
#
cube_to_target: cube_to_target/output/$(model)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube).nc
cube_to_target/output/fv0.9x1.25-gmted2010_modis-cam_fv_smooth-intermediate_ncube$(ncube).nc: bin_to_cube/gmted2010_modis-ncube$(ncube).nc cam_fv_topo-smoothing/gmted2010_modis-fv0.9x1.25-cam_fv_smooth.nc
	echo cube_to_target/$(model)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube).nc
	echo ./run.sh $(model) $(raw_data) $(smoothing) $(ncube)
	(cd cube_to_target; make; chmod +x run.sh; ./run.sh $(model) $(raw_data) $(smoothing) $(ncube))
	(cd cesm_meta_data_compliance; $(python_command) meta.py ../cube_to_target/output/fv0.9x1.25-gmted2010_modis-cam_fv_smooth-intermediate_ncube$(ncube).nc fv0.9x1.25-gmted2010_modis-cam_fv_smooth-intermediate_ncube$(ncube).metadata)

cube_to_target/output/fv0.9x1.25-gtopo30-cam_fv_smooth-intermediate_ncube$(ncube).nc: bin_to_cube/gtopo30-ncube$(ncube).nc cam_fv_topo-smoothing/gtopo30-fv0.9x1.25-cam_fv_smooth.nc
	echo cube_to_target/$(model)-$(raw_data)-$(smoothing).nc
	(cd cube_to_target; make; chmod +x run.sh; ./run.sh $(model) $(raw_data) $(smoothing))
	(cd cesm_meta_data_compliance; $(python_command) meta.py ../cube_to_target/output/fv0.9x1.25-gtopo30-cam_fv_smooth-intermediate_ncube$(ncube).nc fv0.9x1.25-gtopo30-cam_fv_smooth-intermediate_ncube$(ncube).metadata)
cube_to_target/output/ne30np40-gtopo30-no_smooth.nc: bin_to_cube/gtopo30-ncube$(ncube).nc
	echo cube_to_target/$(model)-$(raw_data)-$(smoothing)-intermediate_ncube$(ncube).nc
	(cd cube_to_target; make; chmod +x run.sh; ./run.sh $(model) $(raw_data) $(smoothing))

#********************************
#
# smooth PHIS the CAM-FV way
#
#********************************
#
cam_fv_smooth:  cam_fv_topo-smoothing/$(raw_data)-$(model)-$(smoothing).nc
cam_fv_topo-smoothing/$(raw_data)-$(model)-$(smoothing).nc: $(sm)/input/10min-$(raw_data)-phis-raw.nc
	(cd $(sm)/definesurf; make; ./definesurf -t ../input/10min-$(raw_data)-phis-raw.nc  -g ../input/outgrid/$(model).nc -l ../input/landm_coslat.nc -remap ../$(raw_data)-$(model)-$(smoothing).nc)
cam_fv_topo-smoothing/input/10min-gmted2010_modis-phis-raw.nc: create_netCDF_from_rawdata/gmted2010_modis-rawdata.nc
	(cd $(sm)/input; ncl < make-10min-raw-phis.ncl 'gmted2010_modis=True')
cam_fv_topo-smoothing/input/10min-gtopo30-phis-raw.nc: create_netCDF_from_rawdata/gtopo30-rawdata.nc
	(cd $(sm)/input; ncl < make-10min-raw-phis.ncl 'gmted2010_modis=False')

#
#********************************
#
# raw data Make commands
#
#********************************
#
raw_netCDF_gmted2010_modis: create_netCDF_from_rawdata/gmted2010_modis-rawdata.nc
create_netCDF_from_rawdata/gmted2010_elevation_and_landfrac_modis_sft.nc: $(cr)/modis/landwater.nc $(cr)/gmted2010/mea.nc
	$(python_command) $(cr)/create_gmted2010_modis.py $(cr)/modis/landwater.nc $(cr)/gmted2010/mea.nc $(cr)/gmted2010_elevation_and_landfrac_modis.nc
	$(python_command) $(cr)/shift.py $(cr)/gmted2010_elevation_and_landfrac_modis.nc $(cr)/gmted2010_elevation_and_landfrac_modis_sft.nc
	rm create_netCDF_from_rawdata/gmted2010_elevation_and_landfrac_modis.nc
create_netCDF_from_rawdata/gmted2010_modis-rawdata.nc: create_netCDF_from_rawdata/gmted2010_elevation_and_landfrac_modis_sft.nc
	(cd create_netCDF_from_rawdata/gmted2010/fix-inland-water-elevation; make; ./fix_inland_water_elevation)
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
# GTOPO30
#
raw_netCDF_gtopo30: create_netCDF_from_rawdata/gtopo30-rawdata.nc
create_netCDF_from_rawdata/gtopo30-rawdata.nc:
	(cd $(cr)/gtopo30; chmod +x unpack.sh; ./unpack.sh; cd src; make; ./create_netCDF_gtopo30_raw_data)
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

#
#=====================================================================================================================
#
# user settings (compiler)
#
FC = gfortran
#FC = nagfor
#FC = pgf95
DEBUG=FALSE
export FC

machine=MAC

 # default settings
 LIB_NETCDF := /opt/local/lib
 INC_NETCDF := /opt/local/include

#------------------------------------------------------------------------
# GFORTRAN
#------------------------------------------------------------------------
#
ifeq ($(FC),gfortran)
  ifeq ($(machine),MAC)
    INC_NETCDF := /opt/local/include
    LIB_NETCDF := /opt/local/lib
  endif
  ifeq ($(machine),HARMON)
    INC_NETCDF :=/usr/local/netcdf-gcc-g++-gfortran/include
    LIB_NETCDF :=/usr/local/netcdf-gcc-g++-gfortran/lib
  endif

  LDFLAGS = -L$(LIB_NETCDF) -lnetcdf -lnetcdff 
  FFLAGS   := -c  -fdollar-ok  -I$(INC_NETCDF)

  ifeq ($(DEBUG),TRUE)
#   FFLAGS += --chk aesu  -Cpp --trace
    FFLAGS += -Wall -fbacktrace -fbounds-check -fno-range-check
  else
    FFLAGS += -O
  endif

endif

#------------------------------------------------------------------------
# NAG
#------------------------------------------------------------------------
ifeq ($(FC),nagfor)

#  INC_NETCDF :=/usr/local/netcdf-gcc-nag/include
#  LIB_NETCDF :=/usr/local/netcdf-pgi/lib

  INC_NETCDF :=/usr/local/netcdf-gcc-nag/include
  LIB_NETCDF :=/usr/local/netcdf-gcc-nag/lib

  LDFLAGS = -L$(LIB_NETCDF) -lnetcdf -lnetcdff
  FFLAGS   := -c  -I$(INC_NETCDF)


  ifeq ($(DEBUG),TRUE)
    FFLAGS += -g -C
  else
    FFLAGS += -O
  endif

endif
#------------------------------------------------------------------------
# PGF95
#------------------------------------------------------------------------
#
# setenv LD_LIBRARY_PATH ${LD_LIBRARY_PATH}:/usr/local/netcdf-4.1.3-gcc-4.4.4-13-lf9581/lib
#

ifeq ($(FC),pgf95)
  INC_NETCDF :=/opt/local/include
  LIB_NETCDF :=/opt/local/lib

  LDFLAGS = -L$(LIB_NETCDF) -lnetcdf -lnetcdff
  FFLAGS   := -c -Mlarge_arrays -I$(INC_NETCDF)


  ifeq ($(DEBUG),TRUE)
    FFLAGS += -g -Mbounds -traceback -Mchkfpstk
  else
    FFLAGS += -O
  endif

endif


export INC_NETCDF
export LIB_NETCDF
export LDFLAGS
export FFLAGS

