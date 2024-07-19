#!/bin/tcsh

if ( "$#argv" != 3) then
  echo "Wrong number of arguments specified:"
  set n = 1
  echo "          ogrid   = argv[1]"
  echo "smoothing scale   = argv[2]"
  echo "            tag   = argv[3]"
  echo "     "
  echo "possible ogrid values: fv_0.9x1.25, ne30pg3 ..."
  exit
endif

set case = $argv[1]"_Sco"$argv[2]"_"$argv[3]
echo $case

mkdir -p ../cases/${case}/output
cp *.F90 ../cases/${case}
cp Makefile ../cases/${case}
cp ./machine_settings.make ../cases/${case}

# Copy myself for record-keeping
cp create_topo.csh ../cases/${case}

cd ../cases/${case}

# Assumes you are in the right directory, i.e, the one with F90 files and namelists
#----------------------------------------------------------------------------------
mkdir -p output
mkdir -p output/raw
mkdir -p output/clean


#module load compiler/gnu/default

module load gcc
gmake -f Makefile clean
gmake -f Makefile



# Set output grid (i.e. model grid)
set n = 1
set ogrid = "$argv[$n]"

# Set "smoothing scale"
set n = 2
set Co = "$argv[$n]"
set Fi = "0"

echo "Here you are "
set Yfac = "1"

if ( $ogrid == 'geos_fv_c48' ) then
   set scrip='PE48x288-CF.nc4'
endif
if ( $ogrid == 'geos_fv_c90' ) then
   set scrip='PE90x540-CF.nc4'
endif
if ( $ogrid == 'geos_fv_c180' ) then
   set scrip='PE180x1080-CF.nc4'
endif
if ( $ogrid == 'geos_fv_c360' ) then
   set scrip='PE360x2160-CF.nc4'
endif
if ( $ogrid == 'geos_fv_c720' ) then
   set scrip='PE720x4320-CF.nc4'
endif
if ( $ogrid == 'geos_fv_c1440' ) then
   set scrip='PE1440x8640-CF.nc4'
endif
if ( $ogrid == 'fv_0.9x1.25' ) then
   set scrip='fv_0.9x1.25.nc'
endif
if ( $ogrid == 'scam' ) then
   set scrip='fv_0.9x1.25.nc'
endif
if ( $ogrid == 'ne120pg3' ) then
   set scrip='ne120pg3_scrip_170628.nc'
endif
if ( $ogrid == 'ne240pg3' ) then
   set scrip='ne240pg3_scrip_170628.nc'
endif
if ( $ogrid == 'ne30pg3' ) then
   set scrip='ne30pg3.nc'
endif
if ( $ogrid == 'Arctic' ) then
   ## set scrip='ne0ARCTICne30x4_scrip_c191212.nc'
   set scrip='ne0ARCTICne30x4_np4_SCRIP.nc'
   set Yfac = "4"
endif
if ( $ogrid == 'POLARRES' ) then
   ## set scrip='ne0ARCTICne30x4_scrip_c191212.nc'
   set scrip='POLARRES_ne30x4_np4_SCRIP.nc'
   set Yfac = "4"
endif
if ( $ogrid == 'HMA' ) then
   set scrip='HMACUBIT_ne30x16_np4_SCRIP.nc'
   #set Yfac = "16"
endif


#set scrip = '/project/amp/juliob/Topo-generate-devel/Topo/inputdata/grid-descriptor-file/'${scrip}
#set cstopo = '/project/amp/juliob/Topo-generate-devel/Topo/inputdata/cubed-sphere-topo/gmted2010_modis_bedmachine-ncube3000-220518.nc'

set scrip = '/glade/work/juliob/GridFiles/Scrip/'${scrip}
set cstopo = '/glade/work/juliob/Topo/CubeData/gmted2010_modis_bedmachine-ncube3000-220518.nc'


set cog = `printf "%.3d" $Co`
set cog = "Co"$cog
set fig = `printf "%.3d" $Fi`
set fig = "Fi"$fig

if ( $Fi == 0 ) then
   set smtopo = 'topo_smooth_gmted2010_bedmachine_nc3000_'$cog'.nc'
else
   set smtopo = 'topo_smooth_gmted2010_bedmachine_nc3000_'$cog'_'$fig'.nc'
endif

echo  $cog
echo  $fig

set smtopo = 'N/A'

#set smtopo = '/glade/work/juliob/Topo/MyTopo/cases/ne120pg3_Sco25_test_02/output/topo_smooth_gmted2010_modis_bedmachine_nc3000_Co015.nc'


echo  "SMooth topo file= "$smtopo
#ln -sf $smtopo output/topo_smooth.nc



#READ IN Smooth and find ridges
./cube_to_target --grid_descriptor_file=$scrip --intermediate_cs_name=$cstopo --output_grid=$ogrid --smoothing_scale=$Co --fine_radius=$Fi -u 'juliob@ucar.edu' -q 'output/' -z
#./cube_to_target --grid_descriptor_file=$scrip --intermediate_cs_name=$cstopo --output_grid=$ogrid --rrfac_max=$Yfac --smoothing_scale=$Co --fine_radius=$Fi -u 'juliob@ucar.edu' -q 'output/' -z

#./cube_to_target --grid_descriptor_file=$scrip --intermediate_cs_name=$cstopo --output_grid=$ogrid --smoothing_scale=$Co --fine_radius=$Fi --smooth_topo_file=$smtopo -u 'juliob@ucar.edu' -q 'output/' -z




exit
