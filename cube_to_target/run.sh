#!/bin/csh
echo "arguments are:" $argv[1],$argv[2],$argv[3]
if ($argv[1] == fv0.9x1.25 && $argv[2] == gmted2010_modis && $argv[3] == cam_fv_smooth) then
  cat > topo.nl <<EOF
&topoparams
  grid_descriptor_fname           = 'inputdata/grid-descriptor-file/fv-0.9x1.25.nc'
  intermediate_cubed_sphere_fname = '../bin_to_cube/gmted2010_modis-ncube3000.nc'
  output_fname                    = 'output/fv0.9x1.25-gmted2010_modis-cam_fv_smooth.nc'
  externally_smoothed_topo_file   = '../cam_fv_topo-smoothing/gmted2010_modis-fv0.9x1.25-cam_fv_smooth.nc'
  lsmooth_terr = .true.
  lexternal_smooth_terr = .true.
  lzero_out_ocean_point_phis = .false.
  lsmooth_on_cubed_sphere = .false.
  ncube_sph_smooth_coarse = 20  
  ncube_sph_smooth_fine = 1
  lfind_ridges = .false.
  nwindow_halfwidth = 14
  nridge_subsample = 14
/

EOF
  ./cube_to_target >& output/fv0.9x1.25-gmted2010_modis-smooth_cam.out &
  tail -f output/fv0.9x1.25-gmted2010_modis-smooth_cam.out
else if ($argv[1] == fv0.9x1.25 && $argv[2] == gtopo30 && $argv[3] == cam_fv_smooth) then
  echo "Grid is ",$argv[1]
  echo "Raw ~1km data is ",$argv[2]
  echo "Using externally smoothed PHIS; smoothed with cam_fv_topo-smoothing program"
  echo "Intermediate cubed-sphere is ../bin_to_cube/gtopo30-ncube3000.nc"
  cat > topo.nl <<EOF
&topoparams
  grid_descriptor_fname           = 'inputdata/grid-descriptor-file/fv-0.9x1.25.nc'
  intermediate_cubed_sphere_fname = '../bin_to_cube/gtopo30-ncube3000.nc'
  output_fname                    = 'output/fv0.9x1.25-gtopo30-cam_fv_smooth.nc'
  externally_smoothed_topo_file   = '../cam_fv_topo-smoothing/gtopo30-fv0.9x1.25-cam_fv_smooth.nc'
  lsmooth_terr = .true.
  lexternal_smooth_terr = .true.
  lzero_out_ocean_point_phis = .false.
  lsmooth_on_cubed_sphere = .false.
  ncube_sph_smooth_coarse = 20  
  ncube_sph_smooth_fine = 1
  lfind_ridges = .false.
  nwindow_halfwidth = 14
  nridge_subsample = 14
/

EOF
  ./cube_to_target >& output/fv0.9x1.25-gtopo30-smooth_cam.out &
  tail -f output/fv0.9x1.25-gtopo30-smooth_cam.out
else if ($argv[1] == ne30np4 && $argv[2] == gtopo30 && $argv[3] == smooth_se) then
  echo "Grid is ",$argv[1]
  echo "Raw ~1km data is ",$argv[2]
  echo "Using externally smoothed PHIS; smoothed with CAM-SE ",$argv[3]
  echo "Intermediate cubed-sphere is ../bin_to_cube/gtopo30-ncube3000.nc"
  cat > topo.nl <<EOF
&topoparams
  grid_descriptor_fname           = 'inputdata/grid-descriptor-file/ne30np4_091226_pentagons.nc'
  intermediate_cubed_sphere_fname = '../bin_to_cube/gtopo30-ncube3000.nc'
  output_fname                    = 'output/ne30np4-gtopo30-smooth_se.nc'
  externally_smoothed_topo_file   = 'inputdata/externally-smoothed-PHIS/USGS-gtopo30_ne30np4_16xdel2.nc'
  lsmooth_terr = .true.
  lexternal_smooth_terr = .true.
  lzero_out_ocean_point_phis = .false.
  lsmooth_on_cubed_sphere = .false.
  ncube_sph_smooth_coarse = 20  
  ncube_sph_smooth_fine = 1
  lfind_ridges = .false.
  nwindow_halfwidth = 14
  nridge_subsample = 14
/

EOF
  ./cube_to_target >& output/ne30np4-gtopo30-smooth_se.out &
  tail -f output/ne30np4-gtopo30-smooth_se.out

else if ($argv[1] == ne30np4 && $argv[2] == gtopo30 && $argv[3] == no_smooth) then
  echo "Grid is ",$argv[1]
  echo "Raw ~1km data is ",$argv[2]
  echo "No smoothing of topography ",$argv[3]
  echo "Intermediate cubed-sphere is ../bin_to_cube/gtopo30-ncube3000.nc"
  cat > topo.nl <<EOF

&topoparams
  grid_descriptor_fname           = 'inputdata/grid-descriptor-file/ne30np4_091226_pentagons.nc'
  intermediate_cubed_sphere_fname = '../bin_to_cube/gtopo30-ncube3000.nc'
  output_fname                    = 'output/ne30np4-gtopo30-no_smooth.nc'
  externally_smoothed_topo_file   = 'inputdata/externally-smoothed-PHIS/USGS-gtopo30_ne30np4_16xdel2.nc'
  lsmooth_terr = .false.
  lexternal_smooth_terr = .false.
  lzero_out_ocean_point_phis = .false.
  lsmooth_on_cubed_sphere = .false.
  ncube_sph_smooth_coarse = 20  
  ncube_sph_smooth_fine = 1
  lfind_ridges = .false.
  nwindow_halfwidth = 14
  nridge_subsample = 14
/

EOF
  ./cube_to_target >& output/ne30np4-gtopo30-no_smooth.out &
  tail -f output/ne30np4-gtopo30-no_smooth.out
else 
  echo "ERROR: no valid argument for run.sh"
  echo "valid options are gmted2010 and gtopo30"
endif


