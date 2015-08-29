#!/bin/csh
echo "arguments are:",$argv[1],$argv[2]
if ($argv[1] == fv0.9x1.25 && $argv[2] == gmted2010_modis) then
  cat > topo.nl <<EOF
&topoparams
  grid_descriptor_fname           = 'inputdata/grid-descriptor-file/fv-0.9x1.25.nc'
  intermediate_cubed_sphere_fname = '../bin_to_cube/gmted2010_modis-ncube3000.nc'
  output_fname                    = 'output/fv0.9x1.25-gmted2010_modis-smooth_cam.out'
  externally_smoothed_topo_file   = '../cam_fv_topo-smoothing/fv-gmted2010_modis-0.9x1.25.nc'
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
else if ($argv[1] == fv0.9x1.25 && $argv[2] == gtopo30) then
  echo "Grid is ",$argv[1]
  echo "Raw ~1km data is ",$argv[2]
  echo "Using externally smoothed PHIS; smoothed with cam_fv_topo-smoothing program"
  echo "Intermediate cubed-sphere is ../bin_to_cube/gtopo30-ncube3000.nc"
  cat > topo.nl <<EOF
&topoparams
  grid_descriptor_fname           = 'inputdata/grid-descriptor-file/fv-0.9x1.25.nc'
  intermediate_cubed_sphere_fname = '../bin_to_cube/gtopo30-ncube3000.nc'
  output_fname                    = 'output/fv0.9x1.25-gtopo30-smooth_cam.out'
  externally_smoothed_topo_file   = '../cam_fv_topo-smoothing/fv-gtopo30-0.9x1.25.nc'
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
else
  echo "ERROR: no valid argument for run.sh"
  echo "valid options are gmted2010 and gtopo30"
endif


