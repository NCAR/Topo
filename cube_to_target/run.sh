#!/bin/csh
echo $argv[1] $argv[2] $argv[3] $argv[4] $argv[5]
echo "Model_grid : "$argv[1]
echo "Raw data   : "$argv[2]
echo "Smoothing  : "$argv[3]
echo "Intermediate cubed-sphere resolution - ncube:" $argv[4]
echo "Anisotropic sub-grid vars                   :" $argv[5]

if ($argv[1] == fv_0.9x1.25 || $argv[1] == fv_1.9x2.5) then
  set model = fv
endif
if ($argv[1] == se_ne30np4 || $argv[1] == se_cordex_ne120np4_ne30np4) then
  set model = se
endif



#if ($argv[1] == fv_0.9x1.25 && $argv[3] == cam_fv_smooth) then
if ($model == fv && $argv[3] == cam_fv_smooth) then
#
# fv0.9x1.25-$(raw_data)-cam_fv_smooth-intermediate_ncube$(ncube)-no_anisoSGH.nc
#
  cat > cube_to_target.nl <<EOF
&topoparams
  grid_descriptor_fname           = 'inputdata/grid-descriptor-file/$argv[1].nc'
  intermediate_cubed_sphere_fname = '../bin_to_cube/$argv[2]-ncube$argv[4].nc'
  output_fname                    = 'output/$argv[1]-$argv[2]-$argv[3]-intermediate_ncube$argv[4]-$argv[5].nc'
  externally_smoothed_topo_file   = '../cam_fv_topo-smoothing/$argv[2]-$argv[1]-$argv[3].nc'
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
#
#
#
else if ($argv[1] == se_ne30np4 && $argv[3] == julio_smooth) then
  cat > cube_to_target.nl <<EOF
&topoparams
  grid_descriptor_fname           = 'inputdata/grid-descriptor-file/ne30np4_091226_pentagons.nc'
  intermediate_cubed_sphere_fname = '../bin_to_cube/$argv[2]-ncube$argv[4].nc'
  output_fname                    = 'output/$argv[1]-$argv[2]-$argv[3]-intermediate_ncube$argv[4]-$argv[5].nc'
  externally_smoothed_topo_file   = 'inputdata/externally-smoothed-PHIS/USGS-gtopo30_ne30np4_16xdel2.nc'
  lsmooth_terr = .true.
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
else if ($argv[1] == se_cordex_ne120np4_ne30np4 && $argv[3] == cam_se_smooth) then
  cat > cube_to_target.nl <<EOF
&topoparams
  grid_descriptor_fname           = 'inputdata/grid-descriptor-file/cordex_ne120np4_ne30np4_scrip.nc'
  intermediate_cubed_sphere_fname = '../bin_to_cube/$argv[2]-ncube$argv[4].nc'
  output_fname                    = 'output/$argv[1]-$argv[2]-$argv[3]-intermediate_ncube$argv[4]-$argv[5].nc'
  externally_smoothed_topo_file   = 'inputdata/externally-smoothed-PHIS/gtopo30_modis-cordex_ne120np4_ne30np4_smooth.nc'
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
#  tail -f output/ne30np4-gtopo30-smooth_se.out
else 
  echo "ERROR: no valid argument for run.sh"
endif
 ./cube_to_target >& output/$argv[1]-$argv[2]-$argv[3]-intermediate_ncube$argv[4]-$argv[5].out

