&topoparams
  grid_descriptor_fname           = 'inputdata/grid-descriptor-file/fv0.9x1.25.nc'
  intermediate_cubed_sphere_fname = '../bin_to_cube/gtopo30-ncube540.nc'
  output_fname                    = 'output/fv0.9x1.25-gtopo30-cam_fv_smooth-intermediate_ncube540-no_anisoSGH.nc'
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

