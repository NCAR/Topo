&topoparams
  grid_descriptor_fname           = 'inputdata/grid-descriptor-file/fv_1.9x2.5.nc'
  intermediate_cubed_sphere_fname = '../bin_to_cube/gmted2010_modis-ncube3000.nc'
  output_fname                    = 'output/fv_1.9x2.5-gmted2010_modis-cam_fv_smooth-intermediate_ncube3000-no_anisoSGH.nc'
  externally_smoothed_topo_file   = '../cam_fv_topo-smoothing/gmted2010_modis-fv_1.9x2.5-cam_fv_smooth.nc'
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

