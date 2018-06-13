&topoparams
  grid_descriptor_fname           = 'inputdata/grid-descriptor-file/ne30np4_091226_pentagons.nc'
  output_grid                     = 'ne30np4'
  intermediate_cubed_sphere_fname = '../bin_to_cube/gmted2010_modis-ncube540.nc' 
  lsmooth_on_cubed_sphere = .true.
  ncube_sph_smooth_coarse = 80
  ncube_sph_smooth_fine = 1
  luse_multigrid = .true.
  luse_prefilter = .true.
  lstop_after_smoothing = .true.
/

