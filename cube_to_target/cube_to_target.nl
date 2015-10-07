&topoparams
  grid_descriptor_fname           = 'inputdata/grid-descriptor-file/ne30np4_091226_pentagons.nc'
  intermediate_cubed_sphere_fname = '../bin_to_cube/gtopo30-ncube3000.nc'
  output_fname                    = 'output/se_ne30np4-gtopo30-julio_smooth-intermediate_ncube3000-julio_anisoSGH.nc'
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

