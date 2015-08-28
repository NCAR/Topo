&topoparams
  grid_descriptor_fname           = '/project/amp/pel/topo-unstructured/data/se/grid-descriptor-files/ne30np4_091226_pentagons.nc'
  intermediate_cubed_sphere_fname = '/project/amp/juliob/topo-data-trunk/intermediate-cube-data/USGS-topo-cube540.nc'
  output_fname                    = 'out.nc'
  lsmooth_terr = .false.
  lexternal_smooth_terr = .false.
  lzero_out_ocean_point_phis = .false.
  lsmooth_on_cubed_sphere = .true.
  ncube_sph_smooth_coarse = 20  
  ncube_sph_smooth_fine = 1
  lfind_ridges = .true.
  nwindow_halfwidth = 14
  nridge_subsample = 14
/
#
# the externally smoothed fv topography has been created with definesurf:
#
# ./definesurf -t input/make_raw_phis_from_usgs/10min-phis-raw.nc -g input/initial_data.cam.fv.0181x0360L30.410.bw.nc -l input/landm_coslat.nc -remap output/smooth-phis.nc
#

