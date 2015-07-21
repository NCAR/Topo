&topoparams
  grid_descriptor_fname           = 'inputdata/grid-descriptor-file/fv.0096x0144.nc'
  intermediate_cubed_sphere_fname = '../bin_to_cube/USGS-topo-cube.nc'
  externally_smoothed_topo_file   = 'inputdata/externally-smoothed-PHIS/USGS-gtopo30_fv.0096x0144.nc'
  output_fname                    = 'out.nc'
  lsmooth_terr = .false.
  lexternal_smooth_terr = .false.
  lzero_out_ocean_point_phis = .false.
/
#
# the externally smoothed fv topography has been created with definesurf:
#
# ./definesurf -t input/make_raw_phis_from_usgs/10min-phis-raw.nc -g input/initial_data.cam.fv.0181x0360L30.410.bw.nc -l input/landm_coslat.nc -remap output/smooth-phis.nc
#
