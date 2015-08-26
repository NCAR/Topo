#./definesurf -t input/make_raw_phis_from_usgs/10min-phis-raw.nc -g input/cami_1987-01-01_0.9x1.25_L26_c060703.nc -l input/landm_coslat.nc -remap  output/smooth-phis.nc
#
# old topo generation
#
#./definesurf -t input/data_from_fsdir/topo10min.merged_c030506.nc  -g input/data_from_fsdir/fv_0.9x1.25.nc -l input/data_from_fsdir/landm_coslat.nc -remap  fv-original-0.9x1.25.nc

./definesurf -t input/make_raw_phis_from_usgs/10min-phis-gmted2010-raw.nc  -g input/data_from_fsdir/fv_0.9x1.25.nc -l input/data_from_fsdir/landm_coslat.nc -remap  fv-gmted2010-0.9x1.25.nc

