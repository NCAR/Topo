#
# Level of smoothing
#
export ncube_sph_smooth_coarse=120
#
# final grid and associated SCRIP grid desciptor file
#
export output_grid=fv_1.9x2.5
export grid_descriptor_dir=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/
export grid_descriptor_fname=$(grid_descriptor_dir)/fv_1.9x2.5.nc
#
# Compute ridges or not
#
export rdgwin=_NoAniso
#
#######################################################################################################
#
# DO NOT MODIFY BELOW OR YOU MIGHT VIOLATE NAMING CONVENTIONS OR WHAT RAW DATA IS USED
#
#######################################################################################################
#
export raw_data=gmted2010_modis
export ncube=3000
#export ncube=0540
export intermediate_cubed_sphere_file=$(PWD)/bin_to_cube/gmted2010_modis-ncube$(ncube)-stitch.nc
export ncube_sph_smooth_fine=001
#'_MulG' #valid options are '_MulG' or ''
export MulG=_MulG
#export MulG=''
#valid options are '_PF' or ''
export PF=_PF
export case_name=nc$(ncube)_Co$(ncube_sph_smooth_coarse)_Fi$(ncube_sph_smooth_fine)$(MulG)$(PF)_nullRR
export smooth_topo_file=cube_to_target/inputdata/smooth_topo_cube/topo_smooth_$(case_name)_v02.dat
export topo_smooth_nl=cube_to_target/inputdata/namelist_defaults/topo_smooth_$(case_name)_v02.nl
export topo_smooth_nl_subdir=inputdata/namelist_defaults/topo_smooth_$(case_name)_v02.nl
export topo_file_nl_subdir=inputdata/namelist_defaults/final_$(case_name)_v02$(rdgwin).nl
export topo_file_nl=cube_to_target/$(topo_file_nl_subdir)
export topo_file=cube_to_target/outout/$(output_grid)_$(case_name)_20180608.nc
