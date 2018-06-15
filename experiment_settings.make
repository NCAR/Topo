#
# USER MUST SET THE FOLLOWING VARIABLES
#
#
# 	ncube_sph_smooth_coarse		: Level of smoothing
# 	output_grid			: target grid
# 	directory with grid SCRIP
# 	grid descriptor file		: grid_descriptor_dir
# 	grid descriptor file name	: grid_descriptor_fname (full path)
# 	compute sub-grid ridges		: rdgwin (if no set to _NoAniso)
#
# OR SIMPLY SET A PRE-DEFINED CASE NAME
#


export case=fv_1.9x2.5_Co0120_NoAniso
export case=fv_0.9x1.25_Co0120_NoAniso


ifeq ($(case),fv_1.9x2.5_Co0120_NoAniso)
  export ncube_sph_smooth_coarse=120
  export output_grid=fv_1.9x2.5
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/fv_1.9x2.5.nc
  export rdgwin=_NoAniso
endif

ifeq ($(case),fv_0.9x1.25_Co0120_NoAniso)
  export ncube_sph_smooth_coarse=120
  export output_grid=fv_1.9x2.5
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/fv_0.9x1.5.nc
  export rdgwin=_NoAniso
endif

