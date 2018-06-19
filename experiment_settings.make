#
# USER MUST SET THE FOLLOWING VARIABLES
#
#
# 	ncube_sph_smooth_coarse		: level of smoothing (must be 3 digits, e.g., 030, 060, 120)
# 	output_grid			: target grid
# 	directory with grid SCRIP
# 	grid descriptor file		: grid_descriptor_dir
# 	grid descriptor file name	: grid_descriptor_fname (full path)
# 	compute sub-grid ridges		: rdgwin (no ridges set to _NoAniso)
#	nwindow_halfwidth		: ??? (must be 3 digits, e.g., 085)
#					  typically nwindow_halfwidth=ncube_sph_smooth_coarse/(sqrt(2))
#	stitch				: use stitched data (stitch=-stitch) or not (stitch=)
#
#
# OR SIMPLY SET A PRE-DEFINED CASE NAME FROM LIST BELOW
#
#
# Note: if user has pre-smoothed cubed-sphere data already, point to the directory where file is
#
# 	smooth_topo_file_dir
#

#export case=ne30np4_Co0080_ridge
export case=ne16pg3_Co0160_ridge
#export case=ne30pg3_Co0080_ridge
#export case=ne30pg2_Co0080_ridge
#export case=ne60pg2_Co0040_ridge
#
# Experimental setups
#
#export case=fv_1.9x2.5_Co0120_NoAniso
#export case=fv_0.9x1.25_Co0120_NoAniso
#export case=fv_0.47x0.63_Co0120_NoAniso
#export case=fv_1.9x2.5_Co0120_ridge
#export case=fv_0.9x1.25_Co060_ridge
#export case=quick_test_fv
#export case=ne30np4_Co0120_NoAnis
#not run yet
#export case=ne16pg3_Co0120_NoAniso
#running
#export case=ne30pg3_Co0120_NoAniso
#not run yet
#export case=ne60pg3_Co0120_NoAniso
#no run yet
#export case=ne120pg3_Co0120_NoAniso

case_found=False

#
# Scientifically supported setups
#
ifeq ($(case),ne30np4_Co0080_ridge)
  export ncube_sph_smooth_coarse=080
  export output_grid=ne30np4
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/ne30np4_091226_pentagons.nc
  export nwindow_halfwidth=057
  export rdgwin=_Nsw$(nwindow_halfwidth)
  export stitch=-stitch
  export ncube=3000
  case_found=
endif
ifeq ($(case),ne30pg3_Co0080_ridge)
  export ncube_sph_smooth_coarse=080
  export output_grid=ne30pg3
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/$(output_grid).nc
  export nwindow_halfwidth=057
  export rdgwin=_Nsw$(nwindow_halfwidth)
  export stitch=-stitch
  export ncube=3000
  case_found=
endif
ifeq ($(case),ne16pg3_Co0160_ridge)
  export ncube_sph_smooth_coarse=160
  export output_grid=ne16pg3
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/$(output_grid).nc
  export nwindow_halfwidth=113
  export rdgwin=_Nsw$(nwindow_halfwidth)
  export stitch=-stitch
  export ncube=3000
  case_found=
endif
ifeq ($(case),ne30pg2_Co0080_ridge)
  export ncube_sph_smooth_coarse=080
  export output_grid=ne30pg2
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/$(output_grid).nc
  export nwindow_halfwidth=057
  export rdgwin=_Nsw$(nwindow_halfwidth)
  export stitch=-stitch
  export ncube=3000
  case_found=
endif
ifeq ($(case),ne60pg2_Co0040_ridge)
  export ncube_sph_smooth_coarse=040
  export output_grid=ne60pg2
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/$(output_grid).nc
  export nwindow_halfwidth=028
  export rdgwin=_Nsw$(nwindow_halfwidth)
  export stitch=-stitch
  export ncube=3000
  case_found=
endif
#
# Experimental setup
#
ifeq ($(case),quick_test_fv)
  export ncube_sph_smooth_coarse=006
  export output_grid=fv_10x15
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/fv_10x15.nc
  export rdgwin=_NoAniso
  export stitch=
  export ncube=040
  case_found=
endif
#
#
#
ifeq ($(case),ne30np4_Co0120_NoAniso)
  export ncube_sph_smooth_coarse=120
  export output_grid=ne30np4
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/ne30np4_091226_pentagons.nc
  export rdgwin=_NoAniso
  export stitch=-stitch
  export ncube=3000
  case_found=
endif
#
# ne16pg3 smoothed to Co120 (recommended for ne30pg3 is Co160-ish)
# no ridges
#
ifeq ($(case),ne16pg3_Co0120_NoAniso)
  export ncube_sph_smooth_coarse=120
  export output_grid=ne16pg3
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/$(output_grid).nc
  export rdgwin=_NoAniso
  export stitch=-stitch
  export ncube=3000
  case_found=
endif
#
# ne30pg3 smoothed to Co120 (recommended for ne30pg3 is Co080)
# no ridges
#
ifeq ($(case),ne30pg3_Co0120_NoAniso)
  export ncube_sph_smooth_coarse=120
  export output_grid=ne30pg3
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/$(output_grid).nc
  export rdgwin=_NoAniso
  export stitch=-stitch
  export ncube=3000
  case_found=
endif
#
# ne60pg3 smoothed to Co120 (recommended for ne30pg3 is Co040)
# no ridges
#
ifeq ($(case),ne60pg3_Co0120_NoAniso)
  export ncube_sph_smooth_coarse=120
  export output_grid=ne60pg3
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/$(output_grid).nc
  export rdgwin=_NoAniso
  export stitch=-stitch
  export ncube=3000
  case_found=
endif
#
# ne120pg3 smoothed to Co120 (recommended for ne30pg3 is Co020)
# no ridges
#
ifeq ($(case),ne120pg3_Co0120_NoAniso)
  export ncube_sph_smooth_coarse=120
  export output_grid=ne120pg3
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/$(output_grid).nc
  export rdgwin=_NoAniso
  export stitch=-stitch
  export ncube=3000
  case_found=
endif

#
# Standard 2 degree smoothing for FV: Co120
# Map to fv_1.9x2.5
# Do not compute sub-grid ridges
#
ifeq ($(case),fv_1.9x2.5_Co0120_NoAniso)
  export ncube_sph_smooth_coarse=120
  export output_grid=fv_1.9x2.5
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/fv_1.9x2.5.nc
  export rdgwin=_NoAniso
  export stitch=-stitch
  export ncube=3000
  case_found=
endif
#
# Standard 2 degree smoothing for FV: Co120
# Map to fv_0.9x1.25 (i.e. extra smooth for 1 degree dycore)
# Do not compute sub-grid ridges (compatible with CAM4,CAM5)
#
ifeq ($(case),fv_0.9x1.25_Co0120_NoAniso)
  export ncube_sph_smooth_coarse=120
  export output_grid=fv_0.9x1.25
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/fv_0.9x1.25.nc
  export rdgwin=_NoAniso
  export stitch=-stitch
  export ncube=3000
  case_found=
endif


ifeq ($(case),fv_0.47x0.63_Co0120_NoAniso)
  export ncube_sph_smooth_coarse=120
  export output_grid=fv_0.47x0.63
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/fv_0.47x0.63.nc
  export rdgwin=_NoAniso
  export stitch=-stitch
  export ncube=3000
  case_found=
endif


#
# Standard 1 degree smoothing for FV: Co060
# Map to fv_0.9x1.25
# Compute ridges (compatible with CAM6)
#
ifeq ($(case),fv_0.9x1.25_Co060_ridge)
  export ncube_sph_smooth_coarse=060
  export output_grid=fv_0.9x1.25
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/fv_0.9x1.25.nc
  export nwindow_halfwidth=042
  export rdgwin=_Nsw$(nwindow_halfwidth)
  export lfind_ridges=.true.
  export stitch=-stitch
  export ncube=3000
  case_found=
endif
#
# Standard 2 degree smoothing for FV: Co120
# Map to fv_1.9x2.5
# Compute ridges (compatible with CAM6)
#
ifeq ($(case),fv_1.9x2.5_Co0120_ridge)
  export ncube_sph_smooth_coarse=120
  export output_grid=fv_1.9x2.5
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/fv_1.9x2.5.nc
  export nwindow_halfwidth=085
  export rdgwin=_Nsw$(nwindow_halfwidth)
  export lfind_ridges=.true.
  export stitch=-stitch
  export ncube=3000
  case_found=
endif

ifeq ($(case_found),False)
  echo "CASE NOT FOUND - ABORT"
endif
