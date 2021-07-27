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
#       lread_smooth_topofile           : logical indicating if smoothed topography needs to be generated (false)
#                                       : or read in from a .dat file (true)
#
#
# OR SIMPLY SET A PRE-DEFINED CASE NAME FROM LIST BELOW
#
#
# Note: if user has pre-smoothed cubed-sphere data already, point to the directory where file is
#
# 	smooth_topo_file_dir
#
#export case=fv_0.9x1.25_Co060_ridge
#export case=fv_1.9x2.5_Co0120_ridge
#export case=fv_4x5_Co0240_ridge
#export case=fv_10x15_Co0480_ridge
#export case=ne30np4_Co0080_ridge
#export case=ne30pg3_Co0070_ridge
#export case=ne16pg3_Co0160_ridge
#export case=ne30pg3_Co0060_ridge
#export case=ne30pg2_Co0080_ridge
#export case=ne60pg2_Co0040_ridge
#export case=ne120np4_Co0015_ridge
#export case=ne480pg3_Co0008_ridge
#export case=ne480pg2_Co0008_ridge
#export case=mpas_120_Co060_ridge
#export case=mpas_480_Co240_ridge
#export case=mpas_60-3-cal_ridge
export case=mpas_15-3-conus_Co009_ridge
#
# Experimental setups
#
#export case=fv_1.9x2.5_Co0120_NoAniso
#export case=fv_0.9x1.25_Co0120_NoAniso
#export case=fv_0.47x0.63_Co0120_NoAniso
#export case=fv_1.9x2.5_Co0120_ridge
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
#export case=fv3_C96_Co0060_ridge
#export case=fv3_C384_Co0015_ridge
#export case=conus_30_x8_Co0060_ridge
#export case=fv_0.47x0.63_Co0030_ridge
#export case=fv_0.23x0.31_Co0015_ridge
#export case=conus_30_x8_Co0008_ridge
#export case=ne30np4_Co0060_ridge
#export case=CONUS_30_x8_Co0060_ridge
#export case=ARCTIC_30_x4_C0060_ridge
#export case=ARCTICGRIS_30_x8_C0060_ridge

case_found=False
export nwindow_halfwidth=-1
export lregional_refinement=.false.
export lread_smooth_topofile=.true.

#
# Scientifically supported setups
#
ifeq ($(case),ne30np4_Co0060_ridge)
  export ncube_sph_smooth_coarse=060
  export output_grid=ne30np4
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/ne30np4_091226_pentagons.nc
  export nwindow_halfwidth=042
  export rdgwin=_NoAniso
  export stitch=-stitch
  export ncube=3000
  case_found=
endif
ifeq ($(case),ne30pg3_Co0060_ridge)
  export ncube_sph_smooth_coarse=060
  export output_grid=ne30pg3
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/$(output_grid).nc
  export nwindow_halfwidth=042
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
ifeq ($(case),ne120np4_Co0015_ridge)
  export ncube_sph_smooth_coarse=015
  export output_grid=ne120np4
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/ne120np4_pentagons_100310.nc
  export nwindow_halfwidth=011
  export rdgwin=_Nsw$(nwindow_halfwidth)
  export stitch=-stitch
  export ncube=3000
  case_found=
endif
ifeq ($(case),ne480pg3_Co0008_ridge)
  export ncube_sph_smooth_coarse=008
  export output_grid=ne480pg3
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/ne480pg3_scrip_200108.nc
  export nwindow_halfwidth=006
  export rdgwin=_Nsw$(nwindow_halfwidth)
  export stitch=-stitch
  export ncube=3000
  case_found=
endif
ifeq ($(case),ne480pg2_Co0008_ridge)
  export ncube_sph_smooth_coarse=008
  export output_grid=ne480pg2
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/ne480pg2_scrip_210402.nc
  export nwindow_halfwidth=006
  export rdgwin=_Nsw$(nwindow_halfwidth)
  export stitch=-stitch
  export ncube=3000
  case_found=
endif
#ifeq ($(case),ne480pg3_Co0008_ridge)
#  export ncube_sph_smooth_coarse=018
#  export output_grid=ne480pg3
#  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/ne480pg3_scrip_200108.nc
#  export nwindow_halfwidth=013
#  export rdgwin=_Nsw$(nwindow_halfwidth)
#  export stitch=-stitch
#  export ncube=9000
#  case_found=
#endif
#
# Experimental setup
#
ifeq ($(case),conus_30_x8_Co0008_ridge)
  export ncube_sph_smooth_coarse=008
  export output_grid=conus_30_x8
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/SCRIP_ne0np4CONUS.ne30x8.ncG
  export nwindow_halfwidth=006
  export rdgwin=_NoAniso
  export stitch=-stitch
  export ncube=3000
  export lregional_refinement=.true.
  case_found=
endif


ifeq ($(case),quick_test_fv)
  export ncube_sph_smooth_coarse=006
  export output_grid=fv_10x15
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/fv_10x15.nc
  export rdgwin=_NoAniso
  export rdgwin=_Nsw$(nwindow_halfwidth)
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
ifeq ($(case),fv_1.9x2.5_Co0120_ridge)
  export ncube_sph_smooth_coarse=120
  export output_grid=fv_1.9x2.5
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/fv_1.9x2.5.nc
  export nwindow_halfwidth=085
  export rdgwin=_Nsw$(nwindow_halfwidth)  
  export stitch=-stitch
  export ncube=3000
  case_found=
endif

ifeq ($(case),fv_4x5_Co0240_ridge)
  export ncube_sph_smooth_coarse=240
  export output_grid=fv_4x5
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/fv_4x5.nc
  export nwindow_halfwidth=170
  export rdgwin=_Nsw$(nwindow_halfwidth)
  export stitch=-stitch
  export ncube=3000
  case_found=
endif
ifeq ($(case),fv_10x15_Co0480_ridge)
  export ncube_sph_smooth_coarse=480
  export output_grid=fv_10x15
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/fv_4x5.nc
  export nwindow_halfwidth=340
  export rdgwin=_Nsw$(nwindow_halfwidth)
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

ifeq ($(case),fv_0.47x0.63_Co0030_ridge)
  export ncube_sph_smooth_coarse=030
  export output_grid=fv_0.47x0.63
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/fv_0.47x0.63.nc
  export nwindow_halfwidth=021
  export rdgwin=_Nsw$(nwindow_halfwidth)
  export stitch=-stitch
  export ncube=3000
  case_found=
endif

ifeq ($(case),fv_0.23x0.31_Co0015_ridge)
  export ncube_sph_smooth_coarse=015
  export output_grid=fv_0.23x0.31
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/fv_0.23x0.31.nc
  export nwindow_halfwidth=011
  export rdgwin=_Nsw$(nwindow_halfwidth)
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
  export stitch=-stitch
  export ncube=3000
  case_found=
endif

ifeq ($(case),ne30pg3_Co0070_ridge)
  export ncube_sph_smooth_coarse=070
  export output_grid=ne30pg3
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/$(output_grid).nc
  export nwindow_halfwidth=049
  export rdgwin=_Nsw$(nwindow_halfwidth)
  export stitch=-stitch
  export ncube=3000
  case_found=
endif
#
# FV3
#
ifeq ($(case),fv3_C96_Co0060_ridge)
  export ncube_sph_smooth_coarse=060
  export output_grid=fv3_C96
  #
  # grid descriptor file copied from /project/amp/jet/mapping/grids/C96_SCRIP_desc_180822.nc
  #
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/fv3_C96.nc
  export nwindow_halfwidth=042
  export rdgwin=_Nsw$(nwindow_halfwidth)
  export stitch=-stitch
  export ncube=3000
  case_found=
endif

ifeq ($(case),fv3_C384_Co0015_ridge)
  export ncube_sph_smooth_coarse=015
  export output_grid=fv3_C384
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/FV3_C384_SCRIP_desc.181018.nc
  export nwindow_halfwidth=011
  export rdgwin=_Nsw$(nwindow_halfwidth)
  export stitch=-stitch
  export ncube=3000
  case_found=
endif
#
# Functionally supported VR-grids
#
ifeq ($(case),CONUS_30_x8_Co0060_ridge)
  export ncube_sph_smooth_coarse=060
  export output_grid=CONUS_30_x8
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/SCRIP_ne0np4CONUS.ne30x8.nc
  export nwindow_halfwidth=042
  export rdgwin=_Nsw$(nwindow_halfwidth)
  #export rdgwin=_NoAniso
  export stitch=-stitch
  export ncube=3000
  export lregional_refinement=.true.
  export rrfac_max=8
  export lread_smooth_topofile=.false.
  case_found=
endif

ifeq ($(case),ARCTIC_30_x4_C0060_ridge)
  export ncube_sph_smooth_coarse=060
  export output_grid=ARCTIC_30_x4
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/SCRIP_ARCTIC_ne30x4_np4.nc
  export nwindow_halfwidth=042
  export rdgwin=_Nsw$(nwindow_halfwidth)
  #export rdgwin=_NoAniso
  export stitch=-stitch
  export ncube=3000
  export lregional_refinement=.true.
  export rrfac_max=4
  export lread_smooth_topofile=.false.
  case_found=
endif

ifeq ($(case),ARCTICGRIS_30_x8_C0060_ridge)
  export ncube_sph_smooth_coarse=60
  export output_grid=ARCTICGRIS_30_x8
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/SCRIP_ARCTICGRIS_ne30x8_np4.nc
  export nwindow_halfwidth=042
  export rdgwin=_Nsw$(nwindow_halfwidth)
  #export rdgwin=_NoAniso
  export stitch=-stitch
  export ncube=3000
  export lregional_refinement=.true.
  export rrfac_max=8
  export lread_smooth_topofile=.false.
  case_found=
endif

ifeq ($(case),mpas_480_Co240_ridge)
  export ncube_sph_smooth_coarse=240
  export output_grid=mpas_480
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/$(grid_descriptor_dir)/mpasa480_SCRIP_desc.200911.nc
  export nwindow_halfwidth=170
  export rdgwin=_Nsw$(nwindow_halfwidth)
  export stitch=-stitch
  export ncube=3000
  case_found=
endif

ifeq ($(case),mpas_60-3-cal_ridge)
  export ncube_sph_smooth_coarse=033
  export output_grid=mpas_60_to_3
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/mpasa60-3.california.SCRIP.nc
  export nwindow_halfwidth=023
  export rdgwin=_Nsw$(nwindow_halfwidth)
  export stitch=-stitch
  export ncube=3000
  export lregional_refinement=.true.
  export rrfac_max=020
  case_found=
endif

ifeq ($(case),mpas_15-3-conus_Co009_ridge)
  export ncube_sph_smooth_coarse=009
  export output_grid=mpas_15-3
  export grid_descriptor_fname=$(PWD)/cube_to_target/inputdata/grid-descriptor-file/mpasa15-3.conus.desc_SCRIP.210504.nc
  export nwindow_halfwidth=006
  export rdgwin=_Nsw$(nwindow_halfwidth)
  export stitch=-stitch
  export ncube=3000
  export lregional_refinement=.true.
  export rrfac_max=005
  case_found=
endif


ifeq ($(case_found),False)
  echo "CASE NOT FOUND - ABORT"
endif
