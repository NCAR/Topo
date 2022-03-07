#!/bin/tcsh
#
#****************************************************************
#
# Author: Peter Hjort Lauritzen (NCAR, pel@ucar)
# Date  :
#
#****************************************************************
#
if ( "$#argv" != 2) then
  echo "Wrong number of arguments specified:"
  echo "  -arg 1 res"
  echo "  -arg 2 user (use quotes)"
  exit
endif
set n = 1
set res = "$argv[$n]" 
set n = 2
set name = "${argv[$n]}" 
echo $name

#
set pre_computed_smooth_topo       = False
if ($pre_computed_smooth_topo == True) then
  set pre_computed_smooth_topo_file = /glade/p/cgd/amp/pel/topo/cubedata/gmted2010_bedmachine-ncube3000.nc
endif

set topo_dir                       = /glade/scratch/pel/topowopts_new/      # location of cube_to_target binary
#set raw_data_dir                  = /glade/p/cgd/amp/pel/topo/cubedata/    # location of intermediate cubed-sphere data
set output_dir                     = /glade/scratch/pel/output              # location of output files

set grid_descriptor_dir            = $CESMDATAROOT/inputdata/share/meshes
set intermediate_cube_data_dir     = /glade/p/cgd/amp/pel/topo/cubedata/
set intermediate_cubed_sphere_data = $pre_computed_smooth_topo_dir/gmted2010_bedmachine-ncube0540.nc
#
# SE grids (uniform resolution)
#

#not used: 

          
            

if ($res == 'ne5np4') then
  # ne5pg4_ESMFmesh_cdf5_c20210118.nc - obsoluete?
  # ne5np4_esmf_mesh_c210121.nc - obsolete?
  set grid_descriptor_file = $grid_descriptor_dir/ne5_ESMFmesh_cdf5_c20210923.nc

  set c                    = 240
endif
if ($res == 'ne5pg3') then
  set grid_descriptor_file = $grid_descriptor_dir/ne5pg3_ESMFmesh_cdf5_c20210118.nc
  set c                    = 240
endif
if ($res == 'ne5pg2') then
  set grid_descriptor_file = $grid_descriptor_dir/ne5pg2_ESMFmesh_cdf5_c20211018.nc
  set c                    = 240
endif
if ($res == 'ne16np4') then
  #ne16np4_ESMFmesh_cdf5_c20200712.nc - obsolete?
  #ne16np4_scrip_171002_ESMFmesh.nc   - obsolete?
  set grid_descriptor_file = $grid_descriptor_dir/ne16np4_ESMFmesh_cdf5_c20211018.nc
                                                  
  set c                    = 120
endif
if ($res == 'ne16pg3') then
  set grid_descriptor_file = $grid_descriptor_dir/ne16pg3_ESMFmesh_cdf5_c20211018.nc
  set c                    = 120
endif
if ($res == 'ne30np4') then
  #ne30np4_091226_pentagons_ESMFmesh.nc - obsolete
  set grid_descriptor_file = $grid_descriptor_dir/ne30np4_ESMFmesh_cdf5_c20211018.nc
  set c                    = 60
endif
if ($res == 'ne30pg3') then
  # ne30pg3_ESMFmesh_c20200821.nc  - obsolete?
  set grid_descriptor_file = $grid_descriptor_dir/ne30pg3_ESMFmesh_cdf5_c20211018.nc
  set c                    = 60
endif
if ($res == 'ne30pg2') then
  #ne30pg2_ESMFmesh_c20200727.nc - obsolete?
  set grid_descriptor_file = $grid_descriptor_dir/ne30pg2_ESMFmesh_cdf5_c20211018.nc
  set c                    = 60
endif


if ($res == 'ne60np4') then
  #ne60np4_pentagons_100408_ESMFmesh.nc - obsolete
  set grid_descriptor_file = $grid_descriptor_dir/ne60np4_ESMFmesh_cdf5_c20211018.nc
  set c                    = 30
endif
if ($res == 'ne60pg3') then
  set grid_descriptor_file = $grid_descriptor_dir/ne60pg3_ESMFmesh_cdf5_c20211018.nc
  set c                    = 30
endif
if ($res == 'ne60pg2') then
  set grid_descriptor_file = $grid_descriptor_dir/ne60pg2_ESMFmesh_cdf5_c20211018.nc
  set c                    = 30
endif


if ($res == 'ne120np4') then
  set grid_descriptor_file = $grid_descriptor_dir/ne120np4_ESMFmesh_cdf5_c20211018.nc
  set c                    = 15
endif
if ($res == 'ne120pg3') then
# ne120np4.pg3_esmf_mesh_c200415.nc - obsolete?
# ne120np4.pg3_esmf_mesh_cdf5_c200415.nc - obsolete?
# ne120np4_pentagons_100310_ESMFmesh.nc - obsolete?
  set grid_descriptor_file = $grid_descriptor_dir/ne120pg3_ESMFmesh_cdf5_c20211018.nc
  set c                    = 15
endif
if ($res == 'ne120pg2') then
  set grid_descriptor_file = $grid_descriptor_dir/ne120pg2_ESMFmesh_cdf5_c20211018.nc
  set c                    = 15
endif
if ($res == 'ne240np4') then
  set grid_descriptor_file = $grid_descriptor_dir/ne240np4_ESMFmesh_cdf5_c20211018.nc
  set c                    = 8
endif
if ($res == 'ne240pg3') then
  set grid_descriptor_file = $grid_descriptor_dir/
  set c                    = 8
  echo "missing ESMF grid descriptor file"
  exit
endif
if ($res == 'ne240pg2') then
  set grid_descriptor_file = $grid_descriptor_dir/ne240pg2_ESMFmesh_cdf5_c20211018.nc
  set c                    = 8
endif
#
# variable resolution SE grids
#
#ne0ARCTICGRISne30x8_ESMFmesh_c20200730.nc            
#ne0ARCTICne30x4_ESMFmesh_c20200727.nc            
#ne0CONUSne30x8_ESMFmesh_c20200727.nc                                                   
#nepal_CISMgrid_ESMFmesh_cdf5_c220222.nc


if ($pre_computed_smooth_topo == True) then
  $topo_dir/cube_to_target/cube_to_target --grid_descriptor_file=$grid_descriptor_file --intermediate_cs_name=$intermediate_cubed_sphere_data --output_grid=$res --coarse_radius=$c --fine_radius=001 -p -r -u "$name" -q $output_dir
else
  $topo_dir/cube_to_target/cube_to_target --grid_descriptor_file=$grid_descriptor_file --intermediate_cs_name=$intermediate_cubed_sphere_data --output_grid=$res --coarse_radius=$c --fine_radius=001 -p -r -u "$name" -q $output_dir
endif
