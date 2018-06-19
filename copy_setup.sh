#!/bin/bash
#
# This script copies an existing setup without copying large input files
# The large input files are symbolic linked to the new directory
#
# This script is useful when needing to generate many topography files
# simultaneously on the same machine
#
from_path="/project/amp/pel/TopoTrunk"
to_path="/project/amp/pel/TopoCopy5"
if [ -d "$to_path" ]; then
    echo "$to_path already exists - ABORT"
else
echo "$to_path"
mkdir $to_path
cd $from_path
tar zcvf $to_path/copy.tgz cube_to_target/*.F90 cube_to_target/Makefile *.make Makefile
cd $to_path
tar zxvf copy.tgz
mkdir $to_path/cube_to_target/inputdata
mkdir $to_path/cube_to_target/inputdata/grid-descriptor-file
mkdir $to_path/cube_to_target/inputdata/namelist_defaults
mkdir $to_path/cube_to_target/inputdata/smooth_topo_cube
cd $to_path/cube_to_target/inputdata/grid-descriptor-file
tmp=$from_path/cube_to_target/inputdata/grid-descriptor-file
for i in $tmp/*.nc; do
  echo "Symbolic link $i"
  ln -s $i .
done
tmp=$from_path/cube_to_target/inputdata/smooth_topo_cube
cd $to_path/cube_to_target/inputdata/smooth_topo_cube
for i in $tmp/*.dat; do
  echo "Symbolic link $i"
  ln -s $i .
done
cd $to_path
mkdir bin_to_cube
cd bin_to_cube
tmp=$from_path/bin_to_cube
for i in $tmp/*.nc; do
  echo "Symbolic link $i"
  ln -s $i .
done
fi
