#!/bin/csh
#PBS -N topodrv

### Charging account
#PBS -A P93300642 
### Request one chunk of resources with N CPU and M GB of memory
#PBS -l select=1:ncpus=1:mem=16GB
### 
#PBS -l walltime=04:00:00
### Route the job to the casper queue
#PBS -q casper
### Join output and error streams into single file
#PBS -j oe



# This job's working directory
#

cd /glade/work/juliob/Topo/MyTopo/cube_to_target


./create_topo.csh ne30pg3 100 Test01
