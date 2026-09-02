#!/bin/csh
#PBS -N topodrv

### Charging account
#PBS -A P93300042 
### Request one chunk of resources with N CPU and M GB of memory
#PBS -l select=1:ncpus=1:mem=128GB
### 
#PBS -l walltime=04:00:00
### Route the job to the casper queue
#PBS -q casper
### Join output and error streams into single file
#PBS -j oe



# This job's working directory
#

#cd /glade/work/juliob/Topo/MyTopo/cube_to_target


./create_topo.py --ogrid=ne30pg3 --smoothing_scale=100 --ridge_window_ratio=0.25 --tag=v6_test
