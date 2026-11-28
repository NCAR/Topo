#!/bin/csh
#PBS -N topodrv

### Charging account
#PBS -A P93300042 
### Request one chunk of resources with N CPU and M GB of memory
###PBS -l select=1:ncpus=1:mem=128GB
#PBS -l select=1:ncpus=1:mem=64GB
### 
#PBS -l walltime=02:30:00
### Route the job to the casper queue
#PBS -q casper
### Join output and error streams into single file
#PBS -j oe



# This job's working directory
#

#cd /glade/work/juliob/Topo/MyTopo/cube_to_target

./create_topo.py --ogrid=ne30pg3 --smoothing_scale=100 --fine_smoothing_scale=-1 --gauss_scales=6,7.5,9,12,18,25,37,50,75 --ridge_window_ratio=0.50 --adaptive_window=5.0 --ridge_iter=2 --ynudge=0.0 --crest_perp_angle=45 --wedge_method=1 --wedge_search=1.0 --tag=scale_pnt_v311_e01
