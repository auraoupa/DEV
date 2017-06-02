#!/bin/bash

ncks -O -v deptht,time_counter,nav_lat /scratch/cnt0024/hmg2840/albert7a/GLORYS2V3/data/2010/GLORYS2V3_NACHOS025_vertext_20100115_R20130808_gridT.nc NARYS2V3_pseudo_coord.nc

ncks -O -v time,nav_lat /scratch/cnt0024/hmg2840/albert7a/NACHOS12.L75/NACHOS12.L75-I/NACHOS12.L75_coordinates_20S_80N.nc NACHOS12_pseudo_coord.nc

pseudo_coord

ncks -O -x -v nav_lat NARYS2V3_pseudo_coord.nc NARYS2V3_pseudo_coord.nc
ncks -O -x -v nav_lat NACHOS12_pseudo_coord.nc NACHOS12_pseudo_coord.nc

