#!/bin/bash


ulimit -s unlimited

cp ../PSEUDO_COORD/NARYS2V3_pseudo_coord.nc .
cp ../PSEUDO_COORD/NACHOS12_pseudo_coord.nc .


ncap2 -O -s "glamt=glamf" NARYS2V3_pseudo_coord.nc NARYS2V3_pseudo_coord.nc
ncap2 -O -s "gphit=gphif" NARYS2V3_pseudo_coord.nc NARYS2V3_pseudo_coord.nc

ncap2 -O -s "glamt=glamf" NACHOS12_pseudo_coord.nc NACHOS12_pseudo_coord.nc
ncap2 -O -s "gphit=gphif" NACHOS12_pseudo_coord.nc NACHOS12_pseudo_coord.nc

./mkweight.ksh -c NACHOS12_pseudo_coord.nc -M NARYS2V3_pseudo_coord.nc -m bilinear

#cp /scratch/cnt0024/hmg2840/albert7a/GLORYS2V3/data/2010/GLORYS2V3_NACHOS025_vertext_20100115_R20130808_gridT.nc .

#scripinterp.exe namelist_scripinterp
