#!/bin/bash

cp /scratch/cnt0024/hmg2840/albert7a/GLORYS2V3/data/2010/drowned_GLORYS2V3_NACHOS025_2010_gridT.nc .
cp /scratch/cnt0024/hmg2840/albert7a/GLORYS2V3/data/2010/drowned_GLORYS2V3_NACHOS025_2010_gridS.nc .

check_nan_and_replace drowned_GLORYS2V3_NACHOS025_2010_gridT.nc votemper
check_nan_and_replace drowned_GLORYS2V3_NACHOS025_2010_gridS.nc vosaline

cp drowned_GLORYS2V3_NACHOS025_2010_gridT.nc /scratch/cnt0024/hmg2840/albert7a/GLORYS2V3/data/2010/clean_drowned_GLORYS2V3_NACHOS025_2010_gridT.nc
cp drowned_GLORYS2V3_NACHOS025_2010_gridS.nc /scratch/cnt0024/hmg2840/albert7a/GLORYS2V3/data/2010/clean_drowned_GLORYS2V3_NACHOS025_2010_gridS.nc

