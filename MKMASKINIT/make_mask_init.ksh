#!/bin/bash

ncks -O -F -d y,1566,1566 -d x,500,1440 /scratch/cnt0024/hmg2840/albert7a/NACHOS12.L75/NACHOS12.L75-I/NACHOS12.L75_bathymetry_20S_80N_nfrontiercleaned.nc new_mask_BDYN.nc


cp /scratch/cnt0024/hmg2840/albert7a/NACHOS12.L75/NACHOS12.L75-I/BDY/NACHOS12.L75-MAATEST_NbdySSH_y2010.nc .
mkmask NACHOS12.L75-MAATEST_NbdySSH_y2010.nc 'sossheig' '2D' 'T'
cp /scratch/cnt0024/hmg2840/albert7a/NACHOS12.L75/NACHOS12.L75-I/BDY/NACHOS12.L75-MAATEST_NbdyTS_y2010.nc .
mkmask NACHOS12.L75-MAATEST_NbdyTS_y2010.nc 'votemper' '3D' 'T'
mkmask NACHOS12.L75-MAATEST_NbdyTS_y2010.nc 'vosaline' '3D' 'T'
cp /scratch/cnt0024/hmg2840/albert7a/NACHOS12.L75/NACHOS12.L75-I/BDY/NACHOS12.L75-MAATEST_NbdyU_y2010.nc .
mkmask NACHOS12.L75-MAATEST_NbdyU_y2010.nc 'vozocrtx' '3D' 'U'
cp /scratch/cnt0024/hmg2840/albert7a/NACHOS12.L75/NACHOS12.L75-I/BDY/NACHOS12.L75-MAATEST_NbdyV_y2010.nc .
mkmask NACHOS12.L75-MAATEST_NbdyV_y2010.nc 'vomecrty' '3D' 'V'

cp NACHOS12.L75-MAATEST_NbdySSH_y2010.nc /scratch/cnt0024/hmg2840/albert7a/NACHOS12.L75/NACHOS12.L75-I/BDY/NACHOS12.L75-MAATEST_NbdySSH_y2010_mask.nc
cp NACHOS12.L75-MAATEST_NbdyTS_y2010.nc /scratch/cnt0024/hmg2840/albert7a/NACHOS12.L75/NACHOS12.L75-I/BDY/NACHOS12.L75-MAATEST_NbdyTS_y2010_mask.nc
cp NACHOS12.L75-MAATEST_NbdyU_y2010.nc /scratch/cnt0024/hmg2840/albert7a/NACHOS12.L75/NACHOS12.L75-I/BDY/NACHOS12.L75-MAATEST_NbdyU_y2010_mask.nc
cp NACHOS12.L75-MAATEST_NbdyV_y2010.nc /scratch/cnt0024/hmg2840/albert7a/NACHOS12.L75/NACHOS12.L75-I/BDY/NACHOS12.L75-MAATEST_NbdyV_y2010_mask.nc
