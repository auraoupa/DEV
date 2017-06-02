#!/bin/bash

cp /scratch/cnt0024/hmg2840/albert7a/NACHOS12.L75/NACHOS12.L75-I/NACHOS12.L75_bathymetry_20S_80N_nfrontiercleaned.nc NACHOS12.L75_bathymetry_20S_80N_nfrontiercleaned_repeat3.nc

modif_bathy NACHOS12.L75_bathymetry_20S_80N_nfrontiercleaned_repeat3.nc

cp NACHOS12.L75_bathymetry_20S_80N_nfrontiercleaned_repeat3.nc /scratch/cnt0024/hmg2840/albert7a/NACHOS12.L75/NACHOS12.L75-I/.
