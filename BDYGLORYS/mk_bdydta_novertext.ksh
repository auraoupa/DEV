#!/bin/ksh


jmin=5
jmax=527

year=2010
dir=/scratch/cnt0024/hmg2840/albert7a/GLORYS2V3/data/${year}

for typ in grid2D gridT gridS gridUV; do

  case $typ in
    grid2D) listfiles=$(ls ${dir}/GLORYS2V3_NACHOS025_${year}*${typ}*);;
         *) listfiles=$(ls ${dir}/GLORYS2V3_NACHOS025_${year}*${typ}*);;
  esac

  case $typ in
    grid2D) var="SSH";;
     gridT) var="T";;
     gridS) var="S";;
    gridUV) var="UV";;
  esac

  for file in $listfiles ; do
 
     tag=$(basename $file | awk -F_ '{print $3}')

    filen=NACHOS025.L75_NBDY${var}_${tag}_novertext.nc
    files=NACHOS025.L75_SBDY${var}_${tag}_novertext.nc
    echo $file $filen $files 

    ncks -O -F -d x,2,537 -d y,$jmin,$jmin $file $files
    ncks -O -F -d x,2,537 -d y,$jmax,$jmax $file $filen
  
  done
done

