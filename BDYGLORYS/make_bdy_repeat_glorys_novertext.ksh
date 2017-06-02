#!/bin/bash

year=2010

ncks -O -F -d y,2,2 -d x,698,1329 /scratch/cnt0024/hmg2840/albert7a/NACHOS12.L75/NACHOS12.L75-I/NACHOS12.L75_mask.nc new_mask_BDYS.nc

ncks -O -F -d y,1567,1567 -d x,500,1440 /scratch/cnt0024/hmg2840/albert7a/NACHOS12.L75/NACHOS12.L75-I/NACHOS12.L75_mask.nc new_mask_BDYN.nc

ncks -O -F -d y,1567,1567 NACHOS12.L75-MAA01_y2010m01d05.5d_gridT.nc gridT_NBDY.nc
ncks -O -F -d y,2,2 NACHOS12.L75-MAA01_y2010m01d05.5d_gridT.nc gridT_SBDY.nc
ncks -O -F -d y,1567,1567 NACHOS12.L75-MAA01_y2010m01d05.5d_gridU.nc gridU_NBDY.nc
ncks -O -F -d y,2,2 NACHOS12.L75-MAA01_y2010m01d05.5d_gridU.nc gridU_SBDY.nc
ncks -O -F -d y,1567,1567 NACHOS12.L75-MAA01_y2010m01d05.5d_gridV.nc gridV_NBDY.nc
ncks -O -F -d y,2,2 NACHOS12.L75-MAA01_y2010m01d05.5d_gridV.nc gridV_SBDY.nc

for var in sossheig votemper vosaline vomecrty vozocrtx; do

  for bdy in N S; do

    case $var in
      sossheig) typ="SSH";typ2="SSH";file_bdy="gridT";dep="deptht";dim="2D";;
      votemper) typ="T";typ2="T";file_bdy="gridT";dep="deptht";dim="3D";;
      vosaline) typ="S";typ2="S";file_bdy="gridT";dep="deptht";dim="3D";;
      vozocrtx) typ="UV";typ2="U";file_bdy="gridU";dep="depthu";dim="3D";;
      vomecrty) typ="UV";typ2="V";file_bdy="gridV";dep="depthv";dim="3D";;
    esac


    for file_glo in $(ls NACHOS025.L75_${bdy}BDY${typ}_*novertext.nc); do
 
      file_glo_rep=$(echo $file_glo | sed "s/NACHOS025.L75/NACHOS12.L75/g" | sed "s/$typ/$typ2/g")
      bdy_final=bdy_${file_glo_rep}
      ncks -O -v nav_lat,nav_lon,$dep,time_counter,$var ${file_bdy}_${bdy}BDY.nc $file_glo_rep
      repeat_bdy $file_glo $var $dim $file_glo_rep
      
      case $bdy in
        N) mask='new_mask_BDYN.nc'; ncks -O -F -d x,500,1440 $file_glo_rep $bdy_final;;
        S) mask='new_mask_BDYS.nc'; ncks -O -F -d x,698,1329 $file_glo_rep $bdy_final;;
      esac

#      case $var in
#        vomecrty) mktmask $bdy_final $var $dim V $mask;;
#        vozocrtx) mktmask $bdy_final $var $dim U $mask;;
#               *) mktmask $bdy_final $var $dim T $mask;;
#      esac

    done

#  ncrcat -O bdy_NACHOS12.L75_${bdy}BDY${typ2}_2010????.nc GLORYS2V3-NACHOS12.L75_${bdy}bdy${typ2}_y2010.nc

  done

done


