#!/bin/ksh

set -vx
CONFIG=ORCA12.L46
CASE=MJM189
regular_res=0.5  # deg
zgr=NATL60_cdf_mesh_zgr.nc
#
## end of customization ##
REGULAR=${CONFIG}-${CASE}_${regular_res}x${regular_res}deg

######################################################################
nemo_file=ORCA12.L46-MJM189_y1998m01.5d_gridT.nc  ####################
######################################################################
tag=$( echo $nemo_file | awk -F_ '{print $2}' )
ulimit -s unlimited

#-----
# (0): check if all needed files are in the current directory
ierr=0
if [ ! -f mesh_hgr.nc ] ; then 
  echo missing mesh_hgr.nc corresponding to $nemo_file
  ierr=$(( ierr + 1 ))
fi
if [ ! -f mask.nc ] ; then 
  echo missing mask.nc corresponding to $nemo_file
  ierr=$(( ierr + 1 ))
fi

if [ ! -f $zgr ] ; then
  echo 'missing target zgr file (contains gdept_0) :' $zgr
  ierr=$(( ierr + 1 ))
fi

for exe in ./cdf2levitusgrid3d ./ic_field_vertical_extent ./ic_changezgrlev ; do
  if [ ! -x $exe ] ; then
     echo 'missing executable ' $(basename $exe ) ' in the current dir'
     ierr=$(( ierr + 1 ))
  fi
done

mask_drown_field.x > /dev/null 2>&1
if [ $? != 0 ] ; then
  echo 'be sure that mask_drown_field.x is in your PATH (sosie package)'
  ierr=$(( ierr + 1 ))
fi

if [ $ierr != 0 ] ; then
   echo 'found ' ierr ' error(s)'
   exit 1
fi

#-----
# (1):  from nemo to regular grid
#  usage : cdf2levitusgrid3d -f IN-file -o OUT-file  -v VAR-name3D [-360]
#         [-r resolution] 
for var in votemper vosaline ; do
  ./cdf2levitusgrid3d -f $nemo_file  -v $var -r $regular_res -o ${REGULAR}_${tag}_$var.nc
done

#-----
# (2): modify regular grid file so that bottom values are extended down to jpk
# USAGE : ic_field_vertical_extent IN-file IN-var

for var in votemper vosaline ; do
  ./ic_field_vertical_extent ${REGULAR}_${tag}_$var.nc $var
done

#-----
# (3): change vertical resolution :
#  usage : ic_changezgrlev -f IN-file -o OUT-file  -zo OUT-zgr ...
#         [-subdomain  jpni jpnj] 
       
for var in votemper vosaline ; do
 g=${REGULAR}.L300_${tag}_$var.nc
 ./ic_changezgrlev  -f ${REGULAR}_${tag}_$var.nc_copy -o $g -zo $zgr -subdomain 1 1
done

#-----
# (4): drown files with SOSIE mask_drown_field.x
for var in votemper vosaline ; do
   mask_drown_field.x -D -m 0 -z deptht -i ${REGULAR}.L300_${tag}_$var.nc -o drowned_${REGULAR}.L300_${tag}_$var.nc -v $var 
done
