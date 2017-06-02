#!/bin/ksh
# @ job_type = serial
# @ requirements = (Feature == "prepost")
# @ wall_clock_limit=20:00:00
# @ job_name = zglorys300
# @ output   = $(job_name).$(jobid)
# @ error    = $(job_name).$(jobid)
# @ queue

#!/bin/bash
#SBATCH -J zglorys300
#SBATCH --nodes=1
#SBATCH --ntasks=1
#SBATCH --ntasks-per-node=24
#SBATCH --time=1:00:00
#SBATCH -e zglorys300.e%j
#SBATCH -o zglorys300.o%j
#SBATCH --exclusiv


set -vx
CONFIG=ATLYS2.L75
CASE=GL2V3
zgr=NATL60_v4.1_cdf_mesh_zgr.nc
IDIR=$WORKDIR/NATL60/NATL60-I
DATADIR=$WORKDIR/$CONFIG/${CONFIG}-${CASE}-MEAN
WRKDIR=$WORKDIR/CI_ATLYS
year1=2012
year2=2013
#
## end of customization ##

mkdir -p $WRKDIR

cp ic_field_vertical_extent ic_changezgrlev $WRKDIR
cd $WRKDIR
ln -sf $IDIR/$zgr ./

ulimit -s unlimited

#-----
# (0): check if all needed files are in the current directory
ierr=0

if [ ! -f $zgr ] ; then
  echo 'missing target zgr file (contains gdept_0) :' $zgr
  ierr=$(( ierr + 1 ))
fi

for exe in ./ic_field_vertical_extent ./ic_changezgrlev ; do
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

for var in votemper vosaline ; do
   case $var in 
   ( 'votemper' ) ; grid='gridT' ;;
   ( 'vosaline' ) ; grid='gridT' ;;
   esac

   for year in $(seq $year1 $year2 ) ; do
       for month in $(seq -f '%02g' 1 12 ) ; do
          tag=y${year}m${month}
          nemo_file=$DATADIR/$year/${CONFIG}-${CASE}_${tag}_$grid.nc
          ln -sf $nemo_file ./
          nemo_file=$(basename $nemo_file )
# (2): modify regular grid file so that bottom values are extended down to jpk
# USAGE : modif_regular_field IN-file IN-var

         ./ic_field_vertical_extent $nemo_file  $var

#-----
# (3): change vertical resolution :
#  usage : ic_changezgrlev -f IN-file -o OUT-file  -zo OUT-zgr ...
#         [-subdomain  jpni jpnj] 
       
         g=${nemo_file%.nc}_L300
         ./ic_changezgrlev  -f ${nemo_file}_copy -o $g.nc -zo $zgr -subdomain 1 1

#-----
# (4): drown files with SOSIE mask_drown_field.x
          mask_drown_field.x -D -m 0 -x nav_lon -y nav_lat -z deptht -i $g.nc -o drowned_$g.nc -v $var  -p -1
          rm ./$nemo_file # remove local link
          rm ./${nemo_file}_copy # remove local file
       done # month
   done # year
done # var
