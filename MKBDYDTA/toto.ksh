#!/bin/ksh

# $Id: mk_bdydta.ksh 806 2016-03-09 11:48:41Z molines $
# This script is used for building bdydta files from global model, in case of strait line BDY (structured)
# nambdy, nambdy_index are used from the model namelist file
# This script assume same grid geometry for the regional model and external model
#set -x

CONFIG_IN=NATL05
CASE_IN=GJM189d
CONFCASE_IN=${CONFIG_IN}-${CASE_IN}

CONFIG_BDY=NATL05
CASE_BDY=MJM36
CONFCASE_BDY=${CONFIG_BDY}-${CASE_BDY}

# need to know limits of CONFIG_BDY in CONFIG_IN : [ position of (1,1) point in  CONFIG_BDY related to CONFIG_IN].
# can be seen in history of the bathy file of CONFIG_BDY, but easier to be an input data
#  e.g. ncks -d x,408,570 -d y,314,373   ### check for -F option (or not ) : this script assume -F ####
imin=1 
jmin=1

# years to process  y1-y2
years=2007-2012

#############################################################################
# WARNING : when setting up the namelist, take care that this script needs  #
# #######  that array data must be given as a comma separated list, without #
#          blank characters.                                                #
#          e.g : nn_dyn2d_dta   =  1,1    # not  nn_dyn2d_dta   =  1 , 1    #
#############################################################################
# namelist with nambdy and nambdy_index
namlist=namelist.${CONFCASE_BDY}
DTADIR_IN=$WORKDIR/${CONFIG_IN}/${CONFCASE_IN}-S
DTADIR_BDY=$WORKDIR/${CONFIG_BDY}/${CONFIG_BDY}-I/BDY
WRK=$WORKDIR/WRK_BDY_DTA

mkdir -p $DTADIR_BDY
mkdir -p $WRK

##############################

LookInNamelist()    {
         if [ $# = 2 ] ; then znamelist=$2 ; else znamelist=namelist ; fi
         eval grep -e $1 $znamelist      | tr -d \' | tr -d \"  | sed -e 's/=/  = /' | awk ' {if ( $1 == str ) print $3 }' str=$1
                    }
# --- 
# For logical value in namelist always return T or F despite the namelist format ( TRUE,true, true etc ...)
normalize()         {
               tmp=$(echo $1 | tr 'a-z' 'A-Z' )
               echo $tmp  | grep -q 'T'
               if [ $? = 0 ] ; then echo T ; else echo F ; fi
                    }
# ---
# extract vertical slices for CONFIG_BDY from CONFIG_IN
get_slices()        {
    echo In get_slices 
    cd $DTADIR_IN/$year/
    for var in $SSH $TS $U $V $ICE; do
      # position of the bdy (nbdyind correspond to U points for W and E, for V points for S and N
      # for S and W all points have the same index (joff=0)
      # for N and E  T points have a +1 offset (joff=1) as well as tangeantial velocities  U and V
      # for N and E  V and U have no offset
      #
      # inrim (+/- 1) indicates the direction of the inner points from the bdy
      lflagu='' ; lglagv=''
      case $bdy in 
      ( 'S' ) ; joff=0 ; inrim=1  ;;
      ( 'N' ) ; joff=1 ; inrim=-1 ; lflagu=1 ;;
      ( 'W' ) ; joff=0 ; inrim=1  ;;
      ( 'E' ) ; joff=1 ; inrim=-1 ; lflagv=1 ;;
      esac 

      case $var in 
      ( 'SSH' ) ; grd='gridT' ; varlst="sossheig"          ;;
      ( 'TS'  ) ; grd='gridT' ; varlst="votemper,vosaline" ;;
      ( 'ICE' ) ; grd='icemod'; varlst="ileadfra,iicethic,isnowthi" ;;
      ( 'U'   ) ; grd='gridU' ; varlst="vozocrtx" ; joff=0 
                  if [ $lflagu ] ; then joff=1 ; fi           ;;
      ( 'V'   ) ; grd='gridV' ; varlst="vomecrty" ; joff=0 
                  if [ $lflagv ] ; then joff=1 ; fi           ;;
      esac

      for ir in $(seq 1 $nn_rimwidth) ; do
        if [ $crosdim = 'y' ] ; then
           ibref=imin ; jbref=jmin
        else  # crosdim = x ( E W ndy)
           ibref=jmin ; jbref=imin
        fi
        i1=$(( ibref - 1 + nbdybeg ))
        i2=$(( ibref - 1 + nbdyend ))
        j1=$(( jbref - 1 + nbdyind + joff + inrim*(ir-1) ))  # +/- ( ir-1 )

        echo extracting rim row $ir at $j1 $jmin $nbdyind $joff $inrim $crosdim $bdy $var

        for f in ${CONFCASE_IN}_y${year}m??d??_$grd.nc ; do
          tag=$( echo $f | awk -F_ '{print $2}' )
          ncks -F -O -d $crecdim,$i1,$i2 -d $crosdim,$j1 -v $varlst $f $WRK/${ir}_${bdy}_${var}_${tag}.nc
        done
      done
    done
                    }
 
# ---
#
concat_time_slices() {
    echo In concat_time_slices
    cd $WRK
    for var in $SSH $TS $U $V $ICE ; do
      for ir in $(seq 1 $nn_rimwidth) ; do
        ncrcat -h -O ${ir}_${bdy}_${var}_y${year}m??d??.nc ${ir}_${bdy}_${var}_y${year}.nc
      done
    done
                     }
# ---
#
concat_slices_in_space() {
    echo In concat_slices_in_space
    cd $WRK
    for var in $SSH $TS $U $V $ICE ; do
      for ir in $(seq 1 $nn_rimwidth) ; do
        # eliminate unlimited dims
        ncks  -O --fix_rec_dmn time_counter  ${ir}_${bdy}_${var}_y${year}.nc ${ir}_ztmp.nc
        # permute x and time
        ncpdq -O -a $crecdim,time_counter ${ir}_ztmp.nc ${ir}_ztmp_permuted.nc
        # transform x dimension unlimited
        ncks -O --mk_rec_dmn  $crecdim ${ir}_ztmp_permuted.nc ${ir}_ztmp_xunlim.nc
      done
      # now concatenate along x
      ncrcat -O *_ztmp_xunlim.nc  ztmp_xunlim.nc
      # turn back x fixed dim
      ncks  -O --fix_rec_dmn $crecdim ztmp_xunlim.nc ztmp_xfixed.nc
      # permute time_counter and x
      ncpdq -O -a time_counter,$crecdim ztmp_xfixed.nc ztmp_xpermutted.nc
      # transform back time_counter as unlimited final name
      ncks -O --mk_rec_dmn time_counter ztmp_xpermutted.nc $DTADIR_BDY/${CONFCASE_BDY}_${bdy}bdy${var}_y${year}.nc
      ncatted -h -a rimwidth,global,c,d,$nn_rimwidth $DTADIR_BDY/${CONFCASE_BDY}_${bdy}bdy${var}_y${year}.nc

      ### JMM I think that for E and W boundaries, files must be transposed in order to allwas have
      ###  variables with [Xt,1,time] dimension. At this point, for E and W,  variables are [1,Xt,time] 

      case $bdy in 
      ( 'E' | 'W') echo "WARNING: swapping of the space dimension for E W ndy " 
        ncrename -d y,Xt $DTADIR_BDY/${CONFCASE_BDY}_${bdy}bdy${var}_y${year}.nc
        ncrename -d x,y  $DTADIR_BDY/${CONFCASE_BDY}_${bdy}bdy${var}_y${year}.nc
        ncpdq -O -a y,Xt    $DTADIR_BDY/${CONFCASE_BDY}_${bdy}bdy${var}_y${year}.nc  ztmp.nc
        mv ztmp.nc $DTADIR_BDY/${CONFCASE_BDY}_${bdy}bdy${var}_y${year}.nc  ;;
      esac
    done
                         }
# Get a namelist block from its name in namelist
getblock()          {
            # if a 2nd argument is passed, it is a namelist name. Default to 'namelist'
            if [ $2 ] ; then namelist=$2 ; else namelist=namelist ; fi
            cat $namelist | awk 'BEGIN {flip=0} { \
            if ( $1 == "&"blk && flip == 0 ) { flip=1   }  \
            if ( $1 != "/"  && flip == 1   ) { print $0 }  \
            if ( $1 == "/"  && flip == 1   ) { print $0 ; flip=0 }    \
                                    }' blk=$1
                    }
# ---

# ---
# get data files associated with the block namelist ( eventually filtered )
getfiles()  {
#set -x 
         zstr="sn_"
         zbdyset=""
         # if a 4th argument is passed it can be either BDY{bdyset} or a namelist filename
         if [ $4 ] ; then 
             if [ $( echo $4 | awk '{ print index( $1,"BDY")}')  = 1 ] ; then
                zstr="bn_"
                ztmp=$( echo $4 | sed -e 's/BDY//')
                zbdyset=$( echo $ztmp | cut -c 1 )
                znbdy=$( echo $ztmp | cut -c 2 )
                namelist=namelist.NATL05-MJM36
             else
                namelist=$4 
             fi
         else 
             namelist=namelist.NATL05-MJM36
         fi
         echo $zstr
         echo $zbdy_set
         echo $namelist
         ZPDIR=$2
         ZFDIR=$3
         cmd="getblock $1 $namelist |  awk '{if ( index(\$1,\"$zstr\") != 0 ) print \$0}'   $filter |  awk '{ print \$3 }'"
         lst=$(eval $cmd | tr -d "'" | tr -d "," )
         # check for eventual weight in the namelist block ( WARNING : problem if mixed ... )
         lstw=$(getblock $1 $namelist | grep $zstr  |  awk -F, '{ print $7 }' | tr -d "'" | tr -d " " )
         lst_arr=($lst)
         nfld=$(( ${#lst_arr[@]} / znbdy ))
         for ifld in $(seq 1 $nfld) ; do
             ii=$(( (zbdyset  )*nfld +ifld -1 ))
             echo  $zbdyset $ifld $ii  ${lst_arr[$ii ]}
         done

         echo ${lst_arr[@]}
         exit

         for f in $lst ; do
           ln_clim=$(getblock $1 $namelist | awk '{if ( index($1,"$zstr") != 0 ) print $0}' | grep $f  |  awk -F, '{ print $5 }' | sort -u )
           ln_clim=$(normalize $ln_clim )
           # look for file_type (ie yearly monthly weekly ... )
           file_type=$(getblock $1 $namelist | awk '{if ( index($1,"$zstr") != 0 ) print $0}' | grep $f  |  awk -F, '{ print $6 }' | tr -d "'" | tr -d " " | sort -u )

           case $file_type in 
           ( yearly )  # get files for 3 years 
           if [ $ln_clim = F ] ; then
             for y in $(seq -f "%04g" $yearm1 $yearp1) ; do
               echo ${f}_y${y}.nc
#              copyfor  $P_FOR_DIR/${f}_y${y}.nc ${f}_y${y}.nc
               if [ ${#lstw} != 0  ] ; then   # there are weight defined, so we do not try to copy nested files (??? AGRIF ??? )
                 copyfor  $ZPDIR/${f}_y${y}.nc ${f}_y${y}.nc
               else
                 rapatrie  ${f}_y${y}.nc $ZPDIR $ZFDIR  ${f}_y${y}.nc
               fi
             done
           else
             echo ${f}.nc
#            copyfor  $P_FOR_DIR/${f}.nc ${f}.nc
             if [ ${#lstw} != 0  ] ; then   # there are weight defined, so we do not try to copy nested files ( ??? AGRIF ??? )
                copyfor  $ZPDIR/${f}.nc ${f}.nc
             else
                rapatrie  ${f}.nc $ZPDIR $ZFDIR  ${f}.nc
             fi
           fi ;;
           ( monthly ) # get file m12 for yearm1, then m01 -m12 for current year , m01 for yearp1
           if [ $ln_clim = F ] ; then
             for y in $(seq -f "%04g" $yearm1 $yearp1) ; do
               if [ $y = $yearm1 ] ; then
                 echo ${f}_y${y}m12.nc
                 if [ ${#lstw} != 0 ] ; then   # there are weight defined, so we do not try to copy nested files (??? AGRIF ??? )
                   copyfor  $ZPDIR/${f}_y${y}m12.nc ${f}_y${y}m12.nc
                 else
                   rapatrie  ${f}_y${y}m12.nc $ZPDIR $ZFDIR  ${f}_y${y}m12.nc
                 fi
               elif [ $y = $yearp1 ] ; then
                 echo ${f}_y${y}m01.nc
                 if [ ${#lstw} != 0 ] ; then   # there are weight defined, so we do not try to copy nested files (??? AGRIF ??? )
                   copyfor  $ZPDIR/${f}_y${y}m01.nc ${f}_y${y}m01.nc
                 else
                   rapatrie  ${f}_y${y}m01.nc $ZPDIR $ZFDIR  ${f}_y${y}m01.nc
                 fi
               else
                 for  mm in $(seq -w 01 12 ) ; do
                    echo ${f}_y${y}m${mm}.nc
                    if [ ${#lstw} != 0 ] ; then   # there are weight defined, so we do not try to copy nested files (??? AGRIF ??? )
                      copyfor  $ZPDIR/${f}_y${y}m${mm}.nc ${f}_y${y}m${mm}.nc
                    else
                      rapatrie  ${f}_y${y}m${mm}.nc $ZPDIR $ZFDIR  ${f}_y${y}m${mm}.nc
                    fi
                 done
                   
               fi
             done
           else
             for  mm in $(seq -w 01 12 ) ; do
                echo ${f}_m${mm}.nc
                if [ ${#lstw} != 0  ] ; then   # there are weight defined, so we do not try to copy nested files ( ??? AGRIF ??? )
                  copyfor  $ZPDIR/${f}_m${mm}.nc ${f}_m${mm}.nc
                else
                  rapatrie  ${f}_m${mm}.nc $ZPDIR $ZFDIR  ${f}_m${mm}.nc
                fi
             done
           fi ;;
           esac
         done   
            }
# ---

##############################
y1=$( echo $years | awk -F- '{ print $1}' )
y2=$( echo $years | awk -F- '{ print $2}' )

nbdy=$( LookInNamelist nb_bdy $namlist ) 
#set -x
cn_dyn2d_lst=($( LookInNamelist cn_dyn2d $namlist | sed -e 's/,/ /g') )
echo $namlist
echo ${cn_dyn2d_lst[@]}
               filter='| grep -v bn_u3d | grep -v bn_v3d '                               # skip dyn3d_dta
               filter="$filter | grep -v bn_tem | grep -v bn_sal"                        # skip tra_dta
               filter="$filter | grep -v bn_frld | grep -v bn_hicif | grep -v bn_hsnif"  # skip lim2_dta

blk=nambdy_dta ; getfiles $blk    dir1 dir2  BDY02
exit
str="bn_"
cmd="getblock nambdy_dta $namlist |  awk '{if ( index(\$1,\"$str\") != 0 ) print \$0}' | grep -v bn_u3d | grep -v bn_v3d | grep -v bn_tem | grep -v bn_sal | grep -v bn_frld | grep -v bn_hicif | grep -v bn_hsnif | awk '{ print \$3}'"
lst=$(eval $cmd | tr -d "'" | tr -d ",")
echo ${lst[0]}
lst_array=($lst)
echo ${lst_array[0]}
exit
