#!/bin/ksh

# $Id: mk_bdydta.ksh 806 2016-03-09 11:48:41Z molines $
# This script is used for building bdydta files from global model, in case of strait line BDY (structured)
# nambdy, nambdy_index are used from the model namelist file
# This script assume same grid geometry for the regional model and external model
#set -x

CONFIG_IN=ZNATL12.L75
CASE_IN=MJMgd16
CONFCASE_IN=${CONFIG_IN}-${CASE_IN}

CONFIG_BDY=NACHOS12.L75
CASE_BDY=MAATEST
CONFCASE_BDY=${CONFIG_BDY}-${CASE_BDY}

# need to know limits of CONFIG_BDY in CONFIG_IN : [ position of (1,1) point in  CONFIG_BDY related to CONFIG_IN].
# can be seen in history of the bathy file of CONFIG_BDY, but easier to be an input data
#  e.g. ncks -d x,408,570 -d y,314,373   ### check for -F option (or not ) : this script assume -F ####
imin=5 
jmin=65

# years to process  y1-y2
years=2010-2010

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
      # bug if rimwidth=1      ncrcat -O *_ztmp_xunlim.nc  ztmp_xunlim.nc
      cp 1_ztmp_xunlim.nc  ztmp_xunlim.nc
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
# ---
##############################
y1=$( echo $years | awk -F- '{ print $1}' )
y2=$( echo $years | awk -F- '{ print $2}' )

nbdy=$( LookInNamelist nb_bdy $namlist ) 
nn_rimwidth_lst=( $( LookInNamelist nn_rimwidth $namlist  | sed -e 's/,/ /g' ) )  # nbdy values separated by,
nn_dyn2d_dta_lst=( $( LookInNamelist nn_dyn2d_dta $namlist  | sed -e 's/,/ /g' ) )
nn_dyn3d_dta_lst=( $( LookInNamelist nn_dyn3d_dta $namlist  | sed -e 's/,/ /g' ) )
nn_tra_dta_lst=( $( LookInNamelist nn_tra_dta $namlist  | sed -e 's/,/ /g' ) )
nn_ice_lim_dta_lst=( $( LookInNamelist nn_ice_lim_dta $namlist  | sed -e 's/,/ /g' ) )

bdy_lst=($( LookInNamelist ctypebdy $namlist ) )
nbdyind_lst=($( LookInNamelist nbdyind $namlist ) )
nbdybeg_lst=($( LookInNamelist nbdybeg $namlist ) )
nbdyend_lst=($( LookInNamelist nbdyend $namlist ) )

for year in $( seq -f "%04g" $y1 $y2 ); do
  echo Processing year $year
  for bdyset in $( seq 0 $((nbdy-1)) ) ; do
    bdy=${bdy_lst[$bdyset]}
    nbdyind=${nbdyind_lst[$bdyset]}
    nbdybeg=${nbdybeg_lst[$bdyset]}
    nbdyend=${nbdyend_lst[$bdyset]}

    nn_rimwidth=${nn_rimwidth_lst[$bdyset]}
    nn_dyn2d_dta=${nn_dyn2d_dta_lst[$bdyset]}
    nn_dyn3d_dta=${nn_dyn3d_dta_lst[$bdyset]}
    nn_tra_dta=${nn_tra_dta_lst[$bdyset]}
    nn_ice_lim_dta=${nn_ice_lim_dta_lst[$bdyset]}

    # select variables to process according to dta requested in the namelist
    if [ $nn_dyn2d_dta = 0   ] ; then SSH='' ;        else  SSH='SSH'     ; fi
    if [ $nn_dyn3d_dta = 0   ] ; then U=''   ; V='' ; else  U='U' ; V='V' ; fi
    if [ $nn_tra_dta = 0     ] ; then TS=''  ;        else  TS='TS'       ; fi
    if [ $nn_ice_lim_dta = 0 ] ; then ICE='' ;        else  ICE='ICE'     ; fi


    case $bdy in 
    ( S ) ; echo " South boundary detected : " $bdyset $bdy
           crecdim='x' ; crosdim='y' ;;
    ( N ) ; echo " North boundary detected : " $bdyset $bdy
           crecdim='x' ; crosdim='y' ;;
    ( W ) ; echo " West boundary detected :  " $bdyset $bdy
           crecdim='y' ; crosdim='x' ;;
    ( E ) ; echo " East boundary detected : "  $bdyset $bdy
           crecdim='y' ;crosdim='x' ;;
    esac

    echo "bdy index : "$nbdyind 
    echo "    from "$nbdybeg " to " $nbdyend " with rimwidth of " $nn_rimwidth

    get_slices
    concat_time_slices
    concat_slices_in_space
  done
done
