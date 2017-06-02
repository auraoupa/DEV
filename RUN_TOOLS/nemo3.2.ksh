# $Id: nemo3.2.ksh 1157 2012-05-12 17:52:01Z molines $
# function.ksh has already been sources in the main script
####### UNDER THIS LINE, YOU DON'T HAVE TO CHANGE ANYTHING #####################

echo Running on $( hostname )
echo NB_NPROC = $NB_NPROC
echo NB_NODES = $NB_NODES
## check existence of directories. Create them if they do'nt exist
 # TMPDIR can be set (if necessary in includefile.ksh)
 chkdir $TMPDIR

 chkdir $P_I_DIR
 chkdir $P_R_DIR
 chkdir $P_S_DIR

## -----------------------------------------------------
echo '(1) get all the working tools on the TMPDIR directory'
echo '-----------------------------------------------------'
cd $TMPDIR

cat << eof > EMPave_old.dat
0
eof

## clean eventual (?) old files
  \rm -f OK
  \rm -f damping*

  pwd > waytmp
  copy waytmp $P_CTL_DIR/

## copy of system and script tools: from P_CTL_DIR to TMPDIR
echo "copy script and get number ($$) usefull for run"

  rcopy $P_UTL_DIR/bin/datfinyyyy ./
  rcopy $P_CTL_DIR/includefile.ksh includefile.ksh 

## copy the executable OPA
set -x
chkfile $EXEC  

if [ $? = 0 ] ; then
  rcopy $EXEC ./opa
else
  echo " OPA must me recompiled. Deleted from workdir"
  exit 1
fi

## copy of the control files ( .db and and template namelist )
  rcopy $P_CTL_DIR/namelist.${CONFIG_CASE} namelist
  rcopy $P_CTL_DIR/namelist_ice namelist_ice
  rcopy $P_CTL_DIR/namelistio namelistio
  if [ $DAILY = 1 ] ; then rcopy $P_CTL_DIR/namelistio_daily namelistio_daily ; fi
  rcopy $P_CTL_DIR/$CONFIG_CASE.db ./

  nmsh=$(LookInNamelist nn_msh)
  if [ $nmsh != 0 ] ; then
    rcopy $P_UTL_DIR/bin/build_nc_iom ./
    rcopy $P_UTL_DIR/bin/build_nc_mask ./
    rcopy $P_UTL_DIR/bin/correct_holes ./
  fi 

  if [ $AGRIF = 1 ] ; then
   init_agrif 
   for idx in ${agrif_pref[@]} ; do
    rcopy $P_CTL_DIR/${idx}_namelist.${CONFIG_CASE} ${idx}_namelist
    rcopy $P_CTL_DIR/${idx}_namelist_ice ${idx}_namelist_ice
    rcopy $P_CTL_DIR/${idx}_namelistio ${idx}_namelistio
   done
  fi


## (2) Set up the namelist for this run
## -------------------------------------

## exchange wildcards with the correc info from db
      no=`tail -1 $CONFIG_CASE.db | awk '{print $1}' `
  nit000=`tail -1 $CONFIG_CASE.db | awk '{print $2}' `
  nitend=`tail -1 $CONFIG_CASE.db | awk '{print $3}' `

  sed -e "s/NUMERO_DE_RUN/$no/" \
    -e "s/NIT000/$nit000/" \
    -e "s/NITEND/$nitend/" \
    -e "s@CN_DIROUT@$WORKDIR/${CONFIG_CASE}-DIMGPROC.$no@" namelist > namelist1
  \cp namelist1 namelist
  chkdir  $WORKDIR/${CONFIG_CASE}-DIMGPROC.$no

## check restart case
  if [ $no != 1 ] ; then
   sed -e "s/RESTART/true/" namelist > namelist1
  else 
   sed -e "s/RESTART/false/" namelist > namelist1
  fi
  \cp namelist1 namelist

## check that the run period is a multiple of the dump period 
  rdt=$(LookInNamelist rn_rdt)
  nwri=$(LookInNamelist nn_write)

  var=` echo 1 | awk "{ a=$nitend ; b=$nit000 ; c=$nwri ; nenr=(a-b+1)/c ; print nenr}"`
  vernenr=` echo 1 | awk "{ a=$var; c=int(a); print c}"`

  if [ $vernenr -ne  $var ] ; then
    echo 'WARNING: the run length is not a multiple of the dump period ...'
#  exit 
  fi


## place holder for time manager (eventually)
  if [ $no != 1 ] ; then
    ndastpdeb=`tail -2 $CONFIG_CASE.db | head -1 | awk '{print $4}' `
  else
    ndastpdeb=$(LookInNamelist nn_date0)
    echo $ndastpdeb
  fi

year=$(( ndastpdeb / 10000 ))
mmdd=$(( ndastpdeb - year * 10000 ))

if [ $mmdd = 1231 ] ; then
 year=$(( year + 1 ))
fi
year=$( printf "%04d" $year)


  rdt=$(LookInNamelist rn_rdt)
  rdt=`echo 1 | awk "{ rdt=int($rdt); print rdt}" `
  echo $rdt

  ndays=` echo 1 | awk "{ a=int( ($nitend - $nit000 +1)*$rdt /86400.) ; print a }" `

  ndastpfin=`./datfinyyyy $ndastpdeb $ndays `
  echo $ndays days to run, starting $ndastpdeb ending $ndastpfin

# 2.2 Tracers
#------------
  if [ $TRACER = 1 ] ; then rcopy $P_CTL_DIR/namelist_top ./           ; fi
# if [ $CFC = 1    ] ; then rapatrie $CFC $P_I_DIR $F_DTA_DIR $OPA_CFC ; fi
# if [ $C14 = 1    ] ; then rapatrie $CO2 $P_I_DIR $F_DTA_DIR $OPA_CO2 ; fi
# if [ $C14 = 1    ] ; then rapatrie $C14 $P_I_DIR $F_DTA_DIR $OPA_C14 ; fi
# if [ $CFC = 1 ] ; then rcopy $P_CTL_DIR/namelist_cfc ./ ; fi
# if [ $C14 = 1 ] ; then rcopy $P_CTL_DIR/namelist_c14 ./ ; fi
# if [ $MYTRC = 1 ] ; then rcopy $P_CTL_DIR/namelist_mytrc ./ ; fi

#--------------------------------------
echo '(3) Look for input files'
echo '------------------------'

echo '[3.1] : configuration files'
echo '=========================='
## bathymetry
  if [ $BATL = 1 ] ; then rapatrie $BATFILE_LEVEL $P_I_DIR $F_DTA_DIR $OPA_BATFILE_LEVEL ; fi
  if [ $BAT  = 1 ] ; then rapatrie $BATFILE_METER $P_I_DIR $F_DTA_DIR $OPA_BATFILE_METER ; fi

## coordinates
  if [ $GR = 1 ] ; then
    rapatrie $COORDINATES $P_I_DIR $F_DTA_DIR $OPA_COORDINATES
  fi

## relaxation coefficient file
  if [ $REL = 1 ] ; then  rapatrie $RELAX $P_I_DIR $F_I_DIR $OPA_RELAX ; fi

## bottom friction file
  if [ $bfr = 1 ] ; then
    rapatrie $BFR $P_I_DIR $F_DTA_DIR $OPA_BFR
  fi

## KZ tides
  if [ $ZTMX = 1 ] ; then
    rapatrie $MASKITF $P_I_DIR $F_DTA_DIR $OPA_MASKITF
    rapatrie $M2ENER $P_I_DIR $F_DTA_DIR $OPA_M2ENER
    rapatrie $K1ENER $P_I_DIR $F_DTA_DIR $OPA_K1ENER
  fi

## TIDAL FRICTION
  if [ $UBAR_TIDE  = 1 ] ; then
    rapatrie $BFR_TIDE  $P_I_DIR $F_DTA_DIR $OPA_BFR_TIDE
  fi

## Extra horizontal LDF
  if [ $AHTH = 1 ] ; then
    rapatrie $HDIFMASK  $P_I_DIR $F_DTA_DIR $OPA_HDIFMASK
  fi

## geothermal heating
  if [ $geo = 1 ] ; then
    rapatrie $GEO $P_I_DIR $F_DTA_DIR $OPA_GEO
  fi

## coeff 2d for ldfdyn
  if [ $COEF2D  = 1 ] ; then
    rapatrie $AHM2D  $P_I_DIR $F_DTA_DIR $OPA_AHM2D
  fi

## [3.2] : Initial conditions, damping files
## =========================================

## temperature
  if [ $TEMP = 1 ] ; then 
     rapatrie $TEMPDTA $P_I_DIR $F_INI_DIR $OPA_TEMPDTA
  fi

## salinity
  if [ $SAL = 1 ] ; then
    rapatrie $SALDTA $P_I_DIR $F_INI_DIR  $OPA_SALDTA
  fi

## temperature damping (3D)
  if [ $TEMP_DMP = 1 ] ; then
     rapatrie $TEMPDTA_DMP $P_I_DIR $F_INI_DIR $OPA_TEMPDTA_DMP
  fi

## salinity damping (3D)
  if [ $SAL_DMP = 1 ] ; then
    rapatrie $SALDTA_DMP $P_I_DIR $F_INI_DIR  $OPA_SALDTA_DMP
  fi

## mooring position
  if [ $MOOR = 1 ] ; then rapatrie init_mooring.$CONFIG $P_I_DIR  $F_DTA_DIR position.moor ; fi

## Float initial position and restart float
  if [ $IFLOAT = 1 ] ;  then
    rapatrie $FLOATFIL  $P_I_DIR $F_DTA_DIR  init_float
  fi

## Ice initial condition only if no = 1
  if [ $ICE_INI = 1 -a $no -eq 1 ] ; then
    rapatrie $ICEINI  $P_I_DIR $F_INI_DIR $OPA_ICEINI
  fi

## Ice damping file 
  if [ $ICE_DMP = 1  ] ; then
    rapatrie $ICEDMP  $P_I_DIR $F_DTA_DIR $OPA_ICEDMP
  fi

## AABW damping
  if [ $AABW_DMP = 1 ] ; then
    rapatrie $WDMP  $P_I_DIR $F_MASK_DIR $OPA_WDMP
  fi


## [3.3] : Forcing fields
#  ======================
  getforcing  # this function read the namelist and look for the name of the
              # forcing files to be fetched. 
              # It also get the weight files and/or katabatic mask (if any)
              # the runoff files and SSS/SST restoring files (as appropriate)

## SHLAT2D file
  if [ $SHLAT2D = 1 ] ; then
    rapatrie "$SHLAT2DFILE" $P_I_DIR $F_DTA_DIR ${OPA_SHLAT2DFILE}
  fi

## Use a climatology of SSS damping in order to correct E-P
  if [ $WAFDMP = 1 ] ; then
    rapatrie $WAFDMP_CLIM $P_I_DIR $F_DTA_DIR $OPA_WAFDMP_CLIM
  fi

## Feed back term file (fbt)
  if [ $FBT = 1 ] ; then
    # nom can be computed to fit the year we need ...
    nom=$FBTDTA
    rapatrie $FBTDTA $P_I_DIR $F_DTA_DIR $nom
  fi

## Open boundaries files
  if [ $OBC = 1 ] ;  then
     tmp=$( LookInNamelist ln_obc_clim ) ; tmp=$( normalize $tmp ) 
     if [ $tmp = T ] ; then  # case of climatological OBC
       if [ $NORTHOBC != xxx ]; then
         rapatrie ${NORTHOBC}_TS.nc  $P_OBC_DIR  $F_OBC_DIR obcnorth_TS.nc
         rapatrie ${NORTHOBC}_U.nc   $P_OBC_DIR  $F_OBC_DIR obcnorth_U.nc
         rapatrie ${NORTHOBC}_V.nc   $P_OBC_DIR  $F_OBC_DIR obcnorth_V.nc
       fi
       if [ $SOUTHOBC != xxx ]; then
         rapatrie ${SOUTHOBC}_TS.nc  $P_OBC_DIR  $F_OBC_DIR obcsouth_TS.nc
         rapatrie ${SOUTHOBC}_U.nc   $P_OBC_DIR  $F_OBC_DIR obcsouth_U.nc
         rapatrie ${SOUTHOBC}_V.nc   $P_OBC_DIR  $F_OBC_DIR obcsouth_V.nc
       fi
       if [ $WESTOBC != xxx ]; then
         rapatrie ${WESTOBC}_TS.nc  $P_OBC_DIR  $F_OBC_DIR obcwest_TS.nc
         rapatrie ${WESTOBC}_U.nc   $P_OBC_DIR  $F_OBC_DIR obcwest_U.nc
         rapatrie ${WESTOBC}_V.nc   $P_OBC_DIR  $F_OBC_DIR obcwest_V.nc
       fi
       if [ $EASTOBC != xxx ]; then
         rapatrie ${EASTOBC}_TS.nc  $P_OBC_DIR  $F_OBC_DIR obceast_TS.nc
         rapatrie ${EASTOBC}_U.nc   $P_OBC_DIR  $F_OBC_DIR obceast_U.nc
         rapatrie ${EASTOBC}_V.nc   $P_OBC_DIR  $F_OBC_DIR obceast_V.nc
       fi
     else
       # determine the number of years to get according to the length of the run
       zn1=$(LookInNamelist nn_it000)
       zn2=$(LookInNamelist nn_itend)
       zrdt=$(LookInNamelist rn_rdt)
       zstpday=$( echo $zrdt | awk '{print 86400./$1 }' )
       znyear=$( echo $zn1 $zn2 $zstpday | awk '{ print int(( $2 - $1 +1)/$3/365+0.5 )}')
       if [ $znyear = 0 ] ; then znyear=1 ; fi
       zyearf=$(( year + znyear - 1 ))

       for zy in $(seq $year $zyearf) ; do
         # NORTH
         if [ $NORTHOBC != xxx ]; then
           rapatrie ${NORTHOBC}_TS_y${zy}m00.nc  $P_OBC_DIR  $F_OBC_DIR obc_north_TS_y${zy}m00.nc
           rapatrie ${NORTHOBC}_U_y${zy}m00.nc   $P_OBC_DIR  $F_OBC_DIR obc_north_U_y${zy}m00.nc
           rapatrie ${NORTHOBC}_V_y${zy}m00.nc   $P_OBC_DIR  $F_OBC_DIR obc_north_V_y${zy}m00.nc
         fi
         # SOUTH
         if [ $SOUTHOBC != xxx ]; then
           rapatrie ${SOUTHOBC}_TS_y${zy}m00.nc  $P_OBC_DIR  $F_OBC_DIR obc_south_TS_y${zy}m00.nc
           rapatrie ${SOUTHOBC}_U_y${zy}m00.nc   $P_OBC_DIR  $F_OBC_DIR obc_south_U_y${zy}m00.nc
           rapatrie ${SOUTHOBC}_V_y${zy}m00.nc   $P_OBC_DIR  $F_OBC_DIR obc_south_V_y${zy}m00.nc
         fi
         # WEST
         if [ $WESTOBC != xxx ]; then
           rapatrie ${WESTOBC}_TS_y${zy}m00.nc  $P_OBC_DIR  $F_OBC_DIR obc_west_TS_y${zy}m00.nc
           rapatrie ${WESTOBC}_U_y${zy}m00.nc   $P_OBC_DIR  $F_OBC_DIR obc_west_U_y${zy}m00.nc
           rapatrie ${WESTOBC}_V_y${zy}m00.nc   $P_OBC_DIR  $F_OBC_DIR obc_west_V_y${zy}m00.nc
         fi
         # EAST
         if [ $EASTOBC != xxx ]; then
           rapatrie ${EASTOBC}_TS_y${zy}m00.nc  $P_OBC_DIR  $F_OBC_DIR obc_east_TS_y${zy}m00.nc
           rapatrie ${EASTOBC}_U_y${zy}m00.nc   $P_OBC_DIR  $F_OBC_DIR obc_east_U_y${zy}m00.nc
           rapatrie ${EASTOBC}_V_y${zy}m00.nc   $P_OBC_DIR  $F_OBC_DIR obc_east_V_y${zy}m00.nc
         fi
       done
     fi
  fi

## [3.4] : restart files
#  ======================
echo ' [3.4] : restart files'
echo '  ===================='
prev_ext=$(( $no - 1 ))      # file extension of previous run

## model restarts
  if [ $no -eq  1 ] ; then
   echo Cold start, no restart to fetch ...
  else

##### O C E A N
###############
    # ** check if the dimg restart file are in the current dir 
    OCE_RST_IN=$(LookInNamelist cn_ocerst_in )
    OCE_RST_OUT=$(LookInNamelist cn_ocerst_out )
    ok=1
    #   Look for restart_xxxx.dimg.$prev_ext  xxx=1,$NB_NPROC
    for (( proc=1 ; proc <= $NB_NPROC ; proc++ )) ; do
      rest=$(printf "${OCE_RST_IN}_%04d.dimg.$prev_ext\n" $proc)
      if [ ! -f $rest ] ; then ok=0 ; fi
    done
    
    if [ $ok -eq 1 ] ; then
      echo All ocean restart files are here
    else
    # look for tar restart files
      for rest in $( lsrestart ${OCE_RST_OUT}_oce_v2.$prev_ext.tar. ) ; do
        rapatrie $rest   $P_R_DIR  $F_R_DIR  $rest
        tar xf $rest
      done
    fi

    # link  the xxx.dimg.$prev_ext files to xxx.dimg  (no extension).
    for rest in ${OCE_RST_IN}_????.dimg.$prev_ext ; do
      ln -sf $rest  ${rest%.$prev_ext} 
    done
   
##### I C E
###########
    if [ $ICE = 1 ] ; then
      # ** check if the dimg restart file are in the current dir 
      ICE_RST_IN=$(LookInNamelistIce cn_icerst_in )
      ICE_RST_OUT=$(LookInNamelistIce cn_icerst_out )
      ok=1
      #   Look for restart_ice_inxxxx.dimg.$prev_ext  xxx=1,$NB_NPROC
      for (( proc=1 ; proc <= $NB_NPROC ; proc++ )) ; do
        rest=$(printf "${ICE_RST_IN}_%04d.dimg.$prev_ext\n" $proc)
        if [ ! -f $rest ] ; then ok=0 ; fi
      done

      if [ $ok -eq 1 ] ; then
        echo All ice restart files are here
      else
        for rest in $( lsrestart ${ICE_RST_OUT}_v2.$prev_ext.tar. ) ; do
          rapatrie $rest   $P_R_DIR  $F_R_DIR  $rest
          tar xf $rest
        done
      fi

    # link  the xxx.dimg.$prev_ext files to xxx.dimg  (no extension).
      for rest in ${ICE_RST_IN}_????.dimg.$prev_ext ; do
        ln -sf $rest  ${rest%.$prev_ext} 
      done
    fi   # ICE = 1
  
##### T R A C E R S 
###################
    if [ $TRACER = 1 ] ; then
     rsttrc=$(LookInNamelistTrc lrsttr ) 

     # test if lrsttr is true or false. If true, then next line return a 1 status ($? )
     echo $rsttrc | grep -q false
     if [ $? = 1 ] ; then
        TRC_RST_IN=$(LookInNamelistTrc cn_trcrst_in )
        TRC_RST_OUT=$(LookInNamelistTrc cn_trcrst_out )
        # 1) look for restart.nc in P_R_DIR ( case of restart from nc file -- change number of proc, for example-- )
        if [ -f $P_R_DIR/${TRC_RST_IN}.nc.$prev_ext ] ; then
           ln -s $P_R_DIR/${TRC_RST_IN}.nc.$prev_ext $TRC_RST_IN.nc
        else
           ok=1
           for (( proc=1 ; proc <= $NB_NPROC ; proc++ )) ; do
             rest=$(printf "${TRC_RST_IN}_%04d.dimg.$prev_ext\n" $proc)
             if [ ! -f $rest ] ; then ok=0 ; fi
           done

           if [ $ok -eq 1 ] ; then
             echo All tracer restart files are here
           else

             # 2) standard procedure: look for tar files
             for rest in $(lsrestart  ${TRC_RST_OUT}_v2.$prev_ext.tar. )  ; do
               rapatrie_res $rest   $P_R_DIR  $F_R_DIR  restart.tar
               tar xf restart.tar
             done
           fi

           # restart files are archived with the correct TRC_RST_IN prefix !
           for rest in ${TRC_RST_IN}_????.dimg.$prev_ext ; do
             ln -sf $rest  ${rest%.$prev_ext} 
           done
        fi
     fi
    fi
    
##### T R D  M L D
##################
    if [ $TRDMLD = 1 ] ; then
      trdmldrst=$(LookInNamelist ln_trdmld_restart )

     # test if trdmldrst  is true or false. If true, then next line return a 1 status ($? )
     echo $trdmldrst | grep -q false
     if [ $? = 1 ] ; then
      TRD_RST_IN=$(LookInNamelist cn_trdrst_in )
      TRD_RST_OUT=$(LookInNamelist cn_trdrst_out )
      # ** check if the dimg restart file are in the current dir
      ok=1
      #   Look for restart_mld_xxxx.dimg.$prev_ext  xxx=1,$NB_NPROC
      for (( proc=1 ; proc <= $NB_NPROC ; proc++ )) ; do
        rest=$(printf "${TRD_RST_IN}_%04d.dimg.$prev_ext\n" $proc)
        if [ ! -f $rest ] ; then ok=0 ; fi
      done

      if [ $ok -eq 1 ] ; then
        echo All trdmld restart files are here
      else
       for rest in $( lsrestart ${TRD_RST_OUT}_v2.$prev_ext.tar.) ; do
        rapatrie $rest   $P_R_DIR  $F_R_DIR  $rest
        tar xf $rest
        done
      fi
      # remove the prev_ext from the name file
      for rest in ${TRD_RST_IN}_????.dimg.$prev_ext ; do
        ln -sf $rest ${rest%.$prev_ext}
      done
     fi
    fi

##### O B C 
###########
    if [ $OBC = 1 ] ; then
      rapatrie restart.obc.$prev_ext $P_R_DIR $F_R_DIR  restart.obc.$prev_ext
      cp -f restart.obc.$prev_ext restart.obc
    fi
  fi

## Float initial position and restart float
  if [ $RFLOAT = 1 -a  $IFLOAT = 1 ] ;  then
     rapatrie restart_float.$prev_ext $P_R_DIR  $F_R_DIR restart_float 
  fi
#

  pwd
#  ls -al
  touch donecopy

## (4) Run the code
## ----------------
echo '(4) Run the code'
echo '----------------'
date
runcode  $NB_NPROC ./opa
date
#--------------------------------------------------------
echo '(5) Post processing of the run'
echo '------------------------------'
cp layout.dat $P_S_DIR/
cp ocean.output* $P_S_DIR/
cp mpp.output_* $P_S_DIR/

echo '[5.1] check the status of the run'
echo '================================'
  # touch OK file if the run finished OK
  if [ "$(tail -20 ocean.output | grep AAAAAAAA)" = 'AAAAAAAA' ] ; then touch OK ; fi

  # gives the rights rx to go
  chmod -R go+rx $TMPDIR

  if [ ! -f OK ] ; then
    # The run crashed :( . Send back some infos on the CTL directory
    ext='ABORT'

   copy ocean.output $P_CTL_DIR/ocean.output.$$.$ext
   copy solver.stat $P_CTL_DIR/solver.stat.$$.$ext

   cp ocean.output $P_S_DIR/ocean.output.$$.$ext
   cp solver.stat  $P_S_DIR/solver.stat.$$.$ext
   cp namelist     $P_S_DIR/namelist_oce.$$.$ext
   cp namelist_ice $P_S_DIR/namelist_ice.$$.$ext

    if [ ! -f time.step ] ; then
      echo "Script stop now after copy of the ctl-file in the CTL directory"
      echo "No time-step are made by OPA, we stop before"   
      exit
    fi 
  fi 

echo '[5.2] Update the CONFIG_CASE.db file, if the run is OK'
echo '======================================================'
  if [ -f OK ]  ; then
     DIR=``
     echo "Run OK"
     nop1=$(( $no + 1 ))

     # add last date at the current line
     nline=$(wc $CONFIG_CASE.db | awk '{print $1}')

     # aammdd is the ndastp of the last day of the run ...
     # where can we get it ???? : in the ocean.output for sure !!
     aammdd=$( tail -10 ocean.output | grep 'run stop' | awk '{print $NF}'  )

    # Look for line in db file  with only 3 columns, keep this line in last
    last=$( cat $CONFIG_CASE.db | awk ' NF == 3 ' )
    ncol=$( echo $last | wc -w )  # ncol = 0 means no 3 colums line found

    if [ $ncol = 0 ] ; then 
      echo "db file is up to date with respect to date $aammdd"
    else
      sed -e "s/$last/$last\ $aammdd/" $CONFIG_CASE.db > tmpdb
      mv -f tmpdb $CONFIG_CASE.db
    fi 

    # add a new last line for the next run
    nstep_per_day=$(( 86400 / $rdt ))

   if [ $ndays = 185 ] ; then
     dif=$(( 180 * $nstep_per_day ))
   elif [ $ndays = 180 ] ; then
     dif=$(( 185 * $nstep_per_day ))
   else
  nit000=`tail -1 $CONFIG_CASE.db | awk '{print $2}' `
  nitend=`tail -1 $CONFIG_CASE.db | awk '{print $3}' `
#     dif=$(( 365 * $nstep_per_day ))
     dif=$((  $nitend - $nit000  + 1  ))
   fi

    nit000=$(( $nitend + 1 ))
    nitend=$(( $nitend + $dif ))

    # offer the opportunity to modify last line of db file on the fly: use newdb last line if any
    if [ -f newdb ] ; then
      line=$( cat newdb )
      echo $line >> $CONFIG_CASE.db
      \rm newdb
    else
      echo $nop1 $nit000 $nitend >> $CONFIG_CASE.db
    fi

    cat $CONFIG_CASE.db
    copy $CONFIG_CASE.db $P_CTL_DIR/

  else
    # Run is !NOT! OK : create $P_S_DIR/ABORT directory
    DIR=/ABORT
    ext=abort.$$
    mv $WORKDIR/${CONFIG_CASE}-DIMGPROC.$no $WORKDIR/${CONFIG_CASE}-DIMGPROC.$ext
    chkdir ${P_S_DIR}${DIR}
    chkdir ${P_R_DIR}${DIR}
  fi 

  if [ -f OK ] ;  then
     ext=$no
  fi

  if [ -f OK ] ; then

echo '[5.3] rename  the restart files'
echo '==============================='
echo making restart files
date

 # clean previous restart file from the WORKDIR/xxx-R directory
 clean_res $prev_ext

 # O C E A N 
 # *********
   OCE_RST_IN=$(LookInNamelist cn_ocerst_in )
   OCE_RST_OUT=$(LookInNamelist cn_ocerst_out )

  renamerst  $OCE_RST_IN $OCE_RST_OUT 

 # I C E 
 # *****
 if [ $ICE = 1 ] ; then
   ICE_RST_IN=$(LookInNamelistIce cn_icerst_in )
   ICE_RST_OUT=$(LookInNamelistIce cn_icerst_out )

   renamerst  $ICE_RST_IN $ICE_RST_OUT 
 fi

 # P A S S I V E   T R A C E R S
 # *****************************
 if [ $TRACER = 1 ] ; then
   TRC_RST_IN=$(LookInNamelistTrc cn_trcrst_in )
   TRC_RST_OUT=$(LookInNamelistTrc cn_trcrst_out )

   renamerst  $TRC_RST_IN $TRC_RST_OUT 
 fi

 # T R D  M L D 
 # ************
 if [ $TRDMLD = 1 ] ; then
   TRD_RST_IN=$(LookInNamelist cn_trdrst_in )
   TRD_RST_OUT=$(LookInNamelist cn_trdrst_out )

   renamerst  $TRD_RST_IN $TRD_RST_OUT 
 fi

 # build a script (to be submitted) for saving the individual dimg.$ext restart files into a set of tar files
 # and expatrie_res them.
 mksavrst  zsrst.$ext.ksh   

 # submit the save-restart script (monoproc). 
 submit ${P_CTL_DIR}/zsrst.$ext.ksh
 # when this script is finished ( asynchronously), there is a touch statement on file RST_DONE.$ext,
 # that need to be checked before cleaning. ( see interactive script clean_dimg_restart.ksh )

 #  Open Boundary Conditions
 # **************************
   if [ $OBC = 1 ] ; then
     mv restart.obc.output restart.obc.$ext
      expatrie_res  restart.obc.$ext $F_R_DIR restart.obc.$ext  $P_R_DIR
   fi

 fi      # RUN OK !
date

echo '[5.4] Move the dimgproc to another directory '
echo '==============================================='

  if [ $DAILY = 1 ] ; then 
    chkdir  $WORKDIR/${CONFIG_CASE}-DIMGPROC.$ext/DAILY
    echo '    [5.4.1] Move the daily dimgproc to another directory '

    for f in $(ls | grep daily ) ; do  # to avoid the use of * with too long lines
      mv $f $WORKDIR/${CONFIG_CASE}-DIMGPROC.$ext/DAILY
    done

    cp coordinates.nc $WORKDIR/${CONFIG_CASE}-DIMGPROC.$ext/DAILY
    cp namelistio_daily $WORKDIR/${CONFIG_CASE}-DIMGPROC.$ext/DAILY

    mkbuildnc_daily zrebuild_daily.$ext.ksh
    submit ${P_CTL_DIR}/zrebuild_daily.$ext.ksh
  fi

    echo '    [5.4.2] Prepare the rebuild of dimgproc files '
  cp coordinates.nc $WORKDIR/${CONFIG_CASE}-DIMGPROC.$ext
  cp namelistio $WORKDIR/${CONFIG_CASE}-DIMGPROC.$ext

  mkbuildnc zrebui.$ext.ksh
  submit ${P_CTL_DIR}/zrebui.$ext.ksh

  if [ $AGRIF = 1 ] ; then
    for idx in ${agrif_pref[@]} ; do
     cp ${idx}_coordinates.nc $WORKDIR/${CONFIG_CASE}-DIMGPROC.$ext
     cp ${idx}_namelistio $WORKDIR/${CONFIG_CASE}-DIMGPROC.$ext
     mkbuildnc zrebui.$ext.ksh_${idx} $idx
     submit ${P_CTL_DIR}/zrebui.$ext.ksh_${idx}
    done
  fi
   
echo '[5.5] Ready to re-submit the job NOW (to take place in the queue)'
echo '=================================================================='
 if [ -f OK ] ; then
    TESTSUB=$( wc $CONFIG_CASE.db | awk '{print $1}' )
    if [ $TESTSUB -le  $MAXSUB ] ; then
      submit  ${P_CTL_DIR}/${SUBMIT_SCRIPT}
#
      cat $TMPDIR/logsubmit
      echo $TMPDIR > prevtmp
    else
      echo Maximum auto re-submit reached.
    fi
 fi 

## check existence of directories. Create them if they do'nt exist
 chkdir $P_I_DIR
 chkdir $P_R_DIR
 chkdir $P_S_DIR

cd $TMPDIR
# retrieve no and extension
  if [ -f OK ] ;  then
     no=$( tail -1 $CONFIG_CASE.db | awk '{print $1}' )
     ext=$(( $no - 1 ))
  else
     ext=abort
  fi

echo '[5.6]  Put annex files in tarfile and send to DATA space '
echo '===== ==================================================='
   if [ -f *diaptr.nc      ] ; then   mv  *diaptr.nc diaptr.$ext.nc ; fi
   if [ -f *diagap.nc      ] ; then   mv  *diagap.nc diagap.$ext.nc ; fi
   mv solver.stat solver.stat.$ext
   mv ocean.output ocean.output.$ext
   mv time.step time.step.$ext
   cp namelist namelist_oce.$ext   # namelist is required later on for nmsh
   cp namelist_ice namelist_ice.$ext

   tar cvf tarfile.${CONFIG_CASE}_annex.$ext   \
           ocean.output.$ext time.step.$ext  \
           diaptr.$ext.nc diagap.$ext.nc solver.stat.$ext namelist_oce.$ext  namelist_ice.$ext

   chmod ugo+r  tarfile.${CONFIG_CASE}_annex.$ext
#  expatrie  tarfile.${CONFIG_CASE}_annex.$ext $F_S_DIR tarfile.${CONFIG_CASE}_annex.$ext


## [5.7] floats
# ================
   if [ $IFLOAT = 1 ] ; then
     expatrie trajec_float  $F_S_DIR ${CONFIG_CASE}_trajfloat.$ext
     expatrie restart_float.out  $F_R_DIR  restart_float.$ext
     if [ $RFLOAT = 0 ] ; then
       echo "Ne pas effacer ce fichier.MERCI." > $P_CTL_DIR/float
       chmod a-wx $P_CTL_DIR/float
     fi
   fi

## [5.8]  Save mooring files on $P_S_DIR
# =======================================
  if [ $MOOR = 1 ] ; then
     moorlist=$( ls *.mooring )
    if [ $moorlist != '' ] ; then
      for f in *.mooring  ; do 
        mv $f $f.$ext
      done

      # group them in tarfile
      tar cvf tarfile.${CONFIG_CASE}_mooring_$ext *.mooring.$ext
      chmod ugo+r tarfile.${CONFIG_CASE}_mooring_$ext
      expatrie tarfile.${CONFIG_CASE}_mooring_$ext $F_S_DIR tarfile.${CONFIG_CASE}_mooring_$ext
    else
      echo ' WARNING: mooring option but no mooring files'
    fi
  fi 

## [5.9] Mesmask files
# ====================
 nmsh=$(LookInNamelist nn_msh)
  if [ $nmsh = 1 ] ; then
    ./build_nc_iom mesh_mask
    expatrie mesh_mask.nc   $F_I_DIR  ${CONFIG}-${CASE}_mesh_mask.nc
  fi

  if [ $nmsh = 2 ] ; then
    ./build_nc_iom mesh
    ./build_nc_mask 
    expatrie mesh.nc  $F_I_DIR ${CONFIG}-${CASE}_mesh.nc
    expatrie mask.nc  $F_I_DIR ${CONFIG}-${CASE}_mask.nc
  fi

  if [ $nmsh = 6 ] ; then
   # mask file are buggued : _xxxx.dimg 
   # for ff in _*.dimg ; do mv $ff mask$ff ; done

   mkbuild_mesh_mask zmkmeshmask.ksh  # avoid _ in name as PBS does not like !
   submit ${P_CTL_DIR}/zmkmeshmask.ksh

  fi

## [5.10] Miscelaneous
# ====================
   cp ocean.output.$ext $P_S_DIR/
   cp solver.stat.$ext $P_S_DIR/
   cp namelist ${P_S_DIR}/namelist_oce.$ext
   cp namelist_ice ${P_S_DIR}/namelist_ice.$ext

## (6) Extra diagnostics launched from the P_machine
## --------------------------------------------------
 
## [6.1] Standard plots
## =====================
  cd $TMPDIR
  if [ $AUTOPLOT = 1 ] ; then
      sed -e "s/set CONFIG_CASE =/set CONFIG_CASE = $CONFIG_CASE/" $P_CTL_DIR/Plotdiagini > toto
      mv toto Plotdiag.sub
      sed -e "s/set DATE =/set DATE = ${tag}/" Plotdiag.sub > toto
      mv toto Plotdiag.sub
      sed -e "s/set EXT =/set EXT = ${ext}/" Plotdiag.sub > toto
      mv toto Plotdiag.sub
      #CINES \cp Plotdiag.sub $P_CTL_DIR/.
      chmod u+rwx Plotdiag.sub
      scp -p Plotdiag.sub $USER@$login_node:$P_CTL_DIR/.
 
    rcp Plotdiag.sub $AUSER@rhodes.idris.fr:Plotdiag.sub
    remsh rhodes.idris.fr -l $AUSER "/usr/local/bin/qsub Plotdiag.sub"
  fi 


#########################################################################
##                                END                                  ##
#########################################################################
