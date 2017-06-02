# $Id: nemo3.2_jade_pbs.ksh 56 2011-03-10 12:28:25Z molines $
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
  rcopy $P_CTL_DIR/includefile_jade.ksh includefile.ksh 

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
  fi

  if [ $AGRIF = 1 ] ; then
   init_agrif   # copy AGRIF_FixedGrids.in into.
                # initialize agrif_pref[0 n-1]
                # initialize timeref[1-n]

   for idx in ${agrif_pref[@]} ; do
    rcopy $P_CTL_DIR/${idx}_namelist.${CONFIG_CASE} ${idx}_namelist
    rcopy $P_CTL_DIR/${idx}_namelist_ice ${idx}_namelist_ice
    rcopy $P_CTL_DIR/${idx}_namelistio ${idx}_namelistio
   done
  fi


## (2) Set up the namelist for this run
## -------------------------------------

## exchange wildcards with the correc info from db
      no=$( tail -1 $CONFIG_CASE.db | awk '{print $1}' )
  nit000=$( tail -1 $CONFIG_CASE.db | awk '{print $2}' )
  nitend=$( tail -1 $CONFIG_CASE.db | awk '{print $3}' )

  if [ $no != 1 ] ; then
    restart=false
  else
    restart=true
  fi

  sed -e "s/NUMERO_DE_RUN/$no/" \
      -e "s/NIT000/$nit000/"    \
      -e "s/NITEND/$nitend/"    \
      -e "s/RESTART/$restart/"  namelist > namelist1
  \cp namelist1 namelist


## Agrif namelist update if any
  if [ $AGRIF = 1 ] ; then
  # for each agrif subgrif, determine nit000 and nitend according to values for G0
  # in the following arrays nit0[@] and nite[@] are the values to be replaced in the namelists
  # note that index 0 correspond to G0. index idx corresponds to G{idx}
    nit0[0]=$nit000
    nite[0]=$nitend

    for idx in ${agrif_pref[@]} ; do
      nit0[idx]=$(( (${nit0[idx-1]} -1)*${timeref[idx]} + 1 ))
      nite[idx]=$((  ${nit0[idx]} - 1 + ( ${nite[idx-1]} - ${nit0[idx-1]} +1 ) *${timeref[idx]} ))

      sed -e "s/NUMERO_DE_RUN/$no/"   \
          -e "s/NIT000/${nit0[idx]}/" \
          -e "s/NITEND/${nite[idx]}/" \
          -e "s/RESTART/$restart/"  ${idx}_namelist > ${idx}_namelist1

      \cp ${idx}_namelist1 ${idx}_namelist
    done
  fi


## check that the run period is a multiple of the dump period 
  rdt=$(LookInNamelist rn_rdt)
  nwri=$(LookInNamelist nn_write)

  var=` echo 1 | awk "{ a=$nitend ; b=$nit000 ; c=$nwri ; nenr=(a-b+1)/c ; print nenr}"`
  vernenr=` echo 1 | awk "{ a=$var; c=int(a); print c}"`

  if [ $vernenr -ne  $var ] ; then
    echo 'WARNING: the run length is not a multiple of the dump period ...'
    exit 1
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
  if [ $TRACER = 1 ] ; then rapatrie $CFC $P_I_DIR $F_DTA_DIR $OPA_CFC ; fi
  if [ $TRACER = 1 ] ; then rapatrie $CO2 $P_I_DIR $F_DTA_DIR $OPA_CO2 ; fi
  if [ $TRACER = 1 ] ; then rapatrie $C14 $P_I_DIR $F_DTA_DIR $OPA_C14 ; fi
# if [ $TRACER = 1 ] ; then rcopy $P_CTL_DIR/namelist_top ./ ; fi
# if [ $CFC = 1 ] ; then rcopy $P_CTL_DIR/namelist_cfc ./ ; fi
# if [ $C14 = 1 ] ; then rcopy $P_CTL_DIR/namelist_c14 ./ ; fi
# if [ $MYTRC = 1 ] ; then rcopy $P_CTL_DIR/namelist_mytrc ./ ; fi

#--------------------------------------
echo '(3) Look for input files'
echo '------------------------'

echo '[3.1] : configuration files'
echo '=========================='
## bathymetry
  if [ $( LookInNamelist nn_bathy ) = 1    ] ; then rapatrie $BATFILE_LEVEL $P_I_DIR $F_DTA_DIR $OPA_BATFILE_LEVEL ; fi
  if [ $( LookInNamelist ln_zco ) = .true. ] ; then rapatrie $BATFILE_METER $P_I_DIR $F_DTA_DIR $OPA_BATFILE_METER ; fi

## coordinates
  if [ $GR = 1 ] ; then
    rapatrie $COORDINATES $P_I_DIR $F_DTA_DIR $OPA_COORDINATES
  fi

## relaxation coefficient file
  if [ $REL = 1 ] ; then  rapatrie $RELAX $P_I_DIR $F_I_DIR $OPA_RELAX ; fi

## bottom friction file
  if [ $( LookInNamelist ln_bfr2d = .true. ] ; then  rapatrie $BFR $P_I_DIR $F_DTA_DIR $OPA_BFR ; fi

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

## Ocean color files
  if [ $RGB = 1 ] ; then
    rapatrie $RGB61  $P_I_DIR $F_DTA_DIR $OPA_RGB61
    rapatrie $CHLO   $P_I_DIR $F_DTA_DIR $OPA_CHLO
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

## Use a climatology of SSS damping in order to correct E-P
  if [ $WAFDMP = 1 ] ; then
    rapatrie $WAFDMP_CLIM $P_I_DIR $F_DTA_DIR $OPA_WAFDMP_CLIM
  fi

## [3.3] : Forcing fields
#  ======================

##  wind stress taux, tauy
  if [ $TAU = 1 ] ; then
     rapatrie $TAUX $P_I_DIR $F_DTA_DIR $OPA_TAUX
     rapatrie $TAUY $P_I_DIR $F_DTA_DIR $OPA_TAUY
  fi

## fluxes or parameters for bulks formulae
  if [ $FLX = 1 ] ; then
     rapatrie "$PRECIP" $P_I_DIR $F_DTA_DIR $OPA_PRECIP
     rapatrie "$LIM"    $P_I_DIR $F_DTA_DIR $OPA_LIM
     rapatrie "$TAIR"   $P_I_DIR $F_DTA_DIR $OPA_TAIR
     rapatrie "$WSPD"   $P_I_DIR $F_DTA_DIR $OPA_WSPD
     rapatrie "$RUNOFF" $P_I_DIR $F_DTA_DIR $OPA_RUNOFF
  fi

  yearp1=$(( year + 1 ))
  yearm1=$(( year - 1))

  if [ $FLXCORE = 1 ] ; then
    if [ $IA = 0 ] ; then
     rapatrie "$PRECIP" $P_I_DIR $P_FOR_DIR $OPA_PRECIP
     rapatrie_res "$WU10" $P_I_DIR $P_FOR_DIR $OPA_WU10
     rapatrie_res "$WV10" $P_I_DIR $P_FOR_DIR $OPA_WV10
     rapatrie_res "$WIND10" $P_I_DIR $P_FOR_DIR $OPA_WIND10
     rapatrie_res "$HUMIDITY" $P_I_DIR $P_FOR_DIR $OPA_HUMIDITY
     rapatrie "$SHORT_WAVE" $P_I_DIR $P_FOR_DIR $OPA_SHORT_WAVE
     rapatrie "$LONG_WAVE" $P_I_DIR $P_FOR_DIR $OPA_LONG_WAVE
     rapatrie_res "$TAIR" $P_I_DIR $P_FOR_DIR $OPA_TAIR
     rapatrie "$SNOW" $P_I_DIR $P_FOR_DIR $OPA_SNOW
    else
    for zyear in ${yearm1} ${year} ${yearp1}
     do
 if [ $ONLINE = 1 ]; then 
     ZPRECIP=${PRECIP}_y${zyear}.nc
     ZU10=${U10}_y${zyear}.nc
     ZV10=${V10}_y${zyear}.nc
     ZHUMIDITY=${HUMIDITY}_y${zyear}.nc
     ZSHORT_WAVE=${SHORT_WAVE}_y${zyear}.nc
     ZLONG_WAVE=${LONG_WAVE}_y${zyear}.nc
     ZTAIR=${TAIR}_y${zyear}.nc
     ZSNOW=${SNOW}_y${zyear}.nc
     ZCCOV=${CCOV}_y${zyear}.nc

     copyfor $P_FOR_DIR/$ZPRECIP ${OPA_PRECIP}_y${zyear}.nc

     copyfor $P_FOR_DIR/$ZPRECIP       ${OPA_PRECIP}_y${zyear}.nc
     copyfor $P_FOR_DIR/$ZSNOW           ${OPA_SNOW}_y${zyear}.nc
     copyfor $P_FOR_DIR/$ZLONG_WAVE     ${OPA_LONG_WAVE}_y${zyear}.nc
     copyfor $P_FOR_DIR/$ZSHORT_WAVE    ${OPA_SHORT_WAVE}_y${zyear}.nc
     copyfor $P_FOR_DIR/$ZU10             ${OPA_U10}_y${zyear}.nc
     copyfor $P_FOR_DIR/$ZV10             ${OPA_V10}_y${zyear}.nc
     copyfor $P_FOR_DIR/$ZTAIR         ${OPA_TAIR}_y${zyear}.nc
     copyfor $P_FOR_DIR/$ZHUMIDITY     ${OPA_HUMIDITY}_y${zyear}.nc  
     copyfor $P_FOR_DIR/$ZCCOV     ${OPA_CCOV}_y${zyear}.nc  
     # climato of radiative corrections
     copyfor $P_FOR_DIR/$COEFRADSW $OPA_COEFRADSW
     copyfor $P_FOR_DIR/$COEFRADLW $OPA_COEFRADLW
     copyfor $P_FOR_DIR/$COEFRADSW_LEAP $OPA_COEFRADSW_LEAP
     copyfor $P_FOR_DIR/$COEFRADLW_LEAP $OPA_COEFRADLW_LEAP
else
     ZPRECIP=${PRECIP}.${zyear}_14.nc
     ZU10=${U10}.${zyear}.nc
     ZV10=${V10}.${zyear}.nc
     ZHUMIDITY=${HUMIDITY}.${zyear}.nc
     ZSHORT_WAVE=${SHORT_WAVE}.${zyear}.nc
     ZLONG_WAVE=${LONG_WAVE}.${zyear}.nc
     ZTAIR=${TAIR}.${zyear}.nc
     ZSNOW=${SNOW}.${zyear}_14.nc
echo precip
if [ -f $P_I_DIR/FORCING/$ZPRECIP ]; then 
     copyfor $P_I_DIR/FORCING/$ZPRECIP precip_y${zyear}.nc
else
     rapatrie $ZPRECIP $P_I_DIR $F_FOR_DIR precip_y${zyear}.nc
fi
echo u10
if [ -f $P_I_DIR/FORCING/$ZU10 ]; then
     copyfor $P_I_DIR/FORCING/$ZU10 u10_y${zyear}.nc
else    
     rapatrie $ZU10 $P_I_DIR $F_FOR_DIR u10_y${zyear}.nc
fi     
echo v10
if [ -f $P_I_DIR/FORCING/$ZV10 ]; then
     copyfor $P_I_DIR/FORCING/$ZV10 v10_y${zyear}.nc
else
     rapatrie $ZV10 $P_I_DIR $F_FOR_DIR v10_y${zyear}.nc
fi
echo q10
if [ -f $P_I_DIR/FORCING/$ZHUMIDITY ]; then
     copyfor $P_I_DIR/FORCING/$ZHUMIDITY q10_y${zyear}.nc
else     
     rapatrie $ZHUMIDITY $P_I_DIR $F_FOR_DIR q10_y${zyear}.nc
fi
echo short_wave
if [ -f $P_I_DIR/FORCING/$ZSHORT_WAVE ]; then
     copyfor $P_I_DIR/FORCING/$ZSHORT_WAVE radsw_y${zyear}.nc
else
     rapatrie $ZSHORT_WAVE $P_I_DIR $F_FOR_DIR radsw_y${zyear}.nc
fi
echo long_wave
if [ -f $P_I_DIR/FORCING/$ZLONG_WAVE ]; then
     copyfor $P_I_DIR/FORCING/$ZLONG_WAVE radlw_y${zyear}.nc
else     
     rapatrie $ZLONG_WAVE $P_I_DIR $F_FOR_DIR radlw_y${zyear}.nc
fi
echo t10
if [ -f $P_I_DIR/FORCING/$ZTAIR ]; then
     copyfor $P_I_DIR/FORCING/$ZTAIR t10_y${zyear}.nc
else     
     rapatrie $ZTAIR $P_I_DIR $F_FOR_DIR t10_y${zyear}.nc
fi     
echo snow
if [ -f $P_I_DIR/FORCING/$ZSNOW ]; then
     copyfor $P_I_DIR/FORCING/$ZSNOW  snow_y${zyear}.nc
else
     rapatrie $ZSNOW $P_I_DIR $F_FOR_DIR snow_y${zyear}.nc
fi
fi      
      done
    fi
  fi

## On line interpolation weight files
  if [ $ONLINE = 1 ]; then
    rapatrie $WEIGHT_BILIN_ERA $P_WEI_DIR/ $F_WEI_DIR/ $OPA_WEIGHT_BILIN_ERA
    rapatrie $WEIGHT_BILIN_NCAR $P_WEI_DIR/ $F_WEI_DIR/ $OPA_WEIGHT_BILIN_NCAR
    rapatrie $WEIGHT_BICUB_ERA $P_WEI_DIR/ $F_WEI_DIR/ $OPA_WEIGHT_BICUB_ERA
  fi

## runoff file
  if [ $RUNOFF = 1 ]; then
    rapatrie $RUNOFFDTA $P_I_DIR $F_DTA_DIR $OPA_RUNOFF
  fi

## Katabatic wind files
  if [ $KATA = 1 ] ; then
    rapatrie "$KATADTA" $P_I_DIR $F_MASK_DIR ${OPA_KATA}
  fi

## DISTCOAST file
  if [ $DCOAST = 1 ] ; then
    rapatrie "$DISTCOAST" $P_I_DIR $F_DTA_DIR ${OPA_DISTCOAST}
  fi

## SSS files
  if [ $SSS = 1 ] ; then
    # nom can be computed to fit the year we need ...
   rapatrie $SSSDTA $P_I_DIR $F_DTA_DIR $OPA_SSSDTA
  fi

## SST files
  if [ $SST = 1 ] ; then
     # nom can be computed to fit the year we need ...
    nom=$SSTDTA
    rapatrie $SSTDTA $P_I_DIR $F_DTA_DIR $nom
  fi

## Feed back term file (fbt)
  if [ $FBT = 1 ] ; then
    # nom can be computed to fit the year we need ...
    nom=$FBTDTA
    rapatrie $FBTDTA $P_I_DIR $F_DTA_DIR $nom
  fi

## Open boundaries files ( do not rapatrie for agrif --> use core_rapatrie instead of rapatrie
  if [ $OBC = 1 ] ;  then
     if [ ${NORTOBCT} != xxx ]; then
     core_rapatrie ${NORTOBCT}_y${year}m00.nc*  $P_I_DIR  $F_OBC_DIR obc_north_TS_y${year}m00.nc
     core_rapatrie ${NORTOBCU}_y${year}m00.nc*  $P_I_DIR  $F_OBC_DIR obc_north_U_y${year}m00.nc
     core_rapatrie ${NORTOBCV}_y${year}m00.nc*  $P_I_DIR  $F_OBC_DIR obc_north_V_y${year}m00.nc
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
    while [ proc -le $NB_NPROC ] ; do
      rest=$(printf "${OCE_RST_IN}_%04d.dimg.$prev_ext\n" $proc)
      if [ ! -f $rest ] ; then ok=0 ; fi
      proc=$(( proc + 1 ))
    done
    
    if [ $ok = 1 ] ; then
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
     no=$(( $no + 1 ))

     # add last date at the current line
     nline=$(wc $CONFIG_CASE.db | awk '{print $1}')

     # aammdd is the ndastp of the last day of the run ...
     # where can we get it ???? : in the ocean.output for sure !!
     aammdd=$( tail -10 ocean.output | grep 'run stop' | awk '{print $NF}'  )

    ncol=0
    while [ $ncol -eq 0 ] ; do
      last=$( tail +$nline $CONFIG_CASE.db )
      ncol=$( echo $last | wc | awk '{ print $2 }' )
      nline=$(( $nline - 1 ))
    done

    if [ $ncol -eq  3 ] ; then
      sed -e "s/$last/$last\ $aammdd/" $CONFIG_CASE.db > tmpdb
      mv -f tmpdb $CONFIG_CASE.db
    else
      echo "fichier db deja a jour de la date $ncol"
    fi 

    # add a new last line for the next run
    nstep_per_day=$(( 86400 / $rdt ))

#   if [ $ndays = 185 ] ; then
#     dif=$(( 180 * $nstep_per_day ))
#   elif [ $ndays = 180 ] ; then
#     dif=$(( 185 * $nstep_per_day ))
#   else
  nit000=`tail -1 $CONFIG_CASE.db | awk '{print $2}' `
  nitend=`tail -1 $CONFIG_CASE.db | awk '{print $3}' `
#     dif=$(( 365 * $nstep_per_day ))
     dif=$((  $nitend - $nit000  + 1  ))
#   fi

    nit000=$(( $nitend + 1 ))
    nitend=$(( $nitend + $dif ))

    if [ -f newdb ] ; then
      line=$( cat newdb )
      echo $line >> $CONFIG_CASE.db
    else
      echo $no $nit000 $nitend >> $CONFIG_CASE.db
    fi

    cat $CONFIG_CASE.db
    copy $CONFIG_CASE.db $P_CTL_DIR/

  else
    # Run is !NOT! OK : create $P_S_DIR/ABORT directory
    DIR=/ABORT
    ext=abort
    if [ ! -d ${P_S_DIR}${DIR} ] ; then mkdir ${P_S_DIR}${DIR} ; fi
    if [ ! -d ${P_R_DIR}${DIR} ] ; then mkdir ${P_R_DIR}${DIR} ; fi
  fi 

  if [ -f OK ] ;  then
     ext=$(( $no - 1 ))
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
 mksavrst  zsaverst.$ext.ksh   

 # submit the save-restart script (monoproc). 
 submit ${P_CTL_DIR}/zsaverst.$ext.ksh
 # when this script script is finished ( asynchronously), there is a touch statement on file RST_DONE.$ext,
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

  chkdir  $WORKDIR/${CONFIG_CASE}-DIMGPROC.$ext
  if [ $DAILY = 1 ] ; then 
    chkdir  $WORKDIR/${CONFIG_CASE}-DIMGPROC.$ext/DAILY
    echo '    [5.4.1] Move the daily dimgproc to another directory '

    cd $WORKDIR/${CONFIG_CASE}-DIMGPROC.$ext
    for f in $(ls | grep daily ) ; do  # to avoid the use of * with too long lines
      mv $f $WORKDIR/${CONFIG_CASE}-DIMGPROC.$ext/DAILY
    done
    cd $TMPDIR

    cp coordinates.nc $WORKDIR/${CONFIG_CASE}-DIMGPROC.$ext/DAILY
    cp namelistio_daily $WORKDIR/${CONFIG_CASE}-DIMGPROC.$ext/DAILY

    mkbuildnc_daily zrebuild_daily.ksh.$ext
    submit ${P_CTL_DIR}/zrebuild_daily.ksh.$ext
  fi


  cp coordinates.nc $WORKDIR/${CONFIG_CASE}-DIMGPROC.$ext
  cp namelistio $WORKDIR/${CONFIG_CASE}-DIMGPROC.$ext

  mkbuildnc zrebuild.ksh.$ext
  submit ${P_CTL_DIR}/zrebuild.ksh.$ext

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
      submit  ${P_CTL_DIR}/${CONFIG_CASE}_jade.ksh  
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

  if [ $nmsh = 3 ] ; then
    # mask file are buggued : _xxxx.dimg 
    for ff in _*.dimg ; do mv $ff mask$ff ; done

    ./build_nc_iom mesh_hgr
    ./build_nc_iom mesh_zgr
    ./build_nc_mask

    expatrie mesh_hgr.nc  $F_I_DIR ${CONFIG}-${CASE}_mesh_hgr.nc
    expatrie mesh_zgr.nc  $F_I_DIR ${CONFIG}-${CASE}_mesh_zgr.nc
    expatrie mask.nc  $F_I_DIR ${CONFIG}-${CASE}_byte_mask.nc
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
