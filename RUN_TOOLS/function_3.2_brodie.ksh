#!/bin/ksh

# $Id: function_3.2_jade.ksh 57 2011-03-10 14:28:15Z molines $
######################################################################################
#    KSH script functions used below:
######################################################################################
# copy()   
# rcopy() 
# copyfor() 
# rapatrie()     
# rapatrie_res() 
# expatrie()
# expatrie_res()
# chkdir() 
# chkdirg()
# chkfile()
# LookInNamelist()
# LookInNamelistTrc()
# LookInNamelistIce()
# number_of_file()
# mkordre()
# submit() 
# runcode()
# lsrestart() 
# renamerst()
# mktarrst()
# mksavrst() 
# mkbuildnc()
# mkbuildnc_daily()
# initagrif() 
#-------------------------------------------------------
# MACHINE DEPENDANT functions
# copy : a wrap up for scp or cp 
copy()    { cp $1 $HOME/$2 ; }
rcopy()   { cp $HOME/$1 $2 ; }
copyfor() { ln -sf $1 $2 ;}
# ---

#---
# MACHINE/SYSTEM INDEPENDANT functions
# The core of the rapatrie function.
core_rapatrie() {
     echo ${1:-none} | grep -iv none && \
    { if [ -f $4 ] ; then      # file already there
        echo $4 found
      elif [ -f $2/$1 ] ; then #copy from local file system
        cp $2/$1 $4
      else                     # copy from remote file system
        rcopy $3/$1 $4  ||  { echo $1 not found anywhere &&  exit ; }
      fi  ; }
           }
# ---
# rapatrie is a shell function which tries to copy $1 on the local dir with the name $4
#         if the name is not defined or is none, it just skip
#         it then first tries to find the file on disk directory $2
#         if it fails it tries to mfget from $3 directory 
#         if it is not found anywhere, stop the script and issue a message
#         login_node is set in includefile.ksh
rapatrie() { 
      core_rapatrie $1 $2 $3 $4
      if [ $AGRIF = 1 ] ; then
         for idx in ${agrif_pref[@]} ; do
           core_rapatrie ${idx}_$1 $2 $3 ${idx}_$4
         done
      fi
           }
# ---

# rapatrie_res is normaly used for restart. On jade it is just the same as rapatrie
#  
rapatrie_res() { 
      rapatrie  $1 $2 $3 $4
               }
# ---

# expatrie  file $1 on the directory $2, with filename $3
expatrie() {
      copy $1 $2/$3
           }
# --- 

# expatrie_res  file $1 on the directory $2, with filename $3; copy $1 on local disk $4
expatrie_res() {
      cp $1 $4/$3 
               }
# --- 

# remove old restart files from the working directory
# clean_res() { \rm  $P_R_DIR/*.$1.tar.* ; }
 clean_res() {
      echo no restart cleaning on jade 
             }
# --- 

# check existence and eventually create directory
chkdir() { if [ ! -d $1 ] ; then mkdir $1 ; fi ; }
# --- 

# chkdirg  : Usage: chkdirg DATA space_directory
#    check the existence of a directory on DATA space . Create it if not present
chkdirg() { rsh gaya "if [ ! -d $1 ] ; then mkdir $1 ; fi " ; }
# --- 

# chkfile  : Usage: chkfile   dir_of_file/file  #CINES ajout 22-07-08
#    check the existence of a directory on dir_of_file on login_node .
chkfile() { if [ ! -f $1 ] ; then exit 1 ; fi  ; }
# --- 

# LookInNamelist returns the value of a variable in the namelist
#         example: aht0=$(LookInNamelist aht0 )
LookInNamelist()    { eval grep -e $1 namelist | tr -d \' | tr -d \"  | sed -e 's/=/  = /' | awk ' {if ( $1 == str ) print $3 }' str=$1 ; }
LookInNamelistTrc() { eval grep -e $1 namelist_top  | tr -d \' | tr -d \"  | sed -e 's/=/  = /' | awk ' {if ( $1 == str ) print $3 }' str=$1 ; }
LookInNamelistIce() { eval grep -e $1 namelist_ice  | tr -d \' | tr -d \"  | sed -e 's/=/  = /' | awk ' {if ( $1 == str ) print $3 }' str=$1 ; }
# --- 

# Give the number of file containing $1 in its name
number_of_file() { ls -1 *$1* 2> /dev/null | wc -l  ; }
# ---

# mkordre
mkordre() { rsh gaya "cd ${CONFIG}/${CONFIG_CASE}-S/ ; ~/bin/mkordre " ; }
# ---

# function for submitting jobs; modified for JADE
submit() { cd ${P_CTL_DIR}; pwd; qsub $1 > $TMPDIR/logsubmit ;}
# ---

# function for running OPA : it takes the number of procs and name of program as argument
runcode() {
         export F_PROGINF=detail
         ./$2
         export F_PROGINF=
          }

#--------------------------------------------------------------------------------------
# Restart file management
# function to perform a ls on the remote restart directory
lsrestart() { 
       rsh  gaya ls $F_R_DIR/ | grep $1
            }
# ---
# function for renaming restart files according to current run
#       example : renamerst restart_ice_in restart_ice 
renamerst() {
            echo renaming restartfile to ${1}_xxxx.dimg.$ext
 for rest in ${CONFIG_CASE}*${2}_????.dimg ; do
     rest_in=${1}_$( tmp=${rest##*_} ; echo ${tmp%.dimg} ).dimg
     mv $rest $rest_in.$ext
  done
            }
# ---
# function for making restart tar file of about 1 Gb. Take 3 arguments: root_name_in root_name_out option
#        example : mktarrst restart restart _oce.v2 --> restart_oce.v2.1.tar.ext
mktarrst() {  echo making tar restart file for ${1}$3

 # ... build tar files of size < MAXTARSIZ (in bytes)
 MAXTARSIZ=1000000000
 set +x   # avoid very long list of file

 ls -l  ${1}_????.dimg.$ext | \
 awk ' BEGIN {s=0 ; arch=0 ; list= ""}    \
       { if ( s < maxtar ) { s=s+$5 ; list=list " " $8 } \
         else { arch=arch+1 ;
         cmd="tar cf " dir name option "." ext ".tar." arch " " list ; \
          system (cmd ); s= $5; list=$8 } \
       } \
       END { arch=arch+1 ; cmd="tar cf  " dir name option "." ext ".tar." arch " " list ; \
             system (cmd ) } ' maxtar=$MAXTARSIZ ext=$ext name=$2 option=$3 dir=${P_R_DIR}/

 echo " end of restart tarfiles ." ; set -x ; }
# ---

# function for building script to save restart: 
mksavrst() {
cat << eof >> $1
#PBS -S /usr/bin/ksh
#PBS -N ${1%%.*}
#PBS -o ${1%%.*}.o%s
#PBS -j o
#PBS -l cputim_prc=01:00:00  # temps max (HH:MM:SS)/ process
#PBS -l cputim_job=01:00:00  # temps max (HH:MM:SS)/job
#PBS -l memsz_job=1gb

 cd $TMPDIR
 . ./includefile.ksh
 . \$HOME/RUN_TOOLS/function_3.2_brodie.ksh
 ext=$ext
 # O C E A N
 # *********
   OCE_RST_IN=\$(LookInNamelist cn_ocerst_in )
   OCE_RST_OUT=\$(LookInNamelist cn_ocerst_out )

  mktarrst \$OCE_RST_IN \$OCE_RST_OUT _oce_v2

 # send them on DATA space
  for f in \${OCE_RST_OUT}_oce_v2.\$ext.tar.*  ; do
     expatrie_res \$f \$F_R_DIR \$f \$P_R_DIR
  done

 # I C E
 # *****
 if [ \$ICE = 1 ] ; then
   ICE_RST_IN=\$(LookInNamelistIce cn_icerst_in )
   ICE_RST_OUT=\$(LookInNamelistIce cn_icerst_out )

   mktarrst \$ICE_RST_IN \$ICE_RST_OUT _v2

 # send them on DATA space
  for f in \${ICE_RST_OUT}_v2.\$ext.tar.*  ; do
     expatrie_res \$f \$F_R_DIR \$f \$P_R_DIR
  done
 fi

 # P A S S I V E   T R A C E R S
 # *****************************
 if [ \$TRACER = 1 ] ; then
   TRC_RST_IN=\$(LookInNamelistTrc cn_trcrst_in )
   TRC_RST_OUT=\$(LookInNamelistTrc cn_trcrst_out )

   mktarrst \$TRC_RST_IN \$TRC_RST_OUT _v2

  # send them on  DATA space
   for f in \${TRC_RST_OUT}_v2.\$ext.tar.*  ; do
     expatrie_res \$f \$F_R_DIR \$f \$P_R_DIR
   done
 fi

 # T R D  M L D
 # ************
 if [ \$TRDMLD = 1 ] ; then
   TRD_RST_IN=\$(LookInNamelist cn_trdrst_in )
   TRD_RST_OUT=\$(LookInNamelist cn_trdrst_out )

   mktarrst \$TRD_RST_IN \$TRD_RST_OUT _v2

   # send them on  DATA space
    for f in \${TRD_RST_OUT}_v2.\$ext.tar.*  ; do
      expatrie_res \$f \$F_R_DIR \$f \$P_R_DIR
    done
 fi
 touch RST_DONE.\$ext
eof
  # copy the script to P_CTL_DIR from where it will be launched by submit
  copy $1 $P_CTL_DIR
              }
# --- 
# function for recombining nc file : it creates a script to be launched by submit
mkbuildnc() {
# if called with 2 arguments, this is an agrif call, second argument is the grid number
ln_dimgnnn=$( LookInNamelist ln_dimgnnn )
cat << eof > $1
#PBS -S /usr/bin/ksh
#PBS -N ${1%%.*}
#PBS -o ${1%%.*}.o%s
#PBS -j o
#PBS -l cputim_prc=01:00:00  # temps max (HH:MM:SS)/ process
#PBS -l cputim_job=01:00:00  # temps max (HH:MM:SS)/job
#PBS -l memsz_job=1gb

set -x
zagrif=$2
tagpos=3
ln_dimgnnn=$ln_dimgnnn

if [ \$ln_dimgnnn = .true. ] ; then
  suff=.0001
else
  suff=
fi

if [ \$zagrif ] ; then
   zagrif=${2}_
   tagpos=4
fi

cd  \$WORKDIR/${CONFIG_CASE}-DIMGPROC.$ext
cp \$WORKDIR/bin/build_nc .

taglist=''
  for f in \${zagrif}*2D_*dimgproc${suf} ; do
    tmp=\${f%.dimgproc} ; tag=\$( echo \$tmp | awk -F_ '{ print \$i }' i=\$tagpos )
    if [ ! -f \${zagrif}${CONFIG_CASE}_\${tag}_gridT.nc ] ; then
      taglist="\$taglist \$tag"
    fi
   done

echo \$taglist
./build_nc \$taglist
eof

  # copy the script to P_CTL_DIR from where it will be launched by submit
  copy $1 $P_CTL_DIR
             }
# ---

# function for recombining nc file : it creates a script to be launched by submit
mkbuildnc_daily() {
cat << eof > $1
#PBS -S /usr/bin/ksh
#PBS -N ${1%%.*}
#PBS -o ${1%%.*}.o%s
#PBS -j o
#PBS -l cputim_prc=01:00:00  # temps max (HH:MM:SS)/ process
#PBS -l cputim_job=01:00:00  # temps max (HH:MM:SS)/job
#PBS -l memsz_job=1gb

set -x
. \$HOME/RUN_TOOLS/function_3.2_jade.ksh
cd  \$WORKDIR/${CONFIG_CASE}-DIMGPROC.$ext/DAILY
cp \$WORKDIR/bin/build_nc_daily .

taglist=''
tmp=\$( ls -1 | grep Tdaily | tail -1 )
lastdom=\${tmp##*.}
for f in *Tdaily*.\$lastdom ; do
  tmp=\${f%.dimgproc.\$lastdom} ; tag=\$( echo \$tmp | awk -F_ '{ print \$3}' )
  if [ ! -f ${CONFIG_CASE}ker_\${tag}_gridT.nc ] ; then
  taglist="\$taglist \$tag"
  fi
done

echo \$taglist
./build_nc_daily \$taglist
eof

  # copy the script to P_CTL_DIR from where it will be launched by submit
  copy $1 $P_CTL_DIR
             }

# ---

#--------------------------------------------------------------------------------------
# AGRIF STUFF
# Init Agrif : set usefull variables in case of AGRIF run ( ARRAY agrif_pref )
initagrif() { 
   nbr_child=0 ;   
   rcopy $P_CTL_DIR/AGRIF_FixedGrids.in AGRIF_FixedGrids.in
   nbr_child=$(cat AGRIF_FixedGrids.in | awk 'BEGIN{s=0} NF==1 {s=s+$1} END{ print s}')
   n=1 ; lst=''
   while (( n <=  nbr_child )) ; do
     lst="$lst ${n}"
      n=$(( n + 1 ))
   done
   set -A agrif_pref $lst 

   refinement=$( cat AGRIF_FixedGrids.in | awk ' NF==7 { print $7}' )
   set -A timeref '  ' $refinement
            } 

# ---
######################################################################################
