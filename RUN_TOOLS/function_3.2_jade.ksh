#!/bin/ksh

# $Id: function_3.2_jade.ksh 1486 2014-09-19 13:19:47Z molines $
######################################################################################
#    KSH script functions used below:
######################################################################################
# copy()
# rcopy()
# copyfor()
# rapatrie()
# core_rapatrie()
# rapatrie_res()
# expatrie()
# expatrie_res()
# clean_res()
# chkdir()
# chkdirg()
# chkfile()
# mkordre()
# submit() 
# runcode()
# runcodescalasca()
# lsrestart()
# mk_batch_hdr
#-------------------------------------------------------
# Source machine independant functions (already done in the script)
# . ./function_3.2_all.ksh
# MACHINE DEPENDANT functions
# copy : a wrap up for scp or cp 
copy()    { \cp $1 $2 ; }
rcopy()   { \cp $1 $2 ; }
copyfor() { ln -sf $1 $2 ;}
# ---

# rapatrie is a shell function which tries to copy $1 on the local dir with the name $4
#         if the name is not defined or is none, it just skip
#         it then first tries to find the file on disk directory $2
#         if it fails it tries to find it from $3 directory on the storage machine
#         if it is not found anywhere, stop the script and issue a message
rapatrie() {
      core_rapatrie $1 $2 $3 $4
      if [ $AGRIF = 1 ] ; then
         for idx in ${agrif_pref[@]} ; do
           core_rapatrie ${idx}_$1 $2 $3 ${idx}_$4
         done
      fi
           }
# ---

# The core of the rapatrie function.: called by rapatrie which also deals with agrif
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

# rapatrie_res is normaly used for restart. On jade it is just the same as rapatrie
#  
rapatrie_res() { 
      rapatrie  $1 $2 $3 $4
               }
# ---

# expatrie  file $1 on the directory $2, with filename $3
expatrie() {
#      copy $1 $2/$3
       cp $1 $2/$3
           }
# ---

# expatrie_res  file $1 on the directory $2, with filename $3; copy $1 on local disk $4
expatrie_res() {
      if [ ! -f $4/$3 ] ; then 
         cp $1 $4/$3 
      else
         echo $3 already exists in $4
      fi
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
chkdirg() { if [ ! -d $1 ] ; then mkdir $1 ; fi  ; }
# --- 

# chkfile  : Usage: chkfile   dir_of_file/file  #CINES ajout 22-07-08
#    check the existence of a directory on dir_of_file on login_node .
chkfile() { if [ ! -f $1 ] ; then exit 1 ; fi  ; }
# --- 

# mkordre
mkordre() { cd ${CONFIG}/${CONFIG_CASE}-S/ ; ~/bin/mkordre  ; }
# ---

# function for submitting jobs; modified for JADE
submit() { ssh $USER@$login_node "cd ${P_CTL_DIR}; pwd; qsub $1 > $TMPDIR/logsubmit" ;}
#submit() { cd ${P_CTL_DIR}; pwd; qsub $1 > $TMPDIR/logsubmit ;}
# ---

# function that source the .bashrc file if necessary (depends on the batch scheduler)
srcbash() {  echo > /dev/null  ; }  # this funcion is useless on jade
# ---

# function for running OPA : it takes the number of procs and name of program as argument
runcode() {
        mpiexec_mpt -n $*
#         mpirun -np  $*
          }
# ---

# function for running mpp program with node binding. On jade, if PLACEMENT is yes, node binding is
# automatic, else .... [ to be checked ]
runcode_u () {
        mpiexec_mpt -n $*
#         mpirun -np  $*
          }
# ---


# ---
# function for running OPA ans XIOS : it takes the number of procs and name of programs as argument
#    runcode_mpmd  nproc1 prog1    nproc2 prog2   ... nprocn progn
runcode_mpmd() {

#        MIX=0
#        if [ $1 = -mix ] ; then MIX=1 ; core_per_node=$2 ; shift 2 ; fi
#        narg=$#
#        npair=$(( narg / 2 ))
#        nrest=$(( narg % 2 ))

#        if [ $nrest != 0 ] ; then
#          echo " You should give an even number of arguments : pair \( nproc  exec \)"
#          return 1
#        fi

#        echo "MIX = $MIX"
#        rm -f ./zapp.conf

#        if [ $MIX = 1 ] ; then
#          if [ $npair != 2 ] ; then
#            echo -mix option is valid only for 2 programs
#            return 1
#          fi
#          ncores=$(( $1 + $3 ))
#          nnodes=$(( ncores / core_per_node ))
#          nserv_per_node=$(( $3 / nnodes ))
#          for nodes in $(seq 1 $nnodes) ; do
#            echo $(( core_per_node - nserv_per_node ))  $2 >> zapp.conf
#            echo $nserv_per_node  $4 >> zapp.conf
#          done
#          ntot=$(cat zapp.conf | awk ' BEGIN{s=0}{ s=s+$1} END{ print s}')
#        else
#          for ipair in $(seq 1 $npair) ; do
#             echo $1 $2 >> zapp.conf
#             shift 2
#          done
#        fi
      mpiexec_mpt -show  -np $1 $2 : -np $3 $4 
      params=$( mpiexec_mpt -show  -np $1 $2 : -np $3 $4  2>&1 | grep mpirun | awk '{print $NF}' )
      echo $params
      cat $params
      mpiexec_mpt -np $1 $2 : -np $3 $4 
#     mpirun  -np $1 $2 : -np $3 $4 
               }


# function for running OPA with scalasca : it takes the number of procs and name of program as argument
runcodescalasca() {
         scan -t mpiexec_mpt -n $1 $2
          }
# ---
# ---
# function save_nc : dummy function on jade
save_nc () {
     echo nothing to do !
           }
# ---

# save_arch_file   file  archive_dir  # used when XIOS in use
save_arch_file () {
      echo nothing to do on jade ! > /dev/null
                  }
# ---

#--------------------------------------------------------------------------------------
# Restart file management
# function to perform a ls on the remote restart directory
lsrestart() { 
       if [ -f $P_R_DIR/$1.1 ] ; then # restarts we are looking for are on P_R_DIR
          ls $P_R_DIR/ | grep $1
       else                           # look on F_R_DIR
          ssh $USER@$login_node ls $F_R_DIR/ | grep $1
       fi
            }
# ---

#-----------------------------------------------------------------------------------------
# Make batch header for submitted scripts
# mk_batch_hdr  --name name --wallclock wallclock --account account --nodes nodes --cores cores --par --seq --option "options line" --help
mk_batch_hdr() {
   # init variables for jade
   name=''
   account=''
   wallclock=01:00:00
   nodes=1
   cores=1
   jobtype='serial'
   cluster='nhm'
   queue=''

   mk_batch_hdr_core $@

# build JADE batch header according to options
  if [ $jobtype = 'serial' ] ; then
     cat << eof 
#!/bin/bash
#PBS -N $name
#PBS -l select=$nodes:ncpus=8:mpiprocs=8
#PBS -l cluster=$cluster
#PBS -l walltime=$wallclock
#PBS -M $MAILTO
#PBS -mb -me
eof
  else
     cat << eof 
#!/bin/bash
#PBS -N $name
#PBS -l select=$nodes:ncpus=8:mpiprocs=8
#PBS -l cluster=$cluster
#PBS -l walltime=$wallclock
#PBS -M $MAILTO
#PBS -mb -me
#PBS -v NB_NODES=${nodes},NPROC=${cores}
eof
  fi

               }
######################################################################################
