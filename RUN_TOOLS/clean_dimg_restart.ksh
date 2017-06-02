#!/bin/bash
# This script is intended to be used after the restart tar files are made asyncroneously
# after a run. In this case, the RST_DONE.ext file exists. So that respective dimg files
# are candidates to be removed. To be elected, we check the .db file to avoid erasing
# usefull restart files [ by the way, as soon as tar files are OK, it is not a 
# catastophic event if by mistakes they are erased ].
#dbg=echo
dbg=

# $Id: clean_dimg_restart.ksh 1670 2015-11-10 10:27:50Z bessiere6l $

usage() {
     echo usage: $( basename $0 )  [-h] [-f] 
     echo "     -h : this help message "
     echo "     -f : force erase even if not in TMPDIR_CONFIG-CASE"
     echo "     -d : debug mode "
     echo "    without argument, check if the script is launched from "
     echo "    a TMPDIR_CONFIG-CASE directory. Then scan an existing "
     echo "    CONFIG-CASE.db file to look for obsolete restart files"
     echo "    Look for RST_DONE.ext file. If this file exists, and ext"
     echo "    corresponds to obsolete restarts, erase them, else do nothing"
     exit 0
        }

# check options 
forced=''
while getopts :hfd opt ; do
  case $opt in
   (h) usage ;;
   (d) set -vx ;;
   (f) forced=1 ;;
  esac
done


here=$(basename $PWD) 
if [ ! $forced ] ; then
   # check directory 
   echo $here | grep -q -e '^TMPDIR_'

   if [ $? = 1 ] ; then 
     echo you must be in a TMPDIR_CONFIG-CASE directory or use -f option
     echo you are in $here
     usage
   fi
   # look for CONFIG  and CASE name from the local directory
   CONFCASE=${here#TMPDIR_}

else   # -f option
   # ask for CONFIG-CASE
   read CONFCASE?'GIVE CONFIG-CASE name : '
fi

   CONFIG=${CONFCASE%-*}
   CASE=${CONFCASE#*-}
   echo CONFIG = $CONFIG
   echo CASE   = $CASE

   # Look for db file 
   if [ -f $CONFCASE.db ] ; then
      echo $CONFCASE.db found
   else
     echo E R R O R :
     echo missing  $CONFCASE.db in $here
     echo "Cannot determine files to be erased :("
     exit 1
   fi

   # determine the last file to be erased (nmax)
   no=$( tail -1 $CONFCASE.db | awk '{print $1}' )
   nmax=$(( no - 2 )) 
   
   # check if ensemble run 
   ln_ens=0
   ls RST_DONE.0*.* > /dev/null 2>&1
   if [ $? = 0 ] ; then ln_ens=1 ; fi

   # check if use restart directories ( check the last that may exist)
   rstdirs=0
   if [ -d ../${CONFCASE}-RST.$nmax ] ; then rstdirs=1 ; fi

   # loop on the candidate segments for erasing 
   for nn in $( seq 1 $nmax ) ; do
    # case of a standard run ( no ensemble )
    rstdone='RST_DONE'.$nn
    if [ -f $rstdone ] ; then 
      echo Cleaning DIMG/nc restart files extension $nn ...

      case $rstdirs in
      ( 0 )
         for f in  *.dimg.$nn ; do
           if [ -f $f ] ; then $dbg rm -f $f ; fi
         done
         for f in  *.nc.$nn ; do
           if [ -f $f ] ; then $dbg rm -f $f ; fi
         done 
         echo DIMG/NC restart files extension $nn erased successfully.  ;;
      ( 1 )  # assume we are in TMPDIR_CONFCASE, then RST dir to erase is in ../
         if [ -d ../${CONFCASE}-RST.$nn ] ; then $dbg rm -rf ../${CONFCASE}-RST.$nn ; fi 
         echo NC restart directory extension $nn erased successfully.  ;;
      esac
      $dbg rm $rstdone
    fi
   done

   # case of ensemble run
   if [ $ln_ens = 1 ] ; then
     for nn in $( seq 1 $nmax ) ; do
       case $rstdirs in
       ( 0 )
          for rstdone in RST_DONE.???.$nn ; do
            if [ -f $rstdone ] ; then
              member=$( echo $rstdone | awk -F. '{print $2}' )
              echo Cleaning DIMG/nc restart files extension $nn member $member
              for f in  r*.${member}_*.nc.$nn ; do
                if [ -f $f ] ; then $dbg rm -f $f ; fi
              done
              $dbg rm $rstdone
              echo DIMG/NC restart files extension $nn erased successfully, member $member
            fi
          done ;;
       ( 1 )  # assume we are in TMPDIR_CONFCASE, then RST dir to erase is in ../
          if [ -d ../${CONFCASE}-RST.$nn ] ; then 
             $dbg rm -rf ../${CONFCASE}-RST.$nn 
             echo NC restart directory extension $nn erased successfully, for all members.  
             for rstdone in RST_DONE.???.$nn ; do
                $dbg rm $rstdone ; 
             done 
          fi ;;
       esac
     done
   fi
