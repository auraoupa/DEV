PROGRAM diagmask
  !--------------------------------------------------------------------------------------
  !                            *** PROGRAM diagmask  ***
  !
  !        ** Purpose: add a vertical dimension to variable
  !
  !   History:
  !---------------------------------------------------------------------------------------
  USE netcdf
  IMPLICIT NONE

  INTEGER :: narg, iargc, jarg, ncol
  INTEGER :: n_bef, n_aft
  INTEGER :: npx, npy, npt, npk, npz
  CHARACTER(LEN=80) :: initfile, varname, maskfile, cldum2, depname
  CHARACTER(LEN=1)  ::  cobc, cvar
  REAL(KIND=4), DIMENSION (:,:,:,:), ALLOCATABLE ::  mask
  REAL(KIND=4), DIMENSION (:,:,:,:), ALLOCATABLE :: var3d
  REAL(KIND=4) :: mean, num
  LOGICAL :: l2d, l3d

  ! Netcdf Stuff
  INTEGER :: istatus, ncid, ncidm, id_x, id_y, idvar, id_t, id_z
  INTEGER :: ncout, ids, idt, idep,idtim, ji, jj, jk, jt, idvarm
  ! * 
  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' USAGE : diagmaskinit file var maskfile'
     STOP
  ENDIF

  CALL getarg (1, initfile)
  CALL getarg (2, varname)
  CALL getarg (3, maskfile)



  istatus=NF90_OPEN(initfile,NF90_WRITE,ncid); PRINT *,'Open file :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncid,'x',id_x); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_x,len=npx)
  istatus=NF90_INQ_DIMID(ncid,'y',id_y); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_y,len=npy)
  istatus=NF90_INQ_DIMID(ncid,'deptht',id_z); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_z,len=npz)
  istatus=NF90_INQ_DIMID(ncid,'time_counter',id_t); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_t,len=npt)

  istatus=NF90_OPEN(maskfile,NF90_WRITE,ncidm); PRINT *,'Open file :',NF90_STRERROR(istatus)

  ALLOCATE( mask(npx,npy,npz,1) )
  ALLOCATE( var3d(npx,npy,npz,npt) )

  istatus=NF90_INQ_VARID(ncid,varname,idvar); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncid,idvar,var3d); PRINT *,'Get var :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_VARID(ncidm,'tmask',idvarm); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncidm,idvarm,mask); PRINT *,'Get var :',NF90_STRERROR(istatus)



  DO ji = 1, npx
   DO jj = 1, npy
    DO jk = 1, npz
     DO jt = 1, npt
        IF ( mask(ji,jj,jk,1) == 1. .AND.  var3d(ji,jj,jk,jt) == 0. ) THEN ;  PRINT *,' mask = 1 et var = 0 ji =',ji,' jj =',jj,' jk =',jk,' jt=',jt
        ENDIF
     END DO
    END DO
   END DO
  END DO
  


  istatus=NF90_CLOSE(ncid)
  istatus=NF90_CLOSE(ncidm)

  DEALLOCATE(var3d, mask)

END PROGRAM diagmask
