PROGRAM check_nan
  !--------------------------------------------------------------------------------------
  !                            *** PROGRAM check_nan  ***
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
  CHARACTER(LEN=80) :: varname, infile, cldum2, depname
  CHARACTER(LEN=1)  ::  cobc, cvar
  REAL(KIND=4), DIMENSION (:,:,:,:), ALLOCATABLE ::  var
  REAL(KIND=4) :: mean, num
  LOGICAL :: l2d, l3d

  ! Netcdf Stuff
  INTEGER :: istatus, ncid, ncidm, ncidn, ncids, id_x, id_y, idvar, id_t, id_z, idmask, idnbdy, idsbdy
  INTEGER :: ncout, ids, idt, idep,idtim, ji, jj, jk, jt, idvarm
  ! * 
  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' USAGE : check_nan file var' 
     STOP
  ENDIF

  CALL getarg (1, infile)
  CALL getarg (2, varname)



  istatus=NF90_OPEN(infile,NF90_WRITE,ncid); PRINT *,'Open file :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncid,'x',id_x); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_x,len=npx)
  istatus=NF90_INQ_DIMID(ncid,'y',id_y); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_y,len=npy)
  istatus=NF90_INQ_DIMID(ncid,'z',id_z); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_z,len=npz)
  istatus=NF90_INQ_DIMID(ncid,'time',id_t); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_t,len=npt)

  ALLOCATE( var(npx,npy,npz,npt) )

  istatus=NF90_INQ_VARID(ncid,varname,idvar); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncid,idvar,var); PRINT *,'Get var :',NF90_STRERROR(istatus)


   DO ji = 1, npx
    DO jj = 1, npy
     DO jk = 1, npz
      DO jt = 1, npt

        IF ( var(ji,jj,jk,jt) > 100 ) THEN 
!           PRINT *,' abnormal value at ji = ',ji,', jj = ',jj,', jk = ',jk,', jt = ',jt
           var(ji,jj,jk,jt) = 9999
        ENDIF

      END DO
     END DO
    END DO
   END DO

  istatus=NF90_PUT_VAR(ncid,idvar,var); PRINT *,'Put var :',NF90_STRERROR(istatus)

  istatus=NF90_CLOSE(ncid)

  DEALLOCATE(var)

END PROGRAM check_nan
