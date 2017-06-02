PROGRAM mktmask
  !--------------------------------------------------------------------------------------
  !                            *** PROGRAM mktmask  ***
  !
  !        ** Purpose: add a vertical dimension to variable
  !
  !   History:
  !---------------------------------------------------------------------------------------
  USE netcdf
  IMPLICIT NONE

  INTEGER :: narg, iargc, jarg, ncol
  INTEGER :: n_bef, n_aft
  INTEGER :: npx, npy, npt, npk, jt, npz, ncidm
  CHARACTER(LEN=80) :: bdyfile, varname, cldum, cldum2, depname, maskfile
  CHARACTER(LEN=1)  :: cobc, cvar
  REAL(KIND=4), DIMENSION (:,:,:,:), ALLOCATABLE ::  mask
  REAL(KIND=4), DIMENSION (:,:,:), ALLOCATABLE :: var2d
  REAL(KIND=4), DIMENSION (:,:,:,:), ALLOCATABLE :: var3d
  REAL(KIND=4), DIMENSION (:), ALLOCATABLE :: mean, num, redo
  LOGICAL :: l2d, l3d

  ! Netcdf Stuff
  INTEGER :: istatus, ncid, id_x, id_y, id_z, idvar, id_t, id_dep
  INTEGER :: ncout, ids, idt, idep,idtim, ji, jj, idvarm, jk
  ! * 
  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' USAGE : mktmask NBDY var 2d/3d T/U/V: clean Northern BDY  '
     STOP
  ENDIF

  CALL getarg (1, bdyfile)
  CALL getarg (2, varname)
  CALL getarg (3, cldum)

  SELECT CASE ( cldum)
      CASE ( '2D' ) ; l2d = .TRUE.
      CASE ( '3D' ) ; l3d = .TRUE.
  END SELECT

  PRINT *,'2D = ', l2d
  PRINT *,'3D = ', l3d

  CALL getarg (4, cldum2)

  SELECT CASE ( cldum2)
      CASE ( 'T' ) ; depname='deptht'
      CASE ( 'U' ) ; depname='depthu'
      CASE ( 'V' ) ; depname='depthv'
  END SELECT

  CALL getarg (5, maskfile)

  istatus=NF90_OPEN(bdyfile,NF90_WRITE,ncid); PRINT *,'Open file :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncid,'x',id_x); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_x,len=npx)
  istatus=NF90_INQ_DIMID(ncid,'time_counter',id_t); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_t,len=npt)

  IF ( l3d ) THEN ; istatus=NF90_INQ_DIMID(ncid,depname,id_dep); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus); istatus=NF90_INQUIRE_DIMENSION(ncid,id_dep,len=npk)
  ENDIF

  IF ( l2d ) THEN ; ALLOCATE( var2d(npx,1,npt) )
  ENDIF

  IF ( l3d ) THEN ; ALLOCATE( var3d(npx,1,npk,npt) )
  ENDIF

  ALLOCATE( mean(1) )
  ALLOCATE( num(1) )
  ALLOCATE( redo(1) )

  istatus=NF90_INQ_VARID(ncid,varname,idvar); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  IF ( l2d ) THEN ; istatus=NF90_GET_VAR(ncid,idvar,var2d); PRINT *,'Get var :',NF90_STRERROR(istatus)
  ENDIF
  IF ( l3d ) THEN ; istatus=NF90_GET_VAR(ncid,idvar,var3d); PRINT *,'Get var :',NF90_STRERROR(istatus)
  ENDIF
  istatus=NF90_OPEN(maskfile,NF90_WRITE,ncidm); PRINT *,'Open file :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_DIMID(ncidm,'z',id_z); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidm,id_z,len=npz)

  PRINT *,' npz =',npz,' npx = ',npx,' npy = =',npy,' npk = ',npk,' npt = ',npt

  ALLOCATE( mask(npx,1,npz,1) )

  istatus=NF90_INQ_VARID(ncidm,'tmask',idvarm); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncidm,idvarm,mask); PRINT *,'Get var :',NF90_STRERROR(istatus)


  IF ( l2d ) THEN

    DO ji = 1, npx
     DO jt = 1, npt
        IF ( mask(ji,1,1,1) < 1. )  var2d(ji,1,jt) = 0.
        IF ( var2d(ji,1,jt) == 0. .AND. mask(ji,1,1,1) == 1. ) THEN
          PRINT *,'pb mask 2D at ji = ',ji,' jt = ',jt
          mean(1)=0.; num(1)=0.
          IF ( var2d(ji-1,1,jt) /= 0. .AND. ABS(var2d(ji-1,1,jt)) < 100.) THEN; mean(1)=mean(1)+var2d(ji-1,1,jt); num(1)=num(1)+1.; ENDIF
          IF ( var2d(ji+1,1,jt) /= 0. .AND. ABS(var2d(ji+1,1,jt)) < 100.) THEN; mean(1)=mean(1)+var2d(ji+1,1,jt); num(1)=num(1)+1.; ENDIF
          IF ( num(1) == 0. ) THEN
            PRINT *, 'mean = 0. tous les voisins sont pourris'
          ELSE
            var2d(ji,1,jt)=mean(1)/num(1); PRINT *,'mean = ',mean(1)
          ENDIF
        END IF
     END DO
    END DO

  ENDIF

  IF ( l3d ) THEN

 redo(1)=1.

  DO WHILE (redo(1) == 1.)

    redo(1)=0.
    DO ji = 1, npx
     DO jk = 1, npz
      DO jt = 1, npt
        IF ( mask(ji,1,jk,1) < 1. )  var3d(ji,1,jk,jt) = 0.
        IF ( var3d(ji,1,jk,jt) == 0. .AND. mask(ji,1,jk,1) == 1. ) THEN
          PRINT *,'pb mask 3D at ji = ',ji,' jt = ',jt,' jk =',jk
          mean(1)=0.; num(1)=0.
          IF ( var3d(ji-1,1,jk  ,jt) /= 0. .AND. ABS(var3d(ji-1,1,jk  ,jt)) < 100. ) THEN; mean(1)=mean(1)+var3d(ji-1,1,jk  ,jt); num(1)=num(1)+1.; ENDIF
          IF ( var3d(ji+1,1,jk  ,jt) /= 0. .AND. ABS(var3d(ji+1,1,jk  ,jt)) < 100. ) THEN; mean(1)=mean(1)+var3d(ji+1,1,jk  ,jt); num(1)=num(1)+1.; ENDIF
          IF ( num(1) == 0. ) THEN
            PRINT *, 'mean = 0. pas de colonnes adjacentes'
            IF ( var3d(ji,1,jk-1  ,jt) /= 0. .AND. ABS(var3d(ji,1,jk-1  ,jt)) < 100. ) THEN; mean(1)=mean(1)+var3d(ji,1,jk-1  ,jt); num(1)=num(1)+1.; ENDIF
            IF ( num(1) == 0. ) THEN
              PRINT *, 'mean = 0. rien au dessus non plus il faut repasser'
              redo(1)=1.
            ELSE
              var3d(ji,1,jk,jt)=mean(1)/num(1); PRINT *,'mean = ',mean(1),' num = ', num(1)
            ENDIF
          ELSE
            var3d(ji,1,jk,jt)=mean(1)/num(1); PRINT *,'mean = ',mean(1),' num = ', num(1)
          ENDIF
        ENDIF

      END DO
     END DO
    END DO
  
  END DO    

  ENDIF

  IF ( l2d ) THEN ;  istatus=NF90_PUT_VAR(ncid,idvar,var2d); PRINT *,'Put var :',NF90_STRERROR(istatus)
  ENDIF
  IF ( l3d ) THEN ;  istatus=NF90_PUT_VAR(ncid,idvar,var3d); PRINT *,'Put var :',NF90_STRERROR(istatus)
  ENDIF
  istatus=NF90_CLOSE(ncid)
  istatus=NF90_CLOSE(ncidm)

  IF ( l2d ) THEN ;   DEALLOCATE(var2d, mask)
  ENDIF
  IF ( l3d ) THEN ;   DEALLOCATE(var3d, mask)
  ENDIF

  
  DEALLOCATE( num )
  DEALLOCATE( mean )

END PROGRAM mktmask
