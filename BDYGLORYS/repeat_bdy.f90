PROGRAM repeat_bdy
  !--------------------------------------------------------------------------------------
  !                            *** PROGRAM repeat_bdy  ***
  !
  !        ** Purpose: add a vertical dimension to variable
  !
  !   History:
  !---------------------------------------------------------------------------------------
  USE netcdf
  IMPLICIT NONE

  INTEGER :: narg, iargc, jarg, ncol
  INTEGER :: n_bef, n_aft, ll, llmin, llmax
  INTEGER :: npx, npy, npt, npk, jt, npz, ncidm, npxm
  CHARACTER(LEN=80) :: bdyfile, varname, cldum, cldum2, depname, repbdyfile
  CHARACTER(LEN=1)  :: cobc, cvar
  REAL(KIND=4), DIMENSION (:,:,:), ALLOCATABLE ::  mask2d
  REAL(KIND=4), DIMENSION (:,:,:,:), ALLOCATABLE ::  mask3d
  REAL(KIND=4), DIMENSION (:,:,:), ALLOCATABLE :: var2d
  REAL(KIND=4), DIMENSION (:,:,:,:), ALLOCATABLE :: var3d
  REAL(KIND=4), DIMENSION (:), ALLOCATABLE :: mean, num
  LOGICAL :: l2d, l3d

  ! Netcdf Stuff
  INTEGER :: istatus, ncid, id_x, id_xm, id_y, id_z, idvar, id_t, id_dep
  INTEGER :: ncout, ids, idt, idep,idtim, ji, jj, idvarm, jk
  ! * 
  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' USAGE : repeat_bdy bdyfile_glo var 2d/3d bdyfile_rep  '
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

  CALL getarg (4, repbdyfile)

  istatus=NF90_OPEN(bdyfile,NF90_WRITE,ncid); PRINT *,'Open file :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncid,'x',id_x); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_x,len=npx)
  istatus=NF90_INQ_DIMID(ncid,'time_counter',id_t); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_t,len=npt)

  IF ( l3d ) THEN ; istatus=NF90_INQ_DIMID(ncid,'deptht',id_dep); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus); istatus=NF90_INQUIRE_DIMENSION(ncid,id_dep,len=npk)
  ENDIF

  IF ( l2d ) THEN ; ALLOCATE( var2d(npx,1,npt) )
  ENDIF

  IF ( l3d ) THEN ; ALLOCATE( var3d(npx,1,npk,npt) )
  ENDIF


  istatus=NF90_INQ_VARID(ncid,varname,idvar); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  IF ( l2d ) THEN ; istatus=NF90_GET_VAR(ncid,idvar,var2d); PRINT *,'Get var :',NF90_STRERROR(istatus)
  ENDIF
  IF ( l3d ) THEN ; istatus=NF90_GET_VAR(ncid,idvar,var3d); PRINT *,'Get var :',NF90_STRERROR(istatus)
  ENDIF

  istatus=NF90_OPEN(repbdyfile,NF90_WRITE,ncidm); PRINT *,'Open file :',NF90_STRERROR(istatus)
  istatus=NF90_INQ_DIMID(ncidm,'x',id_xm); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidm,id_xm,len=npxm)


  IF ( l2d ) THEN ; ALLOCATE( mask2d(npxm,1,npt) )
  ENDIF

  IF ( l3d ) THEN ; ALLOCATE( mask3d(npxm,1,npk,npt) )
  ENDIF

  istatus=NF90_INQ_VARID(ncidm,varname,idvarm); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  IF ( l2d ) THEN ; istatus=NF90_GET_VAR(ncidm,idvarm,mask2d); PRINT *,'Get var :',NF90_STRERROR(istatus)
  ENDIF
  IF ( l3d ) THEN ; istatus=NF90_GET_VAR(ncidm,idvarm,mask3d); PRINT *,'Get var :',NF90_STRERROR(istatus)
  ENDIF


  IF ( l2d ) THEN

    DO ji = 1, npx
     DO jt = 1, npt

       llmin = (ji - 1) * 3 + 1
       llmax = (ji - 1) * 3 + 3
      
      
       DO ll = llmin, llmax
         IF (var2d(ji,1,jt) < 10000.) mask2d(ll,1,jt) = var2d(ji,1,jt)
         IF (var2d(ji,1,jt) > 10000.) mask2d(ll,1,jt) = 0
       END DO
     END DO
    END DO


    mask2d(npxm,1,1) = mask2d(npxm-1,1,1)

  ENDIF


  IF ( l3d ) THEN


    DO ji = 1, npx
     DO jt = 1, npt
      DO jk = 1, npk

       llmin = (ji - 1) * 3 + 1
       llmax = (ji - 1) * 3 + 3

       DO ll = llmin, llmax
         IF (var3d(ji,1,jk,jt) < 10000.) mask3d(ll,1,jk,jt) = var3d(ji,1,jk,jt)
         IF (var3d(ji,1,jk,jt) > 10000.) mask3d(ll,1,jk,jt) = 0
       END DO
     END DO
    END DO
   END DO
 
  mask3d(npxm,:,:,:) = mask3d(npxm-1,:,:,:) 
  ENDIF


!  IF ( l2d ) THEN

!    DO ji = 1, npx
!     DO jt = 1, npt
!        IF ( mask(ji,1,1,1) < 1. )  var2d(ji,1,jt) = 0.
!        IF ( var2d(ji,1,jt) == 0. .AND. mask(ji,1,1,1) == 1. ) THEN
!          PRINT *,'pb mask 2D at ji = ',ji,' jt = ',jt
!          mean(1)=0.; num(1)=0.
!          IF ( var2d(ji-1,1,jt) /= 0. ) THEN; mean(1)=mean(1)+var2d(ji-1,1,jt); num(1)=num(1)+1.; ENDIF
!          IF ( var2d(ji+1,1,jt) /= 0. ) THEN; mean(1)=mean(1)+var2d(ji+1,1,jt); num(1)=num(1)+1.; ENDIF
!          IF ( num(1) == 0. ) THEN
!            PRINT *, 'mean = 0. tous les voisins sont pourris'
!          ELSE
!            var2d(ji,1,jt)=mean(1)/num(1); PRINT *,'mean = ',mean(1)
!          ENDIF
!        END IF
!     END DO
!    END DO

!  ENDIF

!  IF ( l3d ) THEN

!    DO ji = 1, npx
!     DO jk = 1, npz
!      DO jt = 1, npt
!        IF ( mask(ji,1,jk,1) < 1. )  var3d(ji,1,jk,jt) = 0.
!        IF ( var3d(ji,1,jk,jt) == 0. .AND. mask(ji,1,jk,1) == 1. ) THEN
!          PRINT *,'pb mask 3D at ji = ',ji,' jt = ',jt,' jk =',jk
!          mean(1)=0.; num(1)=0.
!          IF ( var3d(ji-1,1,jk-1,jt) /= 0. ) THEN; mean(1)=mean(1)+var3d(ji-1,1,jk-1,jt); num(1)=num(1)+1.; ENDIF
!          IF ( var3d(ji-1,1,jk+1,jt) /= 0. ) THEN; mean(1)=mean(1)+var3d(ji-1,1,jk+1,jt); num(1)=num(1)+1.; ENDIF
!          IF ( var3d(ji-1,1,jk  ,jt) /= 0. ) THEN; mean(1)=mean(1)+var3d(ji-1,1,jk  ,jt); num(1)=num(1)+1.; ENDIF
!          IF ( var3d(ji  ,1,jk-1,jt) /= 0. ) THEN; mean(1)=mean(1)+var3d(ji  ,1,jk-1,jt); num(1)=num(1)+1.; ENDIF
!          IF ( var3d(ji  ,1,jk+1,jt) /= 0. ) THEN; mean(1)=mean(1)+var3d(ji  ,1,jk+1,jt); num(1)=num(1)+1.; ENDIF
!          IF ( var3d(ji+1,1,jk-1,jt) /= 0. ) THEN; mean(1)=mean(1)+var3d(ji+1,1,jk-1,jt); num(1)=num(1)+1.; ENDIF
!          IF ( var3d(ji+1,1,jk+1,jt) /= 0. ) THEN; mean(1)=mean(1)+var3d(ji+1,1,jk+1,jt); num(1)=num(1)+1.; ENDIF
!          IF ( var3d(ji+1,1,jk  ,jt) /= 0. ) THEN; mean(1)=mean(1)+var3d(ji+1,1,jk  ,jt); num(1)=num(1)+1.; ENDIF
!          IF ( num(1) == 0. ) THEN
!            PRINT *, 'mean = 0. tous les voisins sont pourris'
!          ELSE
!            var3d(ji,1,jk,jt)=mean(1)/num(1); PRINT *,'mean = ',mean(1)
!          ENDIF
!        ENDIF

!      END DO
!     END DO
!    END DO
  
!  ENDIF


  IF ( l2d ) THEN ;  istatus=NF90_PUT_VAR(ncidm,idvarm,mask2d); PRINT *,'Put var :',NF90_STRERROR(istatus)
  ENDIF
  IF ( l3d ) THEN ;  istatus=NF90_PUT_VAR(ncidm,idvarm,mask3d); PRINT *,'Put var :',NF90_STRERROR(istatus)
  ENDIF
  istatus=NF90_CLOSE(ncid)
  istatus=NF90_CLOSE(ncidm)

  IF ( l2d ) THEN ;   DEALLOCATE(var2d, mask2d)
  ENDIF
  IF ( l3d ) THEN ;   DEALLOCATE(var3d, mask3d)
  ENDIF

  
!  DEALLOCATE( num )
!  DEALLOCATE( mean )

END PROGRAM repeat_bdy
