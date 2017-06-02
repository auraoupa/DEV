PROGRAM pseudo_coord
  !--------------------------------------------------------------------------------------
  !                            *** PROGRAM pseudo_coord  ***
  !
  !        ** Purpose: create 2 pseudo regular grid from NARYS(GLORYS restricted
  !        to NACHOS area) and NACHOS12
  !
  !   History:
  !---------------------------------------------------------------------------------------
  USE netcdf
  IMPLICIT NONE

  INTEGER :: narg, iargc, jarg, ncol
  INTEGER :: n_bef, n_aft
  INTEGER :: npx, npy, npt, npk, npz,npxn, npyn
  CHARACTER(LEN=80) :: varname, cldum2, depname
  CHARACTER(LEN=1)  ::  cobc, cvar
  REAL(KIND=4), DIMENSION (:), ALLOCATABLE ::  lon
  REAL(KIND=4), DIMENSION (:), ALLOCATABLE ::  lat
  REAL(KIND=4), DIMENSION (:), ALLOCATABLE ::  lonn
  REAL(KIND=4), DIMENSION (:), ALLOCATABLE ::  latn
  REAL(KIND=4), DIMENSION (:,:), ALLOCATABLE ::  glam
  REAL(KIND=4), DIMENSION (:,:), ALLOCATABLE ::  gphi
  REAL(KIND=4), DIMENSION (:,:), ALLOCATABLE ::  glamn
  REAL(KIND=4), DIMENSION (:,:), ALLOCATABLE ::  gphin


  REAL(KIND=4) :: mean, num
  LOGICAL :: l2d, l3d

  ! Netcdf Stuff
  INTEGER :: istatus, ncid, ncidn, id_x, id_y, id_xn, id_yn, idlat, idlon, idlatn, idlonn
  INTEGER :: ncout, ids, idt, idep,idtim, ji, jj, jk, jt, idvarm, idphi, idlam, idphin, idlamn
  ! * 




  istatus=NF90_OPEN('NARYS2V3_pseudo_coord.nc',NF90_WRITE,ncid); PRINT *,'Open file :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncid,'x',id_x); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_x,len=npx)
  istatus=NF90_INQ_DIMID(ncid,'y',id_y); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_y,len=npy)


  PRINT *, ' x = ',npx,' y = ',npy

  ALLOCATE( lon(npx), lat(npy) )
  ALLOCATE( glam(npx,npy), gphi(npx,npy) )

  istatus=NF90_OPEN('NACHOS12_pseudo_coord.nc',NF90_WRITE,ncidn); PRINT *,'Open file :',NF90_STRERROR(istatus)

  PRINT *,'ncidn = ',ncidn

  istatus=NF90_INQ_DIMID(ncidn,'x',id_xn); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidn,id_xn,len=npxn)
  istatus=NF90_INQ_DIMID(ncidn,'y',id_yn); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidn,id_yn,len=npyn)

  PRINT *, ' xn = ',npxn,' yn = ',npyn

  ALLOCATE( lonn(npxn), latn(npyn) )
  ALLOCATE( glamn(npxn,npyn), gphin(npxn,npyn) )


  DO ji = 1, npx

    lon(ji) =  ( 359. / 538. ) * ji

    DO jj = 1, npy
  
      glam(ji,jj) = lon(ji)
 
    END DO

  END DO

  DO jj = 1, npy

    lat(jj) =  ( 180. / 528. ) * jj - 90.

    DO ji = 1, npx

      gphi(ji,jj) = lat(jj)

    END DO

  END DO

  DO ji = 1, npxn

    lonn(ji) =  ( 359. / 538. ) * ( MODULO(ji,3) / 3. + floor( real(ji) / 3. ) + 1. )

    DO jj = 1, npyn

      glamn(ji,jj) = lonn(ji)

    END DO

  END DO

  DO jj = 1, npyn

    latn(jj) =  ( 180. / 528. ) *  ( MODULO((jj + 2),3) / 3. + floor( (real(jj) + 2) / 3 )  ) - 90.

    DO ji = 1, npxn

      gphin(ji,jj) = latn(jj)

    END DO

  END DO

  istatus=NF90_REDEF(ncid); PRINT *,'Redef :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_VAR( ncid, 'lat', NF90_FLOAT,(/id_y/)   , idlat); PRINT *,'Def var :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_VAR( ncid, 'lon', NF90_FLOAT,(/id_x/)   , idlon); PRINT *,'Def var :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_VAR( ncid, 'gphif', NF90_FLOAT,(/id_x,id_y/)   , idphi); PRINT *,'Def var :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_VAR( ncid, 'glamf', NF90_FLOAT,(/id_x,id_y/)   , idlam); PRINT *,'Def var :',NF90_STRERROR(istatus)
  istatus=NF90_ENDDEF(ncid); PRINT *,'Enddef :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncid,idlat,lat); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncid,idlon,lon); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncid,idlam,glam); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncid,idphi,gphi); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_CLOSE(ncid)

  istatus=NF90_REDEF(ncidn); PRINT *,'Redef :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_VAR( ncidn, 'lat', NF90_FLOAT,(/id_yn/)   , idlatn); PRINT *,'Def var :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_VAR( ncidn, 'lon', NF90_FLOAT,(/id_xn/)   , idlonn); PRINT *,'Def var :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_VAR( ncidn, 'gphif', NF90_FLOAT,(/id_xn,id_yn/)   , idphin); PRINT *,'Def var :',NF90_STRERROR(istatus)
  istatus=NF90_DEF_VAR( ncidn, 'glamf', NF90_FLOAT,(/id_xn,id_yn/)   , idlamn); PRINT *,'Def var :',NF90_STRERROR(istatus)
  istatus=NF90_ENDDEF(ncidn); PRINT *,'Enddef :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncidn,idlatn,latn); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncidn,idlonn,lonn); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncidn,idlamn,glamn); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_PUT_VAR(ncidn,idphin,gphin); PRINT *,'Put var :',NF90_STRERROR(istatus)
  istatus=NF90_CLOSE(ncidn)

  DEALLOCATE(lat,lon,latn,lonn)
  DEALLOCATE(gphi,glam,gphin,glamn)

END PROGRAM pseudo_coord
