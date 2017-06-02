PROGRAM modif_bathy
  !--------------------------------------------------------------------------------------
  !                            *** PROGRAM modif_bathy  ***
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
  CHARACTER(LEN=80) :: varname, bathyfile, cldum2, depname
  CHARACTER(LEN=1)  ::  cobc, cvar
  REAL(KIND=4), DIMENSION (:,:), ALLOCATABLE ::  bathy
  REAL(KIND=4), DIMENSION (:,:,:), ALLOCATABLE ::  nbdy
  REAL(KIND=4), DIMENSION (:,:,:), ALLOCATABLE ::  sbdy
  REAL(KIND=4), DIMENSION (:,:,:,:), ALLOCATABLE :: mask
  REAL(KIND=4) :: mean, num
  LOGICAL :: l2d, l3d

  ! Netcdf Stuff
  INTEGER :: istatus, ncid, ncidm, ncidn, ncids, id_x, id_y, idvar, id_t, id_z, idmask, idnbdy, idsbdy
  INTEGER :: ncout, ids, idt, idep,idtim, ji, jj, jk, jt, idvarm
  ! * 
  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' USAGE : modif_bathy bathyfile'
     STOP
  ENDIF

  CALL getarg (1, bathyfile)



  istatus=NF90_OPEN(bathyfile,NF90_WRITE,ncid); PRINT *,'Open file :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncid,'x',id_x); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_x,len=npx)
  istatus=NF90_INQ_DIMID(ncid,'y',id_y); PRINT *,'Inquire dimid :',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncid,id_y,len=npy)

  ALLOCATE( bathy(npx,npy) )

  istatus=NF90_INQ_VARID(ncid,'Bathymetry',idvar); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncid,idvar,bathy); PRINT *,'Get var :',NF90_STRERROR(istatus)


  istatus=NF90_OPEN('NACHOS12.L75_mask_surf.nc',NF90_WRITE,ncidm); PRINT *,'Open file :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_DIMID(ncidm,'z',id_z); PRINT *,'Inquire dimid:',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidm,id_z,len=npz)
  istatus=NF90_INQ_DIMID(ncidm,'t',id_t); PRINT *,'Inquire dimid:',NF90_STRERROR(istatus)
  istatus=NF90_INQUIRE_DIMENSION(ncidm,id_t,len=npt)

  ALLOCATE( mask(npx,npy,npz,npt) )

  istatus=NF90_INQ_VARID(ncidm,'tmask',idmask); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncidm,idmask,mask); PRINT *,'Get var :',NF90_STRERROR(istatus)

  istatus=NF90_OPEN('NACHOS12.L75_NBDYSSH_20100115.nc',NF90_WRITE,ncidn); PRINT *,'Open file :',NF90_STRERROR(istatus)
  istatus=NF90_OPEN('NACHOS12.L75_SBDYSSH_20100115.nc',NF90_WRITE,ncids); PRINT *,'Open file :',NF90_STRERROR(istatus)

  ALLOCATE( nbdy(npx,1,npt) )
  ALLOCATE( sbdy(npx,1,npt) )

  istatus=NF90_INQ_VARID(ncidn,'sossheig',idnbdy); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncidn,idnbdy,nbdy); PRINT *,'Get var :',NF90_STRERROR(istatus)

  istatus=NF90_INQ_VARID(ncids,'sossheig',idsbdy); PRINT *,'Inquire varid :',NF90_STRERROR(istatus)
  istatus=NF90_GET_VAR(ncids,idsbdy,sbdy); PRINT *,'Get var :',NF90_STRERROR(istatus)


   DO ji = 1, npx

        IF ( mask(ji,3,1,1) == 1. .AND. sbdy(ji,1,1) == 0.) THEN 
           PRINT *,'SBDY manque donnees glorys a ji = ',ji, ' or bathy = ',bathy(ji,3)
           bathy(ji,3)=0.
        ENDIF
   END DO

   DO ji = 1, npx
        IF ( mask(ji,1566,1,1) == 1. .AND. nbdy(ji,1,1) == 0.) THEN 
           PRINT *,'NBDY manque donnees glorys a ji = ',ji, ' or bathy = ',bathy(ji,1567)
           bathy(ji,1566)=0.
        ENDIF
   END DO

   DO ji = 1, npx
           bathy(ji,1567)=bathy(ji,1566)
           bathy(ji,1568)=bathy(ji,1566)
           bathy(ji,1)=bathy(ji,3)
           bathy(ji,2)=bathy(ji,3)
   END DO

  istatus=NF90_PUT_VAR(ncid,idvar,bathy); PRINT *,'Put var :',NF90_STRERROR(istatus)

  istatus=NF90_CLOSE(ncid)
  istatus=NF90_CLOSE(ncidm)
  istatus=NF90_CLOSE(ncidn)
  istatus=NF90_CLOSE(ncids)

  DEALLOCATE(bathy, mask, nbdy, sbdy)

END PROGRAM modif_bathy
