PROGRAM ic_field_vertical_extent
  !!======================================================================
  !!                     ***  PROGRAM  ic_field_vertical_extent  ***
  !!=====================================================================
  !!  ** Purpose : Extend bottom values down to last vertical level
  !!
  !!  ** Method  : create a working copy of input file and modify variables
  !!               in the copy, geometry of the variable not changed
  !!
  !! History : 1.0  : 10/2014  : J.M. Molines : Original code
  !!           1.1  : 11/2014  : J.M. Molines : Generalization to NEMO grid
  !!----------------------------------------------------------------------
  !!----------------------------------------------------------------------
  !!   routines      : description
  !!----------------------------------------------------------------------
  USE netcdf
  !!----------------------------------------------------------------------
  !! DATATOOLS , MEOM 2014
  !! $Id$
  !! Copyright (c) 2014, J.-M. Molines
  !!----------------------------------------------------------------------
  IMPLICIT NONE
  INTEGER :: ji,jj,jk, jt
  INTEGER :: ncid, idv3, idvd, id_dim, ierr
  INTEGER :: narg, iargc
  INTEGER :: npilev, npjlev, npklev, nptlev
 

  REAL(KIND=4)                                :: spval
  REAL(KIND=4), DIMENSION(:)    , ALLOCATABLE :: gdep
  REAL(KIND=4), DIMENSION(:,:,:), ALLOCATABLE :: v3d

  CHARACTER(LEN=255) :: cf_in
  CHARACTER(LEN=255) :: cv_in
  CHARACTER(LEN=255) :: cv_dep = 'deptht'
  CHARACTER(LEN=255) :: cf_in_copy
  CHARACTER(LEN=20)  :: c_dimx = 'x'
  CHARACTER(LEN=20)  :: c_dimy = 'y'
  CHARACTER(LEN=20)  :: c_dimz = 'deptht'
  CHARACTER(LEN=20)  :: c_dimt = 'time_counter'
  !!----------------------------------------------------------------------

narg = iargc()
IF ( narg == 0 ) THEN
   PRINT *, ' USAGE : ic_field_vertical_extent IN-file IN-var'
   PRINT *, '   PURPOSE:'
   PRINT *, '      Extend bottom value below ocean floor, define last depth as 10000 m'
   PRINT *, '   METHOD:'
   PRINT *, '      Create a working copy of the input file.'
   PRINT *, '   ARGUMENTS:'
   PRINT *, '       IN-file : input file name to be modified'
   PRINT *, '       IN-var  : input variable to be modified'
   PRINT *, '   OUTPUT:'
   PRINT *, '      netcdf file : <IN-file>_copy'
   PRINT *, '      netcdf variable : <IN-var>'
   STOP
ENDIF

CALL getarg(1, cf_in)
CALL getarg(2, cv_in)

! Create a working copy
cf_in_copy=TRIM(cf_in)//'_copy'
CALL system ( 'cp '//TRIM(cf_in)//' '//TRIM(cf_in_copy))

! Work on the copy
ierr = NF90_OPEN(cf_in_copy, NF90_WRITE, ncid)
ierr = NF90_INQ_DIMID(ncid,c_dimx,id_dim)  ; ierr = NF90_INQUIRE_DIMENSION(ncid,id_dim,len=npilev)
ierr = NF90_INQ_DIMID(ncid,c_dimy,id_dim)  ; ierr = NF90_INQUIRE_DIMENSION(ncid,id_dim,len=npjlev)
ierr = NF90_INQ_DIMID(ncid,c_dimz,id_dim)  ; ierr = NF90_INQUIRE_DIMENSION(ncid,id_dim,len=npklev)
ierr = NF90_INQ_DIMID(ncid,c_dimt,id_dim)  ; ierr = NF90_INQUIRE_DIMENSION(ncid,id_dim,len=nptlev)

ALLOCATE( v3d(npilev, npjlev, npklev),  gdep(npklev) )

ierr = NF90_INQ_VARID(ncid,cv_dep, idvd ) ; ierr = NF90_GET_VAR(ncid,idvd,gdep )
gdep(npklev) = 10000.  ! dummy large depth
ierr = NF90_PUT_VAR(ncid,idvd, gdep)

DO jt = 1, nptlev
! Read 3d variable at once ...
ierr = NF90_INQ_VARID(ncid,cv_in,  idv3 ) ; ierr = NF90_GET_VAR(ncid,idv3,v3d, start=(/1,1,1,jt/), count=(/npilev,npjlev,npklev,1/)  )
IF ( jt == 1 ) THEN
! Look for Fill Value or missing value
ierr = NF90_GET_ATT(ncid,idv3,'_FillValue',spval)
IF ( ierr /= NF90_NOERR ) THEN
   ierr = NF90_GET_ATT(ncid,idv3,'missing_value',spval)
   IF ( ierr /= NF90_NOERR ) THEN 
      PRINT *, ' neither _FillValue nor missing_value attribute found for variable ', TRIM(cv_in),' from file ',TRIM(cf_in)
      STOP
   ENDIF
   ierr = NF90_REDEF(ncid)
   ierr = NF90_PUT_ATT(ncid,idv3,'_FillValue',spval)
   ierr = NF90_ENDDEF(ncid)
ENDIF
ENDIF

DO jj=1, npjlev
  DO ji=1, npilev
    IF ( v3d(ji,jj,1) /= spval ) THEN
     DO jk=2, npklev
       IF ( v3d(ji,jj,jk) == spval ) THEN  
          v3d(ji,jj,jk) =  v3d(ji,jj,jk-1 )
      ENDIF
     ENDDO
    ENDIF
  ENDDO
ENDDO

ierr = NF90_PUT_VAR(ncid,idv3, v3d, start=(/1,1,1,jt/), count=(/npilev,npjlev,npklev,1/))
ENDDO
ierr = NF90_CLOSE(ncid)
 
END PROGRAM ic_field_vertical_extent

