PROGRAM ic_correct_low
  !!======================================================================
  !!                     ***  PROGRAM  c_correct_low  ***
  !!=====================================================================
  !!  ** Purpose : replace values lower than a given limit by more correct
  !!               values found nearby the faulty point.
  !!
  !!  ** Method  : First try to fix low value looking for good points in
  !!               the same water column. Then fix the remnant searching
  !!               for good points in a 3D bubble centered on the faulty
  !!               point.
  !!
  !! History : 1.0  : 11/2016  : J.M. Molines : original code
  !!----------------------------------------------------------------------
  !!----------------------------------------------------------------------
  !!   function      : description
  !!  lpoint_in      : boolean function T when point is in selected area
  !!                                    F otherwise.
  !!----------------------------------------------------------------------

  USE netcdf
  IMPLICIT NONE
  !!----------------------------------------------------------------------
  !! DATATOOLS , MEOM 2016
  !! $Id$
  !! Copyright (c) 2016, J.-M. Molines
  !! Software governed by the CeCILL licence (Licence/DATATOOLSCeCILL.txt)
  !!----------------------------------------------------------------------
  INTEGER(KIND=4) :: ji,jj,jk,jt
  INTEGER(KIND=4) :: ierr, ncid, id
  INTEGER(KIND=4) :: i1,i2,  j1,j2,  k1,k2,  ni,nj,nk
  INTEGER(KIND=4) :: iargc, narg, ijarg
  INTEGER(KIND=4) :: npiglo, npjglo, npk, npt

  REAL(KIND=4)  :: goodval, spval, slimit
  REAL(KIND=4)  :: rlonmin, rlonmax, rlatmin, rlatmax
  REAL(KIND=4), DIMENSION (:,:,:), ALLOCATABLE :: v3d, bubble
  REAL(KIND=4), DIMENSION (:,:),   ALLOCATABLE :: v2d, rlon, rlat

  CHARACTER(LEN=255) :: cf_in
  CHARACTER(LEN=255) :: cv_in
  CHARACTER(LEN=255) :: cdum, cmd

  LOGICAL, DIMENSION(:,:,:), ALLOCATABLE :: lmask
  LOGICAL :: linout
  !!----------------------------------------------------------------------
  narg=iargc()

  IF ( narg /= 8 ) THEN
     PRINT *,' usage :  ic_correct_low IN-file IN-var SLIMIT in/out ...'
     PRINT *,'          ... LONMIN LONMAX LATMIN LATMAX'
     PRINT *,'      '
     PRINT *,'     PURPOSE :'
     PRINT *,'       Correct values lower that slimit with either slimit itself or a good '
     PRINT *,'       value nearby the low point. The change is limited either inside or '
     PRINT *,'       outside a requested zone, defined by its geographical coordinates.' 
     PRINT *,'      '
     PRINT *,'     ARGUMENTS :'
     PRINT *,'        IN-file : input netcdf_file ( will work on a copy of this file) '
     PRINT *,'        IN-var : variable to deal with'
     PRINT *,'        SLIMIT : lower acceptable limit for the variable'
     PRINT *,'        in/out : if ''in'', changes are made in the specified zone'
     PRINT *,'               : if ''out'', changes are made outside the specified zone'
     PRINT *,'        LONMIN LONMAX LATMIN LATMAX : limit of the working zone (in or out)'
     PRINT *,'      '
     PRINT *,'     OPTIONS :'
     PRINT *,'       no options ' 
     PRINT *,'      '
     PRINT *,'     REQUIRED FILES :'
     PRINT *,'       none' 
     PRINT *,'      '
     PRINT *,'     OUTPUT : '
     PRINT *,'       netcdf file : <IN-file>2 '
     PRINT *,'         variables : same as input file'
     PRINT *,' '
     PRINT *,'     SEE ALSO :'
     PRINT *,'         cdfchgrid ic_correct_zero'
     PRINT *,'      '
     STOP
  ENDIF

  ! browse command line
  CALL getarg(1, cf_in )
  CALL getarg(2, cv_in )
  CALL getarg(3, cdum ) ; READ(cdum,*) slimit
  CALL getarg(4, cdum ) ; 
  ! set linout flag
  IF ( cdum == 'in' ) THEN
       linout=.true.
  ELSE
       linout=.false.
  ENDIF
  CALL getarg(5, cdum ) ; READ(cdum,*) rlonmin
  CALL getarg(6, cdum ) ; READ(cdum,*) rlonmax
  CALL getarg(7, cdum ) ; READ(cdum,*) rlatmin
  CALL getarg(8, cdum ) ; READ(cdum,*) rlatmax

  ! copy original file
  cmd='cp '//TRIM(cf_in)//' '//TRIM(cf_in)//'2'
  CALL system ( TRIM(cmd) )
  cf_in=TRIM(cf_in)//'2'

  PRINT *, ' WORKING with ',TRIM(cf_in) 
  PRINT *,'      process ',TRIM(cv_in),' for values <= ', slimit
  PRINT *,'      Domain limit : ', rlonmin, rlonmax, rlatmin, rlatmax

  ierr = NF90_OPEN(cf_in,NF90_WRITE, ncid )
  ierr = NF90_INQ_DIMID (ncid, 'x', id) ; ierr = NF90_INQUIRE_DIMENSION(ncid, id, len=npiglo)
  ierr = NF90_INQ_DIMID (ncid, 'y', id) ; ierr = NF90_INQUIRE_DIMENSION(ncid, id, len=npjglo)
  ierr = NF90_INQ_DIMID (ncid, 'deptht', id) ; ierr = NF90_INQUIRE_DIMENSION(ncid, id, len=npk)
  ierr = NF90_INQ_DIMID (ncid, 'time_counter', id) ; ierr = NF90_INQUIRE_DIMENSION(ncid, id, len=npt)

  ALLOCATE ( v3d( npiglo, npjglo,npk) ,bubble( 61,61,7), lmask(61,61,7), v2d( npiglo,npk) )
  ALLOCATE ( rlon( npiglo, npjglo), rlat( npiglo,npjglo) )

  ierr = NF90_INQ_VARID(ncid, 'nav_lon', id ) ; ierr=NF90_GET_VAR(ncid, id, rlon ) ; print *, NF90_STRERROR(ierr)
  ierr = NF90_INQ_VARID(ncid, 'nav_lat', id ) ; ierr=NF90_GET_VAR(ncid, id, rlat ) ; print *, NF90_STRERROR(ierr)
  ierr = NF90_INQ_VARID(ncid, cv_in, id )
  ierr = NF90_GET_ATT(ncid,id,'_FillValue', spval)

  PRINT *,' SPVAL = ', spval

  DO jk=1,npk
     ierr = NF90_GET_VAR(ncid, id, v3d(:,:,jk), start=(/1,1,jk,1/), count=(/npiglo,npjglo,1,1/) )
  ENDDO
  PRINT *,' salinite (643,3507,1)', v3d(643,3507,1)
  ! correction of 0 with vertical points
  DO jj=1,npjglo
     v2d(:,:) = v3d(:,jj,:)
     DO ji=1, npiglo
        IF ( lpoint_in(ji,jj) == linout  ) THEN
           DO jk = 2, npk
              IF ( v2d(ji,jk) <= slimit ) THEN
                 IF (v2d (ji,jk-1) > slimit .AND. v2d (ji,jk-1) /= spval ) THEN
                   PRINT '(a,3i5,f8.3,a,f8.3)', 'point ', ji,jj,jk, v2d(ji,jk),' replaced by ', v2d (ji,jk-1)
                    v2d(ji,jk) = v2d (ji,jk-1)
                 ELSE
                   PRINT '(a,3i5,f8.3,a)', 'point ', ji,jj,jk, v2d(ji,jk),' still bad (1)'
                 ENDIF
              ENDIF
           ENDDO

           DO jk= npk-1, 1 , -1
              IF ( v2d(ji,jk) <= slimit ) THEN
                 IF (v2d (ji,jk+1) > slimit .AND. v2d (ji,jk+1) /= spval ) THEN
                   PRINT '(a,3i5,f8.3,a,f8.3)', 'point ', ji,jj,jk,v2d(ji,jk),' replaced by ',v2d(ji,jk+1)
                    v2d(ji,jk) = v2d (ji,jk+1)
                 ELSE
                   PRINT '(a,3i5,f8.3,a)', 'point ', ji,jj,jk,v2d(ji,jk),' still bad (2)'

                 ENDIF
              ENDIF
           ENDDO
        ENDIF
     ENDDO

        v3d(:,jj,:) = v2d(:,:)
  ENDDO
  PRINT *,' salinite (643,3507,1)', v3d(643,3507,1)

  ! now try to fix remaining 0 with 3d bubble

  DO jk=1,npk
     DO jj=1, npjglo
        DO ji=1, npiglo
           IF ( lpoint_in(ji,jj) == linout ) THEN
              IF ( v3d(ji,jj,jk) <=slimit  )  THEN
                 i1=MAX(1, ji-30) ; i2=MIN(npiglo,ji+30) ; ni=i2-i1+1
                 j1=MAX(1, jj-30) ; j2=MIN(npjglo,jj+30) ; nj=j2-j1+1
                 k1=MAX(1, jk-3 ) ; k2=MIN(npk, jk +3 )  ; nk=k2-k1+1
                 bubble(:,:,:) = spval ; lmask=.TRUE.
                 bubble(1:ni,1:nj,1:nk) = v3d(i1:i2,j1:j2,k1:k2 )
                 WHERE (bubble <= slimit  .OR. bubble == spval ) lmask=.FALSE.
                 IF ( count(lmask) == 0 ) THEN
                    ! no good points in the bubble, set value to slimit
                    PRINT '(a,3i5,f8.3,a,f8.3)', 'point ', ji,jj,jk,v3d(ji,jj,jk),' defaulted to ',slimit
                    v3d(ji,jj,jk) = slimit+0.0001
                 ELSE
                    goodval=-8888.
                    goodval=MAXVAL(bubble, mask=lmask)
                    IF (goodval /= -8888.) THEN
                       PRINT '(a,3i5,f8.3,a,f8.3)', 'point ', ji,jj,jk,v3d(ji,jj,jk),' replaced by ', goodval
                       v3d(ji,jj,jk) = goodval
                    ELSE
                       PRINT '(a,3i5,f8.3,a)', 'point ', ji,jj,jk,v3d(ji,jj,jk),'  still bad (3)'
                    ENDIF
                 ENDIF
              ENDIF
           ENDIF
        ENDDO
     ENDDO
  ENDDO
  PRINT *, ' LEVEL ',npk,' set to ',spval
  v3d(:,:,npk) = spval
  PRINT *,' salinite (643,3507,1)', v3d(643,3507,1)

     DO jk=1,npk
        ierr = NF90_PUT_VAR( ncid, id, v3d(:,:,jk), start=(/ 1,1,jk,1 /), count=(/npiglo,npjglo,1,1 /) )
        PRINT *, 'Write level',jk,' ',TRIM(NF90_STRERROR(ierr))
     ENDDO
  ierr =NF90_CLOSE(ncid)

CONTAINS
  LOGICAL FUNCTION lpoint_in (ki,kj)
    !!---------------------------------------------------------------------
    !!                  ***  FUNCTION lpoint_in ***
    !!
    !! ** Purpose :   set to true if ki,kj in the zone 
    !!
    !! ** Method  :  use global arrays rlon, rlat to work with 
    !!
    !!----------------------------------------------------------------------
    INTEGER, INTENT(in) :: ki,kj

    lpoint_in = .FALSE.
    IF ( (rlon(ki,kj) > rlonmin) .AND. (rlon(ki,kj) < rlonmax ) .AND. &
         &   (rlat(ki,kj) > rlatmin) .AND. (rlat(ki,kj) < rlatmax ) ) THEN
       lpoint_in = .TRUE.
    ENDIF
  END FUNCTION lpoint_in

END PROGRAM ic_correct_low
