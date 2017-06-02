PROGRAM ic_correct_zero

USE netcdf
IMPLICIT NONE

INTEGER :: ji,jj,jk,jt
INTEGER :: ierr, ncid, id
INTEGER :: i1,i2,  j1,j2,  k1,k2,  ni,nj,nk
INTEGER :: iargc, narg, ijarg
INTEGER(KIND=4) :: npiglo, npjglo, npk, npt

REAL(KIND=4)  :: goodval, spval
REAL(KIND=4), DIMENSION (:,:,:), ALLOCATABLE :: v3d, bubble
REAL(KIND=4), DIMENSION (:,:), ALLOCATABLE :: v2d

CHARACTER(LEN=255) :: cf_in
CHARACTER(LEN=255) :: cv_in
CHARACTER(LEN=255) :: cdum, cmd

LOGICAL :: llcor
LOGICAL, DIMENSION(:,:,:), ALLOCATABLE :: lmask

narg=iargc()
IF ( narg /= 2 ) THEN
  PRINT *, ' Usage : ic_correct_zero IN-file IN-var '
  PRINT *, '  IN-file : name of file to correct ' 
  PRINT *, '  IN-var : name of variable to correct ' 
  PRINT *, '  This program works on a copy of the input-file (2 append to file name)'
  STOP
ENDIF

CALL getarg(1, cf_in )
CALL getarg(2, cv_in )

cmd='cp '//TRIM(cf_in)//' '//TRIM(cf_in)//'2'
CALL system ( TRIM(cmd) )
cf_in=TRIM(cf_in)//'2'
PRINT *, ' WORKING with ',TRIM(cf_in) 

ierr = NF90_OPEN(cf_in,NF90_WRITE, ncid )

ierr = NF90_INQ_DIMID (ncid, 'x', id) ; ierr = NF90_INQUIRE_DIMENSION(ncid, id, len=npiglo)
ierr = NF90_INQ_DIMID (ncid, 'y', id) ; ierr = NF90_INQUIRE_DIMENSION(ncid, id, len=npjglo)
ierr = NF90_INQ_DIMID (ncid, 'deptht', id) ; ierr = NF90_INQUIRE_DIMENSION(ncid, id, len=npk)
ierr = NF90_INQ_DIMID (ncid, 'time_counter', id) ; ierr = NF90_INQUIRE_DIMENSION(ncid, id, len=npt)

ALLOCATE ( v3d( npiglo, npjglo,npk) ,bubble( 61,61,7), lmask(61,61,7), v2d( npiglo,npk) )

ierr = NF90_INQ_VARID(ncid, cv_in, id )
ierr = NF90_GET_ATT(ncid,id,'_FillValue', spval)
print *,' SPVAL = ', spval

DO jk=1,npk
   ierr = NF90_GET_VAR(ncid, id, v3d(:,:,jk), start=(/1,1,jk,1/), count=(/npiglo,npjglo,1,1/) )
ENDDO
! correction of 0 with vertical points
DO jj=1,npjglo
  v2d(:,:) = v3d(:,jj,:)
     DO ji=1, npiglo
     DO jk = 2, npk
       IF ( v2d(ji,jk) == 0. ) THEN
          IF (v2d (ji,jk-1) /= 0. .AND. v2d (ji,jk-1) /= spval ) THEN
              v2d(ji,jk) = v2d (ji,jk-1)
              llcor=.true.
              PRINT *, ' point ', ji,jj,jk, ' replaced by ',  v2d (ji,jk-1)
          ELSE
              PRINT *, ' point ', ji,jj,jk, ' still bad ... '
          ENDIF
       ENDIF
     ENDDO
     DO jk= npk-1, 1 , -1
       IF ( v2d(ji,jk) == 0. ) THEN
          IF (v2d (ji,jk+1) /= 0. .AND. v2d (ji,jk+1) /= spval ) THEN
              v2d(ji,jk) = v2d (ji,jk+1)
              llcor=.true.
              PRINT *, ' point ', ji,jj,jk, ' 2 replaced by ',  v2d (ji,jk+1)
          ELSE
              PRINT *, ' point ', ji,jj,jk, ' 2 still bad ... '
          ENDIF
       ENDIF
     ENDDO
   ENDDO

   IF (llcor ) THEN
      v3d(:,jj,:) = v2d(:,:)
   ENDIF
ENDDO

! now try to fix remaining 0 with 3d bubble

DO jk=1,npk
   DO jj=1, npjglo
      DO ji=1, npiglo
      llcor = .false.
      IF ( v3d(ji,jj,jk) == 0 )  THEN
        i1=MAX(1, ji-30) ; i2=MIN(npiglo,ji+30) ; ni=i2-i1+1
        j1=MAX(1, jj-30) ; j2=MIN(npjglo,jj+30) ; nj=j2-j1+1
        k1=MAX(1, jk-3 ) ; k2=MIN(npk, jk +3 )  ; nk=k2-k1+1
        bubble(:,:,:) = spval ; lmask=.true.
        bubble(1:ni,1:nj,1:nk) = v3d(i1:i2,j1:j2,k1:k2 )
        WHERE (bubble == 0. .OR. bubble == spval ) lmask=.false.
        goodval=-8888.
        goodval=MAXVAL(bubble, mask=lmask)
        IF (goodval /= -8888.) THEN
          v3d(ji,jj,jk) = goodval
          llcor=.true.
          print *, 'point ', ji,jj,jk,' replaced by ', goodval
        ELSE
          print *, 'point ', ji,jj,jk,'  still bad '
        ENDIF
      ENDIF
      ENDDO
   ENDDO
ENDDO
    PRINT *, ' LEVEL ',npk,' set to ',spval
    v3d(:,:,npk) = spval

    IF ( llcor) THEN
      DO jk=1,npk
      ierr = NF90_PUT_VAR( ncid, id, v3d(:,:,jk), start=(/ 1,1,jk,1 /), count=(/npiglo,npjglo,1,1 /) )
      PRINT *, 'Write level',jk,' ',TRIM(NF90_STRERROR(ierr))
      ENDDO
    ENDIF
  ierr =NF90_CLOSE(ncid)

END PROGRAM ic_correct_zero
