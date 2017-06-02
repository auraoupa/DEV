PROGRAM bdy_interp_v
  !!======================================================================
  !!                     ***  PROGRAM  bdy_interp_v ***
  !!=====================================================================
  !!  ** Purpose :  Interpolate normal velocities at high resolution
  !!                from low resolution repeated values file.
  !!
  !!  ** Method  :  Intepolate velocities such as to conserve volume
  !!                transport and avoiding jumps in the vorticity
  !!
  !! History : 1.0  : 04/2016  : J.M. Molines : Original code
  !!----------------------------------------------------------------------
   USE netcdf

  !!----------------------------------------------------------------------
  !! DATA_TOOLS/BDY
  !! $Id$
  !! Copyright (c) 2012, J.-M. Molines
  !! Software governed by the CeCILL licence (Licence/DATA_TOOLS_CeCILL.txt)
  !!----------------------------------------------------------------------
   IMPLICIT NONE

   INTEGER, PARAMETER :: jpiloc=15  ! resolution factor between low and high resolution
   ! definition of the bdy in the refined grid
   CHARACTER(LEN=1)   :: ctypebdy = 'S'
   INTEGER            :: nbdybeg = 81  ! 
   INTEGER            :: nbdyind = 2   !

   INTEGER :: ji,jk,jt, jbloc, jloc      ! dummy loop index
   INTEGER :: ii, ii0, ibloc, nbloc
   INTEGER :: iargc, narg, ijarg
   INTEGER :: npi, npk, npt 
   INTEGER :: ncid, nchgr, ncout, ierr, idv, idimd
   ! lapack solver variables
   REAL(KIND=8), DIMENSION(:,:), ALLOCATABLE :: A
   REAL(KIND=8), DIMENSION(:),   ALLOCATABLE :: B
   INTEGER                    :: iok
   INTEGER, DIMENSION(jpiloc) :: ipiv

   REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: evel, ef, xv, xf
   REAL(KIND=8), DIMENSION(:), ALLOCATABLE :: vo, vi, rdvdxo, rdvdxi

   CHARACTER(LEN=80) :: cf_hgr='NATL60_v4.1_cdf_mesh_hgr.nc'
   CHARACTER(LEN=80) :: cf_bdy
   CHARACTER(LEN=80) :: cf_out
   CHARACTER(LEN=80) :: cv_vel, cv_evel, cv_ef, cv_dep
   CHARACTER(LEN=80) :: cdum
  !!----------------------------------------------------------------------
   narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' usage :  bdy_interp_v  -type [N|S|E|W] -beg nbdybeg -index nbdyind ...'
     PRINT *, '          -bdy bdy_original_file -hgr Mesh_hgr_file '
     PRINT *,'      '
     PRINT *,'     PURPOSE :'
     PRINT *,'        Transform piece-wise constant bdy velocity file to continuous file'
     PRINT *,'      '
     PRINT *,'     ARGUMENTS :'
     PRINT *,'       -type [N|S|E|W] : indicate the kind of boundary corresponding to the'
     PRINT *,'             input bdy file.' 
     PRINT *,'       -beg nbdybeg : indicate nbdybeg (as in nambdy_index)'
     PRINT *,'       -index nbdyind : indicate nbdyind (as in nambdy_index)'
     PRINT *,'       -bdy bdy_file : indicate the name of the original bdy file'
     PRINT *,'       -hgr hgr_file : indicate the name of the mesh_hgr file'
     PRINT *,'      '
     PRINT *,'      '
     PRINT *,'     REQUIRED FILES :'
     PRINT *,'         Those specified on the command line'
     PRINT *,'      '
     PRINT *,'     OUTPUT : '
     PRINT *,'       netcdf file :  input_file.interp'
     PRINT *,'         variables : same as input file'
     PRINT *,'      '
     PRINT *,'      '
     STOP
  ENDIF
   ijarg = 1
   DO WHILE ( ijarg <= narg )
      CALL getarg(ijarg, cdum) ; ijarg = ijarg + 1
      SELECT CASE ( cdum )
      CASE ('-type' ) ; CALL getarg(ijarg, ctypebdy) ; ijarg = ijarg + 1 
      CASE ('-beg'  ) ; CALL getarg(ijarg, cdum) ; ijarg = ijarg + 1 ; READ(cdum,*) nbdybeg
      CASE ('-index') ; CALL getarg(ijarg, cdum) ; ijarg = ijarg + 1 ; READ(cdum,*) nbdyind
      CASE ('-bdy'  ) ; CALL getarg(ijarg, cf_bdy) ; ijarg = ijarg + 1
      CASE ('-hgr'  ) ; CALL getarg(ijarg, cf_hgr) ; ijarg = ijarg + 1
      CASE DEFAULT 
         PRINT *,' Option ', TRIM(cdum), 'not understood !'
         STOP
     END SELECT
   ENDDO
   SELECT CASE ( ctypebdy )
   CASE ('S', 'N') ; cv_vel='vomecrty' ; cv_evel='e1v' ; cv_ef='e1f' ; cv_dep='depthv'
   CASE ('E', 'W') ; cv_vel='vozocrtx' ; cv_evel='e2u' ; cv_ef='e2f' ; cv_dep='depthu'
   CASE DEFAULT
        PRINT *,' Type of boundary ', TRIM(ctypebdy),' unknown !'
        STOP
   END SELECT

   cf_out=TRIM(cf_bdy)//'.interp'
   ! copy original file to cf_out
   CALL SYSTEM ('cp '//TRIM(cf_bdy)//' '//TRIM(cf_out) )

   ! Open bdy original file and extract dimensions
   ierr = NF90_OPEN(cf_bdy,NF90_NOWRITE, ncid)
   ierr = NF90_INQ_DIMID(ncid, 'xt',           idimd) ; ierr = NF90_INQUIRE_DIMENSION(ncid,idimd, len=npi )
   ierr = NF90_INQ_DIMID(ncid, cv_dep,         idimd) ; ierr = NF90_INQUIRE_DIMENSION(ncid,idimd, len=npk )
   ierr = NF90_INQ_DIMID(ncid, 'time_counter', idimd) ; ierr = NF90_INQUIRE_DIMENSION(ncid,idimd, len=npt )
   PRINT *, " NPI = ", npi
   PRINT *, " NPK = ", npk
   ! Allocation for rdvdxo is far too big ... but does not matter !
   ALLOCATE ( evel(npi), ef(npi),xv(npi), xf(npi) )
   ALLOCATE ( vo(npi), vi(npi),rdvdxo(npi), rdvdxi(npi) )

   ALLOCATE( A(jpiloc,jpiloc), B(jpiloc) )
   ! Open output file 
   ierr = NF90_OPEN(cf_out, NF90_WRITE, ncout)

   ! Open hgr file and read corresponding metrics
   ierr = NF90_OPEN(cf_hgr,NF90_NOWRITE, nchgr)
   SELECT CASE (ctypebdy )
   CASE ('S', 'N' )
   ierr = NF90_INQ_VARID(nchgr,cv_evel, idv) ; ierr = NF90_GET_VAR(nchgr, idv,evel(:), start=(/1,nbdyind,1/), count=(/npi,1,1/) )
   ierr = NF90_INQ_VARID(nchgr,cv_ef  , idv) ; ierr = NF90_GET_VAR(nchgr, idv,ef(:), start=(/1,nbdyind,1/), count=(/npi,1,1/) )
   CASE ('E', 'W' )
   ierr = NF90_INQ_VARID(nchgr,cv_evel, idv) ; ierr = NF90_GET_VAR(nchgr, idv,evel(:), start=(/nbdyind,1,1/), count=(/1,npi,1/) )
   ierr = NF90_INQ_VARID(nchgr,cv_ef  , idv) ; ierr = NF90_GET_VAR(nchgr, idv,ef(:), start=(/nbdyind,1,1/), count=(/1,npi,1/) )
   END SELECT
   ierr = NF90_CLOSE(nchgr)

   ! define abscisa of v and f points
   xv(1)=ef(1)/2.d0
   xf(1)=evel(1)
   DO ji=2, npi
      xv(ji) = xv(ji-1)+ ef(ji-1)
      xf(ji) = xf(ji-1)+ evel(ji)
   ENDDO

   DO jt=1, npt
     DO jk=1, npk
       PRINT *,' WORK for jt, jk', jt, jk
       ierr = NF90_INQ_VARID(ncid,cv_vel, idv) ; ierr = NF90_GET_VAR(ncid, idv,vo(:), start=(/1,1,jk,jt/), count=(/npi,1,1,1/) )
       vi(:) = 0.d0
       ! compute rdvdxo 
       rdvdxo(:)=0.d0
       ibloc=0
       DO ji= nbdybeg-1, npi-1,jpiloc
          ii  = (ji + (jpiloc+1)/2 )
          ii0 = (ji - (jpiloc-1)/2 )
          IF ( ii > npi ) CYCLE
          ibloc=ibloc+1

          IF ( vo(ji+1) == 0. .OR. vo(ji) == 0. ) THEN
            rdvdxo(ibloc)= 0.d0
          ELSE
            rdvdxo(ibloc)= (vo(ji+1) -vo(ji) )/(xv(ii) -xv (ii0) )
          ENDIF
       ENDDO
       nbloc=ibloc
       DO jbloc=1, nbloc
         ii=nbdybeg+(jbloc-1)*jpiloc
         DO jloc=1,jpiloc-1
           rdvdxi(jloc)=(xf(ii+jloc-1) - xf(ii -1) )/( xf(ii+jpiloc-1) - xf(ii -1 ) ) *( rdvdxo(jbloc+1) - rdvdxo(jbloc) ) + rdvdxo(jbloc)
         ENDDO
         ! build A matrix
         A(:,:)=0.d0
         DO jloc=1, jpiloc-1
            A(jloc,jloc) = -1.d0
            A(jloc,jloc+1)= 1.d0
         ENDDO
         A(jpiloc, 1:jpiloc) = evel(ii:ii+jpiloc-1)

         ! Build RHS (B matrix)
         DO jloc=1, jpiloc-1
            B(jloc)=ef(ii+jloc-1)*rdvdxi(jloc)
         ENDDO
            B(jpiloc) = vo(ii)*(xf(ii+jpiloc-1)  - xf(ii-1))

         ! Solve A.X=B
         CALL DGESV(jpiloc,1,A,jpiloc,ipiv,B, jpiloc, iok)

         IF ( iok /= 0 ) THEN
           PRINT *, 'problem in bloc jbloc'
         ELSE
           DO jloc=1,jpiloc
              vi(ii+jloc-1) = B(jloc)
           ENDDO
         ENDIF
       ENDDO ! end blocs at jk, jt
       ierr = NF90_PUT_VAR( ncout, idv, vi(:), start=(/1,1,jk,jt/), count=(/npi,1,1,1/) ) 
     ENDDO
   ENDDO
   ierr = NF90_CLOSE(ncid)
   ierr = NF90_CLOSE(ncout)

END PROGRAM bdy_interp_v
