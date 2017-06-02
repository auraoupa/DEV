PROGRAM changezgrlev
  !!======================================================================
  !!                     ***  PROGRAM  changezgrlev  ***
  !!=====================================================================
  !!  ** Purpose : Convert an input file with few levels to an output file
  !!               with more levels
  !!
  !!  ** Method  : Read input data and input zgr, read output zgr and produce
  !!               linerly interpolated file.
  !!
  !! History : 1.0  : 07/2009  : J.M. Molines : Original code
  !!           2.0  : 02/2012  : J.M. Molines : Doctor norm + Lic.
  !!----------------------------------------------------------------------
  !!----------------------------------------------------------------------
  !!   routines      : description
  !!   create        : create output data, copy all var except 3D
  !!   inidom        : Divide the global domain into sub domains 
  !!   chkfile       : check existence of a file
  !!----------------------------------------------------------------------
  USE netcdf
  !!----------------------------------------------------------------------
  !! BUILDNC_2.0 , MEOM 2012
  !! $Id: changezgr.f90 1134 2012-04-21 07:44:42Z molines $
  !! Copyright (c) 2012, J.-M. Molines
  !! Software governed by the CeCILL licence (Licence/BUILDNCCeCILL.txt)
  !!----------------------------------------------------------------------
  IMPLICIT NONE

  INTEGER(KIND=4) :: ji, jj, jk, jt, jvar                  ! dummy loop index
  INTEGER(KIND=4) :: jki, jk0, jk1, jk2, jni, jnj          !   "    "    "
  INTEGER(KIND=4) :: narg, iargc, ijarg                    ! command line 
  INTEGER(KIND=4) :: ik, ibloc
  INTEGER(KIND=4) :: npiglo, npjglo, nt
  INTEGER(KIND=4) :: jpki,   jpko
  INTEGER(KIND=4) :: jpni=10,jpnj=10
  INTEGER(KIND=4) :: nlci, nlcj, nldi, nldj
  INTEGER(KIND=4) :: istatus, id, ido
  INTEGER(KIND=4) :: nczgri, nczgro
  INTEGER(KIND=4) :: ncidi, ncido
  INTEGER(KIND=4) :: n_0d, n_1d, n_2d, n_3d, n_4d
  INTEGER(KIND=4), DIMENSION(10) :: npdim                  ! value of each dimension
  INTEGER(KIND=4), DIMENSION(50) :: npdim1d                ! size of 1d variable
  INTEGER(KIND=4), DIMENSION(50) :: ido_0d, ido_1d, ido_2d, ido_3d, ido_4d
  INTEGER(KIND=4), DIMENSION(50) :: idi_0d, idi_1d, idi_2d, idi_3d, idi_4d
  INTEGER(KIND=4), DIMENSION(:),   ALLOCATABLE :: npi,npj, nimpp, njmpp

  REAL(KIND=8)                                :: dzh, dverydeep=8000.d0, ddz
  REAL(KIND=8)                                :: dalpha
  REAL(KIND=8), DIMENSION(:,:,:), ALLOCATABLE :: dvi, dvo

  REAL(KIND=4), DIMENSION(:),     ALLOCATABLE :: gdepi_0, gdepo_0

  CHARACTER(LEN=80) :: cf_in
  CHARACTER(LEN=80) :: cf_out
  CHARACTER(LEN=80) :: cf_zgro
  CHARACTER(LEN=80) :: cl_dum
  CHARACTER(LEN=80), DIMENSION(50) :: c_0d, c_1d, c_2d, c_3d, c_4d

  LOGICAL                :: ll_mask =.true.
  LOGICAL                :: ll_chk  =.false.
  LOGICAL, DIMENSION(50) :: l_1dtim
  !!----------------------------------------------------------------------
  ! Browse command line
  narg=iargc()
  IF ( narg == 0 ) THEN
     PRINT *,' usage : changezgrlev -f IN-file -o OUT-file  -zo OUT-zgr ...' 
     PRINT *,'        [-subdomain  jpni jpnj] '
     PRINT *,'      '
     PRINT *,'     PURPOSE :'
     PRINT *,'       This program is used for changing the number of vertical levels in ' 
     PRINT *,'       a file. Vertical grids descriptions are taken from mesh_zgr like '
     PRINT *,'       files for both the input and output grid. Output file is masked by'
     PRINT *,'       default but this standard behaviour can be overridden  specifying '
     PRINT *,'       the -nomask option.'
     PRINT *,'      '
     PRINT *,'     ARGUMENTS :'
     PRINT *,'       -f IN-file  : indicate the name of the input file (netcdf)' 
     PRINT *,'       -o OUT-file : indicate the name of the vertically interpolated '
     PRINT *,'               output file (netcdf)' 
     PRINT *,'       -zo OUT-zgr : indicate the name of file with gdept_0 out'
     PRINT *,'      '
     PRINT *,'     OPTIONS :'
     PRINT *,'       -nomask    : overrid the masking of output file' 
     PRINT *,'       -subdomain jpni jpnj : specify a domain decomposition for saving'
     PRINT *,'              memory. Note that the code is not parallel (for now !) and'
     PRINT '(a,2i3)','               subdomains are treated sequentially. Default is ', jpni, jpnj
     PRINT *,'      '
     PRINT *,'     REQUIRED FILES :'
     PRINT *,'       Only those specified on the command line' 
     PRINT *,'      '
     PRINT *,'     OUTPUT : '
     PRINT *,'       netcdf file : OUT-file, whose name is passed on the command line'
     PRINT *,'         variables : the same as the input files, only 3D are modified'
     PRINT *,'      '
     STOP
  ENDIF

  ijarg = 1
  DO WHILE ( ijarg <= narg )
    CALL getarg( ijarg, cl_dum ) ; ijarg = ijarg + 1
    SELECT CASE ( cl_dum )
    CASE ( '-f' ) ; CALL getarg( ijarg, cf_in  ) ; ijarg = ijarg + 1
    CASE ( '-o' ) ; CALL getarg( ijarg, cf_out ) ; ijarg = ijarg + 1
    CASE ( '-zo') ; CALL getarg( ijarg, cf_zgro) ; ijarg = ijarg + 1
    CASE ( '-nomask'   ) ; ll_mask = .false.
    CASE ( '-subdomain') ; CALL getarg( ijarg, cl_dum) ; ijarg = ijarg + 1 ; READ(cl_dum,*) jpni
                         ; CALL getarg( ijarg, cl_dum) ; ijarg = ijarg + 1 ; READ(cl_dum,*) jpnj
    CASE DEFAULT ; PRINT *, 'ERROR : unknown option: ', TRIM(cl_dum)
    END SELECT
  ENDDO
  l_1dtim(:) = .TRUE.

  ! check files
  ll_chk =.false.
  ll_chk = ll_chk .OR. chkfile (cf_in   ) 
  ll_chk = ll_chk .OR. chkfile (cf_zgro ) 
  IF ( ll_chk ) STOP  ! missing files

  ! prepare domain decomposition
  ALLOCATE ( npi(jpni),   npj(jpnj)   )
  ALLOCATE ( nimpp(jpni), njmpp(jpnj) )

  ! Allocation :
  istatus = NF90_OPEN (cf_in, NF90_NOWRITE, nczgri )
  istatus = NF90_INQ_DIMID(nczgri,'x'     ,id) ; istatus = NF90_INQUIRE_DIMENSION(nczgri,id, len=npiglo )
  istatus = NF90_INQ_DIMID(nczgri,'y'     ,id) ; istatus = NF90_INQUIRE_DIMENSION(nczgri,id, len=npjglo )
  istatus = NF90_INQ_DIMID(nczgri,'deptht',id) ; istatus = NF90_INQUIRE_DIMENSION(nczgri,id, len=jpki   )
  istatus = NF90_INQ_DIMID(nczgri,'time_counter',id) ; istatus = NF90_INQUIRE_DIMENSION(nczgri,id, len=nt   )

  istatus = NF90_OPEN (cf_zgro, NF90_NOWRITE, nczgro )  ! only for jpko
  istatus = NF90_INQ_DIMID(nczgro,'z',id) ; istatus = NF90_INQUIRE_DIMENSION(nczgro,id, len=jpko )

  PRINT *, 'JPK_IN  = ', jpki
  PRINT *, 'JPK_OUT = ', jpko

  ALLOCATE ( gdepi_0(0:jpki+1) )
  ALLOCATE ( gdepo_0(jpko ) )
  istatus = NF90_INQ_VARID(nczgri, 'deptht', id) ; istatus = NF90_GET_VAR(nczgri, id, gdepi_0(1:jpki) )

  gdepi_0(0)=0.d0 ; gdepi_0(jpki+1)=dverydeep

  istatus = NF90_INQ_VARID(nczgro, 'gdept_0', id) ; istatus = NF90_GET_VAR(nczgro, id, gdepo_0 )

  ! use domain decomposition for saving memory
  CALL inidom

  CALL create(cf_out)  ! create cf_out, copy all variables from cfilin except 3D var, prepare empty 3D var
  ibloc=0
  PRINT *,' NT = ', nt

  DO jni=1, jpni
     DO jnj=1, jpnj
        ibloc = ibloc + 1
        ! size of local domain
        nlci = npi(jni)   ; nlcj = npj(jnj)
        nldi = nimpp(jni) ; nldj = njmpp(jnj)

        ALLOCATE ( dvi(nlci,nlcj,0:jpki+1) )
        ALLOCATE ( dvo(nlci,nlcj,jpko    ) )

        DO jvar=1,n_4d
           PRINT *, 'WORKING for block ', ibloc,' variable ', TRIM(c_4d(jvar) )
           id  = idi_4d(jvar)
           ido = ido_4d(jvar)  ! should be the same than id

         DO jt=1,nt
           istatus=NF90_GET_VAR(ncidi, id, dvi(:,:,1:jpki),start=(/nldi,nldj,1,jt/),count=(/nlci,nlcj,jpki,1/)  )
           dvi(:,:,0)      = dvi(:,:,1)
           dvi(:,:,jpki+1) = dvi(:,:,jpki)

           ! interpolation (no more masked values)         
           DO jk2=1, jpko
             DO ji=1,nlci
              DO jj=1,nlcj
                 dzh = gdepo_0(jk2) 
                 ! determine indices of surrounding levels in the IN-grid
                 DO jk=1, jpki + 1
                    IF ( dzh < gdepi_0(jk)  ) EXIT
                 ENDDO
                 jk0=jk-1 ; jk1 = jk  
                 ! perform interpolation between levels jk0 and jk1
                 ddz = ( gdepi_0(jk1) - gdepi_0(jk0))
                 IF ( ddz /= 0. ) THEN
                   dalpha= ( dzh - gdepi_0(jk0) )/ ddz
                 ELSE
                   dalpha = 0.d0
                 ENDIF
                 dvo(ji,jj,jk2)= dalpha * dvi(ji,jj,jk1) + (1. - dalpha ) * dvi(ji,jj,jk0) 
                 ! mask interpolated value unless -nomask option set
!                IF ( jk2 > mbathyo(ji,jj) .AND. ll_mask )  dvo(ji,jj,jk2) = 0.d0
              ENDDO
             ENDDO

           END DO

           istatus=NF90_PUT_VAR(ncido, ido, dvo,start=(/nldi,nldj,1,jt/),count=(/nlci,nlcj,jpko,1/)  )
         ENDDO  ! time loop
!          istatus=NF90_SYNC(ncido)

        END DO  ! jvar-loop
        DEALLOCATE ( dvi )
        DEALLOCATE ( dvo )
     END DO   ! subdomains loop I
  END DO ! subdomains loop J
  ! close files
  ! add z variable on output file
  DO jvar=1,n_1d 
   IF ( .NOT. l_1dtim(jvar)  ) THEN  ! false in case of level
     ido=ido_1d(jvar)
     istatus=NF90_PUT_VAR(ncido, ido, gdepo_0 )
   ENDIF
  END DO
  istatus=NF90_CLOSE(ncidi)
  istatus=NF90_CLOSE(ncido)

CONTAINS

  SUBROUTINE create (cd_file)
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE create  ***
    !!
    !! ** Purpose : Create output file and copy all variables from cf_in
    !!              except 3d var that will be interpolated
    !!
    !! ** Method  : Use many global variables (take care )
    !!
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*), INTENT(in) :: cd_file
    ! 
    INTEGER(KIND=4) :: jvar, jn, jid, jat
    INTEGER(KIND=4) :: istatus
    INTEGER(KIND=4) :: id, id_dum, il, id_unlim
    INTEGER(KIND=4) :: ndims, nvars, ndim, itype, natts
    INTEGER(KIND=4), DIMENSION(NF90_max_var_dims) :: idimids

    REAL(KIND=8)                              :: dl_0d
    REAL(KIND=8), DIMENSION( : ), ALLOCATABLE :: dl_1d
    REAL(KIND=8), DIMENSION( npiglo, npjglo ) :: dl_2d

    CHARACTER(LEN=80) :: cl_nam, cl_att
    !!----------------------------------------------------------------------
    ! open input file and set list of dimensions, variable, unlimited dim
    istatus=NF90_OPEN (cf_in,NF90_NOCLOBBER,ncidi)
    istatus=NF90_INQUIRE (ncidi, ndims, nvars, unlimitedDimId=id_unlim )
    DO jid=1, ndims
      istatus = NF90_INQUIRE_DIMENSION(ncidi, jid, len=npdim(jid) ) !  save dimensions in npdim
    ENDDO

    istatus=NF90_CREATE(cd_file,or(nf90_clobber,nf90_64bit_offset),ncido)

    ! scan dims from input file, change vertical to jpko, create dim in cf_out
    DO jid = 1, ndims
       istatus=NF90_INQUIRE_DIMENSION(ncidi, jid, name=cl_nam, len=il )
       IF ( cl_nam == 'z' .OR. cl_nam == 'nav_lev'      .OR. cl_nam == 'deptht' ) il=jpko
       IF ( cl_nam == 't' .OR. cl_nam == 'time_counter' .OR. cl_nam == 'time'   ) THEN 
         nt=il ; il=NF90_UNLIMITED  ! save nt and reset dim to unlimited
!        ALLOCATE ( dl_1d (nt ) )
       ENDIF
       istatus=NF90_DEF_DIM(ncido, cl_nam, il, id_dum)  ! id_dum never used later
    END DO

    ! set list of variables per type and define in output file
    n_0d = 0 ;  n_1d = 0 ; n_2d = 0  ;  n_3d = 0 ; n_4d = 0
    DO jvar=1, nvars  ! scan all variables in input file 
       istatus=NF90_INQUIRE_VARIABLE ( ncidi, jvar, cl_nam, itype, ndim, idimids, natts) 

       SELECT CASE ( ndim  )
       CASE ( 0 )   ! constant variables 
          n_0d         = n_0d + 1
          c_0d(n_0d)   = cl_nam
          istatus      = NF90_DEF_VAR(ncido, cl_nam,itype, id)
          DO jat=1,natts
            istatus=NF90_INQ_ATTNAME(ncidi,jvar,jat,cl_att)
            istatus=NF90_COPY_ATT(ncidi,jvar, cl_att, ncido,jvar)
          ENDDO

          ido_0d(n_0d) = id
          idi_0d(n_0d) = jvar
       CASE ( 1 )   !  T | Z 
          n_1d         = n_1d + 1
          c_1d(n_1d)   = cl_nam
          istatus      = NF90_DEF_VAR(ncido, cl_nam, itype, idimids(1:ndim), id)
          npdim1d(n_1d) = npdim(idimids(1) )  ! size of 1d variable (can be deptht, time,x, or y )
          DO jat=1,natts
            istatus=NF90_INQ_ATTNAME(ncidi,jvar,jat,cl_att)
            istatus=NF90_COPY_ATT(ncidi,jvar, cl_att, ncido,jvar)
          ENDDO
          ido_1d(n_1d) = id
          idi_1d(n_1d) = jvar
          IF( cl_nam == 'deptht' ) l_1dtim(n_1d)      = .FALSE.
!         IF ( idimids(1) == id_unlim ) l_1dtim(n_1d) = .TRUE.  ! flag time variable (unlimited dim)
       CASE ( 2 )    !  X Y 
          n_2d         = n_2d + 1
          c_2d(n_2d)   = cl_nam
          istatus      = NF90_DEF_VAR(ncido, cl_nam, itype, idimids(1:ndim), id)
          DO jat=1,natts
            istatus=NF90_INQ_ATTNAME(ncidi,jvar,jat,cl_att)
            istatus=NF90_COPY_ATT(ncidi,jvar, cl_att, ncido,jvar)
          ENDDO
          ido_2d(n_2d) = id
          idi_2d(n_2d) = jvar
       CASE ( 3 )    ! X Y T
          n_3d         = n_3d + 1
          c_3d(n_3d)   = cl_nam
          istatus      = NF90_DEF_VAR(ncido, cl_nam,itype,idimids(1:ndim),id)
          DO jat=1,natts
            istatus=NF90_INQ_ATTNAME(ncidi,jvar,jat,cl_att)
            istatus=NF90_COPY_ATT(ncidi,jvar, cl_att, ncido,jvar)
          ENDDO
          ido_3d(n_3d) = id
          idi_3d(n_3d) = jvar
       CASE ( 4 )    ! X Y Z T
          n_4d         = n_4d + 1
          c_4d(n_4d)   = cl_nam
!         itype=NF90_FLOAT    ! force FLOAT in any case ?
          istatus      = NF90_DEF_VAR(ncido, cl_nam,itype,idimids(1:ndim),id)
          DO jat=1,natts
            istatus=NF90_INQ_ATTNAME(ncidi,jvar,jat,cl_att)
            istatus=NF90_COPY_ATT(ncidi,jvar, cl_att, ncido,jvar)
          ENDDO
          ido_4d(n_4d) = id
          idi_4d(n_4d) = jvar
       END SELECT
    END DO
    istatus=NF90_ENDDEF (ncido )

    PRINT * ,' FILE ', TRIM(cd_file),' created with '
    PRINT * ,'  - ', n_0d ,' Constant variables '
    PRINT * ,'  - ', n_1d ,' 1D variables '
    PRINT * ,'  - ', n_2d ,' 2D variables '
    PRINT * ,'  - ', n_3d ,' 3D variables '
    PRINT * ,'  - ', n_4d ,' 4D variables '

    ! Copy all variables except those depending on z (ie 1d and 4d )
    DO jn=1, n_0d   ! ( - )
       istatus=NF90_GET_VAR(ncidi, idi_0d(jn), dl_0d )
       istatus=NF90_PUT_VAR(ncido, ido_0d(jn), dl_0d )   
    END DO

    DO jn=1, n_1d   ! ( T ) 
       IF ( l_1dtim(jn)  ) THEN    ! only time not depth
          ALLOCATE( dl_1d(npdim1d(jn) ) )
          istatus=NF90_GET_VAR(ncidi, idi_1d(jn), dl_1d )
          istatus=NF90_PUT_VAR(ncido, ido_1d(jn), dl_1d )
          DEALLOCATE( dl_1d )
       ENDIF
    END DO

    DO jn=1, n_2d   ! ( X Y )
       istatus=NF90_GET_VAR(ncidi, idi_2d(jn), dl_2d )
       istatus=NF90_PUT_VAR(ncido, ido_2d(jn), dl_2d )
    END DO

    DO jn=1, n_3d   ! ( X Y T ) 
       DO jt = 1, nt
         istatus=NF90_GET_VAR(ncidi, idi_3d(jn), dl_2d, start=(/1,1,jt/), count=(/npiglo,npjglo,1/)  )
         istatus=NF90_PUT_VAR(ncido, ido_3d(jn), dl_2d, start=(/1,1,jt/), count=(/npiglo,npjglo,1/)  )
       END DO
    END DO

  END SUBROUTINE create

  SUBROUTINE inidom
    !!---------------------------------------------------------------------
    !!                  ***  ROUTINE inidom  ***
    !!
    !! ** Purpose : Split global domain in subdomains. Set limit for each
    !!              subdomains. Note that no halo are defined.
    !!
    !! ** Method  : Euclidian division of the global size by the number
    !!              of domain in the I direction. The remainder is absorbed
    !!              on the right-most (resp. top-most) domains.
    !!
    !!----------------------------------------------------------------------
    INTEGER(KIND=4) :: ji,jj
    INTEGER(KIND=4) :: iir, ijr, ipii, ipjj
    !!----------------------------------------------------------------------
    ! npiglo = jpni * npi + zri ! zri < jpni
    ! npjglo = jpnj * npj + zrj ! zrj < jpnj
    ipii=npiglo / jpni ; iir = npiglo - jpni * ipii
    ipjj=npjglo / jpnj ; ijr = npjglo - jpnj * ipjj
    !  I- direction 
    nimpp(1)=1
    DO ji=1,jpni-iir
       npi(ji)= ipii
       IF ( ji < jpni )  nimpp(ji+1)=npi(ji) + nimpp(ji)
    ENDDO
    DO ji=jpni-iir+1, jpni
       npi(ji)=ipii + 1
       IF ( ji < jpni )  nimpp(ji+1)=npi(ji) + nimpp(ji)
    END DO

    !  J- direction 
    njmpp(1)=1
    DO jj=1,jpnj-ijr
       npj(jj)= ipjj
       IF ( jj < jpnj )  njmpp(jj+1)=npj(jj) + njmpp(jj)
    ENDDO
    DO jj=jpnj-ijr+1, jpnj
       npj(jj)=ipjj + 1
       IF ( jj < jpnj )  njmpp(jj+1)=npj(jj) + njmpp(jj)
    END DO
  END SUBROUTINE inidom

  LOGICAL FUNCTION chkfile (cd_file)
    !!---------------------------------------------------------------------
    !!                  ***  FUNCTION chkfile  ***
    !!
    !! ** Purpose :  Check if cd_file exists.
    !!               Return false if it exists, true if it does not
    !!               Do nothing is filename is 'none'
    !!
    !! ** Method  : Doing it this way allow statements such as
    !!              IF ( chkfile( cf_toto) ) STOP  ! missing file
    !!
    !!----------------------------------------------------------------------
    CHARACTER(LEN=*), INTENT(in) :: cd_file
    !
    LOGICAL                      :: ll_exist
    !!----------------------------------------------------------------------
    IF ( TRIM(cd_file) /= 'none')  THEN
       INQUIRE (file = TRIM(cd_file), EXIST=ll_exist)
       IF (ll_exist) THEN
          chkfile = .FALSE.
       ELSE
          PRINT *, ' File ',TRIM(cd_file),' is missing '
          chkfile = .TRUE.
       ENDIF
    ELSE
       chkfile = .FALSE.  ! 'none' file is not checked
    ENDIF

  END FUNCTION chkfile


END PROGRAM changezgrlev
