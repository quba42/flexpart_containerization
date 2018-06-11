!**********************************************************************
! Copyright 1998,1999,2000,2001,2002,2005,2007,2008,2009,2010         *
! Andreas Stohl, Petra Seibert, A. Frank, Gerhard Wotawa,             *
! Caroline Forster, Sabine Eckhardt, John Burkhart, Harald Sodemann   *
!                                                                     *
! This file is part of FLEXPART.                                      *
!                                                                     *
! FLEXPART is free software: you can redistribute it and/or modify    *
! it under the terms of the GNU General Public License as published by*
! the Free Software Foundation, either version 3 of the License, or   *
! (at your option) any later version.                                 *
!                                                                     *
! FLEXPART is distributed in the hope that it will be useful,         *
! but WITHOUT ANY WARRANTY; without even the implied warranty of      *
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the       *
! GNU General Public License for more details.                        *
!                                                                     *
! You should have received a copy of the GNU General Public License   *
! along with FLEXPART.  If not, see <http://www.gnu.org/licenses/>.   *
!**********************************************************************

subroutine readwind(indj,n,uuh,vvh,wwh)

  !***********************************************************************
  !*                                                                     *
  !*             TRAJECTORY MODEL SUBROUTINE READWIND                    *
  !*                                                                     *
  !***********************************************************************
  !*                                                                     *
  !*             AUTHOR:      G. WOTAWA                                  *
  !*             DATE:        1997-08-05                                 *
  !*             LAST UPDATE: 2000-10-17, Andreas Stohl                  *
  !*             CHANGE: 01/02/2001, Bernd C. Krueger, Variables tth and *
  !*                     qvh (on eta coordinates) in common block        *
  !*             CHANGE: 16/11/2005, Caroline Forster, GFS data          *
  !*             CHANGE: 11/01/2008, Harald Sodemann, Input of GRIB1/2   *
  !*                     data with the ECMWF grib_api library            *
  !*             CHANGE: 03/12/2008, Harald Sodemann, update to f90 with *
  !*                                 ECMWF grib_api                      *
  !*                                                                     *
  !***********************************************************************
  !*                                                                     *
  !* DESCRIPTION:                                                        *
  !*                                                                     *
  !* READING OF ECMWF METEOROLOGICAL FIELDS FROM INPUT DATA FILES. THE   *
  !* INPUT DATA FILES ARE EXPECTED TO BE AVAILABLE IN GRIB CODE          *
  !*                                                                     *
  !* INPUT:                                                              *
  !* indj               indicates number of the wind field to be read in *
  !* n                  temporal index for meteorological fields (1 to 3)*
  !*                                                                     *
  !* IMPORTANT VARIABLES FROM COMMON BLOCK:                              *
  !*                                                                     *
  !* wfname             File name of data to be read in                  *
  !* nx,ny,nuvz,nwz     expected field dimensions                        *
  !* nlev_ec            number of vertical levels ecmwf model            *
  !* uu,vv,ww           wind fields                                      *
  !* tt,qv              temperature and specific humidity                *
  !* ps                 surface pressure                                 *
  !*                                                                     *
  !***********************************************************************

  use grib_api
  use par_mod
  use com_mod
  use omp_lib

  implicit none
  

  !HSO  new parameters for grib_api
  integer :: ifile
  integer :: iret
  integer :: igrib
  
  !SH
  integer, dimension(1:100000) :: igribind
  integer :: inumfields,iextf,isubfield,chunksize,mynumthreads
  logical, parameter :: parallel = .true.
  !SH W fix 
  !0: fix by zeroes
  !1: fix by last valid value
  !2: no fix
  integer, parameter :: wfix = 0
  integer :: numpwmax, match_zlev
  !SH compactification with pointers
  real, dimension(:,:), pointer :: rarr2_pnt

  integer :: gribVer,parCat,parNum,typSurf,valSurf,discipl
  !HSO end edits
  real, target :: uuh(0:nxmax-1,0:nymax-1,nuvzmax)
  real, target :: vvh(0:nxmax-1,0:nymax-1,nuvzmax)
  real, target :: wwh(0:nxmax-1,0:nymax-1,nwzmax)
  integer :: ii,indj,i,j,k,n,levdiff2,ifield,iumax,iwmax

  ! NCEP
  integer :: numpt,numpu,numpv,numpw,numprh
  real :: help, temp, ew
  real :: elev
  real, target :: ulev1(0:nxmax-1,0:nymax-1),vlev1(0:nxmax-1,0:nymax-1)
  real, target :: tlev1(0:nxmax-1,0:nymax-1)
  real, target :: qvh2(0:nxmax-1,0:nymax-1)

  integer :: i180

  ! VARIABLES AND ARRAYS NEEDED FOR GRIB DECODING
  !HSO kept isec1, isec2 and zsec4 for consistency with gribex GRIB input

  integer :: isec2(3)
  integer, dimension(:,:), allocatable :: isec1_arr
  real(kind=4), dimension(:), allocatable :: xsec18_arr
  real(kind=4), dimension(:,:), allocatable :: zsec4_arr

  real(kind=4)   :: xaux,yaux,xaux0,yaux0
  real,parameter :: eps=spacing(2.0_4*360.0_4)
  real(kind=8)   :: xauxin,yauxin
  real(kind=4)   :: ewss(0:nxmax-1,0:nymax-1),nsss(0:nxmax-1,0:nymax-1)
  real :: plev1,hlev1,ff10m,fflev1

  logical :: hflswitch,strswitch

  !HSO  for grib api error messages
  character(len=24) :: gribErrorMsg = 'Error reading grib file'
  character(len=20) :: gribFunction = 'readwind_gfs'


  interface
    subroutine readwind_grib2_to_1(parcat_in,parnum_in,typsurf_in,valsurf_in,discipl_in,i6_out,i7_out,x8_out)
      implicit none
      integer, intent(in)       :: parcat_in,parnum_in,typsurf_in,valsurf_in,discipl_in
      integer, intent(out)      :: i6_out,i7_out
      real(kind=4), intent(out) :: x8_out
    end subroutine
  end interface

  write(*,*) 'enter readwind_gfs'

  if (parallel) then
    chunksize = 24
    mynumthreads = omp_get_max_threads()
  else
    chunksize = 24
    mynumthreads = 1
  end if

  numpwmax = 0

  hflswitch=.false.
  strswitch=.false.
  levdiff2=nlev_ec-nwz+1
  iumax=0
  iwmax=0

  i180=nint(180./dx)
  ! OPENING OF DATA FILE (GRIB CODE)

  !HSO
  call grib_open_file(ifile,path(3)(1:length(3)) &
         //trim(wfname(indj)),'r',iret)
  if (iret.ne.GRIB_SUCCESS) then
    write(*,*) ' #### FLEXPART MODEL ERROR! WINDFIELD         #### '
    write(*,*) ' #### ',wfname(indj),'                    #### '
    write(*,*) ' #### IS NOT GRIB FORMAT !!!                  #### '
    stop 'Execution terminated'
  endif
  !turn on support for multi fields messages
  call grib_multi_support_on

  numpt=0
  numpu=0
  numpv=0
  numpw=0
  numprh=0
  inumfields=0
  do
    inumfields=inumfields+1  
    call grib_new_from_file(ifile,igribind(inumfields),iret)
    if (iret.eq.GRIB_END_OF_FILE)  then
      inumfields=inumfields-1
      exit    ! EOF DETECTED
    elseif (iret.ne.GRIB_SUCCESS) then
      write(*,*) ' #### FLEXPART MODEL ERROR! WINDFIELD         #### '
      write(*,*) ' #### ',wfname(indj),'                    #### '
      write(*,*) ' #### IS NOT GRIB FORMAT !!!                  #### '
      stop 'Execution terminated'
    endif
  end do

  allocate(isec1_arr(1:chunksize,8))
  allocate(xsec18_arr(1:chunksize))
  allocate(zsec4_arr(1:jpunp,1:chunksize))

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! numfield = 1 w/ preparations  !!!!!!!!
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  write(*,*) 'readwind_gfs number of fields:',inumfields

  ifield=1
  isubfield=1

  !first see if we read GRIB1 or GRIB2
  call grib_get_int(igribind(ifield),'editionNumber',gribVer,iret)
  call grib_check(iret,gribFunction,gribErrorMsg)
  
  if (gribVer.eq.1) then ! GRIB Edition 1
    call grib_get_int(igribind(ifield),'indicatorOfParameter',isec1_arr(isubfield,6),iret)
    call grib_check(iret,gribFunction,gribErrorMsg)
    call grib_get_int(igribind(ifield),'indicatorOfTypeOfLevel',isec1_arr(isubfield,7),iret)
    call grib_check(iret,gribFunction,gribErrorMsg)
    call grib_get_int(igribind(ifield),'level',isec1_arr(isubfield,8),iret)
    call grib_check(iret,gribFunction,gribErrorMsg)
    xsec18_arr(ifield) = real(isec1_arr(isubfield,8))
  else ! GRIB Edition 2
    call grib_get_int(igribind(ifield),'discipline',discipl,iret)
    call grib_check(iret,gribFunction,gribErrorMsg)
    call grib_get_int(igribind(ifield),'parameterCategory',parCat,iret)
    call grib_check(iret,gribFunction,gribErrorMsg)
    call grib_get_int(igribind(ifield),'parameterNumber',parNum,iret)
    call grib_check(iret,gribFunction,gribErrorMsg)
    call grib_get_int(igribind(ifield),'typeOfFirstFixedSurface',typSurf,iret)
    call grib_check(iret,gribFunction,gribErrorMsg)
    call grib_get_int(igribind(ifield),'scaledValueOfFirstFixedSurface', valSurf,iret)
    call grib_check(iret,gribFunction,gribErrorMsg)
    !convert
    isec1_arr(isubfield,6:8) = -1
    xsec18_arr(isubfield) = -1.0
    call readwind_grib2_to_1(parCat,parNum,typSurf,valSurf,discipl,isec1_arr(isubfield,6),isec1_arr(isubfield,7), &
                             xsec18_arr(isubfield))
    isec1_arr(isubfield,8) = nint(xsec18_arr(isubfield))
  endif ! gribVer
  if (isec1_arr(isubfield,6).ne.-1) then
    !get the size and data of the values array
    call grib_get_real4_array(igribind(ifield),'values',zsec4_arr(1:jpunp,isubfield),iret)
    call grib_check(iret,gribFunction,gribErrorMsg)
  endif

  call grib_get_int(igribind(ifield),'numberOfPointsAlongAParallel',isec2(2),iret)
  call grib_check(iret,gribFunction,gribErrorMsg)
  call grib_get_int(igribind(ifield),'numberOfPointsAlongAMeridian',isec2(3),iret)
  call grib_check(iret,gribFunction,gribErrorMsg)
  call grib_get_real8(igribind(ifield),'longitudeOfFirstGridPointInDegrees',xauxin,iret)
  call grib_check(iret,gribFunction,gribErrorMsg)
  call grib_get_real8(igribind(ifield),'latitudeOfLastGridPointInDegrees',yauxin,iret)
  call grib_check(iret,gribFunction,gribErrorMsg)
  xaux=xauxin+real(nxshift)*dx
  yaux=yauxin
  
  ! CHECK GRID SPECIFICATIONS
  
  if(isec2(2).ne.nxfield) stop 'READWIND: NX NOT CONSISTENT'
  if(isec2(3).ne.ny) stop 'READWIND: NY NOT CONSISTENT'
  if(xaux.eq.0.) xaux=-180.0     ! NCEP DATA
  xaux0=xlon0
  yaux0=ylat0
  if(xaux.lt.0.) xaux=xaux+360.
  if(yaux.lt.0.) yaux=yaux+360.
  if(xaux0.lt.0.) xaux0=xaux0+360.
  if(yaux0.lt.0.) yaux0=yaux0+360.
  if(abs(xaux-xaux0).gt.eps .or. abs(yaux-yaux0).gt.eps) then
       write (*, *) 'xaux/xaux0/yaux/yaux0: xaux, xaux0, yaux, yaux0'
       stop 'READWIND: LOWER LEFT LONGITUDE or LATITUDE NOT CONSISTENT'
  endif

  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  ! OK done now do real stuff
  !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
  write(*,*) 'grib main readin loop: ',mynumthreads,' parallel threads process chunks of ',chunksize,' fields'
  
  do iextf=1,inumfields,chunksize
!$omp   parallel default(none) &
!$omp     shared(isec1_arr,xsec18_arr,zsec4_arr,iextf,inumfields,igribind,chunksize) &
!$omp     private(ifield,isubfield,iret,discipl,gribVer,parCat,parNum, &
!$omp             typSurf,ValSurf,gribfunction,griberrormsg) num_threads(mynumthreads)
!$omp   do schedule(guided)
    do ifield=iextf,min(inumfields,iextf+chunksize-1)
      isubfield=ifield-iextf+1
      !write(*,*) isubfield
      !next field 
      !HERE, isec1, xsec18 are read
      !and   zsec4  
      !and (locally needed) discipl, parCat, parNum, typSurf, valSurf 
     
      !write(*,*) ifield
      !write(*,*) igribind(ifield),igribind(ifield+1)
      !first see if we read GRIB1 or GRIB2
      call grib_get_int(igribind(ifield),'editionNumber',gribVer,iret)
      call grib_check(iret,gribFunction,gribErrorMsg)
    
      if (gribVer.eq.1) then ! GRIB Edition 1
        !read the grib1 identifiers
        call grib_get_int(igribind(ifield),'indicatorOfParameter',isec1_arr(isubfield,6),iret)
        call grib_check(iret,gribFunction,gribErrorMsg)
        call grib_get_int(igribind(ifield),'indicatorOfTypeOfLevel',isec1_arr(isubfield,7),iret)
        call grib_check(iret,gribFunction,gribErrorMsg)
        call grib_get_int(igribind(ifield),'level',isec1_arr(isubfield,8),iret)
        call grib_check(iret,gribFunction,gribErrorMsg)
        !JMA / SH: isec1(8) not evaluated any more below
        !b/c with GRIB 2 this may be a real variable
        xsec18_arr(isubfield) = real(isec1_arr(isubfield,8))
      else ! GRIB Edition 2
       !read the grib2 identifiers
        call grib_get_int(igribind(ifield),'discipline',discipl,iret)
        call grib_check(iret,gribFunction,gribErrorMsg)
        call grib_get_int(igribind(ifield),'parameterCategory',parCat,iret)
        call grib_check(iret,gribFunction,gribErrorMsg)
        call grib_get_int(igribind(ifield),'parameterNumber',parNum,iret)
        call grib_check(iret,gribFunction,gribErrorMsg)
        call grib_get_int(igribind(ifield),'typeOfFirstFixedSurface',typSurf,iret)
        call grib_check(iret,gribFunction,gribErrorMsg)
        call grib_get_int(igribind(ifield),'scaledValueOfFirstFixedSurface', valSurf,iret)
        call grib_check(iret,gribFunction,gribErrorMsg)
        isec1_arr(isubfield,6:8) = -1
        xsec18_arr(isubfield) = -1.0
        !convert
        call readwind_grib2_to_1(parCat,parNum,typSurf,valSurf,discipl,isec1_arr(isubfield,6),isec1_arr(isubfield,7), &
                                 xsec18_arr(isubfield))
      endif ! gribVer
      if (isec1_arr(isubfield,6).ne.-1) then
        if ((isec1_arr(isubfield,6).eq.011 .and. isec1_arr(isubfield,7).eq.100) .or. & 
            (isec1_arr(isubfield,6).eq.033 .and. isec1_arr(isubfield,7).eq.100) .or. & 
            (isec1_arr(isubfield,6).eq.034 .and. isec1_arr(isubfield,7).eq.100) .or. & 
            (isec1_arr(isubfield,6).eq.052 .and. isec1_arr(isubfield,7).eq.100) .or. & 
            (isec1_arr(isubfield,6).eq.001 .and. isec1_arr(isubfield,7).eq.001) .or. & 
            (isec1_arr(isubfield,6).eq.039 .and. isec1_arr(isubfield,7).eq.100) .or. & 
            (isec1_arr(isubfield,6).eq.066 .and. isec1_arr(isubfield,7).eq.001) .or. & 
            (isec1_arr(isubfield,6).eq.002 .and. isec1_arr(isubfield,7).eq.102) .or. & 
            (isec1_arr(isubfield,6).eq.071 .and. isec1_arr(isubfield,7).eq.244) .or. & 
            (isec1_arr(isubfield,6).eq.033 .and. isec1_arr(isubfield,7).eq.105 .and. nint(xsec18_arr(isubfield)).eq.10) .or. &
            (isec1_arr(isubfield,6).eq.034 .and. isec1_arr(isubfield,7).eq.105 .and. nint(xsec18_arr(isubfield)).eq.10) .or. &
            (isec1_arr(isubfield,6).eq.011 .and. isec1_arr(isubfield,7).eq.105 .and. nint(xsec18_arr(isubfield)).eq.2)  .or. &
            (isec1_arr(isubfield,6).eq.017 .and. isec1_arr(isubfield,7).eq.105 .and. nint(xsec18_arr(isubfield)).eq.2)  .or. &
            (isec1_arr(isubfield,6).eq.062 .and. isec1_arr(isubfield,7).eq.001) .or. &
            (isec1_arr(isubfield,6).eq.063 .and. isec1_arr(isubfield,7).eq.001) .or. &
            (isec1_arr(isubfield,6).eq.007 .and. isec1_arr(isubfield,7).eq.001) .or. &
            (isec1_arr(isubfield,6).eq.081 .and. isec1_arr(isubfield,7).eq.001) .or. &
            (isec1_arr(isubfield,6).eq.221 .and. isec1_arr(isubfield,7).eq.001) .or. &
            (isec1_arr(isubfield,6).eq.052 .and. isec1_arr(isubfield,7).eq.105 .and. nint(xsec18_arr(isubfield)).eq.02) .or. &
            (isec1_arr(isubfield,6).eq.011 .and. isec1_arr(isubfield,7).eq.107) .or. &
            (isec1_arr(isubfield,6).eq.033 .and. isec1_arr(isubfield,7).eq.107) .or. &
            (isec1_arr(isubfield,6).eq.034 .and. isec1_arr(isubfield,7).eq.107)) then
          !write(*,*) 'read',isec1_arr(isubfield,6),isec1_arr(isubfield,7)

          !get the size and data of the values array
          call grib_get_real4_array(igribind(ifield),'values',zsec4_arr(1:jpunp,isubfield),iret)
          call grib_check(iret,gribFunction,gribErrorMsg)
        end if
      endif
    end do
!$omp end do
!$omp end parallel

    do ifield=iextf,min(inumfields,iextf+chunksize-1)
      isubfield=ifield-iextf+1
      if (isec1_arr(isubfield,6).ne.-1) then
        if ((isec1_arr(isubfield,6).eq.011 .and. isec1_arr(isubfield,7).eq.100) .or. & 
            (isec1_arr(isubfield,6).eq.033 .and. isec1_arr(isubfield,7).eq.100) .or. &
            (isec1_arr(isubfield,6).eq.034 .and. isec1_arr(isubfield,7).eq.100) .or. &
            (isec1_arr(isubfield,6).eq.052 .and. isec1_arr(isubfield,7).eq.100) .or. &
            (isec1_arr(isubfield,6).eq.039 .and. isec1_arr(isubfield,7).eq.100) .or. &
            (isec1_arr(isubfield,6).eq.001 .and. isec1_arr(isubfield,7).eq.001) .or. &
            (isec1_arr(isubfield,6).eq.066 .and. isec1_arr(isubfield,7).eq.001) .or. &
            (isec1_arr(isubfield,6).eq.002 .and. isec1_arr(isubfield,7).eq.102) .or. &
            (isec1_arr(isubfield,6).eq.071 .and. isec1_arr(isubfield,7).eq.244) .or. &
            (isec1_arr(isubfield,6).eq.033 .and. isec1_arr(isubfield,7).eq.105 .and. nint(xsec18_arr(isubfield)).eq.10) .or. &
            (isec1_arr(isubfield,6).eq.034 .and. isec1_arr(isubfield,7).eq.105 .and. nint(xsec18_arr(isubfield)).eq.10) .or. &
            (isec1_arr(isubfield,6).eq.011 .and. isec1_arr(isubfield,7).eq.105 .and. nint(xsec18_arr(isubfield)).eq.2)  .or. &
            (isec1_arr(isubfield,6).eq.017 .and. isec1_arr(isubfield,7).eq.105 .and. nint(xsec18_arr(isubfield)).eq.2)  .or. &
            (isec1_arr(isubfield,6).eq.062 .and. isec1_arr(isubfield,7).eq.001) .or. &
            (isec1_arr(isubfield,6).eq.063 .and. isec1_arr(isubfield,7).eq.001) .or. &
            (isec1_arr(isubfield,6).eq.007 .and. isec1_arr(isubfield,7).eq.001) .or. &
            (isec1_arr(isubfield,6).eq.081 .and. isec1_arr(isubfield,7).eq.001) .or. &
            (isec1_arr(isubfield,6).eq.221 .and. isec1_arr(isubfield,7).eq.001) .or. &
            (isec1_arr(isubfield,6).eq.052 .and. isec1_arr(isubfield,7).eq.105 .and. nint(xsec18_arr(isubfield)).eq.2) .or. &
            (isec1_arr(isubfield,6).eq.011 .and. isec1_arr(isubfield,7).eq.107) .or. &
            (isec1_arr(isubfield,6).eq.033 .and. isec1_arr(isubfield,7).eq.107) .or. &
            (isec1_arr(isubfield,6).eq.034 .and. isec1_arr(isubfield,7).eq.107)) then

          if ((isec1_arr(isubfield,6).eq.011 .and. isec1_arr(isubfield,7).eq.100)  .or. & 
              (isec1_arr(isubfield,6).eq.033 .and. isec1_arr(isubfield,7).eq.100)  .or. &
              (isec1_arr(isubfield,6).eq.034 .and. isec1_arr(isubfield,7).eq.100)  .or. &
              (isec1_arr(isubfield,6).eq.052 .and. isec1_arr(isubfield,7).eq.100)  .or. &
              (isec1_arr(isubfield,6).eq.039 .and. isec1_arr(isubfield,7).eq.100)) then
            do ii=1,nuvz
              if (abs(xsec18_arr(isubfield)*100.0-akz(ii)) < &
                 10.0*max(spacing(akz(ii)),spacing(xsec18_arr(isubfield)*100.0))) then
                match_zlev=ii
              end if
            end do
          end if
 
          ! TEMPERATURE 
          if ((isec1_arr(isubfield,6).eq.011).and.(isec1_arr(isubfield,7).eq.100)) rarr2_pnt => tth(:,:,match_zlev,n)
          ! U VELOCITY
          if ((isec1_arr(isubfield,6).eq.033).and.(isec1_arr(isubfield,7).eq.100)) then
            rarr2_pnt => uuh(:,:,match_zlev) 
            !NCEP ISOBARIC LEVELS
            iumax=iumax+1 
          end if
          ! V VELOCITY
          if ((isec1_arr(isubfield,6).eq.034).and.(isec1_arr(isubfield,7).eq.100)) rarr2_pnt => vvh(:,:,match_zlev)
          ! RELATIVE HUMIDITY -> CONVERT TO SPECIFIC HUMIDITY LATER
          if ((isec1_arr(isubfield,6).eq.052).and.(isec1_arr(isubfield,7).eq.100)) rarr2_pnt => qvh(:,:,match_zlev,n)
          ! W VELOCITY
          if ((isec1_arr(isubfield,6).eq.039).and.(isec1_arr(isubfield,7).eq.100)) then
            numpwmax=max(match_zlev,numpwmax)
            rarr2_pnt => wwh(:,:,match_zlev)
          end if
          ! SURFACE PRESSURE
          if ((isec1_arr(isubfield,6).eq.001).and.(isec1_arr(isubfield,7).eq.001)) rarr2_pnt => ps(:,:,1,n)
          ! SNOW DEPTH
          if ((isec1_arr(isubfield,6).eq.066).and.(isec1_arr(isubfield,7).eq.001)) rarr2_pnt => sd(:,:,1,n)
          ! MEAN SEA LEVEL PRESSURE
          if ((isec1_arr(isubfield,6).eq.002).and.(isec1_arr(isubfield,7).eq.102)) rarr2_pnt => msl(:,:,1,n)          
          ! TOTAL CLOUD COVER
          if ((isec1_arr(isubfield,6).eq.071).and.(isec1_arr(isubfield,7).eq.244)) rarr2_pnt => tcc(:,:,1,n)
          ! 10M U VELOCITY
          if ((isec1_arr(isubfield,6).eq.033).and.(isec1_arr(isubfield,7).eq.105).and.(nint(xsec18_arr(isubfield)).eq.10)) &
            rarr2_pnt => u10(:,:,1,n)  
          ! 10M V VELOCITY
          if ((isec1_arr(isubfield,6).eq.034).and.(isec1_arr(isubfield,7).eq.105).and.(nint(xsec18_arr(isubfield)).eq.10)) &
            rarr2_pnt => v10(:,:,1,n)  
          ! 2M TEMPERATURE
          if ((isec1_arr(isubfield,6).eq.011).and.(isec1_arr(isubfield,7).eq.105).and.(nint(xsec18_arr(isubfield)).eq.2)) &
            rarr2_pnt => tt2(:,:,1,n)
          ! 2M DEW POINT TEMPERATURE
          if ((isec1_arr(isubfield,6).eq.017).and.(isec1_arr(isubfield,7).eq.105).and.(nint(xsec18_arr(isubfield)).eq.2)) &
            rarr2_pnt => td2(:,:,1,n)
          ! LARGE SCALE PREC. 
          if ((isec1_arr(isubfield,6).eq.062).and.(isec1_arr(isubfield,7).eq.001)) rarr2_pnt => lsprec(:,:,1,n)
          ! CONVECTIVE PREC.          
          if ((isec1_arr(isubfield,6).eq.063).and.(isec1_arr(isubfield,7).eq.001)) rarr2_pnt => convprec(:,:,1,n)
          ! TOPOGRAPHY
          if ((isec1_arr(isubfield,6).eq.007).and.(isec1_arr(isubfield,7).eq.001)) then 
            rarr2_pnt => oro(:,:)
            excessoro(i180:i180+min(nxfield,i180)-1,:) = 0.0  
            if (nxfield-1>=i180) excessoro(0:nxfield-1-i180,:) = 0.0
          end if
          ! LAND SEA MASK
          if ((isec1_arr(isubfield,6).eq.081).and.(isec1_arr(isubfield,7).eq.001)) rarr2_pnt => lsm(:,:)
          ! MIXING HEIGHT
          if ((isec1_arr(isubfield,6).eq.221).and.(isec1_arr(isubfield,7).eq.001)) rarr2_pnt => hmix(:,:,1,n)
          ! 2M REALTIVE HUMIDITY
          if ((isec1_arr(isubfield,6).eq.052).and.(isec1_arr(isubfield,7).eq.105).and.(nint(xsec18_arr(isubfield)).eq.2)) &
            rarr2_pnt => qvh2(:,:)
          ! TEMPERATURE LOWEST SIGMA LEVEL 
          if ((isec1_arr(isubfield,6).eq.011).and.(isec1_arr(isubfield,7).eq.107)) rarr2_pnt => tlev1(:,:)
          ! U  VELOCITY LOWEST SIGMA LEVEL
          if ((isec1_arr(isubfield,6).eq.033).and.(isec1_arr(isubfield,7).eq.107)) rarr2_pnt => ulev1(:,:)
          ! V  VELOCITY LOWEST SIGMA LEVEL
          if ((isec1_arr(isubfield,6).eq.034).and.(isec1_arr(isubfield,7).eq.107)) rarr2_pnt => vlev1(:,:)
 
          do j=0,nymin1
            rarr2_pnt(i180+1:i180+min(nxfield,i180), j+1) = &
              zsec4_arr(nxfield*(ny-j-1)+1:nxfield*(ny-j-1)+min(nxfield,i180), isubfield)
          end do
          if (nxfield-1>=i180) then
            do j=0,nymin1
              rarr2_pnt(1:nxfield-i180, j+1) = zsec4_arr(nxfield*(ny-j-1)+i180+1:nxfield*(ny-j-1)+nxfield, isubfield)
            end do
          end if 

          nullify(rarr2_pnt)
        end if
      endif
      call grib_release(igribind(ifield))
    end do                     !! READ NEXT LEVEL OR PARAMETER

  end do
    
  deallocate(isec1_arr)
  deallocate(xsec18_arr)
  deallocate(zsec4_arr)


  !HSO close grib file
  call grib_close_file(ifile)


  !fix W velocities at uppermost levles
  if (numpwmax<nuvz) then
    if (wfix/=2) write(*,*) 'WARNING: W given on less levels than U/V, dirty fix mode:',wfix
    if (wfix==2) write(*,*) 'WARNING (serious): W given on less levels than U/V, fix switched off, uninitialised vars will result'
  end if
  if (wfix==0) then
    do k=numpwmax+1,nuvz
      wwh(:,:,k)=0.0
    end do
  else if (wfix==1) then
    do k=numpwmax+1,nuvz
      wwh(:,:,k)=wwh(:,:,numpwmax)
    end do
  end if


  ! SENS. HEAT FLUX
  sshf(:,:,1,n)=0.0     ! not available from gfs.tccz.pgrbfxx files
  hflswitch=.false.    ! Heat flux not available
  ! SOLAR RADIATIVE FLUXES
  ssr(:,:,1,n)=0.0      ! not available from gfs.tccz.pgrbfxx files
  ! EW SURFACE STRESS
  ewss=0.0         ! not available from gfs.tccz.pgrbfxx files
  ! NS SURFACE STRESS
  nsss=0.0         ! not available from gfs.tccz.pgrbfxx files
  strswitch=.false.    ! stress not available

  ! CONVERT TP TO LSP (GRIB2 only)
  if (gribVer.eq.2) then
    lsprec(0:nxfield-1, 0:nymin1, 1, n) = max( 0.0 , lsprec(0:nxfield-1,0:nymin1,1,n)-convprec(0:nxfield-1,0:nymin1,1,n) )
  endif
  !HSO end edits


  ! TRANSFORM RH TO SPECIFIC HUMIDITY

  do j=0,ny-1
    do i=0,nxfield-1
      do k=1,nuvz
        help=qvh(i,j,k,n)
        temp=tth(i,j,k,n)
        plev1=akm(k)+bkm(k)*ps(i,j,1,n)
        elev=ew(temp)*help/100.0
        qvh(i,j,k,n)=xmwml*(elev/(plev1-((1.0-xmwml)*elev)))
      end do
    end do
  end do

  ! CALCULATE 2 M DEW POINT FROM 2 M RELATIVE HUMIDITY
  ! USING BOLTON'S (1980) FORMULA
  ! BECAUSE td2 IS NOT AVAILABLE FROM NCEP GFS DATA

  do j=0,ny-1
    do i=0,nxfield-1
        help=qvh2(i,j)
        temp=tt2(i,j,1,n)
        elev=ew(temp)/100.*help/100.   !vapour pressure in hPa
        td2(i,j,1,n)=243.5/(17.67/log(elev/6.112)-1)+273.
        if (help.le.0.) td2(i,j,1,n)=tt2(i,j,1,n)
    end do
  end do

  if(levdiff2.eq.0) then
    iwmax=nlev_ec+1
    do i=0,nxmin1
      do j=0,nymin1
        wwh(i,j,nlev_ec+1)=0.
      end do
    end do
  endif


  ! For global fields, assign the leftmost data column also to the rightmost
  ! data column; if required, shift whole grid by nxshift grid points
  !*************************************************************************

  if (xglobal) then
    call shift_field_0(ewss,nxfield,ny)
    call shift_field_0(nsss,nxfield,ny)
    call shift_field_0(oro,nxfield,ny)
    call shift_field_0(excessoro,nxfield,ny)
    call shift_field_0(lsm,nxfield,ny)
    call shift_field_0(ulev1,nxfield,ny)
    call shift_field_0(vlev1,nxfield,ny)
    call shift_field_0(tlev1,nxfield,ny)
    call shift_field_0(qvh2,nxfield,ny)
    call shift_field(ps,nxfield,ny,1,1,2,n)
    call shift_field(sd,nxfield,ny,1,1,2,n)
    call shift_field(msl,nxfield,ny,1,1,2,n)
    call shift_field(tcc,nxfield,ny,1,1,2,n)
    call shift_field(u10,nxfield,ny,1,1,2,n)
    call shift_field(v10,nxfield,ny,1,1,2,n)
    call shift_field(tt2,nxfield,ny,1,1,2,n)
    call shift_field(td2,nxfield,ny,1,1,2,n)
    call shift_field(lsprec,nxfield,ny,1,1,2,n)
    call shift_field(convprec,nxfield,ny,1,1,2,n)
    call shift_field(sshf,nxfield,ny,1,1,2,n)
    call shift_field(ssr,nxfield,ny,1,1,2,n)
    call shift_field(hmix,nxfield,ny,1,1,2,n)
    call shift_field(tth,nxfield,ny,nuvzmax,nuvz,2,n)
    call shift_field(qvh,nxfield,ny,nuvzmax,nuvz,2,n)
    call shift_field(uuh,nxfield,ny,nuvzmax,nuvz,1,1)
    call shift_field(vvh,nxfield,ny,nuvzmax,nuvz,1,1)
    call shift_field(wwh,nxfield,ny,nwzmax,nwz,1,1)
  endif

  do i=0,nxmin1
    do j=0,nymin1
  ! Convert precip. from mm/s -> mm/hour
      convprec(i,j,1,n)=convprec(i,j,1,n)*3600.
      lsprec(i,j,1,n)=lsprec(i,j,1,n)*3600.
      surfstr(i,j,1,n)=sqrt(ewss(i,j)**2+nsss(i,j)**2)
    end do
  end do

  if ((.not.hflswitch).or.(.not.strswitch)) then
  !  write(*,*) 'WARNING: No flux data contained in GRIB file ',
  !    +  wfname(indj)

  ! CALCULATE USTAR AND SSHF USING THE PROFILE METHOD
  !***************************************************************************

    do i=0,nxmin1
      do j=0,nymin1
        hlev1=30.0                     ! HEIGHT OF FIRST MODEL SIGMA LAYER
        ff10m= sqrt(u10(i,j,1,n)**2+v10(i,j,1,n)**2)
        fflev1=sqrt(ulev1(i,j)**2+vlev1(i,j)**2)
        call pbl_profile(ps(i,j,1,n),td2(i,j,1,n),hlev1, &
             tt2(i,j,1,n),tlev1(i,j),ff10m,fflev1, &
             surfstr(i,j,1,n),sshf(i,j,1,n))
        if(sshf(i,j,1,n).gt.200.) sshf(i,j,1,n)=200.
        if(sshf(i,j,1,n).lt.-400.) sshf(i,j,1,n)=-400.
      end do
    end do
  endif


  if(iumax.ne.nuvz) stop 'READWIND: NUVZ NOT CONSISTENT'
  if(iumax.ne.nwz)    stop 'READWIND: NWZ NOT CONSISTENT'
  write(*,*) 'exit readwind_gfs'

end subroutine readwind

subroutine readwind_grib2_to_1(parcat_in,parnum_in,typsurf_in,valsurf_in,discipl_in,i6_out,i7_out,x8_out)
  implicit none
  integer, intent(in)       :: parcat_in,parnum_in,typsurf_in,valsurf_in,discipl_in
  integer, intent(out)      :: i6_out,i7_out
  real(kind=4), intent(out) :: x8_out

  !convert to grib1 identifiers
  !JMA / SH: isec1(8) not evaluated any more below
  !b/c with GRIB 2 this may be a real variable
  if ((parcat_in.eq.0).and.(parnum_in.eq.0).and.(typsurf_in.eq.100)) then ! T
    i6_out=11          ! indicatorOfParameter
    i7_out=100         ! indicatorOfTypeOfLevel
    x8_out=valsurf_in/100.0 ! level, convert to hPa
  elseif ((parcat_in.eq.2).and.(parnum_in.eq.2).and.(typsurf_in.eq.100)) then ! U
    i6_out=33          ! indicatorOfParameter
    i7_out=100         ! indicatorOfTypeOfLevel
    x8_out=valsurf_in/100.0 ! level, convert to hPa
  elseif ((parcat_in.eq.2).and.(parnum_in.eq.3).and.(typsurf_in.eq.100)) then ! V
    i6_out=34          ! indicatorOfParameter
    i7_out=100         ! indicatorOfTypeOfLevel
    x8_out=valsurf_in/100.0 ! level, convert to hPa
  elseif ((parcat_in.eq.2).and.(parnum_in.eq.8).and.(typsurf_in.eq.100)) then ! W
    i6_out=39          ! indicatorOfParameter
    i7_out=100         ! indicatorOfTypeOfLevel
    x8_out=valsurf_in/100.0 ! level, convert to hPa
  elseif ((parcat_in.eq.1).and.(parnum_in.eq.1).and.(typsurf_in.eq.100)) then ! RH
    i6_out=52          ! indicatorOfParameter
    i7_out=100         ! indicatorOfTypeOfLevel
    x8_out=valsurf_in/100.0 ! level, convert to hPa
  elseif ((parcat_in.eq.1).and.(parnum_in.eq.1).and.(typsurf_in.eq.103)) then ! RH2
    i6_out=52          ! indicatorOfParameter
    i7_out=105         ! indicatorOfTypeOfLevel
    x8_out=real(2)
  elseif ((parcat_in.eq.0).and.(parnum_in.eq.0).and.(typsurf_in.eq.103)) then ! T2
    i6_out=11          ! indicatorOfParameter
    i7_out=105         ! indicatorOfTypeOfLevel
    x8_out=real(2)
  elseif ((parcat_in.eq.2).and.(parnum_in.eq.2).and.(typsurf_in.eq.103)) then ! U10
    i6_out=33          ! indicatorOfParameter
    i7_out=105         ! indicatorOfTypeOfLevel
    x8_out=real(10)
  elseif ((parcat_in.eq.2).and.(parnum_in.eq.3).and.(typsurf_in.eq.103)) then ! V10
    i6_out=34          ! indicatorOfParameter
    i7_out=105         ! indicatorOfTypeOfLevel
    x8_out=real(10)
  elseif ((parcat_in.eq.3).and.(parnum_in.eq.1).and.(typsurf_in.eq.101)) then ! SLP
    i6_out=2           ! indicatorOfParameter
    i7_out=102         ! indicatorOfTypeOfLevel
    x8_out=real(0)
  elseif ((parcat_in.eq.3).and.(parnum_in.eq.0).and.(typsurf_in.eq.1)) then ! SP
    i6_out=1           ! indicatorOfParameter
    i7_out=1           ! indicatorOfTypeOfLevel
    x8_out=real(0)
  elseif ((parcat_in.eq.1).and.(parnum_in.eq.13).and.(typsurf_in.eq.1)) then ! SNOW
    i6_out=66          ! indicatorOfParameter
    i7_out=1           ! indicatorOfTypeOfLevel
    x8_out=real(0)
  elseif ((parcat_in.eq.0).and.(parnum_in.eq.0).and.(typsurf_in.eq.104)) then ! T sigma 0
    i6_out=11          ! indicatorOfParameter
    i7_out=107         ! indicatorOfTypeOfLevel
    x8_out=0.995         ! lowest sigma level
  elseif ((parcat_in.eq.2).and.(parnum_in.eq.2).and.(typsurf_in.eq.104)) then ! U sigma 0
    i6_out=33          ! indicatorOfParameter
    i7_out=107         ! indicatorOfTypeOfLevel
    x8_out=0.995         ! lowest sigma level
  elseif ((parcat_in.eq.2).and.(parnum_in.eq.3).and.(typsurf_in.eq.104)) then ! V sigma 0
    i6_out=34          ! indicatorOfParameter
    i7_out=107         ! indicatorOfTypeOfLevel
    x8_out=0.995         ! lowest sigma level
  elseif ((parcat_in.eq.3).and.(parnum_in.eq.5).and.(typsurf_in.eq.1)) then ! TOPO
    i6_out=7           ! indicatorOfParameter
    i7_out=1           ! indicatorOfTypeOfLevel
    x8_out=real(0)
  elseif ((parcat_in.eq.0).and.(parnum_in.eq.0).and.(typsurf_in.eq.1) .and.(discipl_in.eq.2)) then ! LSM
    i6_out=81          ! indicatorOfParameter
    i7_out=1           ! indicatorOfTypeOfLevel
    x8_out=real(0)
  elseif ((parcat_in.eq.3).and.(parnum_in.eq.196).and.(typsurf_in.eq.1)) then ! BLH
    i6_out=221         ! indicatorOfParameter
    i7_out=1           ! indicatorOfTypeOfLevel
    x8_out=real(0)
  elseif ((parcat_in.eq.1).and.(parnum_in.eq.7).and.(typsurf_in.eq.1)) then ! LSP/TP
    i6_out=62          ! indicatorOfParameter
    i7_out=1           ! indicatorOfTypeOfLevel
    x8_out=real(0)
  elseif ((parcat_in.eq.1).and.(parnum_in.eq.196).and.(typsurf_in.eq.1)) then ! CP
    i6_out=63          ! indicatorOfParameter
    i7_out=1           ! indicatorOfTypeOfLevel
    x8_out=real(0)
  endif
end subroutine readwind_grib2_to_1



