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

subroutine verttransform(n,uuh,vvh,wwh,pvh)
  !                         i  i   i   i   i
  !*****************************************************************************
  !                                                                            *
  !     This subroutine transforms temperature, dew point temperature and      *
  !     wind components from eta to meter coordinates.                         *
  !     The vertical wind component is transformed from Pa/s to m/s using      *
  !     the conversion factor pinmconv.                                        *
  !     In addition, this routine calculates vertical density gradients        *
  !     needed for the parameterization of the turbulent velocities.           *
  !                                                                            *
  !     Author: A. Stohl, G. Wotawa                                            *
  !                                                                            *
  !     12 August 1996                                                         *
  !     Update: 16 January 1998                                                *
  !                                                                            *
  !     Major update: 17 February 1999                                         *
  !     by G. Wotawa                                                           *
  !     CHANGE 17/11/2005 Caroline Forster, NCEP GFS version                   *
  !                                                                            *
  !   - Vertical levels for u, v and w are put together                        *
  !   - Slope correction for vertical velocity: Modification of calculation    *
  !     procedure                                                              *
  !                                                                            *
  !*****************************************************************************
  !  Changes, Bernd C. Krueger, Feb. 2001:
  !   Variables tth and qvh (on eta coordinates) from common block
  !*****************************************************************************
  !                                                                            *
  ! Variables:                                                                 *
  ! nx,ny,nz                        field dimensions in x,y and z direction    *
  ! uu(0:nxmax,0:nymax,nzmax,2)     wind components in x-direction [m/s]       *
  ! vv(0:nxmax,0:nymax,nzmax,2)     wind components in y-direction [m/s]       *
  ! ww(0:nxmax,0:nymax,nzmax,2)     wind components in z-direction [deltaeta/s]*
  ! tt(0:nxmax,0:nymax,nzmax,2)     temperature [K]                            *
  ! pv(0:nxmax,0:nymax,nzmax,2)     potential voriticity (pvu)                 *
  ! ps(0:nxmax,0:nymax,2)           surface pressure [Pa]                      *
  ! clouds(0:nxmax,0:nymax,0:nzmax,2) cloud field for wet deposition           *
  !                                                                            *
  !*****************************************************************************

  use par_mod
  use com_mod
  use cmapf_mod
  use omp_lib

  implicit none

  integer :: ix,jy,kz,iz,n,ix1,jy1,ixp,jyp,ixm,jym
  integer :: rain_cloud_above,kz_inv
  real :: f_qvsat,pressure
  real :: rh,lsp,convp
  real :: ew,pint,tv,tvold,pold,dz1,dz2,dz,ui,vi
  !SH
  real :: tmp_real
  real :: tv_arr(1:nuvz),pint_arr(1:nuvz)
  real :: rhoh_arr(0:nxmax-1,0:nymax-1,nuvzmax)  
  real :: pinmconv_arr(0:nxmax-1,0:nymax-1,nzmax)    
  integer :: llev_arr(0:nxmax-1,0:nymax-1),hind_above_arr(0:nxmin1,0:nymin1)
  integer, dimension(0:nxmax-1,0:nymax-1,1:nz) :: match_height_arr
  integer :: loopcnt

  real :: xlon,ylat,xlonr,dzdx,dzdy
  real :: dzdx1,dzdx2,dzdy1,dzdy2
  real :: uuaux,vvaux,uupolaux,vvpolaux,ddpol,ffpol,wdummy
  real :: uuh(0:nxmax-1,0:nymax-1,nuvzmax)
  real :: vvh(0:nxmax-1,0:nymax-1,nuvzmax)
  real :: pvh(0:nxmax-1,0:nymax-1,nuvzmax)
  real :: wwh(0:nxmax-1,0:nymax-1,nwzmax)
  real :: uvwzlev(0:nxmax-1,0:nymax-1,nzmax)

  ! NCEP version
  ! in this case, nz==nuvz is crucial for correctness and assumed below.

  if (nz/=nuvz) stop 'FATAL FLEXPART-GFS, nz != nuvz'
  write(*,*) 'enter verttransform...'
  uvwzlev(:,:,:) = 0.0
  
  !*************************************************************************
  ! If verttransform is called the first time, initialize heights of the   *
  ! z levels in meter. The heights are the heights of model levels, where  *
  ! u,v,T and qv are given, and of the interfaces, where w is given. So,   *
  ! the vertical resolution in the z system is doubled. As reference point,*
  ! the lower left corner of the grid is used.                             *
  ! Unlike in the eta system, no difference between heights for u,v and    *
  ! heights for w exists.                                                  *
  !*************************************************************************
  if (.not. verttransform_inited) then

    ! Search for a point with high surface pressure (i.e. not above significant topography)
    ! Then, use this point to construct a reference z profile, to be used at all times
    !*****************************************************************************

    yloop_init: do jy=0,nymin1
      do ix=0,nxmin1
        if (ps(ix,jy,1,n).gt.100000.) then
          ixm=ix
          jym=jy
          exit yloop_init
        endif
      end do
    end do yloop_init
    write(*,*) 'verttransform start: init ixm/jym:',ixm,jym

    height(1) = 0.
    tv_arr(1) = tt2(ixm,jym,1,n) * (1. + 0.378*ew(td2(ixm,jym,1,n))/ps(ixm,jym,1,n))
    tv_arr(2:nuvz) = tth(ixm,jym,2:nuvz,n) * (1. + 0.608*qvh(ixm,jym,2:nuvz,n))
    
    pint_arr(1) = ps(ixm,jym,1,n)
    pint_arr(2:nuvz) = akz(2:nuvz)+bkz(2:nuvz)*ps(ixm,jym,1,n)

    do kz=2,nuvz
      if (abs(tv_arr(kz)-tv_arr(kz-1)).gt.0.2) then
        height(kz) = height(kz-1) + (r_air/ga)*log(pint_arr(kz-1)/pint_arr(kz)) * (tv_arr(kz)-tv_arr(kz-1)) / &
                                                                                  log(tv_arr(kz)/tv_arr(kz-1))
      else
        height(kz) = height(kz-1) + (r_air/ga)*log(pint_arr(kz-1)/pint_arr(kz)) * tv_arr(kz)
      endif
    end do
    do kz=nuvz+1,ubound(height,1)
      height(kz)=height(nuvz)
    end do
      
    ! Determine highest levels that can be within PBL
    !************************************************
    do kz=1,nz
      if (height(kz).gt.hmixmax) exit
    end do
    !FIXME CHECK NEW SH: formerly was undefined if condition never fulfilled; now is nz+1
    nmixz=kz

    ! Do not repeat initialization of the Cartesian z grid
    !*****************************************************
    verttransform_inited=.true.
  endif

  write(*,*) 'verttransform_gfs.f90 mostly parallel using ',omp_get_max_threads(),' threads'

!$omp    parallel default(none) private(ix,jy,kz,loopcnt,tvold,pold,tv,pint) &
!$omp      shared(n,nz,nxmin1,nymin1,nuvz,akz,bkz,ps,llev_arr, &
!$omp             tth,qvh,uvwzlev,rhoh_arr,pinmconv_arr,aknew,bknew) &
!$omp      num_threads(omp_get_max_threads())
!$omp do
  do jy=0,nymin1
    do ix=0,nxmin1
      !NCEP version: find first level above ground
      !FIXME Assume AKZ ist absteigend in i
      do loopcnt=1,nuvz-2
        llev_arr(ix,jy)=loopcnt
        if (akz(loopcnt) <= ps(ix,jy,1,n)) exit
      end do
    end do
  end do
!$omp end do

!$omp do
  do jy=0,nymin1
    do ix=0,nxmin1
      ! compute height of pressure levels above ground
      !***********************************************
      tvold=tth(ix,jy,llev_arr(ix,jy),n)*(1.+0.608*qvh(ix,jy,llev_arr(ix,jy),n))
      pold=akz(llev_arr(ix,jy))
      uvwzlev(ix,jy,llev_arr(ix,jy))=0.
      rhoh_arr(ix,jy,llev_arr(ix,jy))=pold/(r_air*tvold)

      do kz=llev_arr(ix,jy)+1,nuvz
        pint=akz(kz)+bkz(kz)*ps(ix,jy,1,n)
        tv=tth(ix,jy,kz,n)*(1.+0.608*qvh(ix,jy,kz,n))
        rhoh_arr(ix,jy,kz)=pint/(r_air*tv)

        if (abs(tv-tvold).gt.0.2) then
          uvwzlev(ix,jy,kz) = uvwzlev(ix,jy,kz-1) + (r_air/ga)*log(pold/pint) * (tv-tvold)/log(tv/tvold)
        else
          uvwzlev(ix,jy,kz) = uvwzlev(ix,jy,kz-1) + (r_air/ga)*log(pold/pint) * tv
        endif

        tvold=tv
        pold=pint
      end do

      pinmconv_arr(ix,jy,llev_arr(ix,jy))=(uvwzlev(ix,jy,llev_arr(ix,jy)+1)-uvwzlev(ix,jy,llev_arr(ix,jy)))/ &
           ((aknew(llev_arr(ix,jy)+1)+bknew(llev_arr(ix,jy)+1)*ps(ix,jy,1,n))- &
           (aknew(llev_arr(ix,jy))+bknew(llev_arr(ix,jy))*ps(ix,jy,1,n)))
      do kz=llev_arr(ix,jy)+1,nz-1
        pinmconv_arr(ix,jy,kz)=(uvwzlev(ix,jy,kz+1)-uvwzlev(ix,jy,kz-1))/((aknew(kz+1)+bknew(kz+1)*ps(ix,jy,1,n))- &
                                                                          (aknew(kz-1)+bknew(kz-1)*ps(ix,jy,1,n)))
      end do
      pinmconv_arr(ix,jy,nz)=(uvwzlev(ix,jy,nz)-uvwzlev(ix,jy,nz-1))/((aknew(nz)+bknew(nz)*ps(ix,jy,1,n))- &
                                                                      (aknew(nz-1)+bknew(nz-1)*ps(ix,jy,1,n)))
    end do
  end do
!$omp end do
!$omp end parallel


!  write(*,*) 'verttransform_gfs.f90 mostly parallel 2/3 using ',omp_get_max_threads(),' threads'

!$omp    parallel default(none) private(ix,jy,iz,kz,dz,dz1,dz2) &
!$omp      shared(nymin1,nxmin1,nz,nuvz,nwz,n, &
!$omp             height,hind_above_arr,match_height_arr,llev_arr,uvwzlev, &
!$omp             uu,uuh,vv,vvh,tt,tth,qv,qvh,pv,pvh,rho,rhoh_arr,pplev,akz, &
!$omp             ww,wwh,pinmconv_arr,drhodz) &
!$omp      num_threads(omp_get_max_threads())
!$omp do  
  do jy=0,nymin1
    do ix=0,nxmin1
      !1 is always OK (zero - zero), 2 is thus the first number which can appear
      do iz=2,nz
        if(height(iz) > uvwzlev(ix,jy,nuvz)) exit
      end do
      hind_above_arr(ix,jy)=iz 
    end do
  end do
!$omp end do

!$omp do
  !matching level z in uvwzlev to each height level in height
  !default -1 so that anything going wrong will be found by array bounds checker
  do jy=0,nymin1
    do ix=0,nxmin1
      do iz=1,nz
        match_height_arr(ix,jy,iz)=-1
      end do
    end do
  end do
!$omp end do
!$omp do  
  do jy=0,nymin1
    do ix=0,nxmin1
      do iz=2,min(nz,hind_above_arr(ix,jy))-1
        do kz=llev_arr(ix,jy)+1,nuvz
          !hind_above_arr such that condition is *always* fulfilled at some point
          !meaning kz-based uwvzlev goes from llev_arr height (zero) to nuvz-height
          !and     iz-based height  goes from 2-height to some  hind_above_arr-height
          !so a matching kz can always be found
          !
          !WHEN OPTIMISING MIND THE FACT THAT SOME OF THESE ARRAYS MAY HAVE ZERO PADDING BEFORE/AFTER!         
          !
          if ( height(iz) > uvwzlev(ix,jy,kz-1) .and. height(iz) <= uvwzlev(ix,jy,kz) ) exit
        end do
        if (kz==nuvz+1) then
          write(*,*) 'FATAL hindex search verttransform'
          stop
        end if
        match_height_arr(ix,jy,iz)=kz
      end do
    end do
  end do
!$omp end do 

!$omp do
  ! Levels, where u,v,t and q are given
  !************************************
  do iz=min(minval(hind_above_arr),nz),nz
    uu(   0:nxmin1,0:nymin1,iz,n)=uuh(0:nxmin1,0:nymin1,nuvz)
    vv(   0:nxmin1,0:nymin1,iz,n)=vvh(0:nxmin1,0:nymin1,nuvz)
    tt(   0:nxmin1,0:nymin1,iz,n)=tth(0:nxmin1,0:nymin1,nuvz,n)
    qv(   0:nxmin1,0:nymin1,iz,n)=qvh(0:nxmin1,0:nymin1,nuvz,n)
    pv(   0:nxmin1,0:nymin1,iz,n)=pvh(0:nxmin1,0:nymin1,nuvz)
    rho(  0:nxmin1,0:nymin1,iz,n)=rhoh_arr(0:nxmin1,0:nymin1,nuvz)
    pplev(0:nxmin1,0:nymin1,iz,n)=akz(nuvz)
  end do
!$omp end do
!$omp do
  do jy=0,nymin1
    do ix=0,nxmin1
      uu(ix,jy,1,n)=uuh(ix,jy,llev_arr(ix,jy))
      vv(ix,jy,1,n)=vvh(ix,jy,llev_arr(ix,jy))
      tt(ix,jy,1,n)=tth(ix,jy,llev_arr(ix,jy),n)
      qv(ix,jy,1,n)=qvh(ix,jy,llev_arr(ix,jy),n)
      pv(ix,jy,1,n)=pvh(ix,jy,llev_arr(ix,jy))
      rho(ix,jy,1,n)=rhoh_arr(ix,jy,llev_arr(ix,jy))
      pplev(ix,jy,1,n)=akz(llev_arr(ix,jy))
    end do
  end do
!$omp end do
!$omp do
  do jy=0,nymin1
    do ix=0,nxmin1
      do iz=2,min(nz,hind_above_arr(ix,jy))-1
        kz=match_height_arr(ix,jy,iz)
        dz1=height(iz)-uvwzlev(ix,jy,kz-1)
        dz2=uvwzlev(ix,jy,kz)-height(iz)
        dz=dz1+dz2
        uu(ix,jy,iz,n)=(uuh(ix,jy,kz-1)*dz2+uuh(ix,jy,kz)*dz1)/dz
        vv(ix,jy,iz,n)=(vvh(ix,jy,kz-1)*dz2+vvh(ix,jy,kz)*dz1)/dz
        tt(ix,jy,iz,n)=(tth(ix,jy,kz-1,n)*dz2+tth(ix,jy,kz,n)*dz1)/dz
        qv(ix,jy,iz,n)=(qvh(ix,jy,kz-1,n)*dz2+qvh(ix,jy,kz,n)*dz1)/dz
        pv(ix,jy,iz,n)=(pvh(ix,jy,kz-1)*dz2+pvh(ix,jy,kz)*dz1)/dz
        rho(ix,jy,iz,n)=(rhoh_arr(ix,jy,kz-1)*dz2+rhoh_arr(ix,jy,kz)*dz1)/dz
        pplev(ix,jy,iz,n)=(akz(kz-1)*dz2+akz(kz)*dz1)/dz
      end do
    end do
  end do
!$omp end do
 
  ! Levels, where w is given
  !*************************
!$omp workshare
  ww(0:nxmin1,0:nymin1,nz,n)=wwh(0:nxmin1,0:nymin1,nwz)*pinmconv_arr(0:nxmin1,0:nymin1,nz)
!$omp end workshare
!$omp do
  do iz=minval(hind_above_arr),nz-1
    !SH: FIXME shouldn't it be nwz here in the last thingy??? -> unclear
    ww(0:nxmin1,0:nymin1,iz,n)=wwh(0:nxmin1,0:nymin1,nwz)*pinmconv_arr(0:nxmin1,0:nymin1,nz)
  end do
!$omp end do
!$omp do
  do jy=0,nymin1
    do ix=0,nxmin1
      ww(ix,jy,1,n)=wwh(ix,jy,llev_arr(ix,jy))*pinmconv_arr(ix,jy,llev_arr(ix,jy))
    end do
  end do
!$omp end do
!$omp do
  do jy=0,nymin1
    do ix=0,nxmin1
      do iz=2,min(nz,hind_above_arr(ix,jy))-1
        kz=match_height_arr(ix,jy,iz)
        dz1=height(iz)-uvwzlev(ix,jy,kz-1)
        dz2=uvwzlev(ix,jy,kz)-height(iz)
        ww(ix,jy,iz,n)=(wwh(ix,jy,kz-1)*pinmconv_arr(ix,jy,kz-1)*dz2 + wwh(ix,jy,kz)*pinmconv_arr(ix,jy,kz)*dz1)/(dz1+dz2)
      end do
    end do
  end do
!$omp end do

  ! Compute density gradients at intermediate levels
  !*************************************************
!$omp workshare
  drhodz(0:nxmin1,0:nymin1,1,n)=(rho(0:nxmin1,0:nymin1,2,n)-rho(0:nxmin1,0:nymin1,1,n))/(height(2)-height(1))
!$omp end workshare
!$omp do
  do kz=2,nz-1
    drhodz(0:nxmin1,0:nymin1,kz,n)=(rho(0:nxmin1,0:nymin1,kz+1,n)-rho(0:nxmin1,0:nymin1,kz-1,n))/(height(kz+1)-height(kz-1))
  end do
!$omp end do
!$omp workshare
  drhodz(0:nxmin1,0:nymin1,nz,n)=drhodz(0:nxmin1,0:nymin1,nz-1,n)
!$omp end workshare

!$omp end parallel  


!  write(*,*) 'verttransform_gfs.f90 mostly parallel 3/3 using ',omp_get_max_threads(),' threads'

!$omp    parallel default(none) private(ix,jy,iz,kz,dz,dz1,dz2,ix1,jy1, &
!$omp      ixp,jyp,dzdx1,dzdx2,dzdx,dzdy1,dzdy2,dzdy,ui,vi,tmp_real)    &
!$omp      shared(n,nz,nx,ny,uu,vv,dxconst,dyconst,dy,ylat0,ww,         &
!$omp             hind_above_arr,match_height_arr,height,uvwzlev)       &
!$omp      num_threads(omp_get_max_threads())
!$omp do
  !****************************************************************
  ! Compute slope of eta levels in windward direction and resulting
  ! vertical wind correction
  !****************************************************************
  do jy=1,ny-2
    tmp_real=cos((real(jy)*dy+ylat0)*pi180)
    do ix=1,nx-2
      do iz=2,min(nz,hind_above_arr(ix,jy))-1
        kz=match_height_arr(ix,jy,iz)

        dz1=height(iz)-uvwzlev(ix,jy,kz-1)
        dz2=uvwzlev(ix,jy,kz)-height(iz)
        dz=dz1+dz2

        ix1=ix-1
        jy1=jy-1
        ixp=ix+1
        jyp=jy+1

        dzdx1=(uvwzlev(ixp,jy,kz-1)-uvwzlev(ix1,jy,kz-1))/2.
        dzdx2=(uvwzlev(ixp,jy,kz)-uvwzlev(ix1,jy,kz))/2.
        dzdx=(dzdx1*dz2+dzdx2*dz1)/dz

        dzdy1=(uvwzlev(ix,jyp,kz-1)-uvwzlev(ix,jy1,kz-1))/2.
        dzdy2=(uvwzlev(ix,jyp,kz)-uvwzlev(ix,jy1,kz))/2.
        dzdy=(dzdy1*dz2+dzdy2*dz1)/dz

        ui=uu(ix,jy,iz,n)*dxconst/tmp_real
        vi=vv(ix,jy,iz,n)*dyconst
        ww(ix,jy,iz,n)=ww(ix,jy,iz,n)+(dzdx*ui+dzdy*vi)
      end do
    end do
  end do
!$omp end do
!$omp end parallel

!  write(*,*) 'verttransform_gfs.f90 mostly parallel 3a/3 using ',omp_get_max_threads(),' threads'

  ! If north pole is in the domain, calculate wind velocities in polar
  ! stereographic coordinates
  !*******************************************************************

  if (nglobal) then
!$omp  parallel default(none) private(jy,iz,ylat,xlon)     &
!$omp    shared(n,nz,switchnorthg,nxmin1,nymin1,xlon0,ylat0,dy,dx,  &
!$omp           northpolemap,uu,vv,uupol,vvpol)            &
!$omp    num_threads(omp_get_max_threads())
!$omp  do
    do jy=int(switchnorthg)-2,nymin1
      ylat=ylat0+real(jy)*dy
      do ix=0,nxmin1
        xlon=xlon0+real(ix)*dx
        do iz=1,nz
          call cc2gll(northpolemap,ylat,xlon,uu(ix,jy,iz,n), &
                      vv(ix,jy,iz,n),uupol(ix,jy,iz,n),vvpol(ix,jy,iz,n))
        end do
      end do
    end do
!$omp end do
!$omp end parallel

    do iz=1,nz
      ! CALCULATE FFPOL, DDPOL FOR CENTRAL GRID POINT
      xlon=xlon0+real(nx/2-1)*dx
      xlonr=xlon*pi/180.
      ffpol=sqrt(uu(nx/2-1,nymin1,iz,n)**2+vv(nx/2-1,nymin1,iz,n)**2)
      if(vv(nx/2-1,nymin1,iz,n).lt.0.) then
        ddpol = atan(uu(nx/2-1,nymin1,iz,n)/vv(nx/2-1,nymin1,iz,n))-xlonr
      elseif (vv(nx/2-1,nymin1,iz,n).gt.0.) then
        ddpol = pi + atan(uu(nx/2-1,nymin1,iz,n)/vv(nx/2-1,nymin1,iz,n))-xlonr
      else
        ddpol = pi/2 - xlonr
      endif
      if(ddpol.lt.0.) ddpol=2.0*pi+ddpol
      if(ddpol.gt.2.0*pi) ddpol=ddpol-2.0*pi

      ! CALCULATE U,V FOR 180 DEG, TRANSFORM TO POLAR STEREOGRAPHIC GRID
      uuaux=-ffpol*sin(pi+ddpol)
      vvaux=-ffpol*cos(pi+ddpol)
      call cc2gll(northpolemap,90.0,180.0,uuaux,vvaux,uupolaux,vvpolaux)

      uupol(0:nxmin1,nymin1,iz,n)=uupolaux
      vvpol(0:nxmin1,nymin1,iz,n)=vvpolaux
    end do

    ! Fix: Set W at pole to the zonally averaged W of the next equator-
    ! ward parallel of latitude
    do iz=1,nz
      ww(0:nxmin1,nymin1,iz,n)=sum(ww(0:nxmin1,ny-2,iz,n))/real(nx)
    end do
  endif


  ! If south pole is in the domain, calculate wind velocities in polar
  ! stereographic coordinates
  !*******************************************************************

  if (sglobal) then
!$omp  parallel default(none) private(jy,iz,ylat,xlon)     &
!$omp    shared(n,nz,switchsouthg,nymin1,nxmin1,xlon0,ylat0,dy,dx, &
!$omp           southpolemap,uu,vv,uupol,vvpol)    &
!$omp    num_threads(omp_get_max_threads())
!$omp  do
    do jy=0,int(switchsouthg)+3
      ylat=ylat0+real(jy)*dy
      do ix=0,nxmin1
        xlon=xlon0+real(ix)*dx
        do iz=1,nz
          call cc2gll(southpolemap,ylat,xlon,uu(ix,jy,iz,n), &
               vv(ix,jy,iz,n),uupol(ix,jy,iz,n),vvpol(ix,jy,iz,n))
        end do
      end do
    end do
!$omp end do
!$omp end parallel

    do iz=1,nz
      ! CALCULATE FFPOL, DDPOL FOR CENTRAL GRID POINT
      xlon=xlon0+real(nx/2-1)*dx
      xlonr=xlon*pi/180.
      ffpol=sqrt(uu(nx/2-1,0,iz,n)**2+vv(nx/2-1,0,iz,n)**2)
      if(vv(nx/2-1,0,iz,n).lt.0.) then
        ddpol=atan(uu(nx/2-1,0,iz,n)/vv(nx/2-1,0,iz,n))+xlonr
      elseif (vv(nx/2-1,0,iz,n).gt.0.) then
        ddpol=pi+atan(uu(nx/2-1,0,iz,n)/vv(nx/2-1,0,iz,n))-xlonr
      else
        ddpol=pi/2-xlonr
      endif
      if(ddpol.lt.0.) ddpol=2.0*pi+ddpol
      if(ddpol.gt.2.0*pi) ddpol=ddpol-2.0*pi

      ! CALCULATE U,V FOR 180 DEG, TRANSFORM TO POLAR STEREOGRAPHIC GRID
      uuaux=+ffpol*sin(pi-ddpol)
      vvaux=-ffpol*cos(pi-ddpol)
      call cc2gll(northpolemap,-90.0,180.0,uuaux,vvaux,uupolaux,vvpolaux)

      uupol(0:nxmin1,0,iz,n)=uupolaux
      vvpol(0:nxmin1,0,iz,n)=vvpolaux
    end do

    ! Fix: Set W at pole to the zonally averaged W of the next equator-
    ! ward parallel of latitude
    do iz=1,nz
      ww(0:nxmin1,0,iz,n)=sum(ww(0:nxmin1,1,iz,n))/real(nx)
    end do
  endif


  !   create a cloud and rainout/washout field, clouds occur where rh>80%
  !   total cloudheight is stored at level 0
  do jy=0,nymin1
    do ix=0,nxmin1
      rain_cloud_above=0
      lsp=lsprec(ix,jy,1,n)
      convp=convprec(ix,jy,1,n)
      cloudsh(ix,jy,n)=0
      do kz_inv=1,nz-1
         kz=nz-kz_inv+1
         pressure=rho(ix,jy,kz,n)*r_air*tt(ix,jy,kz,n)
         rh=qv(ix,jy,kz,n)/f_qvsat(pressure,tt(ix,jy,kz,n))
         clouds(ix,jy,kz,n)=0
         if (rh.gt.0.8) then ! in cloud
           if ((lsp.gt.0.01).or.(convp.gt.0.01)) then ! cloud and precipitation
             rain_cloud_above=1
             cloudsh(ix,jy,n)=cloudsh(ix,jy,n)+ &
                  height(kz)-height(kz-1)
             if (lsp.ge.convp) then
               clouds(ix,jy,kz,n)=3 ! lsp dominated rainout
             else
               clouds(ix,jy,kz,n)=2 ! convp dominated rainout
             endif
           else ! no precipitation
             clouds(ix,jy,kz,n)=1 ! cloud
           endif
         else ! no cloud
           if (rain_cloud_above.eq.1) then ! scavenging
             if (lsp.ge.convp) then
               clouds(ix,jy,kz,n)=5 ! lsp dominated washout
             else
               clouds(ix,jy,kz,n)=4 ! convp dominated washout
             endif
           endif
         endif
      end do
    end do
  end do
  
  write(*,*) 'exit verttransform...'
  
end subroutine verttransform

