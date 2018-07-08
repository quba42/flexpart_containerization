
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

       subroutine calcpv(n,uuh,vvh,pvh)
         !                  i  i   i   o
         !*****************************************************************************
         !                                                                            *
         !  Calculation of potential vorticity on 3-d grid.                           *
         !                                                                            *
         !     Author: P. James                                                       *
         !     3 February 2000                                                        *
         !                                                                            *
         !     Adaptation to FLEXPART, A. Stohl, 1 May 2000                           *
         !                                                                            *
         !*****************************************************************************
         !                                                                            *
         ! Variables:                                                                 *
         ! n                  temporal index for meteorological fields (1 to 2)       *
         !                                                                            *
         ! Constants:                                                                 *
         !                                                                            *
         !*****************************************************************************
       
         use par_mod
         use com_mod
         use omp_lib
 
         implicit none
       
         integer :: n,ix,jy,i,j,k,kl,jj,klvrp,klvrm,klpt,kup,kdn,kch
         integer :: jux,juy,ivrm,ivrp,ivr
         integer :: nlck
         !SH additions/replacements
         integer :: khi,klo
         integer :: jyvp_arr(0:nymin1),jyvm_arr(0:nymin1),jumpy_arr(0:nymin1)
         integer :: jybords(2),ixvp_arr(0:nxmin1),ixvm_arr(0:nxmin1),jumpx_arr(0:nxmin1)
         !end SH
         real :: vx(2),uy(2),phi,tanphi,cosphi,dvdx,dudy,f
         real :: theta,thetap,thetam,dthetadp,dt1,dt2,dt,ppmk
         real :: pvavr
         real :: thup,thdn
         !SH additions
         real :: th_pre(0:nxmax-1,0:nymax-1,nuvzmax)
         real :: ppmstore(0:nxmin1,0:nymin1,1:nuvz)
         !end SH
         !PARAMETERS
         real,parameter :: eps=1.e-5
         !DUMMY ARGUMENTS
         real :: uuh(0:nxmax-1,0:nymax-1,nuvzmax)
         real :: vvh(0:nxmax-1,0:nymax-1,nuvzmax)
         real :: pvh(0:nxmax-1,0:nymax-1,nuvzmax)

         write(*,*) 'enter calcpv parallelised...'
         pvh(:,:,:)=0.0_8
         
         ! Set number of levels to check for adjacent theta
         nlck=nuvz/3

         ! *** Precalculate all theta levels for efficiency
         do jy=0,nymin1
           do ix=0,nxmin1
             ppmstore(ix,jy,1:nuvz) = akz(1:nuvz)+bkz(1:nuvz)*ps(ix,jy,1,n)
           end do
         end do
         !write(*,*) 'calcpv kappa exponentiation with kappa=',kappa

!$omp    parallel default(none) shared(th_pre,tth,ppmstore, &
!$omp      nxmin1,nymin1,nuvz,n) num_threads(omp_get_max_threads())
!$omp    workshare
         th_pre(0:nxmin1,0:nymin1,1:nuvz)=tth(0:nxmin1,0:nymin1,1:nuvz,n)* &
           (100000./ppmstore(0:nxmin1,0:nymin1,1:nuvz))**kappa
!$omp    end workshare
!$omp    end parallel
         write(*,*) 'calcpv rest...'
         
         jybords=[0,nymin1]
         if (sglobal) jybords(1)=1
         if (nglobal) jybords(2)=nymin1-1
         jyvp_arr(jybords(1):jybords(2)) = [ (jy,jy=jybords(1)+1,jybords(2)+1) ]
         jyvm_arr(jybords(1):jybords(2)) = [ (jy,jy=jybords(1)-1,jybords(2)-1) ]
         jyvp_arr(jybords(2))=jybords(2)
         jyvm_arr(jybords(1))=jybords(1)
         jumpy_arr=2
         jumpy_arr(jybords(1:2))=1

         ixvp_arr(0:nxmin1) = [ (ix,ix=1,nxmin1+1) ]
         ixvm_arr(0:nxmin1) = [ (ix,ix=-1,nxmin1-1) ]
         jumpx_arr=2
         if (.not. xglobal) then
           ixvp_arr(nxmin1)=nxmin1
           ixvm_arr(0)=0
           jumpx_arr([0,nxmin1])=1
         else
           ixvp_arr(nxmin1)=1
           ixvm_arr(0)=nxmin1-1
         end if

         ! Loop over entire grid
         !**********************
!$omp     parallel default(none) shared(jybords,jumpx_arr,jumpy_arr, &
!$omp       ixvp_arr,ixvm_arr,jyvp_arr,jyvm_arr,th_pre,ppmstore,     &
!$omp       vvh,pvh,nuvz,nxmin1,ylat0,dx,dy,n,nlck,xglobal,uuh)      &
!$omp       private(f,cosphi,tanphi,phi,juy,jux,theta,thetap,thetam, &
!$omp               thdn,thup,vx,dvdx,kup,kdn,ivrp,ivr,ivrm,         &
!$omp               uy,dudy,dthetadp,jj,j,khi,klo,   &
!$omp               klvrp,klvrm,k,klpt,kl,kch,i,dt1,dt2) &
!$omp       num_threads(omp_get_max_threads())
!$omp     do
          DO jy=jybords(1),jybords(2)
           phi = (ylat0 + jy * dy) * pi / 180.
           f = SIN(phi)
           cosphi = COS(phi)
           tanphi = f/cosphi
           f = 0.00014585 * f
          
           juy=jumpy_arr(jy)
           
           DO ix=0,nxmin1
             ! Define absolute gap length
             jux=jumpx_arr(ix)
             
             ! Loop over the vertical
             !***********************
             DO  kl=1,nuvz
               theta=th_pre(ix,jy,kl)
               klvrp=kl+1
               klvrm=kl-1
               klpt=kl
               ! If top or bottom level, dthetadp is evaluated between the current
               ! level and the level inside, otherwise between level+1 and level-1
               
               IF (klvrp > nuvz) klvrp=nuvz
               IF (klvrm < 1) klvrm=1
               thetap=th_pre(ix,jy,klvrp)
               thetam=th_pre(ix,jy,klvrm)

               dthetadp=(thetap-thetam)/(ppmstore(ix,jy,klvrp)-ppmstore(ix,jy,klvrm))
               
               ! Compute vertical position at pot. temperature surface on subgrid
               ! and the wind at that position
               !*****************************************************************
               ! a) in x direction
               xloop: DO i=1,2  
                 if (i==1) ivr=ixvm_arr(ix)
                 if (i==2) ivr=ixvp_arr(ix)

                 ! Search adjacent levels for current theta value
                 ! Spiral out from current level for efficiency
                 kup=klpt
                 kdn=klpt-1
                 kch=0

                 do while (kch<nlck) ! No more levels to check, no values found

                   ! Upward branch and downward branch check
                   do k=kup,kdn,kdn-kup
                     if (k<nuvz .and. k>=1) then
                       kch=kch+1

                       if (th_pre(ivr,jy,k)<=th_pre(ivr,jy,k+1)) then
                         khi=k+1
                         klo=k
                       else
                         khi=k
                         klo=k+1 
                       end if

                       dt1=th_pre(ivr,jy,khi)-theta
                       dt2=theta-th_pre(ivr,jy,klo)
                       if (dt1>=0 .and. dt2>=0) then
                         if (dt1+dt2<eps) then
                           dt1=0.5   ! Avoid division by zero error
                           dt2=0.5   ! G.W., 10.4.1996
                         end if
                         vx(i)=(vvh(ivr,jy,khi)*dt2+vvh(ivr,jy,klo)*dt1)/(dt1+dt2)
                         cycle xloop
                       end if
                     end if
                   end do

                   ! Downward branch k
                   kdn=kdn-1
                   ! Upward branch k
                   kup=kup+1
                 end do
                 ! This section used when no values were found
                 ! Must use vvh at current level and long. jux becomes smaller by 1
                 vx(i)=vvh(ix,jy,kl)
                 jux=jux-1
                 ! Otherwise OK
               end do xloop

               IF (jux > 0) THEN
                 dvdx=(vx(2)-vx(1))/real(jux)/(dx*pi/180.)
               ELSE
                 ivrp=ixvp_arr(ix)
                 ivrm=ixvm_arr(ix)
                 if (ix == 0) then
                   if (xglobal) then
                     ivrm=nxmin1-1
                   end if
                 else if (ix == nxmin1) then
                   if (xglobal) then
                     ivrp=1
                   end if
                 end if
                 dvdx=vvh(ivrp,jy,kl)-vvh(ivrm,jy,kl)
                 dvdx=dvdx/real(jumpx_arr(ix))/(dx*pi/180.)
                 ! Only happens if no equivalent theta value
                 ! can be found on either side, hence must use values
                 ! from either side, same pressure level.
               END IF
               
               ! b) in y direction
               yloop: DO jj=1,2
                 if (jj==1) j=jyvm_arr(jy)
                 if (jj==2) j=jyvp_arr(jy)
                 ! Search adjacent levels for current theta value
                 ! Spiral out from current level for efficiency
                 kup=klpt
                 kdn=klpt-1
                 kch=0
                 DO WHILE (kch<nlck) ! No more levels to check, no values found
                   ! Upward branch and downward branch check
                   do k=kup,kdn,kdn-kup
                     if (k<nuvz .and. k>=1) then
                       kch=kch+1

                       if (th_pre(ix,j,k)<=th_pre(ix,j,k+1)) then
                         khi=k+1
                         klo=k
                       else
                         khi=k
                         klo=k+1 
                       end if

                       dt1=th_pre(ix,j,khi)-theta
                       dt2=theta-th_pre(ix,j,klo)
                       if (dt1>=0 .and. dt2>=0) then
                         if (dt1+dt2<eps) then
                           dt1=0.5   ! Avoid division by zero error
                           dt2=0.5   ! G.W., 10.4.1996
                         end if
                         uy(jj)=(uuh(ix,j,khi)*dt2+uuh(ix,j,klo)*dt1)/(dt1+dt2)
                         cycle yloop
                       end if
                     end if
                   end do

                   ! Downward branch k
                   kdn=kdn-1
                   ! Upward branch k
                   kup=kup+1
                 END DO
                 ! This section used when no values were found
                 ! Must use uu at current level and lat. juy becomes smaller by 1
                 uy(jj)=uuh(ix,jy,kl)
                 juy=juy-1
                 ! Otherwise OK
               END DO yloop
               IF (juy > 0) THEN
                 dudy=(uy(2)-uy(1))/real(juy)/(dy*pi/180.)
               ELSE
                 dudy=uuh(ix,jyvp_arr(jy),kl)-uuh(ix,jyvm_arr(jy),kl)
                 dudy=dudy/real(jumpy_arr(jy))/(dy*pi/180.)
               END IF
               
               pvh(ix,jy,kl)=dthetadp*(f+(dvdx/cosphi-dudy+uuh(ix,jy,kl)*tanphi)/r_earth)*(-1.e6)*9.81
               
               !"RESET"
               jux=jumpx_arr(ix)
               juy=jumpy_arr(jy)
             END DO
           END DO
         END DO
!$omp    end do
!$omp    end parallel
         
         ! Fill in missing PV values on poles, if present
         ! Use mean PV of surrounding latitude ring
         
         if (sglobal) then
           do  kl=1,nuvz
             pvh(0:nxmin1,0,kl)=sum(pvh(0:nxmin1,1,kl))/real(nxmin1+1)
           end do
         end if
         if (nglobal) then
           do  kl=1,nuvz
             pvh(0:nxmin1,nymin1,kl)=sum(pvh(0:nxmin1,nymin1-1,kl))/real(nxmin1+1)
           end do
         end if
         write(*,*) 'leave calcpv'
         
       end subroutine
       
