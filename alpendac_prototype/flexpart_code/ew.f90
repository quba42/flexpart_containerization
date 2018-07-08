
       function ew(t_in)
         implicit none
         real             :: ew
         real, intent(in) :: t_in
         interface
           elemental function ew_nocheck(t_in)
             implicit none
             real, intent(in) :: t_in
             real :: ew_nocheck,w1,w2
           end function 
         end interface
         if(t_in <= 0.) then
           write(*,*) 'TEMP: ',t_in
           stop 'SORRY: T NOT IN [K]'
         end if
         ew=ew_nocheck(t_in)
       end function

       elemental function ew_nocheck(t_in)
         implicit none
         real, intent(in) :: t_in
         real :: ew_nocheck,w1,w2
         interface
           elemental function ew_gg(x)
             implicit none
             real :: ew_gg
             real, intent(in) :: x
           end function 
           elemental function ew_wobus_refined(t_in_wrf) result(ewrf)
             implicit none
             real :: ewrf
             real, intent(in) :: t_in_wrf
           end function
         end interface

         if (abs(t_in-300.0)<100.0) then
           ew_nocheck=ew_wobus_refined(t_in)
         else if (t_in>399.9) then
           w1=min(t_in,410.0)
           w2=(w1-400.0)
           w1=(410.0-w1)
           ew_nocheck=(w1*ew_wobus_refined(t_in)+w2*ew_gg(t_in))/(410.0-400.0)
         else if (t_in<200.1) then
           w1=max(t_in,194.0)
           w2=(w1-194.0)
           w1=(200.0-w1)
           ew_nocheck=(w2*ew_wobus_refined(t_in)+w1*ew_gg(t_in))/(200.0-194.0)
         end if
       end function

       elemental FUNCTION ew_wobus_refined(t_in_wrf) result(ewrf)
         implicit none
         real :: ewrf
         real, intent(in) :: t_in_wrf
         real(8) :: ref_pol,td
         real(8), parameter :: acoeff_ewrf(0:10)=[       & 
           -6958.391726914337D0, 245.45012981061453D0,   & 
           -3.8799554209110156D0, 3.619958715118083D-2,  & 
           -2.207540481565966D-4, 9.194341350199019D-7,   & 
           -2.6487536137880456D-9, 5.211800006142822D-12,  & 
           -6.703450445729384D-15, 5.089509368882543D-18,  & 
           -1.7321846403884217D-21 ]
         interface
           elemental function ew_wobus(t_in) result(esw)
             real, intent(in) :: t_in
             real :: esw
           end function ew_wobus
         end interface
         td=real(t_in_wrf,8)
         
         ref_pol =  acoeff_ewrf(0)+ &
                    td*(acoeff_ewrf(1)+ &
                       td*(acoeff_ewrf(2)+ &
                          td*(acoeff_ewrf(3)+ &
                             td*(acoeff_ewrf(4)+ &
                                td*(acoeff_ewrf(5)+ &
                                   td*(acoeff_ewrf(6)+ &
                                     td*(acoeff_ewrf(7)+ &
                                         td*(acoeff_ewrf(8)+ &
                                             td*(acoeff_ewrf(9)+ &
                                                 td*acoeff_ewrf(10) )))))))))
         
         ewrf=ew_wobus(t_in_wrf)*real(ref_pol)
       end function

       elemental function ew_wobus(t_in)
         !       this function returns the saturation vapor pressure esw (millibars)
         !       over liquid water given the temperature t (kelvin). the polynomial
         !       approximation below is due to herman wobus, a mathematician who
         !       worked at the navy weather research facility, norfolk, virginia,
         !       but who is now retired. the coefficients of the polynomial were
         !       chosen to fit the values in table 94 on pp. 351-353 of the smith-
         !       sonian meteorological tables by roland list (6th edition). the
         !       approximation is valid for -50 < t < 100c.
         !
         !       baker, schlatter  17-may-1982    original version.
         !
         !       es0 = saturation vapor ressure over liquid water at 0c
         implicit none
         real, intent(in) :: t_in
         real :: ew_wobus
         double precision :: pol,t
         double precision :: es0
         es0=6.1078D0
         t=dble(t_in-273.15)
         pol = 0.99999683D0      + t*(-0.90826951D-02 +  &
             t*(0.78736169D-04   + t*(-0.61117958D-06 +  &
             t*(0.43884187D-08   + t*(-0.29883885D-10 +  &
             t*(0.21874425D-12   + t*(-0.17892321D-14 +  &
             t*(0.11112018D-16   + t*(-0.30994571D-19)))))))))
         ew_wobus = real(100.0D0*es0 / pol**8)
       end function ew_wobus

       elemental function ew_gg(x)
         !     ****************************************************************
         !     SAETTIGUNGSDAMPFDRUCK UEBER WASSER IN PA. X IN KELVIN.
         !     NACH DER GOFF-GRATCH-FORMEL.
         !     ****************************************************************
         implicit none
         real :: ew_gg
         real, intent(in) :: x
         real :: y,a,c,d
         y = 373.16/x
         a = -7.90298 * (y-1.)
         a = a + (5.02808*0.43429*LOG(y))
         c = -1. +  10.0**( 11.344 - ( (11.344/373.16) * x ) )
         c = -1.3816E-7 * c
         d = (1.-y)*3.49149
         d = 8.1328E-3 * ( -1. + (10.**d) )
         y = a + c + d
         ew_gg=1013.246*(10.**y)*100.       ! Saettigungsdampfdruck in Pa
       end function
 
       elemental FUNCTION ew_fitted(t_in_ewf)
         ! a 14th order polynomial fit to GG -- performes worse than Wobus however
         implicit none
         integer :: i
         real, intent(in) :: t_in_ewf
         real :: ew_fitted
         real(8) :: t_in_ewf_dble,ref_pol
         real(8), parameter :: a_ewfit(0:14) = [            & 
           -1.2241382741591574D6,   6.2760007286859395D4,   &
           -1.4467525836456739D3,   1.9735651467825693D1,   &
           -0.17595965373830060D0,  0.10629184255214715D-2, &
           -4.3022048695559316D-6,  1.0568061161848794D-8,  &
           -7.9252050616823303D-12, -4.5604622615056078D-14,& 
            1.9821751947758235D-16, -4.0185846042019225D-19,& 
            4.7379290439536481D-22, -3.1275571418968759D-25,& 
            8.9966308005149801D-29 ]
         t_in_ewf_dble=real(t_in_ewf,8)
         ref_pol=0.0D0
         do i=14,1,-1
           ref_pol = t_in_ewf_dble*(a_ewfit(i)+ref_pol)
         end do
         ref_pol=ref_pol+a_ewfit(0)
         ew_fitted=real(ref_pol)
       end function
          
