 program main
 implicit none
 
 integer i,j,k
! COEFFICIENTS
!***********************************************************************
 double precision S_T, P_T, k_coef, max_pi, pibar_tmp
 integer nlag
!***********************************************************************

 integer, parameter :: ncoeffs=27, nexog=39, nendog=18, nper=234, npershocks=300, nshocks=13, ntot=336
 real(8), dimension(nexog,nper) :: lags_exog
 real(8), dimension(nendog,nper) :: lags_endog
 real(8), dimension(npershocks,nshocks) :: shocks
 real(8), dimension(ntot,1) :: gov, surplus, NB, rNB, E_pi, pibar, lnNB, r, NBbase, lnNBbase, r5, pinf
 real(8), dimension(ntot,2) :: rsim_out
 real(8), dimension(ntot+20,1) :: r_ext
 real(8) coef(ncoeffs), ar_coef(nshocks), gov_shock(1), surplus_shock(1)
 
 open(10,file='nlag',status='old')
 read(10,*)nlag
 close(10)

 open(11,file='coef.data',status='old')
 read(11,*)coef
 close(11)
 open(12,file='ar_coeffs.data',status='old')
 read(12,*)ar_coef
 close(12)

 S_T=coef(26)

 open(13,file='lags_exog',status='old')
 do i=1,nexog
    read(13,*)lags_exog(i,1:nlag)
 end do
 close(13)
 open(14,file='lags_endog',status='old')
 do i=1,nendog
    read(14,*)lags_endog(i,1:nlag)
 end do
 close(14)
 open(15,file='qerror',status='old')
 do i=1,npershocks
    read(15,*)shocks(i,:)
 end do
 close(15)
 open(16,file='r_sim.out',status='old')
 do i=1,ntot
!    read(16,10110)rsim_out(i,:)
    read(16,*)rsim_out(i,:)
 end do
 close(16)
!10110 format(24f50.8)

 open(17,file='NBbase.data',status='old')
 do i=1,ntot
    read(17,*)lnNBbase(i,1)
    NBbase(i,1)=exp(lnNBbase(i,1)/100.0d0)
 end do
 close(17)
 
 surplus(1:nlag,1)=lags_exog(36,1:nlag)
 r(1:nlag,1)=lags_endog(1,1:nlag)
 r(nlag+1:ntot,1)=rsim_out(nlag+1:ntot,1)
 pinf(1:nlag,1)=lags_endog(5,1:nlag)
 pinf(nlag+1:ntot,1)=rsim_out(nlag+1:ntot,2)
 surplus_shock=shocks(nlag+1,13)
 lnNB(1:nlag,1)=lags_exog(37,1:nlag)
 NB(1:nlag,1)=exp(lags_exog(37,1:nlag)/100.0d0)
 do i=1,nlag
    rNB(i,1)=(r(i,1)/100.0d0)*NB(i,1)
 end do

! extend interest rate forward so we can calculate R5 (5 year ahead average)
 do i=1,ntot+20
    if (i.LE.ntot) then
       r_ext(i,1)=r(i,1)
    else
       r_ext(i,1)=r(ntot,1)
    endif
 end do
 do i=1,ntot
    r5(i,1)=sum(r_ext(i:i+20,1))/20.0d0
 end do

 do i=nlag+1,ntot
    if (i==nlag+1) then
       surplus(i,1)=surplus(i-1,1)+coef(27)*(surplus(i-1,1)-surplus(i-2,1))+surplus_shock(1)
    else
       surplus(i,1)=surplus(i-1,1)+coef(27)*(surplus(i-1,1)-surplus(i-2,1))
    end if
 end do 

 do i=nlag+1,ntot
!    if (i.LT.nper) then
       NB(i,1)=NB(i-1,1)-surplus(i,1)+(r5(i-1,1)/100.0d0)*(NB(i-1,1)-20.0d0*((r5(i,1)/100.0d0)-(r5(i-1,1)/100.0d0))*NB(i-1,1)) &
       -(0.075d0*(pinf(i-1,1)/100.0d0)*NB(i-1,1))
!       rNB(i,1)=(r(i,1)/100.0d0)*NB(i,1)
       lnNB(i,1)=log(NB(i,1))*100.0d0
!    else 
!       NB(i,1)=NB(i-1,1)+exp(gov(i,1)/100.0d0)-exp(tau(i,1)/100.0d0)+(r(nper,1)/100.0d0)*(NB(i-1,1)-0.05d0*(log(r(nper,1)/100.0d0)-log(r(nper-1,1)/100.0d0))*NB(i-1,1))
!       rNB(i,1)=(r(nper,1)/100.0d0)*NB(i,1)
!       lnNB(i,1)=log(NB(i,1))*100.0d0
!    endif
 end do

!rstar=2%
! P_T=0.02d0*NB(nlag+50,1)-S_T
! P_T=0.02d0*NB(nlag+50,1)/S_T
 P_T=0.02d0*NB(nlag+50,1)/(S_T*NBbase(nlag+50,1))
 pibar_tmp=P_T/50.0d0

! max_pi=3.0d0
! if (pibar_tmp > max_pi) then
!    do i=1,ntot
!       lnNB(i,1)=0.0d0
!       pibar(i,1)=max_pi
!    end do
!    else
       do i=1,ntot
          pibar(i,1)=P_T/50.0d0
       end do
! end if

 do i=1,ntot
    write(9988,1002)surplus(i,1), r5(i,1), lnNB(i,1), lnNBbase(i,1), pibar(i,1), pinf(i,1)
 end do
1002 format(6f30.10)
 open(10,file='NBandPibar',status='new')
    write(10,1001)lnNB,pibar
 close(10)
1001 format(4f30.10)

 end

