 program main
 implicit none
 
 CALL CALC_RESIDS
 CALL CALC_SHOCKS
  
 end

 subroutine CALC_RESIDS
 implicit none
 integer i,j,k
! COEFFICIENTS
!***********************************************************************
 double precision CONSTEPINF, CONSTEBETA, CONSTELAB, CTREND,CBETA
 double precision CGAMMA, CTOU, CLANDAW, CG, CURVP, CURVW, CGY, CLANDAP 
 double precision CBETABAR, CRK, CW, CIKBAR, CIK, CLK, CKY, CIY, CCY 
 double precision CRKKY, CWHLC, CWLY 
 double precision CSADJCOST, CSIGMA, CHABB, CPROBW, CSIGL, CPROBP, CINDW 
 double precision CINDP, CZCAP, CFC, CRPI, CRR, CRY, CRDY, CALFA, PARW
 double precision PARR, CCHI, PSIM, PSIM1, PSIS, S_T
 double precision SURP_RHO
!***********************************************************************

 integer, parameter :: ncoeffs=27, nper=234, nact=20
 real(8) coef(ncoeffs)
 real(8), dimension(nper,nact) :: act_data, act_sim_data
 real(8) inve(nper), q1tmp(nper-1,1), var_beta(7,7)
 real(8), dimension(nper,7) :: expectation1, expect_matrix
 real(8), dimension(nper,8) :: expectation
 real(8), dimension(1,1) ::  beta_inv, beta_rk, beta_mpk, beta_q
 real(8), dimension(nper,1) :: inv, kapital, mpk, exprk, expmpk, q1, q
 real(8), dimension(nper-1,13) :: f, lr, resids
 real(8), dimension(nper,1) :: shockdummy
 
 open(10,file='coef.data',status='old')
 read(10,*)coef
 close(10)
 
 CSADJCOST=coef(1)
 CSIGMA=coef(2)
 CHABB=coef(3)
 CPROBW=coef(4)
 CSIGL=coef(5)
 CPROBP=coef(6)
 CINDW=coef(7)
 CINDP=coef(8)
 CZCAP=coef(9)
 CFC=coef(10)
 CRPI=coef(11)
 CRR=coef(12)
 CRY=coef(13)
 CRDY=coef(14)
 CALFA=coef(15)
 PARW=coef(16)
 PARR=coef(17)
 CCHI=coef(18)
 PSIM=coef(19)
 PSIM1=coef(20)
 PSIS=coef(21)

! S(T) is the real surplus, treated as an arbitrary constant that is estimated
 S_T=coef(26)
 SURP_RHO=coef(27)


 CONSTEPINF=0.78
 CONSTEBETA=0.16
 CONSTELAB=0.53
 CTREND=0.43
 CBETA=1.0/(1+CONSTEBETA/100.0)
 CGAMMA=CTREND/100.0+1
 CTOU=0.025
 CLANDAW=1.5
 CG=0.18
 CURVP=10
 CURVW=10
 CGY=0.52612
 CLANDAP=coef(10)
 CBETABAR=CBETA*CGAMMA**(-coef(2))
 CRK=(1.7**coef(18))*(CBETA**(-1))*(CGAMMA**coef(2))-(1.0-CTOU)
 CW=(coef(15)**coef(15)*(1.0-coef(15))**(1-coef(15)) &
  /(CLANDAP*CRK**coef(15)))**(1.0/(1.0-coef(15)))
 CIKBAR=(1.0-(1.0-CTOU)/CGAMMA)
 CIK=(1.0-(1.0-CTOU)/CGAMMA)*CGAMMA
 CLK=((1.0-coef(15))/coef(15))*(CRK/CW)
 CKY=coef(10)*(CLK)**(coef(15)-1.0)
 CIY=CIK*CKY
 CCY=1.0-CG-CIK*CKY
 CRKKY=CRK*CKY
 CWHLC=(1.0/CLANDAW)*(1.0-coef(15))/coef(15)*CRK*CKY/CCY
 CWLY=1.0-CRK*CKY
 
 open(11,file='act_data.data',status='old')
 do i=1,nper
   read(11,*)act_data(i,:)
 end do
 close(11)

 inv(:,1)=act_data(:,2)

 call AR_param(act_data,nper,nact,2,beta_inv)
 call AR_param(act_data,nper,nact,13,beta_rk)

 do i=1,nper
   kapital(i,1)=(1.0-(1.0-CTOU)/CGAMMA)*inv(i,1)/(1.0-(1.0-CTOU)/CGAMMA/beta_inv(1,1))
   mpk(i,1)=CZCAP*(act_data(i,4)+act_data(i,6)-kapital(i-1,1))
   exprk(i,1)=beta_rk(1,1)*act_data(i-1,13)
 end do
 mpk(1,1)=0.0
 exprk(1,1)=0.0

 call AR_param(mpk,nper,1,1,beta_mpk)
 do i=1,nper
   expmpk(i,1)=beta_mpk(1,1)*mpk(i-1,1)
 end do

!******************************************************************************
! expmpk has 2 zeros at the start, could use expmpk(2,1)=1/beta_mpk*mpk(3,1)
!******************************************************************************
 expmpk(1,1)=0.0

 do i=1,nper
   q1(i,1)=CRK/(CRK+1.0-CTOU)*expmpk(i+1,1)-exprk(i+1,1)
 end do
 q1(nper,1)=0.0

 do i=1,nper-1
   q1tmp(i,1)=q1(i,1)
 end do

 call AR_param(q1tmp,nper-1,1,1,beta_q)
 do i=1,nper
   q(i,1)=q1(i,1)/(1.0-(1.0-CTOU)/(CRK+1.0-CTOU)*beta_q(1,1))
 end do


! act_sim_data order:
! c, inv, y, lab, inf, w, r, k, q, mpk, networth, s, rk, ce, mzero, mtwo, gov, tau, NB, surplus
 do i=1,nper
   do j=1,7
     act_sim_data(i,j)=act_data(i,j)
   end do
   act_sim_data(i,8)=kapital(i,1)
   act_sim_data(i,9)=q(i,1)
   act_sim_data(i,10)=mpk(i,1)
   do j=11,nact
     act_sim_data(i,j)=act_data(i,j)
   end do
 end do

! open(11,file='act_sim_data.data',status='new')
! do i=1,nper
!   write(11,221)act_sim_data(i,:)
!   write(2222,*)kapital(i,1),q(i,1),mpk(i,1)
! end do
! close(11)
! 221 format(16f20.12)



! expect_matrix order;
! c inv lab inf q w rk
 do i=1,nper
   expect_matrix(i,1)=act_sim_data(i,1)
   expect_matrix(i,2)=act_sim_data(i,2)
   expect_matrix(i,3)=act_sim_data(i,4)
   expect_matrix(i,4)=act_sim_data(i,5)
   expect_matrix(i,5)=act_sim_data(i,9)
   expect_matrix(i,6)=act_sim_data(i,6)
   expect_matrix(i,7)=act_sim_data(i,12)
 end do

 call VARE_NOCONST(expect_matrix,nper,7,var_beta)
 do i=1,nper
   do j=1,7
     if (i==1) then
       expectation1(i,j)=0.0
     else 
       expectation1(i,j)=var_beta(1,j)*expect_matrix(i-1,1) & 
                       +var_beta(2,j)*expect_matrix(i-1,2) &
                       +var_beta(3,j)*expect_matrix(i-1,3) &
                       +var_beta(4,j)*expect_matrix(i-1,4) &
                       +var_beta(5,j)*expect_matrix(i-1,5) &
                       +var_beta(6,j)*expect_matrix(i-1,6) &
                       +var_beta(7,j)*expect_matrix(i-1,7)
     end if
   end do
 end do

! reordering the expectation matrix
 do i=1,nper
   do j=1,5
     expectation(i,j)=expectation1(i,j)
   end do
   expectation(i,6)=expmpk(i,1)
   do j=6,7
     expectation(i,j+1)=expectation1(i,j)
   end do
 end do

 do j=1,nper
   shockdummy(j,1)=0
 end do
 do j=91,101
   shockdummy(j,1)=1
 end do
! Calculate RHS
 do j=1,nper-1
   if (j==1) then
     do k=1,13
       f(j,k)=0.0
     end do
   else
! act_sim_data order:
! c, inv, y, lab, inf, w, r, k, q, mpk, networth, s, rk, ce, mzero, mtwo, gov, tau, NB, surplus

! GDP identity - consumption (government spending shock) (last term-warning)
!   f(j,1)=(act_sim_data(j,3)-ciy*act_sim_data(j,2) & 
!         -crkky*(1/(czcap/(1-czcap)))*act_sim_data(j,10)*crk-0.01*act_sim_data(j,14))/ccy
! government spending from data
   f(j,1)=act_sim_data(j,17)
! Investment equation - investment (investment shock)
   f(j,2)=(1/(1+cbetabar*cgamma))*(act_sim_data(j-1,2)+cbetabar*cgamma*expectation(j+1,2) &
         +(1/(cgamma*cgamma*csadjcost))*act_sim_data(j,9))
! Production function - output (productivity shock)
   f(j,3)=cfc*(calfa*(act_sim_data(j-1,8)+1/(czcap/(1-czcap))*act_sim_data(j,10)) &
         +(1-calfa)*act_sim_data(j,4));
! Taylor rule -inflation (No YFlex yet)
   f(j,4)=(act_sim_data(j,7)-crr*act_sim_data(j-1,7)-cry*(1-crr)*(act_sim_data(j,3)) &
         -crdy*(act_sim_data(j,3)-act_sim_data(j-1,3)))/(crpi*(1-crr))
! Wage setting -real wage(wage mark-up)
!**********************************************************************************************
! CHANGE TO BE CONSISTENT WITH FORTRAN (TAKE W TO THE RHS)?
!**********************************************************************************************
   f(j,5)=((1/(1+cbetabar*cgamma))*act_sim_data(j-1,6) &
         +(cbetabar*cgamma/(1+cbetabar*cgamma))*expectation(j+1,7) &
         +(cindw/(1+cbetabar*cgamma))*act_sim_data(j-1,5) & 
         -(1+cbetabar*cgamma*cindw)/(1+cbetabar*cgamma)*act_sim_data(j,5) &
         +(cbetabar*cgamma)/(1+cbetabar*cgamma)*expectation(j+1,4) &
         +(1-cprobw)*(1-cbetabar*cgamma*cprobw)/((1+cbetabar*cgamma)*cprobw) &
         *(1/((clandaw-1)*curvw+1))*(csigl*act_sim_data(j,4)+(1/(1-chabb/cgamma))*act_sim_data(j,1) &
         -((chabb/cgamma)/(1-chabb/cgamma))*act_sim_data(j-1,1)-act_sim_data(j,6)))
! Consumption equation - interest rate(Preference shock)
   f(j,6)=expectation(j+1,4) & 
         +1/((1-chabb/cgamma)/(csigma*(1+chabb/cgamma)))*((chabb/cgamma)/(1+chabb/cgamma)*act_sim_data(j-1,1) &
         +(1/(1+chabb/cgamma))*expectation(j+1,1) &
         +((csigma-1)*cwhlc/(csigma*(1+chabb/cgamma)))*(act_sim_data(j,4)-expectation(j+1,3)) &
         -act_sim_data(j,1))
! Price setting - return to capital (price mark up shock)
   f(j,7)=(((act_sim_data(j,5)/(1/(1+cbetabar*cgamma*cindp)) &
         -(cbetabar*cgamma*expectation(j+1,4) &
         +cindp*act_sim_data(j-1,5)))/(((1-cprobp)*(1-cbetabar*cgamma*cprobp)/cprobp)/((cfc-1)*curvp+1)) &
         -(1-calfa)*act_sim_data(j,6))/calfa)
! Labour supply shock
   f(j,8)=csigl*act_sim_data(j,4) &
         +(1/(1-chabb/cgamma))*(act_sim_data(j,1) &
         -(chabb/cgamma)*act_sim_data(j-1,1)) &
         -(act_sim_data(j,5)-expectation(j,4))
! external premium equation-premium(premium shock)
!   f(j,9)=-1/cchi*act_sim_data(j,12)+act_sim_data(j,9)+act_sim_data(j,8);
   f(j,9)=cchi*(act_sim_data(j,9)+act_sim_data(j,8)-act_sim_data(j,11))-PSIS*act_sim_data(j,15)
! networth equation-networth (networth shock)
!   f(j,10)=1/1.7*(act_sim_data(j,11)-0.99*act_sim_data(j-1,11)+0.7*expectation(j,8));
   f(j,10)=1.7*((crk/(crk+(1-ctou)))*(act_sim_data(j,3)-act_sim_data(j,8)) &
          +((1-ctou)/(crk+(1-ctou)))*act_sim_data(j,9)-act_sim_data(j-1,9)) &
          -0.7*(act_sim_data(j-1,7)-expectation(j,4)+act_sim_data(j-1,12)) &
          +0.99*act_sim_data(j-1,11)
! mzero equation (mzero shock) (no_crisis)
   f(j,11)=act_sim_data(j-1,15) &
          +psim1*(act_sim_data(j,16)-act_sim_data(j-1,16))
! mzero equation (mzero shock) (crisis)
   f(j,12)=act_sim_data(j-1,15) &
          +psim*act_sim_data(j,12)
! Surplus from data. Surplus is an ARIMA(1,1,0), with the coefficients part of the estimation
   if (j<3) then
      f(j,13)=0.0d0
   else 
   f(j,13)=act_sim_data(j-1,20)+SURP_RHO*(act_sim_data(j-1,20)-act_sim_data(j-2,20))
   end if
          
! act_sim_data order:
! c, inv, y, lab, inf, w, r, k, q, mpk, networth, s, rk, ce, mzero, mtwo, gov, tau, NB, surplus
          
   end if
 end do


! Calculate LHS-RHS
 do j=1,nper-1
   if (j==1) then
     do k=1,13
       lr(j,k)=0.0
     end do
   else
!     lr(j,1)=(act_sim_data(j,1)-f(j,1))*(-ccy)
! government spending from data
     lr(j,1)=f(j,1)
     lr(j,2)=act_sim_data(j,2)-f(j,2)
     lr(j,3)=(act_sim_data(j,3)-f(j,3))/cfc
     lr(j,4)=(act_sim_data(j,5)-f(j,4))*(-crpi*(1-crr))
     lr(j,5)=(act_sim_data(j,6)-f(j,5))
     lr(j,6)=(act_sim_data(j,7)-f(j,6))*(1-chabb/cgamma)/(1+chabb/cgamma)/csigma
     lr(j,7)=-((act_sim_data(j,10)-f(j,7)) &
            -lr(j,3)/calfa)*calfa*1/(1+cbetabar*cgamma*cindp)*(1-cbetabar*cgamma*cprobp) &
         *(1-cprobp)/cprobp/((cfc-1)*curvp+1)
     lr(j,8)=act_sim_data(j,6)-f(j,8)
     lr(j,9)=(act_sim_data(j,12)-f(j,9))
     lr(j,10)=(act_sim_data(j,11)-f(j,10))
     lr(j,11)=act_sim_data(j,15)-f(j,11)
     lr(j,12)=act_sim_data(j,15)-f(j,12)
     lr(j,13)=act_sim_data(j,20)-f(j,13)
   end if
 end do

  do i=1,nper-2
   resids(i,1)=lr(i+1,1)
   resids(i,2)=lr(i+1,6)
   resids(i,3)=lr(i+1,2)
   resids(i,4)=lr(i+1,4)
   resids(i,5)=lr(i+1,3)
   resids(i,6)=lr(i+1,7)
   resids(i,7)=lr(i+1,5)
   resids(i,8)=lr(i+1,8)
   resids(i,9)=lr(i+1,9)
   resids(i,10)=lr(i+1,10)
   resids(i,11)=lr(i+1,11)
   resids(i,12)=lr(i+1,12)
   resids(i,13)=lr(i+2,13)
 end do

! resids from period 2 to 110
 open(unit=12,file='resids.data')
 do i=1,nper-2
   write(12,222) resids(i,:)
 end do
 close(12)
 222 format(13f20.8)

 END SUBROUTINE CALC_RESIDS
 
 SUBROUTINE CALC_SHOCKS
 implicit none
 integer i,j,k

 integer, parameter :: nper=232, nvar=13
 real(8), dimension(nper,2) :: X_trend
 real(8), dimension(nper,1) :: Y_trend, trend_resid
 real(8), dimension(nper-1,1) :: Y_ar, X_ar, ar_resid, d_prod, d_gov, d_tau
 real(8), dimension(nper-2,2) :: d_prod_X, err1_X, d_gov_X
 real(8), dimension(nper-2,2) :: d_tau_X
 real(8), dimension(nper-2,1) :: d_prod_Y, prod_shock, err1_Y, shock1, d_gov_Y, gov_shock, d_tau_Y, tau_shock
 real(8), dimension(2,1) :: beta_trend
 real(8), dimension(2,1) :: beta_trend_tau
 real(8), dimension(1,1) :: beta_ar, bgp_const
 real(8), dimension(nvar,1) :: all_beta_ar
 real(8), dimension(nper,nvar) :: resids, resids_dt
 real(8), dimension(nper-1,nvar) :: shocks
 
 integer, parameter :: ncoeffs=27
 real(8) coef(ncoeffs)
 double precision S_T
 
 open(10,file='coef.data',status='old')
 read(10,*)coef
 close(10)
 S_T=coef(26)

! resids from period 2 to 110
 open(10,file='resids.data',status='old')
 do i=1,nper
     read(10,101)resids(i,:)
 end do
 close(10)
 101 format(13f20.8)
 
 do i=1,nper
   X_trend(i,1)=1
   X_trend(i,2)=i
 end do
 do i=nvar,1,-1
! detrend residuals
   Y_trend(:,1)=resids(:,i)
   call OLS(Y_trend,X_trend,nper,2,beta_trend,trend_resid)
   resids_dt(:,i)=trend_resid(:,1)

   if (i==5) then
! calculate d(prod) constant and rho
     d_prod(:,1)=resids(2:nper,i)-resids(1:nper-1,i)
     do j=1,nper-2
       d_prod_X(j,1)=1
       d_prod_X(j,2)=d_prod(j,1)
       d_prod_Y(j,1)=d_prod(j+1,1)
     end do
     call OLS(d_prod_Y,d_prod_X,nper-2,2,beta_trend,prod_shock)
     bgp_const(1,1)=beta_trend(1,1)
     open(11,file='bgp_const.data')
     write(11,*)bgp_const
     close(11)
     all_beta_ar(i,1)=beta_trend(2,1)
     shocks(1,i)=0.0
     shocks(2:nper-1,i)=prod_shock(:,1)
   else if (i==1) then
!     err1_Y(:,1)=resids_dt(3:nper,1)
!     err1_X(:,1)=resids_dt(2:nper-1,1)
!     err1_X(:,2)=prod_shock(:,1)
!     call OLS(err1_Y,err1_X,nper-2,2,beta_trend,shock1)
!     all_beta_ar(i,1)=beta_trend(1,1)
!     all_beta_ar(i+1,1)=beta_trend(2,1)
!     shocks(1,i)=0.0
!     shocks(2:nper-1,i)=shock1(:,1)
! government spending is an ARIMA(1,1,0)
     d_gov(:,1)=resids(2:nper,i)-resids(1:nper-1,i)
     do j=1,nper-2
       d_gov_X(j,1)=1
       d_gov_X(j,2)=d_gov(j,1)
       d_gov_Y(j,1)=d_gov(j+1,1)
     end do
     call OLS(d_gov_Y,d_gov_X,nper-2,2,beta_trend,gov_shock)
     all_beta_ar(i,1)=beta_trend(2,1)
     shocks(1,i)=0.0
     shocks(2:nper-1,i)=gov_shock(:,1)
! surplus is an ARIMA(1,1,0). Coeffs are from the search. 
   else if (i==13) then
     shocks(1,i)=0.0
     shocks(2:nper-1,i)=resids(2:nper-1,i)
   else
! calculate AR coefficient
     Y_ar(:,1)=resids_dt(2:nper,i)
     X_ar(:,1)=resids_dt(1:nper-1,i)
     call OLS(Y_ar,X_ar,nper-1,1,beta_ar,ar_resid)
! calculate shocks
     shocks(:,i)=ar_resid(:,1)
     if(beta_ar(1,1)>=1.0) then
       beta_ar(1,1)=0.999
       do j=2,nper-1
         shocks(j,i)=Y_ar(j,1)-beta_ar(1,1)*X_ar(j,1)
       end do
     end if
     all_beta_ar(i,1)=beta_ar(1,1)
   end if
 end do


 open(11,file='ar_coeffs.data')
 do i=1,nvar
   write(11,102)all_beta_ar(i,1)
 end do
 close(11)
 102 format(1f12.8)

! shocks from period 4 to 110
 open(12,file='shocks.data')
 do i=2,nper-1
   write(12,103)shocks(i,:)
 end do
 close(12)
 103 format(13f16.10)

 END SUBROUTINE CALC_SHOCKS
 

 SUBROUTINE COVAR(series,nper,nvar,covar_series)
 IMPLICIT NONE
! Estimate AR parameter for data:
! INPUTS: 
! series=data
! nper=number of periods
! nvar=number of variables
 integer nper, nvar, i, j
 real(8), dimension (nper,nvar) :: series
 real(8), dimension (nper,1) ::  series1_dm, series2_dm, series1_2
 real(8), dimension (1,nvar) :: mean_series
 real(8), dimension (nvar,nvar) :: covar_series

 mean_series(1,:)=sum(series,DIM=1)/(nper)
 do i=1,nvar
   do j=1,nvar
     series1_dm(:,1)=series(:,i)-mean_series(1,i)
     series2_dm(:,1)=series(:,j)-mean_series(1,j)
     series1_2(:,1)=series1_dm(:,1)*series2_dm(:,1)
     covar_series(i,j)=sum(series1_2(:,1))/(nper-1)
   end do
 end do

 END SUBROUTINE COVAR


 SUBROUTINE VARIANCE(series,nper,nvar,var_series)
 IMPLICIT NONE
! Estimate AR parameter for data:
! INPUTS: 
! series=data
! nper=number of periods
! nvar=number of variables
 integer nper, nvar, i, j
 real(8), dimension (nper,nvar) :: series, series_dm, series_dm_sq
 real(8), dimension (1,nvar) :: mean_series, var_series

 mean_series(1,:)=sum(series,DIM=1)/(nper)
 do j=1,nvar
   series_dm(:,j)=series(:,j)-mean_series(1,j)
   series_dm_sq(:,j)=series_dm(:,j)*series_dm(:,j)
   var_series(1,j)=sum(series_dm_sq(:,j))/(nper-1)
 end do
 
 END SUBROUTINE VARIANCE

 SUBROUTINE VAR1_X(series,exog,nper,nendog,nexog,beta,residual)
 IMPLICIT NONE
! Estimate AR parameter for data:
! INPUTS: 
! series=data
! nper=number of periods
! nendog=number of endogenous variables
! nexog=number of exogenous variables
! adds a constant to the end of X
 integer nper, nvar, i, j, nendog, nexog
 real(8), dimension (nper,nendog) :: series
 real(8), dimension (nper,nexog) :: exog
 real(8), dimension (nendog+nexog+1,nendog) ::  beta, XTY
 real(8), dimension (nendog+nexog+1,nendog+nexog+1) ::  XTX, XTXI
 real(8), dimension (nper-1,nendog) :: Y, residual, Xbeta
 real(8), dimension (nper-1,nendog+nexog+1) :: X
 real(8), dimension (nendog+nexog+1,nper-1) :: XT
 integer, dimension(nendog+nexog+1) :: INDX
 
 do i=1,nper-1
   do j=1,nendog
     Y(i,j)=series(i+1,j)
     X(i,j)=series(i,j)
   end do
   do j=1,nexog
     X(i,nendog+j)=exog(i+1,j)
   end do
   X(i,nendog+nexog+1)=1.0
 end do

 XT=transpose(X)
 XTX=matmul(XT,X)

! call inverse(XTX,XTXI,nendog+nexog+1)
! call matinv(nendog+nexog+1,XTX,XTXI)
! call MIGS(XTX,nendog+nexog+1,XTXI,INDX)
 call matinv2(XTX,nendog+nexog+1)
 XTXI=XTX

 XTY=matmul(XT,Y)
 beta=matmul(XTXI,XTY)
 Xbeta=matmul(X,beta)
 
 residual(:,:)=Y(:,:)-Xbeta(:,:)

 END SUBROUTINE VAR1_X

 SUBROUTINE OLS(Y,X,nper,nexog,beta,residual)
 IMPLICIT NONE
! Estimate OLS regression Y on X
! INPUTS: 
! Y=endogenous variables
! X=exogenous variables
! nper=number of periods
! nexog=number of variables in X
 integer nper, nexog, i
 real(8), dimension(nper,nexog) :: X
 real(8), dimension(nper,1) :: Y, Xbeta, residual
 real(8), dimension (nexog,nexog) ::  XTX, XTXI, XTY
 real(8), dimension (nexog,nper) :: XT
 real(8), dimension (nexog,1) :: beta
 integer, dimension(nexog) :: INDX
  
 XT=transpose(X)
 XTX=matmul(XT,X)
! call inverse(XTX,XTXI,nexog)
! call matinv(nexog,XTX,XTXI)
! call MIGS(XTX,nexog,XTXI,INDX)
 call matinv2(XTX,nexog)
 XTXI=XTX

 XTY=matmul(XT,Y)
 beta=matmul(XTXI,XTY)
   
 Xbeta=matmul(X,beta)
 
 do i=1,nper
   residual(i,1)=Y(i,1)-Xbeta(i,1)
 end do

 END SUBROUTINE OLS

 SUBROUTINE AR_param(act_data,nper,nact,var,beta)
 IMPLICIT NONE
! Estimate AR parameter for data:
! INPUTS: 
! act_data=data
! nper=number of periods in act_data
! nact=number of variables in act_data
! var=column the data you want to do AR on
 integer nper, nact, var, i
 real(8) act_data(nper,nact), beta(1,1)
 real(8) Y(nper-1,1), X(nper-1,1), XT(1,nper-1), XTX(1,1)
 real(8) XTXI(1,1), XTY(1,1)
 
 Y(:,1)=act_data(2:nper,var)
 X(:,1)=act_data(1:nper-1,var)
 XT=transpose(X)

 XTX=matmul(XT,X)
 XTXI=1.0/XTX
 XTY=matmul(XT,Y)
 beta=XTXI*XTY

 END SUBROUTINE AR_param

 SUBROUTINE VARE_NOCONST(series,nper,nvar,beta)
 IMPLICIT NONE
! Estimate AR parameter for data:
! INPUTS: 
! series=data
! nper=number of periods
! nvar=number of variables
 integer nper, nvar, i, j
 real(8), dimension (nper,nvar) :: series
 real(8), dimension (nvar,nvar) ::  beta, XTX, XTXI, XTY
 real(8), dimension (nper-1,nvar) :: Y, X
 real(8), dimension (nvar,nper-1) :: XT
 integer, dimension(nvar) :: INDX
 
 do i=1,nper-1
   do j=1,nvar
     Y(i,j)=series(i+1,j)
     X(i,j)=series(i,j)
   end do
 end do
 XT=transpose(X)

 XTX=matmul(XT,X)

! call inverse(XTX,XTXI,nvar)
! call matinv(nvar,XTX,XTXI)
! call MIGS(XTX,nvar,XTXI,INDX)
 call matinv2(XTX,nvar)
 XTXI=XTX

 XTY=matmul(XT,Y)
 beta=matmul(XTXI,XTY)

 END SUBROUTINE VARE_NOCONST

 SUBROUTINE VARE(series,nper,nvar,beta)
 IMPLICIT NONE
! Estimate AR parameter for data:
! INPUTS: 
! series=data
! nper=number of periods
! nvar=number of variables
 integer nper, nvar, i, j
 real(8), dimension (nper,nvar) :: series
 real(8), dimension (nvar,nvar) ::  beta, XTX, XTXI, XTY
 real(8), dimension (nper-1,nvar) :: Y, X
 real(8), dimension (nvar,nper-1) :: XT
 integer, dimension(nvar) :: INDX
 
 do i=1,nper-1
   do j=1,nvar
     Y(i,j)=series(i+1,j)
     X(i,j)=series(i,j)
   end do
   X(i,nvar+1)=1.0
 end do
 XT=transpose(X)

 XTX=matmul(XT,X)

! call inverse(XTX,XTXI,nvar)
! call matinv(nvar,XTX,XTXI)
! call MIGS(XTX,nvar,XTXI,INDX)
 call matinv2(XTX,nvar)
 XTXI=XTX

 XTY=matmul(XT,Y)
 beta=matmul(XTXI,XTY)

 END SUBROUTINE VARE

  subroutine inverse(aa,cc,nn)
!============================================================
! Inverse matrix
! Method: Based on Doolittle LU factorization for Ax=b
! Alex G. December 2009
!-----------------------------------------------------------
! input ...
! aa(nn,nn) - array of coefficients for matrix A
! nn      - dimension
! output ...
! cc(nn,nn) - inverse matrix of A
! comments ...
! the original matrix aa(nn,nn) will be destroyed 
! during the calculation
!===========================================================
implicit none 
integer nn
real(8) aa(nn,nn), cc(nn,nn), aatmp(nn,nn)
real(8) LL(nn,nn), UU(nn,nn), bb(nn), dd(nn), xx(nn)
real(8) coeff
integer i, j, k

! step 0: initialization for matrices L and U and b
! Fortran 90/95 aloows such operations on matrices
LL=0.0
UU=0.0
bb=0.0

! duplicate a matrix so it won't be destroyed
aatmp=aa

! step 1: forward elimination
do k=1, nn-1
   do i=k+1,nn
      coeff=aatmp(i,k)/aatmp(k,k)
      LL(i,k) = coeff
      do j=k+1,nn
         aatmp(i,j) = aatmp(i,j)-coeff*aatmp(k,j)
      end do
   end do
end do

! Step 2: prepare L and U matrices 
! L matrix is a matrix of the elimination coefficient
! + the diagonal elements are 1.0
do i=1,nn
  LL(i,i) = 1.0
end do
! U matrix is the upper triangular part of A
do j=1,nn
  do i=1,j
    UU(i,j) = aatmp(i,j)
  end do
end do

! Step 3: compute columns of the inverse matrix cc
do k=1,nn
  bb(k)=1.0
  dd(1) = bb(1)
! Step 3a: Solve Ld=b using the forward substitution
  do i=2,nn
    dd(i)=bb(i)
    do j=1,i-1
      dd(i) = dd(i) - LL(i,j)*dd(j)
    end do
  end do
! Step 3b: Solve Ux=d using the back substitution
  xx(nn)=dd(nn)/UU(nn,nn)
  do i = nn-1,1,-1
    xx(i) = dd(i)
    do j=nn,i+1,-1
      xx(i)=xx(i)-UU(i,j)*xx(j)
    end do
    xx(i) = xx(i)/UU(i,i)
  end do
! Step 3c: fill the solutions x(nn) into column k of cc
  do i=1,nn
    cc(i,k) = xx(i)
  end do
  bb(k)=0.0
end do
end subroutine inverse

! Matrix inversion: B = Inv(A) by Gauss-Jordan method
! A and B are n by n matrices
Subroutine Matinv(n,A,B)  
  ! Labels: 10, 20, 30
  parameter(MMAX=25,NMAX=10)
  integer n
  real(8)  A(MMAX,MMAX), B(MMAX,2*MMAX)  
  integer i,j,k
  real(8) bb

  do i = 1, n
    do j = 1, n
      B(i,j + n) = 0.d0
      B(i,j) = A(i,j)
    end do
    B(i,i + n) = 1.d0
  end do 

  do k = 1, n
    if (k.eq.n) goto 10
    m = k
    do i = k+1, n
      if (abs(B(i,k)) > abs(B(m,k)))  m = i
    end do
    if (m == k) goto 10
    do j = k, 2*n
      bb = B(k,j)
      B(k,j) = B(m,j)
      B(m,j) = bb
    end do
10  do j = k+1, 2*n 
      B(k,j) = B(k,j) / B(k,k)
    end do
    if (k.eq.1) goto 20
    do i = 1, k-1
      do j = k+1, 2*n
        B(i,j) = B(i,j) - B(i,k) * B(k,j)
      end do
    end do
    if (k.eq.n) goto 30
20  do i = k+1, n
      do j = k+1, 2*n
        B(i,j) = B(i,j) - B(i,k) * B(k,j)
      end do
    end do
  end do    ! k loop

30  do i = 1, n
      do j = 1, n   
        B(i,j) = B(i,j + n)
      end do
    end do
  return
end ! Matinv()


 SUBROUTINE SORT(ARR,n)
!SORT ARRAY INTO NUMERICAL ORDER
 IMPLICIT NONE
 INTEGER i,j,k,n
 real(8), dimension(n,1), intent(INOUT) :: arr 
! real(8), dimension(:), intent(INOUT) :: arr 
 real(8) a
 
! n=size(arr,1)
 do j=2,n
   a=arr(j,1)
   do i=j-1,1,-1
     if (arr(i,1) <= a ) exit
     arr(i+1,1)=arr(i,1)
   end do
   arr(i+1,1)=a
 end do

 END SUBROUTINE SORT


SUBROUTINE matinv2(a,n)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n
  INTEGER :: i, j
  real(8), DIMENSION(n,n), INTENT(INOUT)  :: a
  real(8), ALLOCATABLE :: y(:,:)
  real(8) :: d
  INTEGER, ALLOCATABLE :: indx(:)

  ALLOCATE (y( n, n))  ; ALLOCATE ( indx (n))
  y=0.
  !     setup identity matrix
  DO i=1,n
     y(i,i)=1.
  ENDDO
  !     LU decompose the matrix just once
  CALL  lu_decompose(a,n,indx,d)

  !     Find inverse by columns
  DO j=1,n
     CALL lu_linear_equation(a,n,indx,y(:,j))
  ENDDO
  !     The original matrix a was destroyed, now we equate it with the inverse y 
  a=y

  DEALLOCATE ( y ); DEALLOCATE ( indx )

END SUBROUTINE matinv2

!     Given an NxN matrix A(N,N), this routine replaces it by the LU 
!     decomposed one, where the matrix elements are stored in the same 
!     matrix A. The array indx is  an output vector which records the row
!     permutation effected by the partial pivoting. d is the determinant
!
SUBROUTINE lu_decompose(a,n,indx,d)
  IMPLICIT NONE
  INTEGER :: n, i, j, k, imax
  real(8) :: sum , tiny, aamax, dum, d
  real(8), DIMENSION(n,n) :: a
  INTEGER, DIMENSION(n) :: indx
  real(8), ALLOCATABLE :: vv(:)

  tiny=1.0e-20
  ALLOCATE ( vv(n) )
  D=1.
  DO i=1,n
     aamax=0.
     DO j=1,n
        IF (ABS(a(i,j)) > aamax) aamax=ABS(a(i,j))
     ENDDO
     !     Zero is the largest element
     IF (aamax == 0.) STOP 'Singular matrix.'
     !     No nonzero largest element
     vv(i)=1./aamax
  ENDDO
  !     loop over columns
  DO j=1,n
     !     solves equation 2.3.12 except for i=j of Numerical Recipes
     IF (j > 1) THEN
        DO i=1,j-1
           sum=a(i,j)
           IF (i > 1)THEN
              DO k=1,i-1
                 sum=sum-a(i,k)*a(k,j)
              ENDDO
              a(i,j)=sum
           ENDIF
        ENDDO
     ENDIF
     !    start searching for largest pivot element
     aamax=0.
     DO i=j,n
        sum=a(i,j)
        IF (j > 1)THEN
           DO k=1,j-1
              sum=sum-a(i,k)*a(k,j)
           ENDDO
           a(i,j)=sum
        ENDIF
        dum=vv(i)*ABS(sum)
        IF (dum >= aamax) THEN
           imax=i
           aamax=dum
        ENDIF
     ENDDO
     !    interchange of rows
     IF (j /= imax)THEN
        DO k=1,n
           dum=a(imax,k)
           a(imax,k)=a(j,k)
           a(j,k)=dum
        ENDDO
        !    change of parity for determinant
        d=-d
        vv(imax)=vv(j)
     ENDIF
     indx(j)=imax
     IF(j /= n) THEN
        IF(a(j,j) == 0.) a(j,j)=tiny
        dum=1./a(j,j)
        DO i=j+1,n
           a(i,j)=a(i,j)*dum
        ENDDO
     ENDIF
     !    set up determinant
     d=d*a(j,j)
  ENDDO
  IF(a(n,n) == 0.)  a(n,n)=tiny
  DEALLOCATE ( vv)

END SUBROUTINE lu_decompose

!     Solves set of linear equations Ax=b, A is input as an LU decompomsed
!     matrix and indx keeps track of the permutations of the rows. b is input
!     as the right-hand side vector b and returns the solution x. A, n and indx
!     are not modified by this routine. This function takes into that b can contain
!     many zeros and is therefore suitable for matrix inversion


SUBROUTINE lu_linear_equation(a,n,indx,b)
  IMPLICIT NONE
  INTEGER :: n, ii, ll, i, j
  real(8) :: sum 
  real(8), DIMENSION(n,n) :: a
  real(8), DIMENSION(n) :: b
  INTEGER, DIMENSION(n) :: indx

  ii=0
  !     First we solve equation 2.3.6 of numerical recipes 
  DO i=1,n
     ll=indx(i)
     sum=b(ll)
     b(ll)=b(i)
     IF (ii /= 0)THEN
        DO j=ii,i-1
           sum=sum-a(i,j)*b(j)
        ENDDO
     ELSEIF (sum /= 0.) THEN
        ii=i
     ENDIF
     b(i)=sum
  ENDDO
  !     then we solve equation 2.3.7
  DO i=n,1,-1
     sum=b(i)
     IF (i < n) THEN
        DO j=i+1,n
           sum=sum-a(i,j)*b(j)
        ENDDO
     ENDIF
     !     store a component of the solution x in the same place as b
     b(i)=sum/a(i,i)
  ENDDO

END SUBROUTINE lu_linear_equation
