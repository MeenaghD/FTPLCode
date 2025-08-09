 program main
 implicit none
 integer nboot, i, j
 double precision, dimension(1,1) :: act_mmetric

 open(10,file='nboot',status='old')
 read(10,*)nboot
 close(10)
 IF (nboot .GT. 51) THEN
   CALL ADD_BGP_TO_SIMS(nboot)
   CALL CALC_WALD(nboot)
 ELSE
   act_mmetric=1000.0
   open(14,file='Wald')
   write(14,111)act_mmetric
   close(14)
   111 format(f24.12)
 END IF

 end


 SUBROUTINE ADD_BGP_TO_SIMS(nsims)
 implicit none
 integer i,j,k, nsims
  
 integer, parameter :: nper=232, nvar=27
 double precision, dimension(nper,nvar) :: base, tmp
 double precision, dimension(nper,nvar,nsims) :: all_sims, all_diff, all_diff_bgp
 double precision, dimension(1,1) :: bgp_const, bgp_rho
 double precision, dimension(1,1) :: c_bgp,kp_bgp,rk_bgp,l_bgp,n_bgp,pk_bgp,r_bgp
 double precision, dimension(1,1) :: s_bgp,y_bgp,pi_bgp,cy_bgp,i_bgp,w_bgp
 double precision, dimension(1,1) :: c_bgp_shock,kp_bgp_shock,rk_bgp_shock,l_bgp_shock
 double precision, dimension(1,1) :: n_bgp_shock,pk_bgp_shock,r_bgp_shock,s_bgp_shock,y_bgp_shock
 double precision, dimension(1,1) :: pi_bgp_shock,cy_bgp_shock,i_bgp_shock,w_bgp_shock
 double precision, dimension(nper,1) :: c_bgp_trend,kp_bgp_trend,rk_bgp_trend,l_bgp_trend
 double precision, dimension(nper,1) :: n_bgp_trend,pk_bgp_trend,r_bgp_trend,s_bgp_trend,y_bgp_trend
 double precision, dimension(nper,1) :: pi_bgp_trend,cy_bgp_trend,i_bgp_trend,w_bgp_trend
 double precision, dimension(13,1) :: all_beta_ar
 double precision, dimension(26,1) :: coeffs
 character(len=13), dimension(nsims,1) :: filenames
 character fname*13
 
 open(10,file='base.out',status='old')
 do i=1,nper
   read(10,*) base(i,:)
 end do
 close(10)

! ADDING BGP TO THE SIMULATIONS. ONLY PRODUCTIVITY IS USED FOR THE BGP
! coefficient on constant
 open(10,file='bgp_const.data',status='old')
 read(10,*)bgp_const
 close(10)

! coefficient on lag
 open(10,file='ar_coeffs.data',status='old')
 read(10,*)all_beta_ar
 close(10)
 bgp_rho=all_beta_ar(6,1)
 
 open(10,file='coef.data',status='old')
 read(10,*)coeffs
 close(10)
 
 c_bgp_shock=1.0/(1.0-coeffs(15,1))
 c_bgp=c_bgp_shock*(bgp_const/(1.0-bgp_rho))

 kp_bgp_shock=0.0 
 kp_bgp=kp_bgp_shock*(bgp_const/(1.0-bgp_rho))

 rk_bgp_shock=0.0
 rk_bgp=rk_bgp_shock*(bgp_const/(1.0-bgp_rho))

 l_bgp_shock=0.0
 l_bgp=l_bgp_shock*(bgp_const/(1.0-bgp_rho))

 n_bgp_shock=0.0
 n_bgp=n_bgp_shock*(bgp_const/(1.0-bgp_rho))

 pk_bgp_shock=1.0/(1.0-coeffs(15,1))
 pk_bgp=pk_bgp_shock*(bgp_const/(1.0-bgp_rho))

 r_bgp_shock=0.0
 r_bgp=r_bgp_shock*(bgp_const/(1.0-bgp_rho))

 s_bgp_shock=0.0
 s_bgp=s_bgp_shock*(bgp_const/(1.0-bgp_rho))

 y_bgp_shock=1.0/(1.0-coeffs(15,1))
 y_bgp=y_bgp_shock*(bgp_const/(1.0-bgp_rho))

 pi_bgp_shock=0.0
 pi_bgp=pi_bgp_shock*(bgp_const/(1.0-bgp_rho))

 cy_bgp_shock=0.0
 cy_bgp=cy_bgp_shock*(bgp_const/(1.0-bgp_rho))

 i_bgp_shock=1.0/(1.0-coeffs(15,1))
 i_bgp=i_bgp_shock*(bgp_const/(1.0-bgp_rho))

 w_bgp_shock=1.0/(1.0-coeffs(15,1))
 w_bgp=w_bgp_shock*(bgp_const/(1.0-bgp_rho))


 c_bgp_trend(1,1)=0.0
 kp_bgp_trend(1,1)=0.0
 rk_bgp_trend(1,1)=0.0
 l_bgp_trend(1,1)=0.0
 n_bgp_trend(1,1)=0.0
 pk_bgp_trend(1,1)=0.0
 r_bgp_trend(1,1)=0.0
 s_bgp_trend(1,1)=0.0
 y_bgp_trend(1,1)=0.0
 pi_bgp_trend(1,1)=0.0
 cy_bgp_trend(1,1)=0.0
 i_bgp_trend(1,1)=0.0
 w_bgp_trend(1,1)=0.0
 do i=2,nper
   c_bgp_trend(i,1)=c_bgp_trend(i-1,1)+c_bgp(1,1)
   kp_bgp_trend(i,1)=kp_bgp_trend(i-1,1)+kp_bgp(1,1)
   rk_bgp_trend(i,1)=rk_bgp_trend(i-1,1)+rk_bgp(1,1)
   l_bgp_trend(i,1)=l_bgp_trend(i-1,1)+l_bgp(1,1)
   n_bgp_trend(i,1)=n_bgp_trend(i-1,1)+n_bgp(1,1)
   pk_bgp_trend(i,1)=pk_bgp_trend(i-1,1)+pk_bgp(1,1)
   r_bgp_trend(i,1)=r_bgp_trend(i-1,1)+r_bgp(1,1)
   s_bgp_trend(i,1)=s_bgp_trend(i-1,1)+s_bgp(1,1)
   y_bgp_trend(i,1)=y_bgp_trend(i-1,1)+y_bgp(1,1)
   pi_bgp_trend(i,1)=pi_bgp_trend(i-1,1)+pi_bgp(1,1)
   cy_bgp_trend(i,1)=cy_bgp_trend(i-1,1)+cy_bgp(1,1)
   i_bgp_trend(i,1)=i_bgp_trend(i-1,1)+i_bgp(1,1)
   w_bgp_trend(i,1)=w_bgp_trend(i-1,1)+w_bgp(1,1)
 end do

  
 open(10,file='filenames',status='old')
 do i=1,nsims
   read(10,*)filenames(i,1)
 end do
 do i=1,nsims
   fname=filenames(i,1)
   open(11,file=fname)
   do j=1,nper
     read(11,*)all_sims(j,:,i)
   end do
   close(11)

   all_diff(:,:,i)=all_sims(:,:,i)-base(:,:)
   do j=1,nper
     tmp(j,:)=all_diff(j,:,i)-(all_diff(1,:,i)-base(1,:))
   end do
   all_diff(:,:,i)=tmp(:,:)

   all_diff_bgp(:,:,i)=tmp(:,:)
   all_diff_bgp(:,7,i)=all_diff_bgp(:,7,i)+c_bgp_trend(:,1)
   all_diff_bgp(:,4,i)=all_diff_bgp(:,4,i)+kp_bgp_trend(:,1)
   all_diff_bgp(:,10,i)=all_diff_bgp(:,10,i)+rk_bgp_trend(:,1)
   all_diff_bgp(:,9,i)=all_diff_bgp(:,9,i)+l_bgp_trend(:,1)
   all_diff_bgp(:,13,i)=all_diff_bgp(:,13,i)+n_bgp_trend(:,1)
   all_diff_bgp(:,3,i)=all_diff_bgp(:,3,i)+pk_bgp_trend(:,1)
   all_diff_bgp(:,1,i)=all_diff_bgp(:,1,i)+r_bgp_trend(:,1)
   all_diff_bgp(:,11,i)=all_diff_bgp(:,11,i)+s_bgp_trend(:,1)
   all_diff_bgp(:,8,i)=all_diff_bgp(:,8,i)+y_bgp_trend(:,1)
   all_diff_bgp(:,5,i)=all_diff_bgp(:,5,i)+pi_bgp_trend(:,1)
   all_diff_bgp(:,12,i)=all_diff_bgp(:,12,i)+cy_bgp_trend(:,1)
   all_diff_bgp(:,2,i)=all_diff_bgp(:,2,i)+i_bgp_trend(:,1)
   all_diff_bgp(:,6,i)=all_diff_bgp(:,6,i)+w_bgp_trend(:,1)
! writing inflation cut off as actual number
   all_diff_bgp(:,27,i)=all_sims(:,27,i)

   open(12,file='all_sims.out')
   do j=1,nper
     write(12,110)all_sims(j,:,i)
   end do
!   open(12,file='all_diff.out')
!   do j=1,nper
!     write(12,110)all_diff(j,:,i)
!   end do
   open(13,file='all_diff_bgp.data')
   do j=1,nper
     write(13,110)all_diff_bgp(j,:,i)
   end do
   110 format(27f60.12)
   
 end do
 close(12)
 close(13)

 END SUBROUTINE ADD_BGP_TO_SIMS 
 
 SUBROUTINE CALC_WALD(nsims)
 implicit none
 integer i,j,k,n95,nsims,jj,jpval,jjj
 
 integer, parameter :: nper=232, nvar=27, nact=234
 double precision, dimension(nper,nvar,nsims) :: all_diff_bgp
 double precision, dimension(nper,1,nsims) :: err_y_trend
 double precision, dimension(nact,18) :: act_data
 double precision, dimension(nact-1,1) :: act_err_y_trend
 double precision, dimension(nper,12) :: resids
 double precision, dimension(nper-3,1) :: trend
 double precision, dimension(nper-3,3) :: Y
 double precision, dimension(nper-4,3) :: data_dt
 double precision, dimension(nper-3,2) :: Z
 double precision, dimension(nact-4,3) :: act_Y
 double precision, dimension(nact-5,3) :: act_data_dt
 double precision, dimension(nact-4,2) :: act_Z
 double precision, dimension(nact-4,1) :: act_trend
 double precision, dimension(1,3) :: sum_data_dt, var_data_dt, act_var_data_dt
 double precision, dimension(1,1) :: prod_rho, mmtmp3, act_mmetric, Trans_Mdis_norm
 double precision, dimension(1,1) :: p_val
 double precision, dimension(13,1) :: all_beta_ar
 double precision, dimension(26,1) :: coeffs
 double precision, dimension(6,3) :: var_beta, act_var_beta
 double precision, dimension(nsims,12) :: all_coeffs, all_coeffs_dm 
 double precision, dimension(12,nsims) :: all_coeffs_dmt
 double precision, dimension(12,12) :: covar_coeffs, inv_covar_coeffs
 double precision, dimension(1,12) :: coeffs_bar, mmtmp1, act_coeffs, act_coeffs_dm
 double precision, dimension(12,1) :: mmtmp2, act_coeffs_dmt
 double precision, dimension(nsims,1) :: mmetric, smmetric
 double precision :: YMAX, YMIN, YMAX_PC, YMIN_PC
 integer, dimension(12) :: INDX

! productivity rho
 open(10,file='ar_coeffs.data',status='old')
 read(10,*)all_beta_ar
 close(10)
 prod_rho=all_beta_ar(5,1)
 
 open(10,file='all_diff_bgp.data',status='old')
 do i=1,nsims
   do j=1,nper
     read(10,*)all_diff_bgp(j,:,i)
   end do
 end do
 close(10)
 
 do i=1,nsims
   do j=3,nper
     err_y_trend(j,1,i)=all_diff_bgp(j-1,19,i) &
                       +(prod_rho(1,1)/(1.0-prod_rho(1,1))) &
                       *(all_diff_bgp(j-1,19,i)-all_diff_bgp(j-2,19,i))
   end do
 end do

 do i=1,nper-3
   trend(i,1)=i
 end do

 jj=0
 do i=1,nsims
   Y(:,1)=all_diff_bgp(4:nper,8,i)
   Y(:,2)=all_diff_bgp(4:nper,5,i)
   Y(:,3)=all_diff_bgp(4:nper,1,i)
   Z(:,1)=err_y_trend(3:nper-1,1,i)
   Z(:,2)=trend(:,1)

   YMAX=MAXVAL(Y(:,1),DIM=1)
   YMIN=MINVAL(Y(:,1),DIM=1)
!   YMAX_PC=YMAX/Y(1,1)
!   YMIN_PC=YMIN/Y(1,1)
   YMAX_PC=YMAX/851.89
   YMIN_PC=YMIN/851.89

   IF ((YMIN_PC .GT. 0.5) .AND. (YMAX_PC .LT. 2.0)) THEN
!   IF ((YMIN_PC .GT. 0.1) .AND. (YMAX_PC .LT. 3.0)) THEN

     jj=jj+1

     call VAR1_X(Y,Z,nper-3,3,2,var_beta,data_dt)

     do k=1,3
       do j=1,3
         all_coeffs(jj,j+3*(k-1))=var_beta(j,k)
       end do 
     end do
   
     call VARIANCE(data_dt,nper-4,3,var_data_dt)

     do j=1,3
       all_coeffs(jj,j+9)=var_data_dt(1,j)
     end do

     open(19,file='all_coeffs.out')
       write(19,110)all_coeffs(jj,:)
     110 format(12f20.12)
!     open(20,file='stable_sims')
!       write(20,*)jj
!     close(20)
       else 
      write(882,*)i
   END IF
 end do

 open(10,file='nsims')
 write(10,*)jj
 close(10)

 IF (jj .GT. 201) THEN

!   call COVAR(all_coeffs,jj,12,covar_coeffs)
   call COVAR(all_coeffs(1:jj,:),jj,12,covar_coeffs)
  do jjj=1,12
     write(991,993)covar_coeffs(jjj,:);
  end do
993 format(12f20.12)
  ! call INVERSE(covar_coeffs,inv_covar_coeffs,12)
  ! call matinv(12,covar_coeffs,inv_covar_coeffs)
  ! call MIGS(covar_coeffs,12,inv_covar_coeffs,INDX)
  ! call matinv1(covar_coeffs,12,12,j)
   call matinv2(covar_coeffs,12)
   inv_covar_coeffs=covar_coeffs
  do jjj=1,12
     write(992,993)inv_covar_coeffs(jjj,:);
  end do
   coeffs_bar(1,:)=SUM(all_coeffs,DIM=1)/jj

   do i=1,12
     all_coeffs_dm(:,i)=all_coeffs(:,i)-coeffs_bar(1,i)
   end do
   all_coeffs_dmt=transpose(all_coeffs_dm)

   do i=1,jj
     mmtmp1(1,:)=matmul(all_coeffs_dm(i,:),inv_covar_coeffs(:,:))
     mmtmp2(:,1)=all_coeffs_dmt(:,i)
     mmtmp3=matmul(mmtmp1,mmtmp2)
     mmetric(i,1)=mmtmp3(1,1)
     write(993,*)mmetric(i,1)
   end do


  ! Ordering of act_data: consumption(1) investment(2) output(3) hours(4)
  ! inflation(5) wage(6)
  ! interest(7) capital(8) q(9) mpk(10) networth(11) premium(12) rk(13) entconsumption(14)

   open(11,file='act_data.data',status='old')
   do i=1,nact
     read(11,*)act_data(i,:)
   end do
   close(11)

   open(12,file='resids.data ',status='old')
   do i=1,nper
!     read(12,*)resids(i+1,:)
     read(12,*)resids(i,:)
   end do
   write(883,*)resids(1,5),resids(2,5),resids(nact-1,5)
   close(12)
   do j=4,nact-1
     act_err_y_trend(j,1)=resids(j-1,5)+(prod_rho(1,1)/(1.0-prod_rho(1,1))) &
                       *(resids(j-1,5)-resids(j-2,5))
   end do


  ! ACTUAL DATA PERIODS 1:112
  ! SHOCKS PERIODS 4:111 (LOSE 1 FROM TOP AND BOTTOM FOR RESIDS, 2 FROM BOTTOM FOR SHOCKS)
   do i=1,nact-4
     act_trend(i,1)=i
   end do
   act_Y(:,1)=act_data(5:nact,3)
   act_Y(:,2)=act_data(5:nact,5)
   act_Y(:,3)=act_data(5:nact,7)
   act_Z(:,1)=act_err_y_trend(4:nact-1,1)
   act_Z(:,2)=act_trend(:,1)

   call VAR1_X(act_Y,act_Z,nact-4,3,2,act_var_beta,act_data_dt)
   do k=1,3
     do j=1,3
       act_coeffs(1,j+3*(k-1))=act_var_beta(j,k)
     end do 
   end do
  ! lose an observation in act_var_data_dt, so nact-5
   call VARIANCE(act_data_dt,nact-5,3,act_var_data_dt)
   do j=1,3
     act_coeffs(1,j+9)=act_var_data_dt(1,j)
   end do
     open(21,file='act_coeffs.out')
       write(21,110)act_coeffs
   do i=1,12
     act_coeffs_dm(1,i)=act_coeffs(1,i)-coeffs_bar(1,i)
   end do
   act_coeffs_dmt=transpose(act_coeffs_dm)
   mmtmp1(1,:)=matmul(act_coeffs_dm(1,:),inv_covar_coeffs(:,:))
   mmtmp2(:,1)=act_coeffs_dmt(:,1)
   mmtmp3=matmul(mmtmp1,mmtmp2)
   act_mmetric(1,1)=mmtmp3(1,1)

   smmetric(:,1)=mmetric(:,1)
   call SORT(smmetric,jj)
   n95=jj*0.95

   Trans_Mdis_norm=((sqrt(2.0*act_mmetric(1,1))-sqrt(2.0*(size(act_coeffs,2)-1)))&
                  /(sqrt(2.0*smmetric(n95,1))-sqrt(2.0*(size(act_coeffs,2)-1))))&
                  *1.645

   jpval=0
   do i=1,jj
      if ( act_mmetric(1,1).gt.smmetric(i,1) ) then
      jpval=jpval+1
      end if
   end do
   write(2222,*)jpval,jj
   p_val=1-real(jpval,8)/real(jj,8)

 ELSE
   act_mmetric=1000.0
   Trans_Mdis_norm=1000.0
   p_val=1000.0
 END IF

 open(14,file='Wald')
 write(14,111)act_mmetric
 close(14)
 open(15,file='Trans')
 write(15,111)Trans_Mdis_norm
 close(15)
 open(15,file='Pval')
 write(15,111)p_val
 close(15)
 111 format(f24.12)
 END SUBROUTINE CALC_WALD



 SUBROUTINE COVAR(series,nper,nvar,covar_series)
 IMPLICIT NONE
! Estimate AR parameter for data:
! INPUTS: 
! series=data
! nper=number of periods
! nvar=number of variables
 integer nper, nvar, i, j
 double precision, dimension (nper,nvar) :: series
 double precision, dimension (nper,1) ::  series1_dm, series2_dm, series1_2
 double precision, dimension (1,nvar) :: mean_series
 double precision, dimension (nvar,nvar) :: covar_series

 mean_series(1,:)=sum(series,DIM=1)/(nper)
 do i=1,nvar
   do j=1,nvar
     series1_dm(:,1)=series(:,i)-mean_series(1,i)
     series2_dm(:,1)=series(:,j)-mean_series(1,j)
     series1_2(:,1)=series1_dm(:,1)*series2_dm(:,1)
     covar_series(i,j)=sum(series1_2(:,1),DIM=1)/(nper-1)
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
 double precision, dimension (nper,nvar) :: series, series_dm, series_dm_sq
 double precision, dimension (1,nvar) :: mean_series, var_series

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
 double precision, dimension (nper,nendog) :: series
 double precision, dimension (nper,nexog) :: exog
 double precision, dimension (nendog+nexog+1,nendog) ::  beta, XTY
 double precision, dimension (nendog+nexog+1,nendog+nexog+1) ::  XTX, XTXI
 double precision, dimension (nper-1,nendog) :: Y, residual, Xbeta
 double precision, dimension (nper-1,nendog+nexog+1) :: X
 double precision, dimension (nendog+nexog+1,nper-1) :: XT
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
! call matinv1(XTX,nendog+nexog+1,nendog+nexog+1,j)
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
 integer nper, nexog, i, j
 double precision, dimension(nper,nexog) :: X
 double precision, dimension(nper,1) :: Y, Xbeta, residual
 double precision, dimension (nexog,nexog) ::  XTX, XTXI, XTY
 double precision, dimension (nexog,nper) :: XT
 double precision, dimension (nexog,1) :: beta
 integer, dimension(nexog) :: INDX
  
 XT=transpose(X)
 XTX=matmul(XT,X)
! call inverse(XTX,XTXI,nexog)
! call matinv(nexog,XTX,XTXI)
! call MIGS(XTX,nexog,XTXI,INDX)
! call matinv1(XTX,nexog,nexog,j)
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
 double precision act_data(nper,nact), beta(1,1)
 double precision Y(nper-1,1), X(nper-1,1), XT(1,nper-1), XTX(1,1)
 double precision XTXI(1,1), XTY(1,1)
 
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
 double precision, dimension (nper,nvar) :: series
 double precision, dimension (nvar,nvar) ::  beta, XTX, XTXI, XTY
 double precision, dimension (nper-1,nvar) :: Y, X
 double precision, dimension (nvar,nper-1) :: XT
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
! call matinv1(XTX,nvar,nvar,j)
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
 double precision, dimension (nper,nvar) :: series
 double precision, dimension (nvar,nvar) ::  beta, XTX, XTXI, XTY
 double precision, dimension (nper-1,nvar) :: Y, X
 double precision, dimension (nvar,nper-1) :: XT
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
! call matinv1(XTX,nvar,nvar,j)
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
double precision aa(nn,nn), cc(nn,nn), aatmp(nn,nn)
double precision LL(nn,nn), UU(nn,nn), bb(nn), dd(nn), xx(nn)
double precision coeff
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
!  parameter(MMAX=25,NMAX=10)
  integer n
  double precision  A(n,n), B(n,2*n)  
!  double precision  A(MMAX,MMAX), B(MMAX,2*MMAX)  
  integer i,j,k
  double precision bb

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
 double precision, dimension(n,1), intent(INOUT) :: arr 
! double precision, dimension(:), intent(INOUT) :: arr 
 double precision a
 
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


      SUBROUTINE MATINV1 (A,LDA,N,IFLAG) 
!
!-----------------------------------------------------------------------
!   MATINV   WRITTEN BY !HARLES P. REEVE, STATISTI!AL ENGINEERING
!            DIVISION, NATIONAL BUREAU OF STANDARDS, GAITHERSBURG,
!            MARYLAND  20899
!
!   FOR: !OMPUTING THE INVERSE OF A GENERAL N BY N MATRIX IN PLA!E,
!        I.E., THE INVERSE OVERWRITES THE ORIGINAL MATRIX.  THE STEPS 
!        OF THE ALGORITHM ARE DES!RIBED BELOW AS THEY O!!UR.  ROW
!        INTER!HANGES ARE DONE AS NEEDED IN ORDER TO IN!REASE THE
!        A!!URA!Y OF THE INVERSE MATRIX.  WITHOUT INTER!HANGES THIS
!        ALGORITHM WILL FAIL WHEN ANY OF THE LEADING PRIN!IPAL
!        SUBMATRI!ES ARE SINGULAR OR WHEN THE MATRIX ITSELF IS
!        SINGULAR.  WITH INTER!HANGES THIS ALGORITHM WILL FAIL ONLY
!        WHEN THE MATRIX ITSELF IS SINGULAR.  THE LEADING PRIN!IPAL
!
!                                   [A B !]
!        SUBMATRI!ES OF THE MATRIX  [D E F]  ARE  [A]  AND  [A B] .
!                                   [G H I]                 [D E]
!
!   SUBPROGRAMS !ALLED: -NONE-
!
!   !URRENT VERSION !OMPLETED JANUARY 15, 1987
!
!   REFEREN!E: STEWART, G.W., 'INTRODU!TION TO MATRIX !OMPUTATIONS',
!              A!ADEMI! PRESS, IN!., 1973
!-----------------------------------------------------------------------
!   DEFINITION OF PASSED PARAMETERS
!
!     * A = MATRIX (SIZE NXN) TO BE INVERTED (double precision)
!
!   * LDA = LEADING DIMENSION OF MATRIX A [LDA>=N] (INTEGER)
!
!     * N = NUMBER OF ROWS AND !OLUMNS OF MATRIX A (INTEGER)
!
!   IFLAG = ERROR INDI!ATOR ON OUTPUT (INTEGER)   INTERPRETATION: 
!           -2 -> TOO MANY ROW INTER!HANGES NEEDED - IN!REASE MX
!           -1 -> N>LDA
!            0 -> NO ERRORS DETE!TED
!            K -> MATRIX A FOUND TO BE SINGULAR AT THE KTH STEP OF
!                 THE !ROUT REDU!TION (1<=K<=N)
!
!   * INDI!ATES PARAMETERS REQUIRING INPUT VALUES 
!-----------------------------------------------------------------------
!
      PARAMETER (MX=100)
      DIMENSION A(LDA,*),IEX(MX,2)
      IFLAG = 0
!
!--- !HECK CONSISTENCY OF PASSED PARAMETERS
!
      IF (N.GT.LDA) THEN
         IFLAG = -1 
         RETURN
      ENDIF
!
!--- COMPUTE A = LU BY THE CROUT REDUCTION WHERE L IS LOWER TRIANGULAR
!--- AND U IS UNIT UPPER TRIANGULAR (ALGORITHM 3.4, P. 138 OF THE
!--- REFERENCE)
!
      NEX = 0
      DO 70 K = 1, N
         DO 20 I = K, N
            S = A(I,K)
            DO 10 L = 1, K-1
               S = S-A(I,L)*A(L,K)
   10       CONTINUE
            A(I,K) = S
   20    CONTINUE
!
!--- INTERCHANGE ROWS IF NECESSARY
!
         Q = 0.0
         L = 0
         DO 30 I = K, N
            R = ABS(A(I,K))
            IF (R.GT.Q) THEN
               Q = R
               L = I
            ENDIF
   30    CONTINUE
         IF (L.EQ.0) THEN
            IFLAG = K
            RETURN
         ENDIF
         IF (L.NE.K) THEN
            NEX = NEX+1
            IF (NEX.GT.MX) THEN
               IFLAG = -2
               RETURN
            ENDIF
            IEX(NEX,1) = K
            IEX(NEX,2) = L
            DO 40 J = 1, N
               Q = A(K,J)
               A(K,J) = A(L,J)
               A(L,J) = Q
   40       CONTINUE
         ENDIF
!
!--- END ROW INTERCHANGE SECTION
!
         DO 60 J = K+1, N
            S = A(K,J)
            DO 50 L = 1, K-1
               S = S-A(K,L)*A(L,J)
   50       CONTINUE
            A(K,J) = S/A(K,K) 
   60    CONTINUE
   70 CONTINUE
!
!--- INVERT THE LOWER TRIANGLE L IN PLACE (SIMILAR TO ALGORITHM 1.5,
!--- P. 110 OF THE REFERENCE) 
!
      DO 100 K = N, 1, -1
         A(K,K) = 1.0/A(K,K)
         DO 90 I = K-1, 1, -1 
            S = 0.0 
            DO 80 J = I+1, K
               S = S+A(J,I)*A(K,J)
   80       CONTINUE
            A(K,I) = -S/A(I,I)
   90    CONTINUE
  100 CONTINUE
!
!--- INVERT THE UPPER TRIANGLE U IN PLACE (ALGORITHM 1.5, P. 110 OF
!--- THE REFERENCE) 
!
      DO 130 K = N, 1, -1
         DO 120 I = K-1, 1, -1
            S = A(I,K)
            DO 110 J = I+1, K-1
               S = S+A(I,J)*A(J,K)
  110       CONTINUE
            A(I,K) = -S
  120    CONTINUE
  130 CONTINUE
!
!--- COMPUTE INV(A) = INV(U)*INV(L)
!
      DO 160 I = 1, N
         DO 150 J = 1, N
            IF (J.GT.I) THEN
               S = 0.0
               L = J
            ELSE
               S = A(I,J)
               L = I+1
            ENDIF
            DO 140 K = L, N
               S = S+A(I,K)*A(K,J)
  140       CONTINUE
            A(I,J) = S
  150    CONTINUE
  160 CONTINUE
!
!--- INTERCHANGE COLUMNS OF INV(A) TO REVERSE EFFECT OF ROW 
!--- INTERCHANGES OF A
!
      DO 180 I = NEX, 1, -1
         K = IEX(I,1)
         L = IEX(I,2)
         DO 170 J = 1, N
            Q = A(J,K)
            A(J,K) = A(J,L)
            A(J,L) = Q
  170    CONTINUE
  180 CONTINUE
      RETURN
      END 


!            Routines to do mtx inversion, from Numerical
!            Recepies, Teukolsky et al. Routines included
!            below are MATINV, LUDCMP and LUBKSB. See chap 2
!            of Numerical Recipes for further details
!            Recoded in FORTRAN 90 by M. Hjorth-Jensen
!
SUBROUTINE matinv2(a,n)
  IMPLICIT NONE
  INTEGER, INTENT(IN) :: n
  INTEGER :: i, j
  double precision, DIMENSION(n,n), INTENT(INOUT)  :: a
  double precision, ALLOCATABLE :: y(:,:)
  double precision :: d
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
  double precision :: sum , tiny, aamax, dum, d
  double precision, DIMENSION(n,n) :: a
  INTEGER, DIMENSION(n) :: indx
  double precision, ALLOCATABLE :: vv(:)

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
  double precision :: sum 
  double precision, DIMENSION(n,n) :: a
  double precision, DIMENSION(n) :: b
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

