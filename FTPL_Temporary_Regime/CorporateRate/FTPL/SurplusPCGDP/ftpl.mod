
var r inv pk kp pinf w c y lab rk s cy n yk mzero mtwo wNK wNC epinf ecy SH_g SH_1 SH_2 SH_3 SH_4 SH_5 SH_6 SH_NC6 SH_7 SH_8 SH_9C surplus;

varexo e_g e_1 e_2 e_3 e_4 e_5 e_6 e_nc6 e_7 e_8 e_9 e_surp exppinf;

parameters CONSTEPINF CONSTEBETA CONSTELAB CTREND CBETA CGAMMA CTOU CLANDAW CG CURVP CURVW CGY CLANDAP CBETABAR CRK CW CIKBAR CIK CLK CKY CIY CCY CRKKY CWHLC CWLY 
     Z1 Z2 Z3 Z4 Z5 Z6 Z7 Z8 Z9 Z10 Z11 Z12 Z13 Z14 Z15 Z16 Z17 Z18 Z19 Z20 Z21 Z22 Z23 Z24 Z25 Z26 Z27 Z28 Z29 Z30 Z31 Z32 Z33 Z34 Z35 Z36 Z37 Z38 Z39 Z40 Z41;


load IIcoef ;
Z1=coef(1);
Z2=coef(2);
Z3=coef(3);
Z4=coef(4);
Z5=coef(5);
Z6=coef(6);
Z7=coef(7);
Z8=coef(8);
Z9=coef(9);
Z10=coef(10);
Z11=coef(11);
Z12=coef(12);
Z13=coef(13);
Z14=coef(14);
Z15=coef(15);
Z16=coef(16);
Z17=coef(17);
Z18=coef(18);
Z19=coef(19);
Z20=coef(20);
Z21=coef(21);
Z22=coef(22);
Z23=coef(23);
Z24=coef(24);
Z25=coef(25);
Z26=coef(26);
Z27=coef(27);
Z28=coef(28);

CONSTEPINF=0.78;
CONSTEBETA=0.16;
CONSTELAB=0.53;
CTREND=0.43;
CBETA=1/(1+CONSTEBETA/100.0);
CGAMMA=CTREND/100.0+1;
CTOU=0.025;
CLANDAW=1.5;
CG=0.18;
CURVP=10;
CURVW=10;
CGY=0.52612;
CLANDAP=Z10;
CBETABAR=CBETA*CGAMMA^(-Z2);
CRK=(1.7^Z18)*(CBETA^(-1))*(CGAMMA^Z2)-(1-CTOU);
CW=(Z15^Z15*(1-Z15)^(1-Z15)/(CLANDAP*CRK^Z15))^(1/(1-Z15));
CIKBAR=(1-(1-CTOU)/CGAMMA);
CIK=(1-(1-CTOU)/CGAMMA)*CGAMMA;
CLK=((1-Z15)/Z15)*(CRK/CW);
CKY=Z10*(CLK)^(Z15-1);
CIY=CIK*CKY;
CCY=1-CG-CIK*CKY;
CRKKY=CRK*CKY;
CWHLC=(1/CLANDAW)*(1-Z15)/Z15*CRK*CKY/CCY;
CWLY=1-CRK*CKY;
PARPI=0;

load rho_est;
Z29=rho_est(1);
Z30=rho_est(2);
Z31=rho_est(3);
Z32=rho_est(4);
Z33=rho_est(5);
Z34=rho_est(6);
Z35=rho_est(7);
Z36=rho_est(8);
Z37=rho_est(9);
Z38=rho_est(10);
Z39=rho_est(11);
Z40=rho_est(12);
Z41=rho_est(13);

model(linear);

epinf=exppinf;
ecy=cy(1);

//( 1).....r :
      r = epinf+1/((1-Z3/CGAMMA)/(Z2*(1+Z3/CGAMMA)))*((Z3/CGAMMA)/(1+Z3/CGAMMA)*c(-1)
           +(1/(1+Z3/CGAMMA))*c(+1)
           +((Z2-1)*CWHLC/(Z2*(1+Z3/CGAMMA)))
           *(lab-lab(+1))+SH_1-c);

//( 2).....inv :
      inv = (1/(1+CBETABAR*CGAMMA))*(inv(-1)
           +CBETABAR*CGAMMA*inv(+1)
           +(1/(CGAMMA*CGAMMA*Z1))*pk)+SH_2;

//( 3).....pk :
      pk = -cy(+1)+(CRK/(CRK+(1-CTOU)))*rk(+1)
           +((1-CTOU)/(CRK+(1-CTOU)))*pk(+1) ;

//( 4).....kp :
      kp = (1-CIKBAR)*kp(-1)+CIKBAR*inv
           +CIKBAR*CGAMMA*CGAMMA*Z1*SH_2 ;

//( 5).....pinf :
      pinf = (r-Z12*(r(-1))-Z13*(1-Z12)*y
           -Z14*(y-y(-1))-SH_3)/(Z11*(1-Z12)) ;

//( 6).....wNK :
      wNK = (1/(1+(1-Z4)*((1-CBETABAR*CGAMMA*Z4)/((1+CBETABAR*CGAMMA)*Z4))*(1/((CLANDAW-1)*CURVW+1))))*( (1/(1+CBETABAR*CGAMMA))*w(-1)
           +((CBETABAR*CGAMMA)/(1+CBETABAR*CGAMMA))*w(+1)
           +(Z7/(1+CBETABAR*CGAMMA))*pinf(-1)
           -((1+CBETABAR*CGAMMA*Z7)/(1+CBETABAR*CGAMMA))*pinf
           +((CBETABAR*CGAMMA)/(1+CBETABAR*CGAMMA))*epinf
           +((1-Z4)*((1-CBETABAR*CGAMMA*Z4)/((1+CBETABAR*CGAMMA)*Z4))*(1/((CLANDAW-1)*CURVW+1)))*(Z5*lab+(1/(1-Z3/CGAMMA))*c
           -((Z3/CGAMMA)/(1-Z3/CGAMMA))*c(-1))
           +SH_6 ) ;

//( 7).....wNC :
      wNC = Z5*lab+(1/(1-Z3/CGAMMA))*c
           -((Z3/CGAMMA)/(1-Z3/CGAMMA))*c(-1)
           -(pinf-epinf)
           +SH_NC6 ;

//( 8).....w :
      w = Z16*wNK+(1-Z16)*wNC ;

//( 9).....c :
      c = (y-CIY*inv-SH_g-1*CRKKY*(1/(Z9/(1-Z9)))*rk-0.01*n)/CCY ;

//(10).....y :
      y = Z10*(Z15*(kp(-1)+(1/(Z9/(1-Z9)))*rk)+(1-Z15)*lab+SH_4) ;

//(11).....lab :
      lab = rk-w+(kp(-1)+(1/(Z9/(1-Z9)))*rk) ;

//(12).....RK :
      rk = Z17*((((pinf-SH_5)/(1/(1+CBETABAR*CGAMMA*Z8))
           -(CBETABAR*CGAMMA*epinf+Z8*pinf(-1)))/(((1-Z6)*(1-CBETABAR*CGAMMA*Z6)/Z6)/((Z10-1)*CURVP+1))
           -(1-Z15)*w+SH_4)/Z15)
           +(1-Z17)*((1-Z15)/Z15*(-w)+SH_4/Z15) ;

//(13)..... s:
      s = -Z18*(n-pk-kp)-Z21*mzero+SH_7 ;

//(14)..... cy:
      cy = s(-1)+(r(-1)-epinf) ;

//(15)..... n:
      n = 1.7*yk-0.7*ecy(-1)+0.99*n(-1)+SH_8 ;

//(16)..... yk:
      yk=(CRK/(CRK+(1-CTOU)))*(y-kp)+((1-CTOU)/(CRK+(1-CTOU)))*pk-pk(-1) ;

//(17)..... MZERO:
      mzero = mzero(-1)+Z20*(mtwo-mtwo(-1))+SH_9C ;

//(18)..... MTWO:
      mtwo = (1+0.245928-0.073148)*kp+0.073148*mzero-0.245928*n ;

//.....shock_g : government spending data
      SH_g = Z29*SH_g(-1)+Z30*e_4+e_g ;

//.....shock_1 : interest rate equation
      SH_1 = Z31*SH_1(-1)+e_1 ;

//.....shock_2 : investment equation
      SH_2 = Z32*SH_2(-1)+e_2 ;

//.....shock_3 : inflation equation
      SH_3 = Z33*SH_3(-1)+e_3 ;

//.....shock_4 : output equation
      SH_4 = SH_4(-1)+Z34*(SH_4(-1)-SH_4(-2))+e_4 ;

//.....shock_5 : rk equation
      SH_5 = Z35*SH_5(-1)+e_5 ;

//.....shock_6 : w equation
      SH_6 = Z36*SH_6(-1)+e_6 ;

//.....shock_NC6 : NC w equation
      SH_NC6 = Z37*SH_NC6(-1)+e_nc6 ;

//.....SHOCK_7: PREMIUM SHOCK
      SH_7 = Z38*SH_7(-1)+e_7 ;

//.....shock_8: networth shock
      SH_8 = Z39*SH_8(-1)+e_8 ;

//.....shock_8: mzero shock
      SH_9C = Z40*SH_9C(-1)+e_9 ;

//.....surplus (ARIMA(1,1,0))
//      surplus=surplus(-1)+Z41*(surplus(-1)-surplus(-2))+e_surp;
      surplus=Z41*surplus(-1)+e_surp;

end;

shocks;
var e_9; 
stderr   0.01; 
end;

stoch_simul(noprint,nofunctions,nomoments,nocorr,irf=0);
