      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

C                                                                       

C                                                                       

C=======================================================================

C    THIS FILE WAS CREATED BY EDITING :WORLD.JL50WRME9SM5.              

C=======================================================================

C                                                                       

C                                                                       

C=======================================================================

C                                                                       

C    THE MAIN PROGRAM HAS EIGHT SECTIONS :                              

C    ***********************************                                

C                                                                       

C    1. A LIST OF LIMITS IMPOSED.                                       

C                                                                       

C    2. A LIST OF CHANGES TO THE EQUATIONS SINCE NOVEMBER 1983.         

C                                                                       

C    3. A SECTION TO READ THE SIX CONTROL CARDS.                        

C                                                                       

C    4. A CALL TO DATSIM TO GET THE DATA.                               

C                                                                       

C    5. A SECTION TO STORE THE INPUT VALUES OF EXOGENOUS VARIABLES.     

C                                                                       

C    6. A CALL TO DEBCAL TO SOLVE THE MODEL.                            

C                                                                       

C    7. A SECTION TO WRITE VALUES OF NON-ZERO RESIDUALS TO TAPE 8.      

C                                                                       

C    8. A SECTION TO INITIATE A ROLLING FORECAST.                       

C                                                                       

C=======================================================================

C                                                                       

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 1.    LIST OF LIMITS.                                     

C     *********     **************                                      

C                                                                       

C     SECTION 1 ENDS.                                                   

C                                                                       

C=======================================================================

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 2.    LIST OF CHANGES TO THE MODEL.                       

C     *********     ****************************                        

C                                                                       

C     SECTION 2 ENDS.                                                   

C                                                                       

C=======================================================================

C                                                                       

C=======================================================================

C                                                                       

      CHARACTER*8   NAME, IXOG                                          

      LOGICAL       LFIX2                                               

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          ERX(600,300), EE(300,300), XX(600,300),
     2          ainfl(500,500)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,IPER,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/BASDAT/ XB(600,300)                                         

      COMMON/KGROUP/KK(41)/JGROUP/JJ(41)                                

      COMMON/WOREQN/IBLOC,NCONV,NBLOC                                   

      COMMON/NEWCAL/NEN(100),NEX(100),NENH(100),NEXP,MXEH,IOPTT,IOPTB,  

     1IOPTC                                                             

      COMMON/SIMDIM/NAG,LPER,MAXE,MAXX,MAXP                             

      COMMON/FIX2/ LFIX2, NFIX2, LDOFIX, NFIXMX, NFIXEND                

      COMMON/SHK/err(13,1000), zfiscal_outputgap, zfiscal_pinf

c      open(unit=12,file='/tmp/prakriti/rbcmod/fort.12')
c      open(unit=13,file='/tmp/prakriti/rbcmod/fort.13')
c      open(unit=7,file='/tmp/prakriti/rbcmod/fort.7')
c      open(unit=8,file='/tmp/prakriti/rbcmod/fort.8')
c      open(unit=9,file='/tmp/prakriti/rbcmod/fort.9')
      open(unit=14,file='qerror',status='old')

c************prakriti, reads in errors**********************************

      do 2001 j=1,234
      READ(14,*)(ERR(I,j),I=1,13)
 2001 continue

c************fiscal shock to increase g, and therefore interest rate
      open(unit=142,file='zfiscal_outputgap.data',status='old')
      READ(142,*)zfiscal_outputgap
      close(142)
      open(unit=142,file='zfiscal_pinf.data',status='old')
      READ(142,*)zfiscal_pinf
      close(142)
c***********************************************************************
C                                                                       

C=======================================================================

C                                                                       

C     SECTION 3.    READ THE CONTROL CARDS                              

C     *********     **********************                              

C                                                                       

C                                                                       

      NAG=0                                                             

      MAXE=300                                                          

      LPER=200                                                           

      MAXX=300                                                          

      MAXP=NAG+LPER                                                     

C                                                                       

C                                                                       

C     INSTRUCTIONS SPECIFY :                                            

C     NUMBER OF ENDOGENOUS VARIABLES     = NDOG                         

C     NUMBER OF EXOGENOUS VARIABLES      = LEXOG                        

C     LENGHT OF FORECAST PERIOD          = NPER                         

C     MAXIMUM NUMBER OF LAGS IN SYSTEM   = LAG                          

C     STARTING PERIOD OF FORECAST        = NSTART                       

C     MAXIMUM NUMBER OF ITERATIONS       = ITMX                         

C     DATA INPUT INSTRUCTION             = IB                           

C     MAXIMUM NUMBER OF PARAMETERS       = IPAR                         

C     MAXIMUM NUMBER OF RANDOM ERRORS    = IRAND                        

C     TOLERANCE LEVEL (0.0<TOL<0.25)     = TOL                          

C     PRINT OPTION FOR EXOGENOUS TERMS   = PX                           

C     PRINT OPTION FOR ITERATION STEPS   = P                            

C     DAMPING FACTOR  (0.O<B<1.0)        = B                            

C     BASERUN COMPARISON OPTION          = BR                           

C     BASERUN PRINT OPTION               = PR                           

C     JOINT OR SINGLE SHOCK OPTION       = NSK                          

C                                                                       

C                                                                       

C                                                                       


      WRITE(6,30)                                                       

   30 FORMAT(1H1,130(1H*)/,59X,'PROGRAM SIMM',/,1X,130(1H*)//)          

C.....READ THE FIRST CONTROL CARD.                                      

      READ(5,15) NEGP,NXGP,NPER,LAG,NSTART,ITMX,IB,NCG,IRAND            

     1,NSK,IJX,IOPT,IDYN,IDYN1,NBLOC,IRANDX,IOPTDA,NIT,IOPTAC           

      NG=NEGP+NXGP                                                      

C.....READ THE SECOND AND THIRD CONTROL CARDS                           

C.....(NUMBERS OF VARIABLES IN EACH GROUP).                             

      READ(5,15)(KK(I+1),I=1,NG)                                        

C.....READ THE FOURTH CONTROL CARD                                      

C.....(NUMBERS OF COEFFICIENTS IN EACH GROUP).                          

      READ(5,15)(JJ(I+1),I=1,NCG), IRHO

      JJ(1)=0                                                           

      KK(1)=0                                                           

      DO 1 I=1,NEGP                                                     

    1 KK(I+1)=KK(I+1)+KK(I)                                             

      NDOG=KK(NEGP+1)                                                   

      KK(NEGP+1)=0                                                      

      DO 2 I=1,NXGP                                                     

    2 KK(NEGP+I+1)=KK(NEGP+I+1)+KK(NEGP+I)                              

      LEXOG=KK(NEGP+NXGP+1)                                             

      DO 3 I=1,NCG                                                      

    3 JJ(I+1)=JJ(I+1)+JJ(I)                                             

      IPAR=JJ(NCG+1)                                                    

C.....READ THE FIFTH CONTROL CARD.                                      

      READ(5,21) PX,P,TOL,B,BR,PR,T                                     

C.....READ THE SIXTH CONTROL CARD.                                      

      IF(IDYN.GT.0) READ(5,23) NARG,MAXITR,BSTEP,TOLR,IOPTC,IOPTB,      

     >                         IOPTT, LDOFIX



C!!!! tight RE tolerance

C      IF ( TOLR .LE. 0.0 )  TOLR = 0.0001



   23 FORMAT(I8,I8,F4.0,F8.0,4I4)                                             

C                                                                       

      IF(NSK.NE.0) CALL PSHOCK                                          

C                                                                       


      MAG=LAG+1                                                         

      KAG=LAG+NPER                                                      

      WRITE(6,20)                                                       

      WRITE(6,19) NDOG,LEXOG,NPER,LAG,NSTART,ITMX,IB,IPAR,IRAND,        

     1NSK,IJX,IOPT,IDYN,IDYN1,NBLOC,NARG,IRANDX,IOPTDA,NIT,IOPTAC       

      WRITE(6,22) PX,P,TOL,B,BR,PR,T,BSTEP,TOLR,MAXITR,IOPTC,IOPTB,     

     >            IOPTT, LDOFIX

      IF((NDOG.GT.MAXE).OR.(LEXOG.GT.MAXX)) WRITE(6,10)                 

      IF(LAG.GT.NAG) WRITE(6,11)                                        

      IF(NPER.GT.LPER) WRITE(6,12)                                      

C                                                                       

C     SECTION 3 ENDS.                                                   

C                                                                       

C=======================================================================

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 4.    READ INITIAL VALUES OF ENDOGENOUS AND EXOGENOUS     

C     *********     VARIABLES AND RESIDUALS, AND COEFFICIENT VALUES,    

C                   BY CALL TO DATSIM.                                  

C                                                                       

      DO 4 I=1,MAXE                                                     

      DO 4 J=1,MAXP                                                     

    4 ER(I,J)=0.0D0                                                     

      KEX=0                                                             

C.....CALL DATSIM TO GET VALUES.                                        

      CALL DATSIM(IOPTDA,IOPTAC,KSTART,KEND)                            

      IF(IOPTDA.GT.0) CALL EXO(IOPTAC,KEX)                              

      IF(IOPTDA.EQ.2) NSTART = NSTART + KSTART -1                       

C                                                                       

C     SECTION 4 ENDS.                                                   

C                                                                       

C=======================================================================

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 5.    STORE INITIAL VALUES OF EXOGENOUS VARIABLES.        

C     *********     *******************************************         

C                                                                       

C-----THIS SECTION REMOVED. SEPTEMBER 1989.                             

C                                                                       

C     SECTION 5 ENDS.                                                   

C                                                                       

C=======================================================================

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 6.    SOLVE SYSTEM OF EQUATIONS BY CALL TO DEBCAL.        

C     *********     *******************************************         

C                                                                       

      CALL DEBCAL                                                       

C                                                                       

C     SECTION 6 ENDS.                                                   

C                                                                       

C=======================================================================

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 7.    WRITE VALUES OF RESIDUALS (IF NON-ZERO) TO TAPE 8.  

C     *********                                                         

                                                                        

C                                                                       

C-----THIS SECTION REMOVED.  SEPTEMBER 1989.                            

C                                                                       

C     SECTION 7 ENDS.                                                   

C                                                                       

C=======================================================================

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 8.    ROLLING FORECAST SECTION                            

C     *********     (NOT NORMALLY USED WITH WORLD MODEL).               

C                                                                       

      IF(NIT.EQ.0.AND.IOPTDA.LT.2) GOTO 100                             

      NRUNS=NIT                                                         

      IF(IOPTDA.EQ.2) NRUNS=KEND-KSTART                                 

      IF (NRUNS.LE.0) GOTO 100                                          

      IF(IDYN.GT.0) KEX=1                                               

      DO 200 IT=1,NRUNS                                                 

      CALL UPDAT(IOPTDA,IOPTAC,KSTART)                                  

      CALL EXO(IOPTAC,KEX)                                              

      CALL DEBCAL                                                       

  200 CONTINUE                                                          

  100 CONTINUE                                                          

C                                                                       

C     SECTION 8 ENDS.                                                   

C                                                                       

C=======================================================================


      open (unit=50, file='allout.out')
c      do 102, i=mag-1,kag
      do 102, i=3,234
         write(50,10110)e(1,i),e(2,i),e(3,i),e(4,i),e(5,i),e(6,i),
     >e(7,i),e(8,i),e(9,i),e(10,i),e(11,i),e(12,i),e(13,i),e(14,i),
     >x(22,i),x(23,i),x(24,i),x(25,i),x(26,i),x(27,i),x(28,i),
     >x(29,i),x(30,i),x(31,i),x(36,i),x(37,i),x(38,i)
 102                        continue
      close(50)
      open (unit=50, file='r_sim.out')
      open (unit=51, file='g_sim.out')
      open (unit=52, file='y_sim.out')
      open (unit=53, file='NB_sim.out')
c      do 1021, i=3,234
      do 1021, i=1,336
         write(50,10112)e(1,i),e(5,i)
         write(51,10216)x(22,i)
         write(52,10217)e(8,i),x(8,i),x(26,i)
         write(53,10216)x(37,i)
 1021     continue
      close(50)
      close(51)
      close(52)
      close(53)
10110  format(27f50.8)
10111   format(13f14.8)
10112    format(2f24.8)
10216     format(1f24.8)
10217      format(3f24.8)

      open (unit=52, file='current_pinf')
      write(52,10113)e(5,mag)
      close(52)
      open (unit=51, file='current_r')
      write(51,10113)e(1,mag)
      open (unit=51, file='current_r_base')
      write(51,10113)x(1,mag)
      open (unit=51, file='current_period')
      write(51,10114)mag-3
      open (unit=51, file='lagged_pi')
      do 10116, i=1,234
         if (e(5,i).gt.30.0d0) then
            ainfl(1,i)=30.0d0
         else if (e(5,i).lt.-30.0d0) then
            ainfl(1,i)=-30.0d0
         else
            ainfl(1,i)=e(5,i)
         end if
10116 continue

c      if (mag.le.15) then
c         write(51,10115) (0.00000,j=1,16-mag),
cc     >(e(5,i),i=mag-(mag-1),mag)
c     >(ainfl(1,i),i=mag-(mag-1),mag)
c      else
cc         write(51,10115)(ainfl(1,i),i=mag-15,mag)
cc         write(51,10115)(e(5,i),i=1,mag)
c         write(51,10115)(ainfl(1,i),i=3,mag)
         write(51,10115)(ainfl(1,i),i=1,mag)
c      end if
      close(51)
      open (unit=52, file='lagged_pi_base')
      write(52,10115)(x(5,i),i=1,mag)
      close(52)
10113 format(1f20.8)
10114 format(I8)
10115 format(234f20.8)

      DO 10100 I = 1,NDOG                                               

c      WRITE(13,90000)  ( E(I,J),J=1,KAG )                               

      WRITE(13,90000)  ( E(I,J),J=1,234 )                               

10100 CONTINUE                                                          

      DO 10200 I = 1,LEXOG                                              

c      WRITE(13,90000)  ( X(I,J),J=1,KAG )                               

      WRITE(13,90000)  ( X(I,J),J=1,234 )                               

10200 CONTINUE                                                          

90000 FORMAT(4F18.12)                                                    


      DO 10000 IC = 1,2                                                

      NV  =  KK(IC+1) - KK(IC)                                          

      DO 8000 IV = 1,NV                                                 

      LPRINT = 0                                                        

      KVAR = KK(IC) + IV                                                

      DO 6000 IY = 1,KAG                                                

      V  =  ER(KVAR,IY)                                                 

      IF ( ABS(V) .GT. 0.000001 ) THEN                                  

        LPRINT  =  1                                                    

      END IF                                                            

 6000 CONTINUE

      IF ( LPRINT .EQ. 1 ) THEN                                         

      WRITE(9,7000) IC,IV                                               

c      WRITE(9,7500) ( ER(KVAR,IY),IY=1,KAG )                            

      WRITE(9,7500) ( ER(KVAR,IY),IY=1,234 )                            

      END IF                                                            

 7000 FORMAT(2I2)                                                       

 7500 FORMAT(4F18.12)                                                    

 8000 CONTINUE                                                          

10000 CONTINUE                                                          




      STOP                                                              

   10 FORMAT(46H VARIABLE DIMENSIONS EXCEED MAXIMUM DIMENSIONS)         

   11 FORMAT(50H SPECIFIED NUMBER OF LAGS EXCEED ALLOWABLE MAXIMUM)     

   12 FORMAT(52H SPECIFIED FORECAST PERIOD EXCEEDS ALLOWABLE MAXIMUM)   

   15 FORMAT(20I4)                                                      

   21 FORMAT(20F4.0)                                                    

   16 FORMAT(16H END OF FORECAST)                                       

   19 FORMAT(10X,'NDOG   =',I4/10X,'LEXOG  =',I4/10X,'NPER   =',I4/     

     110X,'LAG    =',I4/10X,'NSTART =',I4/10X,'ITMX   =',I4/            

     210X,'IB     =',I4/10X,'IPAR   =',I4/10X,'IRAND  =',I4/            

     310X,'NSK    =',I4/10X,'IJX    =',I4/10X,'IOPT   =',I4/            

     410X,'IDYN   =',I4/10X,'IDYN1  =',I4/10X,'IBLOC  =',I4/            

     510X,'NARG   =',I5/10X,'IRANDX =',I4/10X,'IOPTDA =',I4/            

     610X,'NIT    =',I4/10X,'IOPTAC =',I4)                              

   20 FORMAT(8X,'INPUT PARAMETERS')                                     

   22 FORMAT(10X,'PX     =',F8.3/10X,'P      =',F8.3/10X,'TOL    =',F8.3

     1/10X,'B      =',F8.3/10X,'BR     =',F8.3/10X,'PR     =',F8.3/     

     210X,'T      =',F8.3/10X,'BSTEP  =',F8.3/10X,'TOLR   =',F8.3       

     3/10X,'MAXITR =',I4/10X,'IOPTC  =',I4/10X,'IOPTB  =',I4/10X,       

     4     'IOPTT  =',I4/10X,'LDOFIX =',I4)

      END                                                               

      SUBROUTINE EQN                                                    

C=======================================================================

C                                                                       

C     SUBROUTINE EQN  IS USED TO CALL THE APPROPRIATE COUNTRY S/R       

C     **************  TO GET NEW VALUES FOR EACH ENDOGENOUS VARIABLE.   

C                                                                       

C                     AFTER THE CALL TO EVALUATE THE VARIABLES IN THE   

C                     COMMON BLOC, SUBROUTINE WORJAC IS CALLED TO APPLY 

C                     A JACOBI CORRECTION AND TEST FOR CONVERGENCE.     

C                                                                       

C=======================================================================

                                                                        

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/V(300),Z(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,XX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,I,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR   

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/KGROUP/K01,K02,K03,K04,K05,K06,K07,K08,K09,K10,K11,K12,K13,

     1K14,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K26,K27,K28,K29,  

     2K30,K31,K32,K33,K34,K35,K36,K37,K38,K39,K40,K41                   

      COMMON/JGROUP/J01,J02,J03,J04,J05,J06,J07,J08,J09,J10,J11,J12,J13,

     1J14,J15,J16,J17,J18,J19,J20,J21,J22,J23,J24,J25,J26,J27,J28,J29,  

     2J30,J31,J32,J33,J34,J35,J36,J37,J38,J39,J40,J41                   

      COMMON/WOREQN/IBLOC,NCONV,NBLOC                                   




      CALL FTPL



      RETURN                                                            

      END                                                               



      SUBROUTINE SUBPXE(II,J,K,ERX,NN)                                  

C=======================================================================

C                                                                       

C     SUBROUTINE SUBPXE  IS USED TO ASSIGN A NEW VALUE TO THE           

C     *****************  J-TH EXOGENOUS VARIABLE OR EXOGENOUS RESIDUAL  

C                        IN THE (II-20)-TH BLOC IN THE K-TH PERIOD      

C                        RELATIVE TO THE CURRENT PERIOD.                

C                        ERX IS THE VALUE TO BE ASSIGNED.               

C                        IF NN = 1 THEN THE VALUE IS ASSIGNED TO        

C                                  THE EXOGENOUS VARIABLE.              

C                        IF NN = 2 THEN THE VALUE IS ASSIGNED TO        

C                                  THE EXOGENOUS RESIDUAL.              

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/V(300),Z(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,XX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,IPER,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/KGROUP/KK(41)                                              

      COMMON/JGROUP/JJ(41)                                              

      COMMON/WOREQN/IBLOC,NCONV,NBLOC                                   

      L=KK(II)+J                                                        

      IF(NN.EQ.2) GOTO 2                                                

    1 X(L,IPER+K)=ERX                                                   

      GOTO 3                                                            

    2 ER(L,IPER+K)=ERX                                                  

    3 RETURN                                                            

      END                                                               

      SUBROUTINE SUBCHX(II,J,K,ERX,NN)                                  

C=======================================================================

C                                                                       

C     SUBROUTINE SUBCHX  IS USED TO ASSIGN A NEW VALUE TO THE           

C     *****************  J-TH EXOGENOUS VARIABLE OR EXOGENOUS RESIDUAL  

C                        IN THE (II-20)-TH BLOC IN THE K-TH PERIOD      

C                        RELATIVE TO THE CURRENT PERIOD.                

C                                                                       

C                        THE ASSIGNED VALUE IS EQUAL TO THE ORIGINAL    

C                        VALUE PLUS ERX MULTIPLIED BY THE JACOBI FACTOR 

C                                                                       

C                                                                       

C                        IF NN = 1 THEN THE VALUE IS ASSIGNED TO        

C                                  THE EXOGENOUS VARIABLE.              

C                        IF NN = 2 THEN THE VALUE IS ASSIGNED TO        

C                                  THE EXOGENOUS RESIDUAL.              

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/V(300),Z(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,XX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,IPER,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/KGROUP/KK(41)                                              

      COMMON/JGROUP/JJ(41)                                              

      COMMON/WOREQN/IBLOC,NCONV,NBLOC                                   

      L=KK(II)+J                                                        

      IF(NN.EQ.2)GOTO 2                                                 

      XR=ERX                                                            

      X(L,IPER+K)=X(L,IPER+K)+B*XR                                      

      GOTO 3                                                            

    2 XR=ERX                                                            

      ER(L,IPER+K)=ER(L,IPER+K)+B*XR                                    

    3 RETURN                                                            

      END                                                               

      SUBROUTINE LIMHVV(II,J,K,RLL,RUL )                                

C=======================================================================

C                                                                       

C     SUBROUTINE LIMHVV  IS USED TO APPLY LIMITS TO THE GROWTH OR DECAY 

C     *****************  OF AN ENDOGENOUS VARIABLE, AS EVALUATED ON THE 

C                        LHS IN A COUNTRY SUBROUTINE.                   

C                        IF THE NEWLY CALCULATED VALUE OF AN ENDOGENOUS 

C                        VARIABLE IS LESS THAN RLL PERCENT LOWER OR     

C                        MORE THAN RUL PERCENT HIGHER THAN THE VALUE IN 

C                        THE PREVIOUS PERIOD THEN THE VALUE IS RESET TO 

C                        THE APPROPRIATE LIMIT.                         

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,XX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,IPER,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/KGROUP/KK(41)                                              

      COMMON/JGROUP/JJ(41)                                              

      COMMON/WOREQN/IBLOC,NCONV,NBLOC                                   

      L=KK(II)                                                          

      IF (K.NE.0.OR.II.GT.NEGP.OR.F(L+J).EQ.0.0D0) GOTO 20              

      QVV=  (F(L+J)-E(L+J,IPER))+E(L+J,IPER)                            

      IF(YLOG(QVV/E(L+J,IPER-1)).LT.RUL) GOTO 10                        

      F(L+J)=E(L+J,IPER-1)*XPN(RUL)                                     

   10 IF(YLOG(QVV/E(L+J,IPER-1)).GT.RLL) GOTO 20                        

      F(L+J)=E(L+J,IPER-1)*XPN(RLL)                                     

   20 CONTINUE         

      RETURN                                                            

      END                                                               

      SUBROUTINE LIMVV(II,J,K,RLL,RUL )                                 

C=======================================================================

C                                                                       

C     SUBROUTINE LIMVV    IS USED TO APPLY LIMITS TO THE GROWTH OR DECAY

C     ****************    OF AN ENDOGENOUS VARIABLE, AS IN LIMHVV.      

C                         HOWEVER, IN THIS CASE A JACOBI CORRECTION IS  

C                         MADE BEFORE THE LIMIT IS APPLIED.             

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,XX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,IPER,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/KGROUP/KK(41)                                              

      COMMON/JGROUP/JJ(41)                                              

      COMMON/WOREQN/IBLOC,NCONV,NBLOC                                   

      L=KK(II)                                                          

      IF( K.NE.0.OR.II.GT.NEGP.OR.F(L+J).EQ.0.0D0) GOTO 20              

      QVV=B*(F(L+J)-E(L+J,IPER))+E(L+J,IPER)                            

      IF(YLOG(QVV/E(L+J,IPER-1)).LT.RUL) GOTO 10                        

      E(L+J,IPER)=E(L+J,IPER-1)*XPN(RUL)                                

      F(L+J)=E(L+J,IPER)                                                

   10 IF(YLOG(QVV/E(L+J,IPER-1)).GT.RLL) GOTO 20                        

      E(L+J,IPER)=E(L+J,IPER-1)*XPN(RLL)                                

      F(L+J)=E(L+J,IPER)                                                

   20 CONTINUE                                                          

      RETURN                                                            

      END                                                               

      SUBROUTINE LIMLVV(II,J,K,RLL,RUL )                                

C=======================================================================

C                                                                       

C     SUBROUTINE LIMLVV    IS USED TO APPLY LIMITS TO THE LEVEL OF AN   

C     *****************    ENDOGENOUS VARIABLE, AS EVALUATED ON THE LHS 

C                          IN A COUNTRY SUBROUTINE.                     

C                          II  : DEFINES THE COUNTRY.                   

C                          J   : DEFINES THE VARIABLE.                  

C                          K   : DEFINES THE PERIOD.                    

C                          RLL : IS THE LOWER LIMIT.                    

C                          RUL : IS THE UPPER LIMIT.                    

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,XX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,IPER,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/KGROUP/KK(41)                                              

      COMMON/JGROUP/JJ(41)                                              

      COMMON/WOREQN/IBLOC,NCONV,NBLOC                                   

      L=KK(II)                                                          

      IF ( K.NE.0.OR.II.GT.NEGP.OR.F(L+J).EQ.0.0D0 ) GOTO 20            

      QVV=1.0D0*(F(L+J)-E(L+J,IPER))+E(L+J,IPER)                        

      IF(QVV.LT.RUL) GOTO 10                                            

      E(L+J,IPER)=  RUL                                                 

      F(L+J)=E(L+J,IPER)                                                

   10 IF(QVV.GT.RLL) GOTO 20                                            

      E(L+J,IPER)= RLL                                                  

      F(L+J)=E(L+J,IPER)                                                

   20 CONTINUE                                                          

      RETURN                                                            

      END                                                               

      SUBROUTINE EXOG(KEX)                                              

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

C=======================================================================

C                                                                       

C     SUBROUTINE EXOG    IS USED TO GENERATE INITIAL VALUES FOR         

C     ***************    EXOGENOUS VARIABLES, VIA CALLS TO EXCO         

C                        AND EXCOM.                                     

C                        IT IS CALLED FROM SUBROUTINE EXO, WHICH IS     

C                        CALLED WHEN IOPTDA ( CONTROL CARD 1 ) IS NOT   

C                        EQUAL TO ZERO.                                 

C=======================================================================

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          ERX(600,300), EE(300,300), XX(600,300)

      COMMON/HELEN/V(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,I   ,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/KGROUP/KK(41)/JGROUP/JJ(41)                                

C                                                                       

      DO 10 IC = 1 , 9                                                  

   10 CALL EXCO (IC)                                                    

      CALL EXCOM                                                        


      RETURN                                                            

      END                                                               

      SUBROUTINE EXCO ( ICE )                                           

C=======================================================================

C                                                                       

C     THIS IS A SUBROUTNE FOR INTIALIZING THE EXOGENOUS                 

C     VARIABLES. IT IS CALLED FROM SUBROUTINE EXOG.                     

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          ERX(600,300), EE(300,300), XX(600,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,I   ,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/KGROUP/KK(41)/JGROUP/JJ(41)                                

C                                                                       

      DV(I,J,K)= VV(I,J,K) - VV(I,J,K-1)                                

      DL(I,J,K)= VV(I,J,K) / VV(I,J,K-1)                                

      N = 1+ I-MAG                                                      

      ICX= 20 + ICE                                                     

C   PEXPL                                                               

      EX1 = VV(ICX,8,-1)                                                

      CALL SUBPXE(ICX,1,0,EX1,1)                                        

C   GDP*                                                                

      EX2 = VV(ICX,2,-1)*(DL(ICX,2,-1)+DL(ICX,2,-2))/2.0D0              

      CALL SUBPXE(ICX,2,0,EX2,1)                                        

C                                                                       

      EX4 = VV(ICX,4,-1)                                                

      CALL SUBPXE(ICX,4,0,EX4,1)                                        

C                                                                       

      EX5 = 0.0D0                                                       

      CALL SUBPXE(ICX,5,0,EX5,1)                                        

C                                                                       

      EX6 = 0.0D0                                                       

      CALL SUBPXE(ICX,6,0,EX6,1)                                        

C    PEXP                                                               

      EX7 = VV(ICX,8,-1)                                                

      CALL SUBPXE(ICX,7,0,EX7,1)                                        

C    DMT                                                                

      EX8 = VV(ICX,8,-1)                                                

      CALL SUBPXE(ICX,8,0,EX8,1)                                        

C    QEXP                                                               

      EX9 = 0.0D0                                                       

      CALL SUBPXE(ICX,9,0,EX9,1)                                        

C    QEXPL                                                              

      EX10 = 0.0D0                                                      

      CALL SUBPXE(ICX,10,0,EX10,1)                                      

C    EXPL                                                               

      EX11 = VV(ICE,15,-N)                                              

      CALL SUBPXE(ICX,11,0,EX11,1)                                      

C    EEXP                                                               

      EX12 = VV(ICE,15,-N)                                              

      CALL SUBPXE(ICX,12,0,EX12,1)                                      

C    EG                                                                 

      EX3  = VV(ICX,2,0)*(DV(ICX,4,0) +(VV(ICE,6,-N)/VV(ICX,2,-N))*     

     1 DV(ICX,8,0) + VV(ICX,3,-1)/VV(ICX,2,-1) )                        

      CALL SUBPXE(ICX, 3,0,EX3 ,1)                                      

C                                                                       

      RETURN                                                            

      END                                                               

      SUBROUTINE EXCOM                                                  

C=======================================================================

C                                                                       

C     THIS IS A SUBROUTNE FOR INTIALIZING THE EXOGENOUS VARIABLES IN THE

C     COMMON BLOC.  IT IS CALLED BY SUBROUTINE EXOG.                    

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          ERX(600,300), EE(300,300), XX(600,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,I   ,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/KGROUP/KK(41)/JGROUP/JJ(41)                                

C                                                                       

      DV(I,J,K)= VV(I,J,K) - VV(I,J,K-1)                                

      DL(I,J,K)= VV(I,J,K) / VV(I,J,K-1)                                

      N = I-MAG                                                         

C   V                                                                   

      CALL SUBPXE(33,1,0,0.0D0,1)                                       

C   CON XV                                                              

      CALL SUBPXE(33,2,0,0.0D0,1)                                       

C   CON XR                                                              

      CALL SUBPXE(33,3,0,0.0D0,1)                                       

C   BE  DS                                                              

      CALL SUBPXE(33,4,0,0.0D0,1)                                       

C   NE  DS                                                              

      CALL SUBPXE(33,5,0,0.0D0,1)                                       

C   RSUSF5                                                              

      CALL SUBPXE(33,6,0,0.0D0,1)                                       

C   RLUSF1                                                              

      CALL SUBPXE(33,7,0,0.0D0,1)                                       

      RETURN                                                            

      END                                                               

      SUBROUTINE DATSIM(IOPTDA,IOPTAC,KSTART,KEND)                      

C=======================================================================

C                                                                       

C     SUBROUTINE DATSIM    HAS THREE SECTIONS WHICH ARE USED WITH THE   

C     *****************    CURRENT ( SEPTEMBER 1987 ) OPTIONS :         

C                                                                       

C                          SECTION 1 : READS THE NAMES OF ENDOGENOUS    

C                                      AND EXOGENOUS VARIABLES.         

C                                                                       

C                          SECTION 2 : READS THE VALUES OF ENDOGENOUS   

C                                      AND EXOGENOUS VARIABLES AND      

C                                      RESIDUALS.                       

C                                                                       

C                          SECTION 3 : READS THE COEFFICIENTS AND       

C                                      WRITES THE VALUES TO THE         

C                                      OUTPUT FILE.                     

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          ERX(600,300), EE(300,300), XX(600,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,IPER,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/KGROUP/KK(41)/JGROUP/JJ(41)                                

      COMMON/POWELL/R(200000),DIR(200000),L(200000),ICTRL                     

      COMMON/NEWCAL/NEN(100),NEX(100),NENH(100),NEXP,MXEH,IOPTT,IOPTB,  

     1IOPTC                                                             

      COMMON/ARFUNC/ ARCOEF(11,50,6)                                    



      IF(IDYN.EQ.0.OR.IOPTC.EQ.1) GOTO 165                              

      K=0                                                               

      DO 160 IG=1,NEGP                                                  

      KKI=KK(IG)+1                                                      

      KKT=KK(IG+1)                                                      

      IF(IG.EQ.NEGP) KKT=NDOG                                           

      IF((KKT-KKI).LT.0) GOTO 160                                       

      DO 155  J=KKI,KKT                                                 

      READ(5,102) NAME(J),NCKEX                                         

      IF(NCKEX.LE.0) GOTO 155                                           

      DO 157 II=1,NCKEX                                                 

      READ(5,103) NEXH,NEXV                                             

      K=K+1                                                             

      NEN(K)=J                                                          

      NEX(K)=KK(IG+NEGP)+NEXV                                           

      NENH(K)=NEXH                                                      

  157 CONTINUE                                                          

  155 CONTINUE                                                          

  160 CONTINUE                                                          

      NEXP=K                                                            

      MXEH=0                                                            

      DO 159 J=1,NEXP                                                   

  159 IF(MXEH.LT.NENH(J)) MXEH=NENH(J)                                  

      NGX=NEXP*NPER                                                     

      IF(IOPTB.EQ.0) NGX=NEXP*(NPER+1)                                  

      NARG=NGX                                                          

      GOTO 175                                                          

C=======================================================================

C                                                                       

C     SECTION 1.    READ THE NAMES FOR THE ENDOGENOUS                   

C                   AND EXOGENOUS VARIABLES.                            

C                                                                       

C=======================================================================

  165 CONTINUE                                                          

      DO 50 I=1,NDOG                                                    

   50 READ(5,101)  NAME(I)                                              

  175 CONTINUE                                                          

      DO 55 I=1,LEXOG                                                   

   55 READ(5,101)  IXOG(I)                                              

C=======================================================================

C                                                                       

C     SECTION 1 ENDS.                                                   

C                                                                       

C=======================================================================

      IF(IOPTC.EQ.1) GOTO 180                                           

      WRITE(6,1005) NARG,NEXP,MXEH,IOPTB                                

      DO 170 K=1,NEXP                                                   

  170 WRITE(6,1006) NEN(K),NAME(NEN(K)),NENH(K),NEX(K),IXOG(NEX(K))     

 1005 FORMAT(/10X,'NARG    =',I4/10X,'NEXP    =',I4/10X,'MXEH    =',    

     1 I4//,'  ENDOG. VAR.   EXP. HORIZON   EXP. VAR. ("EXO. VAR.")'    

     2 ,I4/)                                                            

 1006 FORMAT(/2X,I4,2X,A8,6X,I4,6X,I4,2X,A8)                            

  180 CONTINUE                                                          

      LAG=MAG-1                                                         

      KG1=KAG                                                           

      IF(IOPTDA.EQ.2) GOTO 200                                          

      IF(IOPTDA.EQ.1) KG1=MAG                                           

      IF(IB.EQ.0) GO TO 20                                              

C                                                                       

C      THIS SECTION READS DATA BY OBSERVATION                           

C                                                                       

      DO 1 J=1,MAG                                                      

    1 READ(5,*) (E(I,J),I=1,NDOG)                                       

      DO 10 J=1,KG1                                                     

   10 READ(5,*) (X(I,J),I=1,LEXOG)                                      

      GO TO 60                                                          

   20 CONTINUE                                                          

C=======================================================================

C                                                                       

C     SECTION 2.    READ THE VALUES OF ENDOGENOUS AND EXOGENOUS         

C                   VARIABLES AND RESIDUALS, BY VARIABLE.               

C                                                                       

C=======================================================================

C                                                                       

C.....ENDOGENOUS VARIABLES.                                             

      DO 22 I=1,NDOG                                                    

   22 READ(5,*) (E(I,J),J=1,KAG)                                        

   60 IF(IRAND.EQ.0) GOTO 26                                            

C.....ENDOGENOUS RESIDUALS.                                             

      DO 25 K=1,IRAND                                                   

      READ(5,2) IGP,IVB                                                 

      KKK=KK(IGP)+IVB                                                   

   25 READ(5,*) (ER(KKK,J),J=1,KAG)                                     

   26 CONTINUE                                                          

      IF(IB.NE.0) GOTO 65                                               

C.....EXOGENOUS VARIABLES.                                              

      DO 24 I=1,LEXOG                                                   

   24 READ(5,*) (X(I,J),J=1,KG1)                                        

   65 IF(IRANDX.EQ.0) GOTO 199                                          

C.....EXOGENOUS RESIDUALS.                                              

      DO 27 K=1,IRANDX                                                  

      READ(5,2) IGP,IVB                                                 

      KKK=KK(IGP)+IVB                                                   

   27 READ(5,*) (ERX(KKK,J),J=1,KAG)                                    

    2 FORMAT(2I2)                                                       

      GOTO 199                                                          

C=======================================================================

C                                                                       

C     END OF SECTION 2.                                                 

C                                                                       

C=======================================================================

  200 CONTINUE                                                          

      READ(5,3) ISSZ,KSTART,KEND                                        

      DO 201 I=1,NDOG                                                   

  201 READ(5,*)(EE(I,J) ,J=1,ISSZ)                                      

      DO 202 I=1,LEXOG                                                  

  202 READ(5,*)(XX(I,J) ,J=1,ISSZ)                                      

      KSL=KSTART-LAG                                                    

      IF(KSL.GT.0) GOTO 203                                             

    3 FORMAT(3I4)                                                       

C                                                                       

C                                                                       

      KSTART=1-KSL                                                      

      NSTART=NSTART-KSL+1                                               

      KSL=KSTART-LAG                                                    

  203 CONTINUE                                                          

      MG1=MAG-1                                                         

      DO 204 J=1,NDOG                                                   

      DO 204 I=1,MG1                                                    

  204 E(J,I)=EE(J,KSL-1+I)                                              

      MG1=MAG-1                                                         

      IF(IOPTAC.EQ.1) MG1=MG1+1                                         

      DO 205 J=1,LEXOG                                                  

      DO 205 I=1,MG1                                                    

  205 X(J,I)=XX(J,KSL-1+I)                                              

      DO 206 J=1,NDOG                                                   

  206 E(J,MAG)=E(J,MAG-1)                                               

  199 CONTINUE                                                          

C=======================================================================

C                                                                       

C     SECTION 3.    READ THE COEFFICIENTS AND WRITE THE VALUES          

C                   TO THE OUTPUT FILE.                                 

C                                                                       

C=======================================================================

      WRITE(6,709)                                                      

  709 FORMAT(1H1,55X,'COEFFICIENT VALUES',/)                            
  
c      READ(5,*) (A(I),I=1,IPAR)                                         
c      DO 701 K=1,NCG                                                    
c      KKK=JJ(K+1)-JJ(K)                                                 
c      IF(KKK.EQ.0) GOTO 701                                             
c      WRITE(6,720) K                                                    
c  720 FORMAT(//,2X,'GROUP',I3)                                          
c      KJ=JJ(K)                                                          
c      WRITE(6,710) (A(KJ+M),M=1,KKK)                                    
c  701 CONTINUE                                                          

      OPEN(UNIT=123,FILE='coef.data',STATUS='old')
      READ(123,*) (A(I),I=1,IPAR)                                         
      CLOSE(123)
      OPEN(UNIT=123,FILE='ar_coeffs.data',STATUS='old')
      READ(123,*) (A(I),I=IPAR+1,IPAR+IRHO)                                         
      CLOSE(123)
      WRITE(6,710) (A(M),M=1,IPAR+IRHO)                                    


C=======================================================================

C                                                                       

C     SECTION 3 ENDS.                                                   

C                                                                       

C=======================================================================



  710 FORMAT(10X,10F12.4)                                               

      WRITE(6,304)                                                      

  304 FORMAT(1H1)                                                       

  101 FORMAT(A8)                                                        

  102 FORMAT(A8,I4)                                                     

  103 FORMAT(2I4)                                                       

      RETURN                                                            

      END                                                               

      SUBROUTINE ALISON(NDUM,FX)                                        

C=======================================================================

C                                                                       

C     SUBROUTINE ALISON    IS NOT ACTIVE.                               

C     *****************                                                 

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,IPER,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/KGROUP/KK(41)                                              

      COMMON/POWELL/R(200000),DIR(200000),L(200000),ICTRL                     

      COMMON/NEWCAL/NEN(100),NEX(100),NENH(100),NEXP,MXEH,IOPTT,IOPTB,  

     1IOPTC                                                             

      KX(I,J)=600*(J-1)+I                                               

      KVV(IG,J,K)=KX(KK(IG)+J,I+K)                                      

C                                                                       

C  STATEMENT FUNCTION KX(I,J) GIVES THE ONE DIMENSIONAL POSITION IN X OF

C  N.B. 600 IS MAXIMUM OF 1ST DIMENSION OF X                            

C  THIS MUST BE ADJUSTED IF DIMENSION IS CHANGED IN COMMON BLOCK ARRAY  

C                                                                       

      IF(IDYN.EQ.2) CALL CONT(IDYN1)                                    

C     TERMINAL CONDITIONS SET HERE                                      

      I=KAG                                                             

      MXEH1=MXEH+1                                                      

C                                                                       

C     IOPTT GIVES THE RELEVANT OPTION FOR TERMINALS                     

C                                                                       

      GOTO (10,20,30) (IOPTT+1)                                         

C                                                                       

C     OPTION 0 IS ONE WHERE ENDO. VAR.'S ARE SET TO                     

C     LAST INTIAL VALUE OF THE RELEVANT EXP. VAR.                       

C     N.B. ONLY THE 1ST SPECFIED EXP. VAR. USED                         

C                                                                       

   10 CONTINUE                                                          

      DO 101 IE=1,NEXP                                                  

      IF(IE.EQ.1) GOTO 100                                              

      IF(NEN(IE-1).EQ.NEN(IE)) GOTO 101                                 

  100 CONTINUE                                                          

      DO 1 K =1,MXEH1                                                   

    1 E(NEN(IE),I+K)=X(NEX(IE),I)                                       

  101 CONTINUE                                                          

      GOTO 40                                                           

C                                                                       

C     OPTION 1 SETS  THE TERMINAL VALUES TO BE THE                      

C     LAST VALUE OF THE RELEVANT ENDOGENOUS VAR.                        

C                                                                       

   20 CONTINUE                                                          

      DO 202 IE=1,NEXP                                                  

      IF(IE.EQ.1) GOTO 200                                              

      IF(NEN(IE-1).EQ.NEN(IE)) GOTO 202                                 

  200 CONTINUE                                                          

      DO 2 K =1,MXEH1                                                   

    2 E(NEN(IE),I+K)=E(NEN(IE),I)                                       

  202 CONTINUE                                                          

      GOTO 40                                                           

C                                                                       

C     OPTION 2 SETS THE TERMINAL VALUES TO THE                          

C     NAIVE FORECAST VALUES OF THE RELEVANT ENDO.                       

C     VAR.'S. I.E. THE FINAL GROWTH RATE OF THE                         

C     END. VAR. IS USED TO FORECAST THE TERMINAL                        

C     VALUES.                                                           

C                                                                       

   30 CONTINUE                                                          

      DO 303 IE=1,NEXP                                                  

      IF(IE.EQ.1) GOTO 300                                              

      IF(NEN(IE-1).EQ.NEN(IE)) GOTO 303                                 

  300 CONTINUE                                                          

      IF(E(NEN(IE),I-1).EQ.0.0D0) GOTO 31                               

      DKG=E(NEN(IE),I)/E(NEN(IE),I-1)                                   

      GOTO 32                                                           

   31 DKG=1.0D0                                                         

   32 CONTINUE                                                          

      DO 3 K =1,MXEH1                                                   

    3 E(NEN(IE),I+K)=E(NEN(IE),I+K-1)*DKG                               

  303 CONTINUE                                                          

   40 CONTINUE                                                          

      DO 50 IE=1,NEXP                                                   

   50 X(NEX(IE),I+1)=E(NEN(IE),I+NENH(IE))                              

C                                                                       

C     SET L(J) AND R(J) ARRAYS                                          

C                                                                       

C     THE R(J) ARRAY CONTAINS THE DIFFRENCE BETWEEN PREVIOUS FORCAST AND

C     CURRENT ITERATION FORECAST . THIS IS USED IN JACOBI ALGO. IN  MAIN

C     TO UPDAT PREVIOUS EXPECTATIONAL VAR.'S ("PREV. FORECAST"!!!!!)    

C                                                                       

      J =0                                                              

      DO 4 IE=1,NEXP                                                    

      DO 4 I =MAG,KAG                                                   

      J=J+1                                                             

      L(J)=KX(NEX(IE),I)                                                

      R(J)=E(NEN(IE),I+NENH(IE))-X(NEX(IE),I)                           

    4 CONTINUE                                                          

      I=MAG-1                                                           

      DO 5 IE=1,NEXP                                                    

      J=J+1                                                             

      L(J)=KX(NEX(IE),I)                                                

      R(J)=E(NEN(IE),I+NENH(IE))-X(NEX(IE),I)                           

    5 CONTINUE                                                          

      RETURN                                                            

      END                                                               

      SUBROUTINE UPDAT(IOPTDA,IOPTAC,KSTART)                            

C=======================================================================

C                                                                       

C     SUBROUTINE UPDAT    IS USED TO UPDATE VALUES OF ENDOGENOUS AND    

C     ****************    EXOGENOUS VARIABLES IN A ROLLING FORECAST.    

C                         IT IS NOT NORMALLY USED WITH THE WORLD MODEL. 

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          ERX(600,300), EE(300,300), XX(600,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,IPER,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/KGROUP/KK(41)/JGROUP/JJ(41)                                

      COMMON/POWELL/R(200000),DIR(200000),L(200000),ICTRL                     

      NSTART=NSTART+1                                                   

      MG2=MAG-2                                                         

      IF(MG2.LE.0) GOTO 50                                              

      DO 10 J=1,NDOG                                                    

      DO 10 I=1,MG2                                                     

   10 E(J,I)=E(J,I+1)                                                   

      IF(IOPTAC.EQ.1) MG2=MG2+1                                         

      DO 20 J=1,LEXOG                                                   

      DO 20 I=1,MG2                                                     

   20 X(J,I)=X(J,I+1)                                                   

   50 CONTINUE                                                          

      MG1=MAG-1                                                         

      IF(IOPTDA.EQ.2) GOTO 40                                           

      READ(5,*) (E(J,MG1),J=1,NDOG)                                     

      IF(IOPTAC.EQ.1) MG1=MG1+1                                         

      READ(5,*) (X(J,MG1),J=1,LEXOG)                                    

      GOTO 45                                                           

   40 CONTINUE                                                          

      DO 23 J=1,NDOG                                                    

   23 E(J,MG1)=EE(J,KSTART)                                             

      KSST=KSTART                                                       

      IF(IOPTAC.EQ.1) MG1=MG1+1                                         

      IF(IOPTAC.EQ.1) KSST=KSTART+1                                     

      DO 24 J=1,LEXOG                                                   

   24 X(J,MG1)=XX(J,KSST)                                               

      KSTART=KSTART+1                                                   

   45 CONTINUE                                                          

      DO 25 J=1,NDOG                                                    

   25 E(J,MAG)=E(J,MAG+1)                                               

      RETURN                                                            

      END                                                               

      SUBROUTINE EXO(IOPTAC,KEX)                                        

C=======================================================================

C                                                                       

C     SUBROUTINE EXO    IS CALLED BY THE MAIN PROGRAM WHEN              

C     **************    IOPTDA ( CARD 1 ) IS GREATER THAN ZERO.         

C                       IT IS USED TO INITIATE GENERATION OF VALUES     

C                       FOR EXOGENOUS VARIABLES.                        

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          ERX(600,300), EE(300,300), XX(600,300)

      COMMON/HELEN/f(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,IPER,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/NEWCAL/NEN(100),NEX(100),NENH(100),NEXP,MXEH,IOPTT,IOPTB,  

     1IOPTC                                                             

      MG=MAG                                                            

      IF(IOPTAC.EQ.1) MG=MG+1                                           

      IF (IOPTC.GT.0.OR.KEX.EQ.0) GOTO 10                               

      DO 30 J=1,NEXP                                                    

   30 X(NEX(J),MG-1)=X(NEX(J),MG)                                       

   10 CONTINUE                                                          

      DO 20 I=MG,KAG                                                    

      IPER=I                                                            

      CALL EXOG(KEX)                                                    

      IF (IOPTC.GT.0.OR.KEX.EQ.0) GOTO 20                               

      DO 40 J=1,NEXP                                                    

   40 X(NEX(J),I)=X(NEX(J),I+1)                                         

   20 CONTINUE                                                          

      RETURN                                                            

      END              

      SUBROUTINE DEBCAL

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG, AJX(10)                                 

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300)  ,X(600,300),ER(300,300),

     &          ERX(600,300),EE(300,300),XX(600,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,IPER,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/KGROUP/KK(41)/JGROUP/JJ(41)                                

      COMMON/POWELL/R(200000),DIR(200000),L(200000),ICTRL                     

      COMMON/NEWCAL/NEN(100),NEX(100),NENH(100),NEXP,MXEH,IOPTT,IOPTB,  

     1IOPTC                                                             

      COMMON/FIX2/ LFIX2, NFIX2, LDOFIX, NFIXMX, NFIXEND                

      DIMENSION NJX(10)                                                 

      DIMENSION XEQUIV(120000)                                           

      EQUIVALENCE ( XEQUIV(1),X(1,1) )                                  

      EXTERNAL CALCFX,ALISON                                            

      COMMON/SHK/err(13,1000), zfiscal_outputgap, zfiscal_pinf
      character*18 errname, gen_errname, xname, gen_xname


C                                                                       

C                                                                       

  199 IF(NSK.NE.0) CALL SHOCK                                           

C                                                                       

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 1.  WRITE THE INPUT VALUES OF ENDOGENOUS AND EXOGENOUS    

C                 VARIABLES AND RESIDUALS TO THE OUTPUT FILE.           

C                                                                       

C=======================================================================

C                                                                       

C                                                                       


      WRITE(6,226)                                                      

  226 FORMAT(1H1,60X,'INPUT DATA')                                      

C.....ENDOGENOUS VARIABLES.                                             

      DO 225 K=1,NEGP                                                   

      KKK=KK(K+1)                                                       

      IF(K.EQ.NEGP) KKK=NDOG                                            

      KKK=KKK-KK(K)                                                     

      IF(KKK.EQ.0) GOTO 225                                             

      ISTART=NSTART-MAG                                                 

      WRITE(6,409) K                                                    

      DO 222 I=1,MAG,10                                                 

      DO 223 J=1,10                                                     

  223 JX(J)=ISTART+J                                                    

      IE=MIN0(10,MAG-I+1)                                               

      WRITE(6,408)(JX(J),J=1,IE)                                        

      DO 224 J=1,KKK                                                    

      KJ=KK(K)+J                                                        

  224 WRITE(6,405) NAME(KJ),(E(KJ,I+M-1),M=1,IE)                        

      ISTART=ISTART+10                                                  

  222 CONTINUE                                                          

  225 CONTINUE                                                          

  405 FORMAT(2X,A8,10F12.4)                                             

  408 FORMAT(/14X,I4,9(8X,I4))                                          

  409 FORMAT(//,2X,'GROUP',I3,' ENDOGENOUS VARIABLES')                  

C.....EXOGENOUS VARIABLES.                                              

      DO 525 K=1,NXGP                                                   

      KKK=KK(NEGP+K+1)-KK(NEGP+K)                                       

      IF(KKK.EQ.0) GOTO 525                                             

      K2=K+NEGP                                                         

      WRITE(6,509) K2                                                   

  509 FORMAT(//,2X,'GROUP',I3,' EXOGENOUS VARIABLES')                   

      ISTART=NSTART-MAG                                                 

      DO 522 I=1,KAG,10                                                 

      DO 523 J=1,10                                                     

  523 JX(J)=ISTART+J                                                    

      IE=MIN0(10,KAG-I+1)                                               

      WRITE(6,408)(JX(J),J=1,IE)                                        

      DO 524 J=1,KKK                                                    

      KJ=KK(NEGP+K)+J                                                   

  524 WRITE(6,405) IXOG(KJ),(X(KJ,I+M-1),M=1,IE)                        

      ISTART=ISTART+10                                                  

  522 CONTINUE                                                          

  525 CONTINUE                                                          

C.....ENDOGENOUS RESIDUALS.                                             

      IF(IRAND.EQ.0) GOTO 625                                           

      DO 626 K=1,NEGP                                                   

      KKK=KK(K+1)                                                       

      IF(K.EQ.NEGP) KKK=NDOG                                            

      KKK=KKK-KK(K)                                                     

      IF(KKK.EQ.0) GOTO 626                                             

      WRITE(6,609) K                                                    

  609 FORMAT(//,2X,'GROUP',I3,' RESIDUAL VALUES')                       

      ISTART=NSTART-MAG                                                 

      DO 622 I=1,KAG,10                                                 

      DO 623 J=1,10                                                     

  623 JX(J)=ISTART+J                                                    

      IE=MIN0(10,KAG-I+1)                                               

      WRITE(6,408)(JX(J),J=1,IE)                                        

      DO 624 J=1,KKK                                                    

      KJ=KK(K)+J                                                        

  624 WRITE(6,405) NAME(KJ),(ER(KJ,I+M-1),M=1,IE)                       

      ISTART=ISTART+10                                                  

  622 CONTINUE                                                          

  626 CONTINUE                                                          

  625 CONTINUE                                                          

C.....EXOGENOUS RESIDUALS.                                              

      IF(IRANDX.EQ.0) GOTO 725                                          

      DO 726 K=1,NEGP                                                   

      KKK=KK(NEGP+K+1)                                                  

      IF(K.EQ.NXGP) KKK=LEXOG                                           

      KKK=KKK-KK(NEGP+K)                                                

      IF(KKK.EQ.0) GOTO 726                                             

      WRITE(6,709) (K+NEGP)                                             

  709 FORMAT(//,2X,'GROUP',I3,' RESIDUAL VALUES')                       

      ISTART=NSTART-MAG                                                 

      DO 722 I=1,KAG,10                                                 

      DO 723 J=1,10                                                     

  723 JX(J)=ISTART+J                                                    

      IE=MIN0(10,KAG-I+1)                                               

      WRITE(6,408)(JX(J),J=1,IE)                                        

      DO 724 J=1,KKK                                                    

      KJ=KK(NEGP+K)+J                                                   

  724 WRITE(6,405) IXOG(KJ),(ERX(KJ,I+M-1),M=1,IE)                      

      ISTART=ISTART+10                                                  

  722 CONTINUE                                                          

  726 CONTINUE                                                          

  725 CONTINUE                                                          

      WRITE(6,204)                                                      

C=======================================================================

C                                                                       

C     SECTION 1 ENDS.                                                   

C                                                                       

C=======================================================================

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 2.    THIS SECTION IS USED TO :                           

C                                                                       

C                   1. INTITIALISE THE COUNTER FOR THE NUMBER OF        

C                      RATIONAL EXPECTATIONS ITERATIONS.                

C                                                                       

C                   2. CALL SUBROUTINE CONT, TO SELECT THE REQUIRED     

C                      SOLUTION PROCEDURE.                              

C                                                                       

C                   3. CALL CALCFX, TO CALCULATE THE RESIDUAL VALUES    

C                      FOR THE RATIONAL EXPECTATIONS VARIABLES.         

C                                                                       

C                   4. TEST FOR CONVERGENCE IN THE R.E. VARIABLES.      

C                                                                       

C                   5. CALL PRINT, TO PRINT THE SOLUTION VALUES         

C                      IF THE MODEL HAS CONVERGED OR IF THE MAXIMUM     

C                      NUMBER OF RATIONAL EXPECTATIONS ITERATIONS       

C                      ( MAXITR, CARD 6 ), HAS BEEN REACHED.            

C                      IF THE MODEL HAS NOT CONVERGED AND THE MAXIMUM   

C                      NUMBER OF R.E. ITERATIONS HAS NOT BEEN REACHED   

C                      THEN ANOTHER RATIONAL EXPECTATIONS ITERATION IS  

C                      PERFORMED, AFTER A JACOBI UPDATE OF THE VALUES   

C                      OF THE RATIONAL EXPECTAIONS VARIABLES.           

C                                                                       

C                   6. OPTIONALLY THE CURRENT VALUES OF RATIONAL        

C                      EXPECTATIONS VARIABLES AND RESIDUALS ARE PRINTED 

C                      AT THE END OF AN R.E. ITERATION.                 

C                                                                       

C=======================================================================


C********************roll_f changes*********************************

C.....Define range of starting points for each of teh 59 simulations in
C.....the rolling forecast.
c      NBGN = 3
c      NEND = 42
      open(unit=35,file='nlag')
      read(35,*)nlag
      close(35)
      NBGN = nlag+1
      NEND = nlag+1
      
      open(unit=35,file='lags_endog')
      do 3013 i=1,ndog
 3013    read(35,*)(e(i,j),j=1,nlag)
      close(35)
 
      DO 1111 ILOOP = NBGN,NEND
         MAG = ILOOP
         LAG = MAG - 1
c         KAG = MAG + 17
         KAG = MAG + 49


C***********READING IN THE RESIDUALS********************************c

      errname = gen_errname(ILOOP)
      open(unit=99,file=errname)
      xname = gen_xname(ILOOP)
      open(unit=98,file=xname)
      IC=1
      DO 8010 IV = 1,NDOG                                                 
      KVAR=KK(IC)+IV
      read(99,7001) IC,IVV                                              
      read(99,7501) ( ER(KVAR,IY),IY=1,KAG )                            
 8010 continue
C ************* only read to LEXOG-3 because NB,pibar,NBbase are read
C ************* from the data file, not the exogenous file.
      DO 8020 I=1,LEXOG-3
 8020 read(98,7501) (X(I,J), J=1,KAG)
 7001 FORMAT(2I2)                                                    
 7501 FORMAT(4F30.12)                                                
 
      open(unit=35,file='lags_exog')
      do 3015 i=1,lexog
 3015    read(35,*)(x(i,j),j=1,nlag)
      close(35)

c      write(44,7502)(ER(8,IIC),IIC=1,KAG)
c      write(45,7502)(ER(21,IIC),IIC=1,KAG)
c 7502 format(KAGf18.12)
C*********************************************************************

      ICTRL=0                                                           

C.....INITIALISE THE COUNTER FOR NUMBER OF R.E. ITERATIONS.             

      ITR=-1                                                            

      IF(IDYN.GT.0) ITR=0                                               

      IF(IDYN.EQ.2) GOTO 50                                             

C.....GO TO THE REQUIRED SOLUTION PROCEDURE.                            

  100 CALL CONT(IDYN1)                                                  

      IF(IDYN.EQ.0) GOTO 66                                             

C.....CALCULATE THE RESIDUALS FOR THE R.E. VARIABLES.                   

      IF(IOPTC.GT.0) GOTO 111                                           

      CALL ALISON(NARG,FX)                                              

      GOTO 112                                                          

  111 CALL CALCFX(FX)                                              

  112 CONTINUE                                                          

C.....TEST THE R.E. VARIABLES FOR CONVERGENCE.                          

C DAVID set tolerances for each variable separately

      DO 602 I=1,50
c      TOLR=0.1
      TOLR=5.5
c      TOLR=2.5
      IF(XEQUIV(L(I)).EQ.0.0D0) GOTO 982                                 

      Z=ABS(R(I)/XEQUIV(L(I)))                                          

      IF(Z.GT.TOLR.AND.ABS(R(I)).GT.TOLR) GOTO 65                       

      GOTO 602                                                           

 982  IF(ABS(R(I)).GT.(0.1D0*TOLR)) GOTO 65                             

 602  CONTINUE                                                          

      DO 603 I=51,100
c      TOLR=1.0
      TOLR=20.0
c      TOLR=10.0
      IF(XEQUIV(L(I)).EQ.0.0D0) GOTO 983                                 

      Z=ABS(R(I)/XEQUIV(L(I)))                                          

      IF(Z.GT.TOLR.AND.ABS(R(I)).GT.TOLR) GOTO 65                       

      GOTO 603                                                           

 983  IF(ABS(R(I)).GT.(0.1D0*TOLR)) GOTO 65                             

 603  CONTINUE                                                          

      DO 604 I=101,150
c      TOLR=0.1
      TOLR=2.0
c      TOLR=1.0
      IF(XEQUIV(L(I)).EQ.0.0D0) GOTO 984                                 

      Z=ABS(R(I)/XEQUIV(L(I)))                                          

      IF(Z.GT.TOLR.AND.ABS(R(I)).GT.TOLR) GOTO 65                       

      GOTO 604                                                           

 984  IF(ABS(R(I)).GT.(0.1D0*TOLR)) GOTO 65                             

 604  CONTINUE                                                          

      DO 605 I=151,200
c      TOLR=0.01
      TOLR=7.0
c      TOLR=4.0
      IF(XEQUIV(L(I)).EQ.0.0D0) GOTO 985                                 

      Z=ABS(R(I)/XEQUIV(L(I)))                                          

      IF(Z.GT.TOLR.AND.ABS(R(I)).GT.TOLR) GOTO 65                       

      GOTO 605                                                           

 985  IF(ABS(R(I)).GT.(0.1D0*TOLR)) GOTO 65                             

 605  CONTINUE                                                          

      DO 606 I=201,300
c      TOLR=1.0
      TOLR=30.0
c      TOLR=10.0
      IF(XEQUIV(L(I)).EQ.0.0D0) GOTO 986                                 

      Z=ABS(R(I)/XEQUIV(L(I)))                                          

      IF(Z.GT.TOLR.AND.ABS(R(I)).GT.TOLR) GOTO 65                       

      GOTO 606                                                           

 986  IF(ABS(R(I)).GT.(0.1D0*TOLR)) GOTO 65                             

 606  CONTINUE                                                          

      DO 607 I=301,400
c      TOLR=0.01
c      TOLR=0.5
      TOLR=5.0
c      TOLR=2.0
      IF(XEQUIV(L(I)).EQ.0.0D0) GOTO 987                                 

      Z=ABS(R(I)/XEQUIV(L(I)))                                          

      IF(Z.GT.TOLR.AND.ABS(R(I)).GT.TOLR) GOTO 65                       

      GOTO 607                                                           

 987  IF(ABS(R(I)).GT.(0.1D0*TOLR)) GOTO 65                             

 607  CONTINUE                                                          

c      DO 60 I=51,NARG                                                    

c      IF(XEQUIV(L(I)).EQ.0.0D0) GOTO 98                                 

c      Z=ABS(R(I)/XEQUIV(L(I)))                                          

c      IF(Z.GT.TOLR.AND.ABS(R(I)).GT.TOLR) GOTO 65                       

c      GOTO 60                                                           

c   98 IF(ABS(R(I)).GT.(0.1D0*TOLR)) GOTO 65                             

c   60 CONTINUE                                                          

C.....CALL PRINT TO PRINT THE FINAL SOLUTION VALUES.                    

      WRITE(6,600) ITR                                                  

  600 FORMAT(' RATIONAL EXPECTATIONS VARIABLES CONVERGED IN ',I6,       

     1' ITERATIONS')                                                    

   66 CALL PRINT                                                        


C********************roll_f changes*********************************

        write(*,*)'before qerror'
        write(*,*)'model shocks ',(err(i,mag),i=1,13)

C**************WRITING THE RESIDUALS********************************

c      errname = gen_errname(ILOOP)
c      open(unit=99,file=errname)
c      xname = gen_xname(ILOOP)
c      open(unit=98,file=xname)
c      IC=1
c      DO 8010 IV = 1,NDOG                                                 
c      WRITE(99,7001) IC,IV                                              
c      WRITE(99,7501) ( ER(IV,IY),IY=1,KAG )                            
c 8010 continue
c      DO 8020 I=1,LEXOG
c 8020 WRITE(98,7501) (X(I,J), J=1,KAG)
c 7001 FORMAT(2I2)                                                    
c 7501 FORMAT(4F30.12)                                                

      open(unit=35,file='lags_endog')
      do 3016 i=1,ndog
 3016    write(35,*)(e(i,j),j=1,mag)
      close(35)
      open(unit=35,file='lags_exog')
      do 3017 i=1,lexog
 3017    write(35,*)(x(i,j),j=1,mag)
      close(35)

      new_lag=nlag+1
      open(unit=35,file='nlag')
      write(35,*)new_lag
      close(35)


 1111 continue
      return
C*******************************************************************
                                                    



   65 IF(ITR.LT.MAXITR) GOTO 62                                         

      WRITE(6,601) MAXITR                                               

  601 FORMAT(' RATIONAL EXPECTATIONS VARIABLES FAILED TO CONVERGE IN ', 

     1I4,' ITERATIONS')                                                 

      GO TO 66                                                          

C.....DO JACOBI CORRECTION TO GET NEW VALUES FOR THE R.E. VARIABLES.    

   62 DO 63 I=1,NARG                                                    

      J=L(I)                                                            

   63 XEQUIV(J)=XEQUIV(J)+BSTEP*R(I)                                    

C.....DO ANOTHER R.E. ITERATION, OPTIONALLY PRINTING CURRENT VALUES     

C.....OF R.E. VARIABLES AND RESIDUALS.                                  

      IF(ABS(P).NE.1.0D0) GOTO 100                                      

      WRITE(6,73)                                                       

      DO 71 I=1,NARG,4                                                  

      DO 72 J=1,4                                                       

      IT=(L(I+J-1)-1)/600                                               

      IV=L(I+J-1)-IT*600                                                

      IT=IT+1                                                           

      AJX(J)=IXOG(IV)                                                   

   72 NJX(J+4)=NSTART-MAG+IT                                            

      IE=MIN0(4,NARG-I+1)                                               

      WRITE(6,74) (AJX(J),NJX(J+4),R(I+J-1),XEQUIV(L(I+J-1)),J=1,IE)    

   71 CONTINUE                                                          

   73 FORMAT(4(4X,'VARIABLE',3X,'RESIDUAL NEW VALUE'))                  

   74 FORMAT(4(1X,A8,'(',I4,')',2F9.4))                                 


c      call print
      GOTO 100                                                          

C=======================================================================

C                                                                       

C     SECTION 2 ENDS.                                                   

C                                                                       

C=======================================================================

C                                                                       

C=======================================================================

C                                                                       

C     UNTESTED POWELL SOLUTION SECTION.                                 

C                                                                       

C=======================================================================

   50 DO 51 I=1,NARG                                                    

   51 DIR(I)=1.0D0                                                      

      ICTRL=0                                                           

      IF(IOPTC.GT.0) GOTO 322                                           

      CALL POW

      GOTO 323                                                          

  322 CALL POW

  323 CONTINUE                                                          

      GO TO 66                                                          

c10000 continue
C=======================================================================

C                                                                       

C     SECTION ENDS.                                                     

C                                                                       

C=======================================================================


  101 FORMAT(10A8)                                                      

  201 FORMAT(3(2X,A8,F15.4,10X)/)                                       

  204 FORMAT(1H1)                                                       

      END   

      SUBROUTINE FUNCTO                                                 

C=======================================================================

C                                                                       

C     OLD GAUSS-SEIDEL SOLUTION ROUTINE, WITH NO BLOC ITERATION.        

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,I,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR   

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      ITR=ITR+1                                                         

      IF(ITR.GT.0.AND.ABS(P).EQ.1.0D0) WRITE(6,320) ITR                 

      IDATE  =  NSTART                                                  

      T      =  FLOAT(IDATE-1900)                                       

      DO 3 I=MAG,KAG                                                    

      K=1                                                               

  100 DO 30 J=1,NDOG                                                    

   30 F(J)=0.0D0                                                        

      CALL EQN                                                          

      IF(IOPT.EQ.0) GOTO 55                                             

      DO 51 J=1,NDOG                                                    

      EOLD=E(J,I)                                                       

      F(J)=F(J)-EOLD                                                    

      E(J,I)=E(J,I)+B*(F(J))                                            

      SRES=F(J)/EOLD                                                    

      IF(P.LT.0.0D0) WRITE(6,300) EOLD,E(J,I),SRES,J                    

   51 CONTINUE                                                          

      GO TO 50                                                          

   55 CONTINUE                                                          

      DO 60 J=1,NDOG                                                    

      EOLD=E(J,I)-B*F(J)                                                

      RES=0.0D0                                                         

      IF(EOLD.EQ.0.0D0) GO TO 99                                        

      RES=F(J)/EOLD                                                     

   99 IF (P.LT.0.0D0) WRITE(6,300) EOLD,E(J,I),RES,J                    

   60 CONTINUE                                                          

   50 CONTINUE                                                          

      DO 61 J=1,NDOG                                                    

      IF(E(J,I).EQ.0.0D0) GO TO 98                                      

      Z=ABS(F(J)/E(J,I))                                                

      IF(Z.GE.TOL .AND. ABS(F(J)).GE.0.01D0) GO TO 62                   

      GO TO 61                                                          

   98 IF(ABS(F(J)).GE.0.001D0) GO TO 62                                 

   61 CONTINUE                                                          

      GO TO 63                                                          

   62 K=K+1                                                             

      IF(K.LE.ITMX) GOTO 100                                            

   70 WRITE(6,301) IDATE,K                                              

   63 IF(ABS(P).EQ.1.0D0) WRITE(6,310) IDATE,K                          

      IF(I.EQ.KAG) GO TO 3                                              

      IDATE  =  IDATE + 1                                               

      T      =  FLOAT(IDATE-1900)                                       

      IF(ITR.GT.1) GOTO 3                                               

C     DO 4 J=1,NDOG                                                     

C   4 E(J,I+1)=E(J,I)                                                   

    3 CONTINUE                                                          

      DO 33 J=1,NDOG                                                    

   33 F(J)=0.0D0                                                        

      RETURN                                                            

  300 FORMAT(1H ,3F15.4,I4)                                             

  310 FORMAT(10X,'PERIOD ',I4,' MODEL CONVERGED IN ',I4,' ITERATIONS')  

  301 FORMAT(10X,'PERIOD ',I4,' MODEL FAILED TO CONVERGE IN ',I4,' ITERA

     1TIONS')                                                           

  320 FORMAT(' RATIONAL EXPECTATIONS ITERATION ',I4)                    

      END                                                               

      SUBROUTINE CONT(IDY)                                              

C=======================================================================

C                                                                       

C     SUBROUTINE CONT    IS USED TO SELECT THE REQUIRED SOLUTION        

C     ***************    ROUTINE, ACCORDING TO THE VALUE OF IDY,        

C                        WHICH IS IDYN1 ( CONTROL CARD 1 ).             

C                                                                       

C                        IDY = 0 : OLD GAUSS-SEIDEL ROUTINE (NOT USED). 

C                        IDY = 1 : NEW GAUSS-SEIDEL ROUTINE.            

C                        IDY = 2 : POWELL ROUTINE (NOT TESTED).         

C                        IDY = 3 : POWELL-HYBRID (MINPAC).              

C                        IDY = 4 : LEVENBERG-MARQUARDT (MINPAC).        

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      IF(IDY.NE.0) GOTO (1,2,3,4),IDY                                   

      CALL FUNCTO                                                       

      RETURN                                                            

    1 CALL FUNCT                                                        

      RETURN                                                            

    2 CALL FUNCTP                                                       

      RETURN                                                            

    3 CALL FUNCTH                                                       

      RETURN                                                            

    4 CALL FNCTLM                                                       

      RETURN                                                            

      END                                                               

      SUBROUTINE FUNCT                                                  

C=======================================================================

C                                                                       

C     SUBROUTINE FUNCT    IS THE NEW GAUSS-SEIDEL SOLUTION ROUTINE      

C     ****************    FOR THE WORLD MODEL. THE TOLERANCES MAY       

C                         NEED TIGHTENING (SEPTEMBER 1987).             

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION ( A-H,O-Z)                              

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,I,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR   

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/WOREQN/IBLOC,NCONV,NBLOC                                   

      COMMON/KGROUP/KK(41)/JGROUP/JJ(41)                                

      PARAMETER ( MXWITR = 20,                                          

     +            NUNITY =  1,                                          

     +            NZERO  =  0,                                          

     +            P01    = 0.01,                                        

     +            P001   = 0.001,                                       

     +            P0001  = 0.0001,                                      

     +            P00001 = 0.00001,                                     

     +            SMALL  = 0.000001,                                    

     +            UNITY  = 1.0      )                                   


                                                                        

C  INCREMENT THE RATIONAL EXPECTATIONS COUNTER                          

C  AND INTITIALISE THE DATE AND THE TIME TREND.                         

      ITR    =  ITR + NUNITY                                            

      IDATE  =  NSTART                                                  

      T      =  FLOAT(IDATE-1900)                                       

                                                                        

C----------------------------------------------------------------------C

C                 FORECAST PERIOD LOOP BEGINS.                         C

C----------------------------------------------------------------------C

                                                                        

C  INITIALISE FOR THE BLOC LOOP.                                        

                                                                        

      DO 20000 IPER = MAG,KAG                                           

      I      =  IPER                                                    

      IKW    =  NZERO                                                   

      NCONV  =  NZERO                                                   

 1000 CONTINUE                                                          

      IKW    =  IKW + NUNITY                                            

      NBLOC1 =  NBLOC - NUNITY                                          

                                                                        

C----------------------------------------------------------------------C

C                 BLOC LOOP BEGINS.                                    C

C----------------------------------------------------------------------C

                                                                        

 2000 DO 10000 IC = NUNITY,NBLOC1                                       

      K      =  NZERO                                                   

      IBLOC  =  IC                                                      

      KKIB   =  KK(IC) + NUNITY                                         

      KKIB1  =  KK(IC+NUNITY)                                           

 3000 CALL EQN                                                          

                                                                        

C----------------------------------------------------------------------C

C                 EQUATION LOOP BEGINS.                                C

C----------------------------------------------------------------------C

                                                                        

      DO 6000 J = KKIB,KKIB1                                            

                                                                        

C  DO TESTS FOR CONVERGENCE. IF CONVERGED DO NEXT EQUATION.             

C  OTHERWISE : 1) IF MAXIMUM NUMBER OF ITERATIONS IS NOT EXCEEDED       

C                 THEN RE-SOLVE THE BLOC OF EQUATIONS.                  

C              2) IF MAXIMUM NUMBER OF ITERATIONS IS EXCCEDED           

C                 THEN SOLVE THE NEXT BLOC.                             

                                                                        

                                                                        

      YABS  =  ABS( E(J,IPER) )                                         

      FABS  =  ABS( F(J) )                                              

      RATIO =  UNITY                                                    

                                                                        

      IF  ( YABS .GT. P0001 ) RATIO = FABS/YABS                         

                                                                        

      IF ((RATIO.LT.P0001) .OR. (FABS.LT.P0001.AND.YABS.LT.P0001)) THEN 

                                                                        

          GO TO 6000                                                    

                                                                        

      ELSE                                                              

                                                                        

          IF ( K .LT. ITMX ) THEN                                       

          K  =  K + NUNITY                                              

          GO TO 3000                                                    

          ELSE                                                          

          WRITE(6,4000) IDATE, IC, K                                    

 4000 FORMAT(1X,'**** ',I4,' : GROUP ',I2,' FAILED TO CONVERGE IN ',I4, 

     +          ' ITERATIONS ****')                                     

          GO TO 10000                                                   

          END IF                                                        

                                                                        

      END IF                                                            

                                                                        

 6000 CONTINUE                                                          

                                                                        

C----------------------------------------------------------------------C

C                 EQUATION LOOP ENDED.                                 C

C----------------------------------------------------------------------C

                                                                        

10000 CONTINUE                                                          

                                                                        

C----------------------------------------------------------------------C

C                 BLOC LOOP ENDED.                                     C

C----------------------------------------------------------------------C

                                                                        

C  NOW DO THE COMMON BLOC.                                              

                                                                        

11000 IBLOC  =  NBLOC                                                   

      CALL EQN                                                          

                                                                        

C  TEST FOR CONSISTENCY.                                                

C  IF NOT CONSISTENT AND MAXIMUM NUMBER OF ITERATIONS NOT EXCEEDED      

C  THEN GO THROUGH BLOCS OF EQUATIONS AGAIN.                            

C  IF NOT CONSISTENT AND MAXIMUM NUMBER OF ITERATIONS IS EXCEEDED       

C  THEN WRITE MESSAGE AND CONTINUE WITH NEXT PERIOD.                    

                                                                        

      IF  ( NCONV .EQ. NZERO .AND. IKW .LT. MXWITR ) GO TO 1000         

      IF  ( NCONV .EQ. NZERO .AND. IKW .GE. MXWITR ) THEN               

      WRITE(6,12000) IDATE, IKW                                         

12000 FORMAT(1X,'**** ',I4,' : CONSISTENCY NOT REACHED IN ',I3,         

     +          ' ITERATIONS ****')                                     

      END IF                                                            

                                                                        

C  IF LAST PERIOD GO TO END OF LOOP FOR FORECAST PERIOD.                

C  OTHERWISE INCREMENT THE DATE AND THE TIME TREND.                     

                                                                        

      IF  ( IPER .EQ. KAG )  GO TO 20000                                

      IDATE  =  IDATE + NUNITY                                          

      T      =  FLOAT(IDATE-1900)                                       

                                                                        

C  IF NOT FIRST RATIONAL EXPECTATIONS ITERATION                         

C  THEN GO TO END OF LOOP FOR FORECAST PERIOD.                          

C  OTHERWISE, INCREMENT THE TIME TREND                                  

C  AND SET INITIAL VALUES FOR THE NEXT PERIOD.                          

                                                                        

C     IF  ( ITR .GT. NUNITY ) GO TO 20000                               

C     DO 14000 J = NUNITY,NDOG                                          

C     E(J,IPER+NUNITY)  =  E(J,IPER)                                    

14000 CONTINUE                                                          

                                                                        

20000 CONTINUE                                                          

                                                                        

C----------------------------------------------------------------------C

C                 FORECAST PERIOD LOOP ENDED.                          C

C----------------------------------------------------------------------C

                                                                        

      RETURN                                                            

      END                                                               

      SUBROUTINE FUNCTP                                                 

C=======================================================================

C                                                                       

C     SUBROUTINE FUNCTP    CONTROLS THE POWELL SOLUTION PROCEDURE.      

C     *****************    IT IS UNTESTED AND UNUSED.                   

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,I,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR   

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/POWELL/R(200000),DIR(200000),L(200000),ICTRL                     

      COMMON/WOREQN/IBLOC,NCONV,NBLOC                                   

      COMMON/KGROUP/KK(41)/JGROUP/JJ(41)                                

      EXTERNAL FCNP                                                     

C                                                                       

C  COMMON WOREQN IS FOR USE WITH EQN WHERE CONSISTENCY IS REQUIRED      

C  ACROSS EQUATIONS EG. TRADE EQUATIONS IN WORLD MODEL.                 

C  ALSO DEALS WITH BLOCS OF EQUATIONS .                                 

C  NCONV IS SWITCH WHICH IS 1 WHEN EQNS. CONSISTENT 0 OTHERWISE         

C  FOR NORMAL MODEL SOLN'S SET  NCONV TO 1 IN EQN                       

      ITR=ITR+1                                                         

      IF (ITR.GT.0 .AND. ABS(P).EQ.1.0D0) WRITE(6,320) ITR              

      IDATE  =  NSTART                                                  

      T      =  FLOAT(IDATE-1900)                                       

C MXWITR IS MAX  ITERATIONS TO GET CONSISTENCY                          

      MXWITR=20                                                         

      IC=1                                                              

      IBLOC=IC                                                          

      KKIB=1                                                            

      KKIB1=NDOG                                                        

      ND=NDOG                                                           

      DO 3 I=MAG,KAG                                                    

      NCONV=0                                                           

      IKW=0                                                             

  200 CONTINUE                                                          

      IKW=IKW+1                                                         

      K=1                                                               

      DO 250 IC=1,NBLOC                                                 

      IF(NBLOC.EQ.1) GOTO 100                                           

      IBLOC=IC                                                          

      KKIB=KK(IC)+1                                                     

      KKIB1=KK(IC+1)                                                    

      ND=KK(IC+1)-KK(IC)                                                

  100 CALL EQN                                                          

      IF(NBLOC.GT.1.AND.IC.EQ.NBLOC) GOTO 53                            

      DO 51 J=1,ND                                                      

      DIR(J)=B*F(KK(IC)+J)                                              

      F(KK(IC)+J)=0.0D0                                                 

      R(J)=E(KK(IC)+J,I)                                                

   51 CONTINUE                                                          

      CALL POW

      DO 52 J=1,ND                                                      

   52 E(KK(IC)+J,I)=R(J)                                                

   53 IF (P.GE.0.0D0) GOTO 250                                          

      DO 60 J=KKIB,KKIB1                                                

      WRITE(6,300) E(J,I),J,IBLOC                                       

   60 CONTINUE                                                          

  250 CONTINUE                                                          

      IF(NCONV.EQ.1) GOTO 251                                           

      IF(IKW.LE.MXWITR) GOTO 200                                        

      WRITE(6,302)IDATE,IKW                                             

  251 IF(ABS(P).EQ.1.0D0) WRITE(6,303)IDATE,IKW                         

      IF(I.EQ.KAG) GO TO 3                                              

      IDATE  =  IDATE + 1                                               

      T      =  FLOAT(IDATE-1900)                                       

      IF(ITR.GT.1) GOTO 3                                               

      DO 4 J=1,NDOG                                                     

    4 E(J,I+1)=E(J,I)                                                   

    3 CONTINUE                                                          

      DO 33 J=1,NDOG                                                    

   33 F(J)=0.0D0                                                        

      RETURN                                                            

  300 FORMAT(1H ,F15.4,2I4)                                             

  302 FORMAT(10X,'PERIOD ',I4,' CONSITENCY NOT MET IN',I4,' ITERATIONS')

  303 FORMAT(10X,'PERIOD ',I4,' CONSITENCY     MET IN',I4,' ITERATIONS')

  320 FORMAT(' RATIONAL EXPECTATIONS ITERATION ',I4)                    

      END                                                               

      SUBROUTINE FUNCTH                                                 

C=======================================================================

C                                                                       

C     SUBROUTINE FUNCTH    IS USED TO CONTROL THE POWELL-HYBRID         

C     *****************    SOLUTION PROCEDURE.  S/R FUNCTH HAS THREE    

C                          SECTIONS :                                   

C                                                                       

C                          SECTION 1 IS AN INITIALISATION SECTION.      

C                                                                       

C                          SECTION 2 HAS THREE LOOPS :                  

C                                                                       

C                              THE OUTER LOOP IS A TIME LOOP FOR THE    

C                              FORECAST PERIOD ( MAG TO KAG ). IN THIS  

C                              LOOP THE MODEL IS SOLVED FOR EACH YEAR   

C                              IN TURN.                                 

C                              IN THE INNERMOST LOOP THE MODEL IS SOLVED

C                              FOR EACH COUNTRY (BLOC) IN TURN, FOR THE 

C                              YEAR DEFINED BY THE OUTER LOOP.          

C                              AROUND THE INNERMOST LOOP IS A LOOP      

C                              WHICH IS USED WHEN CONSISTENCY HAS NOT   

C                              BEEN REACHED IN THE COMMON BLOC (WORLD   

C                              VARIABLES).                              

C                                                                       

C                          SECTION 3 SIMPLY RESETS THE ARRAY F TO ZERO. 

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,I,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR   

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/HYBLM/XH(300),FVEC(300),WA(3130)                           

      COMMON/WOREQN/IBLOC,NCONV,NBLOC                                   

      COMMON/KGROUP/KK(41)/JGROUP/JJ(41)                                

      EXTERNAL FCNH                                                     

      NWRITE=6                                                          

      LWA=3130                                                          

C                                                                       

C  COMMON WOREQN IS FOR USE WITH EQN WHERE CONSISTENCY IS REQUIRED      

C  ACROSS EQUATIONS EG. TRADE EQUATIONS IN WORLD MODEL.                 

C  ALSO DEALS WITH BLOCS OF EQUATIONS .                                 

C  NCONV IS SWITCH WHICH IS 1 WHEN EQNS. CONSISTENT 0 OTHERWISE         

C  FOR NORMAL MODEL SOLN'S SET  NCONV TO 1 IN EQN                       

C                                                                       

C  THE VAIABLES JH NH INFO LWA NWRITE  TOLH  FNORM                      

C  XH FVEC WA ARE FOR HYBRD1                                            

C                                                                       

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 1.    INITIALISATION.                                     

C                                                                       

C=======================================================================

C                                                                       

C.....INCREMENT THE COUNTER FOR THE NUMBER OF R.E. ITERATIONS.          

      ITR=ITR+1                                                         

      IF(ITR.GT.0.AND.ABS(P).EQ.1.0D0) WRITE(6,320) ITR                 

C.....INITIALISE THE DATE AND TIME TREND.                               

      IDATE  =  NSTART                                                  

      T      =  FLOAT(IDATE-1900)                                       

C.....SET THE LIMIT FOR THE NUMBER OF ITERATIONS FOR CONSISTENCY        

C.....IN THE COMMON BLOC.                                               

      MXWITR=0                                                      

C.....INITIALISE THE BLOC (COUNTRY) NUMBER.                             

      IC=1                                                              

      IBLOC=IC                                                          

C.....SET VALUES FOR THE NUMBER OF ENDOGENOUS VARIABLES.                

      KKIB=1                                                            

      KKIB1=NDOG                                                        

      ND=NDOG                                                           

C=======================================================================

C                                                                       

C     SECTION 1 ENDS.                                                   

C                                                                       

C=======================================================================

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 2    HAS THREE LOOPS :                                    

C                                                                       

C                  OUTER LOOP (MAG TO KAG) WITHIN WHICH THE MODEL IS    

C                  SOLVED FOR EACH YEAR IN TURN, AND AFTER WHICH THE    

C                  R.E. VARIABLES ARE TESTED FOR CONVERGENCE (IN CALCFX)

C                                                                       

C                  MIDDLE LOOP (IKW = 1 TO (AT MOST) MXWITR, WITHIN     

C                  WHICH CONSISTENCY IN THE COMMON (WORLD) BLOC         

C                  IS SOUGHT.                                           

C                                                                       

C                  INNER LOOP (IC = 1 TO NBLOC) WITHIN WHICH A SOLUTION 

C                  IS FOUND FOR EACH COUNTRY (BLOC) IN TURN.            

C                                                                       

C=======================================================================

C                                                                       

C-------------------------  OUTER LOOP BEGINS  -------------------------

      DO 3 I=MAG,KAG                                                    

C.....INITIALISE INDICATOR AND COUNTER FOR CONSISTENCY                  

      IF ( NBLOC .EQ. 1 ) THEN

      NCONV = 1

      ELSE 

      NCONV=0                                                           

      END IF

      IKW=0                                                             

C-------------------------  MIDDLE LOOP BEGINS  ------------------------

  200 CONTINUE                                                          

C.....INCREMENT THE COUNTER FOR CONSISTENCY ITERATIONS (IKW).           

      IKW=IKW+1                                                         

      K=1                                                               

C-------------------------  INNER LOOP BEGINS  -------------------------

      DO 250 IC=1,NBLOC                                                 

      IF(NBLOC.EQ.1) GOTO 100                                           

      IBLOC=IC                                                          

      KKIB=KK(IC)+1                                                     

      KKIB1=KK(IC+1)                                                    

      ND=KK(IC+1)-KK(IC)                                                

  100 NH=ND                                                             

      IF(NBLOC.GT.1.AND.IC.EQ.NBLOC) GOTO 53                            

C.....INITIALISE ARRAY F (LHS OF EQUATIONS) TO ZERO.                    

      DO 30 J=KKIB,KKIB1                                                

   30 F(J)=0.0D0                                                        

C.....PUT CURRENT VALUES OF ENDOGENOUS VARIABLES FOR THE IC-TH BLOC     

C..... INTO THE ARRAY XH.                                               

      DO 51 JH=1,ND                                                     

   51 XH(JH)=E(KK(IC)+JH,I)                                             

      TOLH=0.01D0*TOL                                                   

C.....CALL HYBRD1 TO SOLVE THE SYSTEM OF EQUATIONS.                     

      CALL HYBRD1(FCNH,NH,XH,FVEC,TOLH,INFO,WA,LWA)                     

      FNORM=ENORM(NH,FVEC)                                              

      IF(ABS(P).EQ.1.0D00) WRITE(6,301) IDATE,FNORM,INFO,IBLOC          

C.....PUT THE NEW SOLVED VALUES INTO ARRAY E.                           

      DO 52 JH=1,ND                                                     

   52 E(KK(IC)+JH,I)=XH(JH)                                             

      GOTO 54                                                           

C.....THIS CALL IS FOR THE COMMON (WORLD) BLOC.                         

   53 CALL EQN                                                          

C   54 write(6,9999) i, ibloc, kkib, kkib1, ( e(j,i), j = kkib,kkib1 )

C 9999 format(4i4,/,3(6f12.4/))

C      IF (P.GE.0.0D0) GOTO 250                                          

   54 IF (P.GE.0.0D0) GOTO 250                                          

      DO 60 J=KKIB,KKIB1                                                

      WRITE(6,300) E(J,I),J,IBLOC                                       

   60 CONTINUE



  250 CONTINUE                                                          

C-------------------------  INNER LOOP ENDED  --------------------------

C.....IF CONSISTENCY ATTAINED OR IF MAXIMUM NUMBER OF ITERATIONS FOR    

C.....CONSISTENCY HAS BEEN REACHED, LEAVE THE LOOP. OTHERWISE REPEAT.   

      IF(NCONV.EQ.1) GOTO 251                                           

      IF(IKW.LE.MXWITR) GOTO 200                                        

      WRITE(6,302)IDATE,IKW                                             

  251 IF(ABS(P).EQ.1.0D0) WRITE(6,303)IDATE,IKW                         

C-------------------------  MIDDLE LOOP ENDED  -------------------------

C.....IF NOT FINAL YEAR OF FORECAST THEN INCREMENT DATE AND TIME TREND. 

      IF(I.EQ.KAG) GO TO 3                                              

      IDATE  =  IDATE + 1                                               

      T      =  FLOAT(IDATE-1900)                                       

C.....IF IN FIRST R.E. ITERATION THEN INITIALISE ENDOGENOUS VARIABLES   

C.....IN NEXT YEAR.                                                     

C     IF(ITR.GT.1) GOTO 3                                               

C     DO 4 J=1,NDOG                                                     

C   4 E(J,I+1)=E(J,I)                                                   

    3 CONTINUE                                                          

C-------------------------  OUTER LOOP ENDED  --------------------------

C=======================================================================

C                                                                       

C     SECTION 2 ENDS.                                                   

C                                                                       

C=======================================================================

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 3    SET F TO ZERO.                                       

C                                                                       

C=======================================================================

      DO 33 J=1,NDOG                                                    

   33 F(J)=0.0D0                                                        

C=======================================================================

C                                                                       

C     SECTION 3 ENDS.                                                   

C                                                                       

C=======================================================================

      RETURN                                                            

  300 FORMAT(1H ,F15.4,2I4)                                             

  301 FORMAT(10X,'PERIOD ',I4,'FINAL L2 NORM OF RESIDUALS ',E15.7,      

     1           ' EXIT PARAMETER ',I4,' IBLOC ',I4)                    

  302 FORMAT(10X,'PERIOD ',I4,' CONSITENCY NOT MET IN',I4,' ITERATIONS')

  303 FORMAT(10X,'PERIOD ',I4,' CONSITENCY     MET IN',I4,' ITERATIONS')

  320 FORMAT(' RATIONAL EXPECTATIONS ITERATION ',I4)                    

      END                                                               

      SUBROUTINE FNCTLM                                                 

C=======================================================================

C                                                                       

C     SUBROUTINE FNCTLM    IS USED TO CONTROL THE LEVENBERG-MARQUARDT   

C     *****************    SOLUTION ROUTINE (MINPAC). IT IS UNTESTED.   

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,I,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR   

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/HYBLM/XH(300),FVEC(300),WA(3130)                           

      COMMON/WOREQN/IBLOC,NCONV,NBLOC                                   

      COMMON/KGROUP/KK(41)/JGROUP/JJ(41)                                

      EXTERNAL FCNLM                                                    

      DIMENSION IWA(300)                                                

      NWRITE=6                                                          

      LWA=3130                                                          

C                                                                       

C  COMMON WOREQN IS FOR USE WITH EQN WHERE CONSISTENCY IS REQUIRED      

C  ACROSS EQUATIONS EG. TRADE EQUATIONS IN WORLD MODEL.                 

C  ALSO DEALS WITH BLOCS OF EQUATIONS .                                 

C  NCONV IS SWITCH WHICH IS 1 WHEN EQNS. CONSISTENT 0 OTHERWISE         

C  FOR NORMAL MODEL SOLN'S SET  NCONV TO 1 IN EQN                       

C                                                                       

C  THE VAIABLES JH NH INFO LWA NWRITE  TOLH  FNORM                      

C  XH FVEC WA ARE FOR HYBRD1 . THIS ROUTINE ADAPTED FOR                 

C  LMDIF SO EXTRA PARAMETER MH AND WORKING    IWA                       

C                                                                       

      ITR=ITR+1                                                         

      IF(ITR.GT.0.AND.ABS(P).EQ.1.0D0) WRITE(6,320) ITR                 

      IDATE=NSTART                                                      

C MXWITR IS MAX  ITERATIONS TO GET CONSISTENCY                          

      MXWITR=20                                                         

      IC=1                                                              

      IBLOC=IC                                                          

      KKIB=1                                                            

      KKIB1=NDOG                                                        

      ND=NDOG                                                           

      DO 3 I=MAG,KAG                                                    

      NCONV=0                                                           

      IKW=0                                                             

  200 CONTINUE                                                          

      IKW=IKW+1                                                         

      K=1                                                               

      DO 250 IC=1,NBLOC                                                 

      IF(NBLOC.EQ.1) GOTO 100                                           

      IBLOC=IC                                                          

      KKIB=KK(IC)+1                                                     

      KKIB1=KK(IC+1)                                                    

      ND=KK(IC+1)-KK(IC)                                                

  100 NH=ND                                                             

      MH=NH                                                             

      IF(NBLOC.GT.1.AND.IC.EQ.NBLOC) GOTO 53                            

      DO 30 J=KKIB,KKIB1                                                

   30 F(J)=0.0D0                                                        

      DO 51 JH=1,ND                                                     

   51 XH(JH)=E(KK(IC)+JH,I)                                             

      TOLH=0.01D0*TOL                                                   

      CALL LMDIF1(FCNLM,MH,NH,XH,FVEC,TOLH,INFO,IWA,WA,LWA)             

      FNORM=ENORM(NH,FVEC)                                              

      IF(ABS(P).EQ.1.0D0) WRITE(6,301) IDATE,FNORM,INFO,IBLOC           

      DO 52 JH=1,ND                                                     

   52 E(KK(IC)+JH,I)=XH(JH)                                             

      GOTO 54                                                           

   53 CALL EQN                                                          

   54 IF (P.GE.0.0D0) GOTO 250                                          

      DO 60 J=KKIB,KKIB1                                                

      WRITE(6,300) E(J,I),J,IBLOC                                       

   60 CONTINUE                                                          

  250 CONTINUE                                                          

      IF(NCONV.EQ.1) GOTO 251                                           

      IF(IKW.LE.MXWITR) GOTO 200                                        

      WRITE(6,302)IDATE,IKW                                             

  251 IF(ABS(P).EQ.1.0D0) WRITE(6,303)IDATE,IKW                         

      IF(I.EQ.KAG) GO TO 3                                              

      IDATE=IDATE+1                                                     

      IF(ITR.GT.1) GOTO 3                                               

      DO 4 J=1,NDOG                                                     

    4 E(J,I+1)=E(J,I)                                                   

      T=T+1                                                             

    3 CONTINUE                                                          

      DO 33 J=1,NDOG                                                    

   33 F(J)=0.0D0                                                        

      RETURN                                                            

  300 FORMAT(1H ,F15.4,2I4)                                             

  301 FORMAT(10X,'PERIOD ',I4,'FINAL L2 NORM OF RESIDUALS ',E15.7,      

     1           ' EXIT PARAMETER ',I4,' IBLOC ',I4)                    

  302 FORMAT(10X,'PERIOD ',I4,' CONSITENCY NOT MET IN',I4,' ITERATIONS')

  303 FORMAT(10X,'PERIOD ',I4,' CONSITENCY     MET IN',I4,' ITERATIONS')

  320 FORMAT(' RATIONAL EXPECTATIONS ITERATION ',I4)                    

      END                                                               

      SUBROUTINE FCNH(NH,XH,FVEC,IFLAG)                                 

C=======================================================================

C                                                                       

C     SUBROUTINE FCNH    IS CALLED BY THE MINPAC POWELL HYBRID SOLVER   

C     ***************    TO CALCULATE THE LHS VALUES OF THE FUNCTIONS   

C                        IN THE COUNTRY MODEL WITH A NEW SET OF VALUES  

C                        FOR THE ENDOGENOUS VARIABLES, AND TO CALCULATE 

C                        THE RESIDUALS.                                 

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,I,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR   

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/WOREQN/IBLOC,NCONV,NBLOC                                   

      COMMON/KGROUP/KK(41)/JGROUP/JJ(41)                                

      DIMENSION XH(NH),FVEC(NH)                                         

C.....PUT THE NEW VALUES FROM THE SOLUTION INTO ARRAY E.                

      IC=IBLOC                                                          

      DO 10 JH=1,NH                                                     

   10 E(KK(IC)+JH,I)=XH(JH)                                             

C.....CALL EQN TO EVALUATE THE FUNCTIONS WITH THE NEW VALUES OF E.      

C.....ENDOGENOUS VARIABLES ON THE RHS.                                  

      CALL EQN                                                          

      IF(IOPT.EQ.0) GOTO 15                                             

C.....CALCULATE THE RESIDUALS.                                          

      DO 11 JH=1,NH                                                     

   11 F(KK(IC)+JH)=F(KK(IC)+JH)-E(KK(IC)+JH,I)                          

   15 CONTINUE                                                          

C.....PUT THE VALUES OF THE RESIDUALS IN FVEC                           

C.....FOR RETURN TO THE CALLING ROUTINE.                                

      DO 20 JH=1,NH                                                     

   20 FVEC(JH)=F(KK(IC)+JH)                                             

      RETURN                                                            

      END                                                               

      SUBROUTINE FCNLM(MH,NH,XH,FVEC,IFLAG)                             

C=======================================================================

C                                                                       

C     SUBROUTINE FCNLM   IS CALLED BY THE MINPAC LEVENBERG-MARQUARDT    

C     ****************   SOLVER TO CALCULATE LHS VALUES OF THE FUNCTIONS

C                        IN THE COUNTRY MODEL WITH A NEW SET OF VALUES  

C                        FOR THE ENDOGENOUS VARIABLES, AND TO CALCULATE 

C                        THE RESIDUALS.                                 

C                                                                       

C                        THIS SUBROUTINE IS UNTESTED.                   

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,I,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR   

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/WOREQN/IBLOC,NCONV,NBLOC                                   

      COMMON/KGROUP/KK(41)/JGROUP/JJ(41)                                

      INTEGER MH,NH,IFLAG                                               

      REAL XH(NH),FVEC(MH)                                              

      IC=IBLOC                                                          

      DO 10 JH=1,NH                                                     

   10 E(KK(IC)+JH,I)=XH(JH)                                             

      CALL EQN                                                          

      IF(IOPT.EQ.0) GOTO 15                                             

      DO 11 JH=1,MH                                                     

   11 F(KK(IC)+JH)=F(KK(IC)+JH)-E(KK(IC)+JH,I)                          

   15 CONTINUE                                                          

      DO 20 JH=1,MH                                                     

   20 FVEC(JH)=F(KK(IC)+JH)                                             

      RETURN                                                            

      END                                                               

      SUBROUTINE FCNP(NH,FX)                                            

C=======================================================================

C                                                                       

C     SUBROUTINE FCNP    IS CALLED BY SUBROUTINE POW (POWELL SOLUTION)  

C     ***************    TO CALULATE LHS VALUES OF THE FUNCTIONS        

C                        IN THE COUNTRY MODEL WITH A NEW SET OF VALUES  

C                        FOR THE ENDOGENOUS VARIABLES, AND TO CALCULATE 

C                        THE RESIDUALS.                                 

C                                                                       

C                        THIS SUBROUTINE IS UNTESTED.                   

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,I,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR   

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/POWELL/R(200000),DIR(200000),L(200000),ICTRL                     

      COMMON/WOREQN/IBLOC,NCONV,NBLOC                                   

      COMMON/KGROUP/KK(41)/JGROUP/JJ(41)                                

      IC=IBLOC                                                          

      FX=0.0D0                                                          

      DO 10 JH=1,NH                                                     

   10 E(KK(IC)+JH,I)=R(JH)                                              

      CALL EQN                                                          

      IF(IOPT.EQ.0) GOTO 15                                             

      DO 11 JH=1,NH                                                     

   11 F(KK(IC)+JH)=F(KK(IC)+JH)-E(KK(IC)+JH,I)                          

   15 CONTINUE                                                          

      DO 20 JH=1,NH                                                     

   20 FX=FX+F(KK(IC)+JH)*F(KK(IC)+JH)                                   

      RETURN                                                            

      END                                                               

      SUBROUTINE WORJAC(ILC)                                            

C=======================================================================

C                                                                       

C     SUBROUTINE WORJAC    TESTS FOR CONVERGENCE IN THE COMMON BLOC.    

C     *****************    WORJAC HAS THREE SECTIONS :                  

C                                                                       

C                          SECTION 1  APPLIES A JACOBI CORRECTION TO    

C                                     THE RHS VALUES OF THE ENDOGENOUS  

C                                     VARIABLES IN THE COMMON (WORLD)   

C                                     BLOC.                             

C                                                                       

C                          SECTION 2  TESTS FOR CONVERGENCE.            

C                                                                       

C                          SECTION 3  SETS THE FUNCTION VALUES EQUAL TO 

C                                     THE CORRECTED RHS VALUES OF THE   

C                                     ENDOGENOUS VARIABLES.             

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/V(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,XX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,IPER,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/KGROUP/KK(41)/JGROUP/JJ(41)                                

      COMMON/WOREQN/IBLOC,NCONV,NBLOC                                   

C                                                                       

C     NOTE      KNE : IS THE NUMBER OF EQUATIONS IN THE LAST BLOC OF    

C               EQUATIONS, I.E. THE ONE CONTAINING THE LINK VARIABLES.  

C               KIC : IS THE NUMBER OF EQUATIONS IN THE REST            

C               OF THE MODEL, I.E. KK(LAST GROUP).                      

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 1 : APPLY JACOBI CORRECTION TO RHS VARIABLES.             

C                                                                       

C=======================================================================

      I=IPER                                                            

      KNE=KK(ILC+1)-KK(ILC)                                             

      KIC=KK(ILC)                                                       

      DO 100 IE=1,KNE                                                   

      EOLD=E(KIC+IE,I)                                                  

C.....CALCULATE RESIDUAL.                                               

      V(KIC+IE)=V(KIC+IE)-E(KIC+IE,I)                                   

C.....APPLY JACOBI CORRECTION.                                          

      E(KIC+IE,I)=E(KIC+IE,I)+B*V(KIC+IE)                               

      ZR=0.0D0                                                          

      IF(EOLD.EQ.0.0D0) GOTO 90                                         

      ZR=V(KIC+IE)/EOLD                                                 

   90 IF(ABS(P).EQ.1.0D0)WRITE(6,300) EOLD,E(KIC+IE,I),ZR,(KIC+IE)      

  100 CONTINUE                                                          

C=======================================================================

C                                                                       

C     SECTION 1 ENDS.                                                   

C                                                                       

C=======================================================================

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 2 : TEST FOR CONVERGENCE.                                 

C                                                                       

C=======================================================================

      DO 105 IE=1,KNE  

      IF(E(KIC+IE,I).EQ.0.0D0) GOTO 200                                 

      ZR=ABS(V(KIC+IE)/E(KIC+IE,I))                                     

      IF(ZR.GT.TOL.AND.ABS(V(KIC+IE)).GT.TOL) GOTO 250                  

      GOTO 105                                                          

  200 IF(ABS(V(KIC+IE)).GT.(0.1D0*TOL)) GOTO 250                        

  105 CONTINUE                                                          

C.....IF NEXT INSTRUCTION IS EXECUTED CONVERGENCE IS IMPLIED.           

      NCONV=1                                                           

  250 CONTINUE                                                          

  300 FORMAT(1H ,3F15.4,I4)                                             

C=======================================================================

C                                                                       

C     SECTION 2 ENDS.                                                   

C                                                                       

C=======================================================================

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 3 : SETS THE LHS OF THE EQUATIONS IN THE COMMON (WORLD)   

C                 BLOC EQUAL TO THE UPDATED VALUES OF THE ENDOGENOUS    

C                 VARIABLES.                                            

C                                                                       

C=======================================================================

      DO 110 IE=1,KNE                                                   

  110 V(KIC+IE)=E(KIC+IE,I)                                             

C=======================================================================

C                                                                       

C     SECTION 3 ENDS.                                                   

C                                                                       

C=======================================================================

      RETURN                                                            

      END              

                                                  

      SUBROUTINE PRINT                                                  

C=======================================================================

C                                                                       

C     SUBROUTINE PRINT    IS CALLED WHEN THE MODEL HAS SOLVED OR WHEN   

C     ****************    THE MAXIMUM NUMBER OF RATIONAL EXPECTATIONS   

C                         ITERATIONS HAS BEEN REACHED.  PRINT HAS FOUR  

C                         SECTIONS.                                     

C                                                                       

C                         SECTION 1 : WRITES THE VALUES OF ENDOGENOUS   

C                                     AND EXOGENOUS VARIABLES FOR ALL   

C                                     PERIODS ( 1 TO KAG ) TO TAPE 12.  

C                                                                       

C                         SECTION 2 : WRITES THE VALUES OF RATIONAL     

C                                     EXPECTATIONS VARIABLES AND        

C                                     RESIDUALS TO TAPE 6.              

C                                                                       

C                         SECTION 3 : ON THE FIRST PASS FINAL VALUES OF 

C                                     ENDOGENOUS AND EXOGENOUS VARIABLES

C                                     (IN LEVELS) ARE WRITTEN TO TAPE 6.

C                                     THERE IS A SECOND PASS IF BR      

C                                     ( CARD 5 ) IS EQUAL TO 1.0 ;      

C                                     ON THE SECOND PASS THE VALUES OF  

C                                     THE ENDOGENOUS VARIABLES ARE      

C                                     DIFFERENCES FROM A BASE RUN.      

C                                                                       

C                         SECTION 4 : IF PR ( CARD 5 ) IS EQUAL TO 1.0  

C                                     THEN THE VALUES OF THE ENDOGENOUS 

C                                     VARIABLES FOR THE FORECAST PERIOD 

C                                     ARE WRITTEN TO TAPE 7. THEY CAN BE

C                                     USED IN A SUBSEQUENT RUN TO FORM  

C                                     DIFFERENCES, WHEN READ IN FROM    

C                                     TAPE 4.                           

C                                     IF BR ( CARD 5 ) IS EQUAL TO 1.0  

C                                     THEN SUBROUTINE BASE IS CALLED TO 

C                                     FORM DIFFERENCES FROM BASE VALUES 

C                                     FOR THE ENDOGENOUS VARIABLES,     

C                                     BR AND PR ARE SET TO ZERO, AND    

C                                     CONTROL RETURNS TO SECTION 3.     

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG, AJX(10)                                 

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      CHARACTER*10  TITLE1, TITLE2                                      

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,IPER,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/KGROUP/KK(41)                                              

      COMMON/POWELL/R(200000),DIR(200000),L(200000),ICTRL                     

      DIMENSION XEQUIV(120000)                                           

      DIMENSION  NJX(10)                                                

      EQUIVALENCE ( XEQUIV(1),X(1,1) )                                  

C=======================================================================

C                                                                       

C     SECTION 1 : WRITE ALL VALUES OF ENDOGENOUS AND EXOGENOUS VARIABLES

C                 TO TAPE 12.                                           

C                                                                       

C=======================================================================

      DO 10000 I = 1,NDOG                                               

c      WRITE(12,90000)  ( E(I,J),J=1,KAG )                               

      WRITE(12,90000)  ( E(I,J),J=1,234 )                               

10000 CONTINUE                                                          

      DO 12000 I = 1,LEXOG                                              

c      WRITE(12,90000)  ( X(I,J),J=1,KAG )                               

      WRITE(12,90000)  ( X(I,J),J=1,234 )                               

12000 CONTINUE                                                          

90000 FORMAT(4F18.12)                                                    

C=======================================================================

C                                                                       

C     SECTION 1 ENDS.                                                   

C                                                                       

C=======================================================================

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 2 : WRITE VALUES OF R.E. VARIABLES AND RESIDUALS          

C                 TO TAPE 6.                                            

C                                                                       

C=======================================================================

      TITLE1='     SIMUL'                                               

      TITLE2='ATION     '                                               

      IF(IDYN.EQ.0) GOTO 501                                            

      WRITE(6,73)                                                       

      DO 71 I=1,NARG,4                                                  

      DO 72 J=1,4                                                       

      IT=(L(I+J-1)-1)/600                                               

      IV=L(I+J-1)-IT*600                                                

      IT=IT+1                                                           

      AJX(J)=IXOG(IV)                                                   

   72 NJX(J+4)=NSTART-MAG+IT                                            

      IE=MIN0(4,NARG-I+1)                                               

      WRITE(6,74) (AJX(J),NJX(J+4),R(I+J-1),XEQUIV(L(I+J-1)),J=1,IE)    

   71 CONTINUE                                                          

   73 FORMAT(4(4X,'VARIABLE',3X,'RESIDUAL     VALUE'))                  

   74 FORMAT(4(1X,A8,'(',I4,')',2F9.4))                                 

C=======================================================================

C                                                                       

C     SECTION 2 ENDS.                                                   

C                                                                       

C=======================================================================

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 3 : WRITE THE SOLUTION TO TAPE 6, IN LEVELS               

C                 AND OPTIONALLY IN DIFFERENCES.                        

C                                                                       

C=======================================================================

      CALL NEWRES                                                       

  501 NSTA=NSTART                                                       

      MG1=MAG                                                           

      IF(MAG.EQ.1) GOTO 500                                             

      MG1=MAG-1                                                         

      NSTA=NSTART-1                                                     

  500 DO 1 K=1,NEGP                                                     

      KK1=KK(K+1)                                                       

      IF(K.EQ.NEGP) KK1=NDOG                                            

      KK1=KK1-KK(K)                                                     

      KK2=0                                                             

      IF(K.LE.NXGP)KK2=KK(NEGP+K+1)-KK(NEGP+K)                          

      IF(KK1.EQ.0.AND.KK2.EQ.0) GOTO 1                                  

c      WRITE(6,400) TITLE1,TITLE2,NSTART,(NSTART+NPER-1)                 

      WRITE(6,400) TITLE1,TITLE2,MAG,kag                 

  400 FORMAT(1H1,40X,2A10,'FROM  ',I4,'  TO  ',I4)                      

      IF(KK1.EQ.0) GOTO 2                                               

      ISTART=NSTA  -1                                                   

      ISTART=MAG-2
      WRITE(6,150) K                                                    

  150 FORMAT(//,2X,'GROUP',I3,' ENDOGENOUS VARIABLES')                  

      mgm1=mag-2
      DO 100 I=MG1,KAG,10                                               

      DO 101 J=1,10                                                     

  101 JX(J)=ISTART+J                                                    

      IE=MIN0(10,KAG-I+1)                                               

      WRITE(6,200) (JX(J),J=1,IE)                                       

  200 FORMAT(/14X,I4,9(8X,I4))                                          

      DO 102 J=1,KK1                                                    

      KJ=KK(K)+J                                                        

  102 WRITE(6,300) NAME(KJ),(E(KJ,I+M-1),M=1,IE)                        

      ISTART=ISTART+10                                                  

  100 CONTINUE                                                          

    2 IF(KK2.EQ.0.OR.PX.GE.0.0) GOTO 1                                  

      K2=K+NEGP                                                         

      ISTART=NSTA  -1                                                   

      WRITE(6,151) K2                                                   

  151 FORMAT(//,2X,'GROUP',I3,' EXOGENOUS VARIABLES')                   

      DO 120 I=MG1,KAG,10                                               

      DO 121 J=1,10                                                     

  121 JX(J)=ISTART+J                                                    

      IE=MIN0(10,KAG-I+1)                                               

      WRITE(6,200) (JX(J),J=1,IE)                                       

      DO 122 J=1,KK2                                                    

      KJ=KK(NEGP+K)+J                                                   

  122 WRITE(6,300) IXOG(KJ),(X(KJ,I+M-1),M=1,IE)                        

      ISTART=ISTART+10                                                  

  120 CONTINUE                                                          

  300 FORMAT(2X,A8,10F12.4)                                             

    1 CONTINUE                                                          

C=======================================================================

C                                                                       

C     SECTION 3 ENDS.                                                   

C                                                                       

C=======================================================================

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 4 : OPTIONS TO SAVE THE FORECAST VALUES OF ENDOGENOUS     

C                 VARIABLES AND FORM DIFFERENCES FROM BASE VALUES.      

C                                                                       

C=======================================================================

      IF(PR.NE.1.0D0) GOTO 3                                            

C.....SAVE THE SOLVED VALUES OF ENDOGENOUS VARIABLES.                   

      DO 4 I=MAG,KAG                                                    

    4 WRITE(7,1000) (E(J,I),J=1 ,NDOG)                                  

 1000 FORMAT(5F16.5)                                                    

    3 IF(BR.NE.1.0D0) RETURN                                            

C.....CALL BASE TO GET DIFFERENCES FROM BASE FOR ENDOGENOUS VARIABLES.  

      CALL BASE                                                         

      XX = 0.0D0                                                        

C.....SET BR AND PR EQUAL TO ZERO SO THAT ON NEXT PASS PRINT WILL FINISH

      BR = 0.0D0                                                        

      PR = 0.0D0                                                        

      TITLE1='        DI'                                               

      TITLE2='FFERENCES '                                               

C.....GO BACK TO SECTION  3 TO PRINT DIFFERENCES.                       

      GOTO 500                                                          

C=======================================================================

C                                                                       

C     SECTION 4 ENDS.                                                   

C                                                                       

C=======================================================================

      END                                                               

                                                 

      SUBROUTINE PRINTX                                                  

C=======================================================================

C                                                                       

C     SUBROUTINE PRINT    IS CALLED WHEN THE MODEL HAS SOLVED OR WHEN   

C     ****************    THE MAXIMUM NUMBER OF RATIONAL EXPECTATIONS   

C                         ITERATIONS HAS BEEN REACHED.  PRINT HAS FOUR  

C                         SECTIONS.                                     

C                                                                       

C                         SECTION 1 : WRITES THE VALUES OF ENDOGENOUS   

C                                     AND EXOGENOUS VARIABLES FOR ALL   

C                                     PERIODS ( 1 TO KAG ) TO TAPE 12.  

C                                                                       

C                         SECTION 2 : WRITES THE VALUES OF RATIONAL     

C                                     EXPECTATIONS VARIABLES AND        

C                                     RESIDUALS TO TAPE 6.              

C                                                                       

C                         SECTION 3 : ON THE FIRST PASS FINAL VALUES OF 

C                                     ENDOGENOUS AND EXOGENOUS VARIABLES

C                                     (IN LEVELS) ARE WRITTEN TO TAPE 6.

C                                     THERE IS A SECOND PASS IF BR      

C                                     ( CARD 5 ) IS EQUAL TO 1.0 ;      

C                                     ON THE SECOND PASS THE VALUES OF  

C                                     THE ENDOGENOUS VARIABLES ARE      

C                                     DIFFERENCES FROM A BASE RUN.      

C                                                                       

C                         SECTION 4 : IF PR ( CARD 5 ) IS EQUAL TO 1.0  

C                                     THEN THE VALUES OF THE ENDOGENOUS 

C                                     VARIABLES FOR THE FORECAST PERIOD 

C                                     ARE WRITTEN TO TAPE 7. THEY CAN BE

C                                     USED IN A SUBSEQUENT RUN TO FORM  

C                                     DIFFERENCES, WHEN READ IN FROM    

C                                     TAPE 4.                           

C                                     IF BR ( CARD 5 ) IS EQUAL TO 1.0  

C                                     THEN SUBROUTINE BASE IS CALLED TO 

C                                     FORM DIFFERENCES FROM BASE VALUES 

C                                     FOR THE ENDOGENOUS VARIABLES,     

C                                     BR AND PR ARE SET TO ZERO, AND    

C                                     CONTROL RETURNS TO SECTION 3.     

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG, AJX(10)                                 

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      CHARACTER*10  TITLE1, TITLE2                                      

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,IPER,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/KGROUP/KK(41)                                              

      COMMON/POWELL/R(200000),DIR(200000),L(200000),ICTRL                     

      DIMENSION XEQUIV(120000)                                           

      DIMENSION  NJX(10)                                                

      EQUIVALENCE ( XEQUIV(1),X(1,1) )                                  

C=======================================================================

C                                                                       

C     SECTION 1 : WRITE ALL VALUES OF ENDOGENOUS AND EXOGENOUS VARIABLES

C                 TO TAPE 12.                                           

C                                                                       

C=======================================================================

      DO 10000 I = 1,NDOG                                               

      WRITE(12,90000)  ( E(I,J),J=1,KAG )                               

10000 CONTINUE                                                          

      DO 12000 I = 1,LEXOG                                              

      WRITE(12,90000)  ( X(I,J),J=1,KAG )                               

12000 CONTINUE                                                          

90000 FORMAT(4F18.12)                                                    

C=======================================================================

C                                                                       

C     SECTION 1 ENDS.                                                   

C                                                                       

C=======================================================================

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 2 : WRITE VALUES OF R.E. VARIABLES AND RESIDUALS          

C                 TO TAPE 6.                                            

C                                                                       

C=======================================================================

      TITLE1='     SIMUL'                                               

      TITLE2='ATION     '                                               

      IF(IDYN.EQ.0) GOTO 501                                            

      WRITE(6,73)                                                       

      DO 71 I=1,NARG,4                                                  

      DO 72 J=1,4                                                       

      IT=(L(I+J-1)-1)/600                                               

      IV=L(I+J-1)-IT*600                                                

      IT=IT+1                                                           

      AJX(J)=IXOG(IV)                                                   

   72 NJX(J+4)=NSTART-MAG+IT                                            

      IE=MIN0(4,NARG-I+1)                                               

      WRITE(6,74) (AJX(J),NJX(J+4),R(I+J-1),XEQUIV(L(I+J-1)),J=1,IE)    

   71 CONTINUE                                                          

   73 FORMAT(4(4X,'VARIABLE',3X,'RESIDUAL     VALUE'))                  

   74 FORMAT(4(1X,A8,'(',I4,')',2F9.4))                                 

C=======================================================================

C                                                                       

C     SECTION 2 ENDS.                                                   

C                                                                       

C=======================================================================

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 3 : WRITE THE SOLUTION TO TAPE 6, IN LEVELS               

C                 AND OPTIONALLY IN DIFFERENCES.                        

C                                                                       

C=======================================================================

      CALL NEWRES                                                       

  501 NSTA=NSTART                                                       

      MG1=MAG                                                           

      IF(MAG.EQ.1) GOTO 500                                             

      MG1=MAG-1                                                         

      NSTA=NSTART-1                                                     

  500 DO 1 K=1,NEGP                                                     

      KK1=KK(K+1)                                                       

      IF(K.EQ.NEGP) KK1=NDOG                                            

      KK1=KK1-KK(K)                                                     

      KK2=0                                                             

      IF(K.LE.NXGP)KK2=KK(NEGP+K+1)-KK(NEGP+K)                          

      IF(KK1.EQ.0.AND.KK2.EQ.0) GOTO 1                                  

      WRITE(6,400) TITLE1,TITLE2,NSTART,(NSTART+NPER-1)                 

  400 FORMAT(1H1,40X,2A10,'FROM  ',I4,'  TO  ',I4)                      

      IF(KK1.EQ.0) GOTO 2                                               

      ISTART=NSTA  -1                                                   

      WRITE(6,150) K                                                    

  150 FORMAT(//,2X,'GROUP',I3,' ENDOGENOUS VARIABLES')                  

      DO 100 I=MG1,KAG,5                                               

      DO 101 J=1,5

  101 JX(J)=ISTART+J                                                    

      IE=MIN0(5,KAG-I+1)                                               

      WRITE(6,200) (JX(J),J=1,IE)                                       

  200 FORMAT(/14X,I4,9(8X,I4))                                          

      DO 102 J=1,KK1                                                    

      KJ=KK(K)+J                                                        

  102 WRITE(6,300) NAME(KJ),(E(KJ,I+M-1),M=1,IE)                        

      ISTART=ISTART+5

  100 CONTINUE                                                          

    2 IF(KK2.EQ.0.OR.PX.GE.0.0) GOTO 1                                  

      K2=K+NEGP                                                         

      ISTART=NSTA  -1                                                   

      WRITE(6,151) K2                                                   

  151 FORMAT(//,2X,'GROUP',I3,' EXOGENOUS VARIABLES')                   

      DO 120 I=MG1,KAG,5

      DO 121 J=1,5

  121 JX(J)=ISTART+J                                                    

      IE=MIN0(5,KAG-I+1)                                               

      WRITE(6,200) (JX(J),J=1,IE)                                       

      DO 122 J=1,KK2                                                    

      KJ=KK(NEGP+K)+J                                                   

  122 WRITE(6,300) IXOG(KJ),(X(KJ,I+M-1),M=1,IE)                        

      ISTART=ISTART+5

  120 CONTINUE                                                          

  300 FORMAT(2X,A8,5F12.4)                                             

    1 CONTINUE                                                          

C=======================================================================

C                                                                       

C     SECTION 3 ENDS.                                                   

C                                                                       

C=======================================================================

C                                                                       

C=======================================================================

C                                                                       

C     SECTION 4 : OPTIONS TO SAVE THE FORECAST VALUES OF ENDOGENOUS     

C                 VARIABLES AND FORM DIFFERENCES FROM BASE VALUES.      

C                                                                       

C=======================================================================

      IF(PR.NE.1.0D0) GOTO 3                                            

C.....SAVE THE SOLVED VALUES OF ENDOGENOUS VARIABLES.                   

      DO 4 I=MAG,KAG                                                    

    4 WRITE(7,1000) (E(J,I),J=1 ,NDOG)                                  

 1000 FORMAT(5F16.5)                                                    

    3 IF(BR.NE.1.0D0) RETURN                                            

C.....CALL BASE TO GET DIFFERENCES FROM BASE FOR ENDOGENOUS VARIABLES.  

      CALL BASE                                                         

      XX = 0.0D0                                                        

C.....SET BR AND PR EQUAL TO ZERO SO THAT ON NEXT PASS PRINT WILL FINISH

      BR = 0.0D0                                                        

      PR = 0.0D0                                                        

      TITLE1='        DI'                                               

      TITLE2='FFERENCES '                                               

C.....GO BACK TO SECTION  3 TO PRINT DIFFERENCES.                       

      GOTO 500                                                          

C=======================================================================

C                                                                       

C     SECTION 4 ENDS.                                                   

C                                                                       

C=======================================================================

      END                                                               

      SUBROUTINE BASE                                                   

C=======================================================                

C                                                      =                

C  SUBROUTINE BASE                                     =                

C  ***************                                     =                

C                                                      =                

C    PURPOSE    SUBROUTINE BASE IS USED TO :           =                

C    --------                                          =                

C               1) READ IN VALUES OF ENDOGENOUS        =                

C                  VARIABLES FROM A PREVIOUS BASE RUN. =                

C               2) IF BR (CARD 5) EQUALS 1.0 THEN                       

C                  PRINT CHARTS AND TABLES ON TAPE 6.                   

C               3) FORM THE DIFFERENCES.               =                

C                                                      =                

C=======================================================                

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          ERX(600,300), EE(300,300), XX(600,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,IPER,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/KGROUP/KK(41)/JGROUP/JJ(41)                                

C                                                                       

C.....ASSUME THE NUMBER OF GROUPS IS THIRTEEN.                          

      NGP  =  13                                                        

C                                                                       

C===== (1) READ THE BASE VARIABLES.                                     

C                                                                       

      DO 1000 I = MAG,KAG                                               

      READ(4,1500)  ( EE(IV,I), IV = 1,NDOG )                           

 1000 CONTINUE                                                          

 1500 FORMAT(5F16.5)                                                    

C                                                                       

C===== (2) CALL TABLE AND CHART OPTIONALLY. (Option removed).

C                                                                       

C=====FORM THE DIFFERENCES.                                             

C                                                                       

      DO 9000 I = MAG,KAG                                               

      DO 9000 IV = 1,NDOG                                               

      E(IV,I)  =  E(IV,I) - EE(IV,I)                                    

 9000 CONTINUE                                                          

                                                                        

      RETURN                                                            

      END                                                               

      SUBROUTINE SHOCK                                                  

C=======================================================================

C                                                                       

C     SUBROUTINE SHOCK    IS USED TO SHOCK THE EXOGENOUS VARIABLES.     

C     ****************                                                  

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,IPER,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/KGROUP/KK(41)                                              

      KJ=KAG                                                            

      IF(NSK.EQ.1) KJ=MAG                                               

      DO 25 J=MAG,KJ                                                    

      DO 24 K=1,IJX                                                     

      IGP=JX(K)/100                                                     

      IVB=JX(K)-IGP*100                                                 

   24 X(KK(IGP)+IVB,J)=X(KK(IGP)+IVB,J)+VX(K)                           

   25 CONTINUE                                                          

      RETURN                                                            

      END                                                               

      SUBROUTINE PSHOCK                                                 

C=======================================================================

C                                                                       

C     SUBROUTINE PSHOCK    IS USED TO WRITE SHOCK VALUES OF EXOGENOUS   

C     *****************    VARIABLES.                                   

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,IPER,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      READ(5,20) (JX(I),I=1,IJX)                                        

      READ(5,*) (VX(I),I=1,IJX)                                         

      IF(NSK.EQ.1) GO TO 5                                              

      WRITE(6,24)                                                       

      GO TO 7                                                           

    5 WRITE(6,25)                                                       

    7 WRITE(6,26) (JX(I),VX(I),I=1,IJX)                                 

      RETURN                                                            

   20 FORMAT(20I4)                                                      

   24 FORMAT(20X,'EXOGENOUS VARIABLES SHOCK:CONTINUOUS')                

   25 FORMAT(20X,'EXOGENOUS VARIABLES SHOCK:IMPACT')                    

   26 FORMAT(20X,'EXOGENOUS VARIABLE',I4,20X,'VALUES=',F10.4)           

      END                                                               

CDC   FUNCTION VV(I,J,K)                                                

      DOUBLE PRECISION FUNCTION VV(I,J,K)                               

C=======================================================================

C                                                                       

C     FUNCTION VV  IS CALLED TO GET THE VALUE OF THE J-TH VARIABLE      

C     ***********  IN THE I-TH BLOC AT TIME K RELATIVE TO THE CURRENT   

C                  PERIOD.                                              

C                                                                       

C                  IF I IS LESS THAN OR EQUAL TO THE NUMBER OF          

C                  ENDOGENOUS GROUPS THEN THE VARIABLE IS ENDOGENOUS.   

C                  OTHERWISE THE VARIABLE IS EXOGENOUS.                 

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/KGROUP/KK(41)/JGROUP/JJ(41)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,IPER,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      L=KK(I)                                                           

      IF(I.LE.NEGP) VV=E(L+J,IPER+K)                                    

      IF(I.GT.NEGP) VV=X(L+J,IPER+K)                                    

      RETURN                                                            

      END                                                               

CDC   FUNCTION RR(I,J,K)                                                

      DOUBLE PRECISION FUNCTION RR(I,J,K)                               

C=======================================================================

C                                                                       

C     FUNCTION RR    IS USED TO GET A VALUE FOR THE RESIDUAL FOR THE    

C     ***********    J-TH ENDOGENOUS VARIABLE IN THE I-TH GROUP AT THE  

C                    K-TH PERIOD RELATIVE TO THE CURRENT PERIOD.        

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION  (A-H,O-Z)                              

      CHARACTER*8   NAME, IXOG                                          

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/KGROUP/KK(41)/JGROUP/JJ(41)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          DUMMY(1500,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,IPER,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      L=KK(I)+J                                                         

      RR=ER(L,IPER+K)                                                   

      RETURN                                                            

      END                                                               

      SUBROUTINE POW 

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

C=======================================================================

C                                                                       

C     SUBROUTINE POW    SOLVES A SET OF EQUATIONS USING THE POWELL      

C     **************    METHOD.  IT IS UNTESTED.                        

C                                                                       

C=======================================================================

      dummy = 0.0

      END                                                               

      SUBROUTINE HYBRD(FCN,N,X,FVEC,XTOL,MAXFEV,ML,MU,EPSFCN,DIAG,      

     *                 MODE,FACTOR,NPRINT,INFO,NFEV,FJAC,LDFJAC,R,LR,   

     *                 QTF,WA1,WA2,WA3,WA4)                             

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      DIMENSION X(N),FVEC(N),DIAG(N),FJAC(LDFJAC,N),R(LR),QTF(N),WA1(N),

     *     WA2(N),WA3(N),WA4(N)                                         

      EXTERNAL FCN                                                      

C     **********                                                        

C                                                                       

C     SUBROUTINE HYBRD                                                  

C                                                                       

C     THE PURPOSE OF HYBRD IS TO FIND A ZERO OF A SYSTEM OF             

C     N NONLINEAR FUNCTIONS IN N VARIABLES BY A MODIFICATION            

C     OF THE POWELL HYBRID METHOD. THE USER MUST PROVIDE A              

C     SUBROUTINE WHICH CALCULATES THE FUNCTIONS. THE JACOBIAN IS        

C     THEN CALCULATED BY A FORWARD-DIFFERENCE APPROXIMATION.            

C                                                                       

C     THE SUBROUTINE STATEMENT IS                                       

C                                                                       

C       SUBROUTINE HYBRD(FCN,N,X,FVEC,XTOL,MAXFEV,ML,MU,EPSFCN,         

C                        DIAG,MODE,FACTOR,NPRINT,INFO,NFEV,FJAC,        

C                        LDFJAC,R,LR,QTF,WA1,WA2,WA3,WA4)               

C                                                                       

C     WHERE                                                             

C                                                                       

C       FCN IS THE NAME OF THE USER-SUPPLIED SUBROUTINE WHICH           

C         CALCULATES THE FUNCTIONS. FCN MUST BE DECLARED                

C         IN AN EXTERNAL STATEMENT IN THE USER CALLING                  

C         PROGRAM, AND SHOULD BE WRITTEN AS FOLLOWS.                    

C                                                                       

C         SUBROUTINE FCN(N,X,FVEC,IFLAG)                                

C         INTEGER N,IFLAG                                               

C         REAL X(N),FVEC(N)                                             

C         ----------                                                    

C         CALCULATE THE FUNCTIONS AT X AND                              

C         RETURN THIS VECTOR IN FVEC.                                   

C         ---------                                                     

C         RETURN                                                        

C         END                                                           

C                                                                       

C         THE VALUE OF IFLAG SHOULD NOT BE CHANGED BY FCN UNLESS        

C         THE USER WANTS TO TERMINATE EXECUTION OF HYBRD.               

C         IN THIS CASE SET IFLAG TO A NEGATIVE INTEGER.                 

C                                                                       

C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        

C         OF FUNCTIONS AND VARIABLES.                                   

C                                                                       

C       X IS AN ARRAY OF LENGTH N. ON INPUT X MUST CONTAIN              

C         AN INITIAL ESTIMATE OF THE SOLUTION VECTOR. ON OUTPUT X       

C         CONTAINS THE FINAL ESTIMATE OF THE SOLUTION VECTOR.           

C                                                                       

C       FVEC IS AN OUTPUT ARRAY OF LENGTH N WHICH CONTAINS              

C         THE FUNCTIONS EVALUATED AT THE OUTPUT X.                      

C                                                                       

C       XTOL IS A NONNEGATIVE INPUT VARIABLE. TERMINATION               

C         OCCURS WHEN THE RELATIVE ERROR BETWEEN TWO CONSECUTIVE        

C         ITERATES IS AT MOST XTOL.                                     

C                                                                       

C       MAXFEV IS A POSITIVE INTEGER INPUT VARIABLE. TERMINATION        

C         OCCURS WHEN THE NUMBER OF CALLS TO FCN IS AT LEAST MAXFEV     

C         BY THE END OF AN ITERATION.                                   

C                                                                       

C       ML IS A NONNEGATIVE INTEGER INPUT VARIABLE WHICH SPECIFIES      

C         THE NUMBER OF SUBDIAGONALS WITHIN THE BAND OF THE             

C         JACOBIAN MATRIX. IF THE JACOBIAN IS NOT BANDED, SET           

C         ML TO AT LEAST N - 1.                                         

C                                                                       

C       MU IS A NONNEGATIVE INTEGER INPUT VARIABLE WHICH SPECIFIES      

C         THE NUMBER OF SUPERDIAGONALS WITHIN THE BAND OF THE           

C         JACOBIAN MATRIX. IF THE JACOBIAN IS NOT BANDED, SET           

C         MU TO AT LEAST N - 1.                                         

C                                                                       

C       EPSFCN IS AN INPUT VARIABLE USED IN DETERMINING A SUITABLE      

C         STEP LENGTH FOR THE FORWARD-DIFFERENCE APPROXIMATION. THIS    

C         APPROXIMATION ASSUMES THAT THE RELATIVE ERRORS IN THE         

C         FUNCTIONS ARE OF THE ORDER OF EPSFCN. IF EPSFCN IS LESS       

C         THAN THE MACHINE PRECISION, IT IS ASSUMED THAT THE RELATIVE   

C         ERRORS IN THE FUNCTIONS ARE OF THE ORDER OF THE MACHINE       

C         PRECISION.                                                    

C                                                                       

C       DIAG IS AN ARRAY OF LENGTH N. IF MODE = 1 (SEE                  

C         BELOW), DIAG IS INTERNALLY SET. IF MODE = 2, DIAG             

C         MUST CONTAIN POSITIVE ENTRIES THAT SERVE AS                   

C         MULTIPLICATIVE SCALE FACTORS FOR THE VARIABLES.               

C                                                                       

C       MODE IS AN INTEGER INPUT VARIABLE. IF MODE = 1, THE             

C         VARIABLES WILL BE SCALED INTERNALLY. IF MODE = 2,             

C         THE SCALING IS SPECIFIED BY THE INPUT DIAG. OTHER             

C         VALUES OF MODE ARE EQUIVALENT TO MODE = 1.                    

C                                                                       

C       FACTOR IS A POSITIVE INPUT VARIABLE USED IN DETERMINING THE     

C         INITIAL STEP BOUND. THIS BOUND IS SET TO THE PRODUCT OF       

C         FACTOR AND THE EUCLIDEAN NORM OF DIAG*X IF NONZERO, OR ELSE   

C         TO FACTOR ITSELF. IN MOST CASES FACTOR SHOULD LIE IN THE      

C         INTERVAL (.1,100.). 100. IS A GENERALLY RECOMMENDED VALUE.    

C                                                                       

C       NPRINT IS AN INTEGER INPUT VARIABLE THAT ENABLES CONTROLLED     

C         PRINTING OF ITERATES IF IT IS POSITIVE. IN THIS CASE,         

C         FCN IS CALLED WITH IFLAG = 0 AT THE BEGINNING OF THE FIRST    

C         ITERATION AND EVERY NPRINT ITERATIONS THEREAFTER AND          

C         IMMEDIATELY PRIOR TO RETURN, WITH X AND FVEC AVAILABLE        

C         FOR PRINTING. IF NPRINT IS NOT POSITIVE, NO SPECIAL CALLS     

C         OF FCN WITH IFLAG = 0 ARE MADE.                               

C                                                                       

C       INFO IS AN INTEGER OUTPUT VARIABLE. IF THE USER HAS             

C         TERMINATED EXECUTION, INFO IS SET TO THE (NEGATIVE)           

C         VALUE OF IFLAG. SEE DESCRIPTION OF FCN. OTHERWISE,            

C         INFO IS SET AS FOLLOWS.                                       

C                                                                       

C         INFO = 0   IMPROPER INPUT PARAMETERS.                         

C                                                                       

C         INFO = 1   RELATIVE ERROR BETWEEN TWO CONSECUTIVE ITERATES    

C                    IS AT MOST XTOL.                                   

C                                                                       

C         INFO = 2   NUMBER OF CALLS TO FCN HAS REACHED OR EXCEEDED     

C                    MAXFEV.                                            

C                                                                       

C         INFO = 3   XTOL IS TOO SMALL. NO FURTHER IMPROVEMENT IN       

C                    THE APPROXIMATE SOLUTION X IS POSSIBLE.            

C                                                                       

C         INFO = 4   ITERATION IS NOT MAKING GOOD PROGRESS, AS          

C                    MEASURED BY THE IMPROVEMENT FROM THE LAST          

C                    FIVE JACOBIAN EVALUATIONS.                         

C                                                                       

C         INFO = 5   ITERATION IS NOT MAKING GOOD PROGRESS, AS          

C                    MEASURED BY THE IMPROVEMENT FROM THE LAST          

C                    TEN ITERATIONS.                                    

C                                                                       

C       NFEV IS AN INTEGER OUTPUT VARIABLE SET TO THE NUMBER OF         

C         CALLS TO FCN.                                                 

C                                                                       

C       FJAC IS AN OUTPUT N BY N ARRAY WHICH CONTAINS THE               

C         ORTHOGONAL MATRIX Q PRODUCED BY THE QR FACTORIZATION          

C         OF THE FINAL APPROXIMATE JACOBIAN.                            

C                                                                       

C       LDFJAC IS A POSITIVE INTEGER INPUT VARIABLE NOT LESS THAN N     

C         WHICH SPECIFIES THE LEADING DIMENSION OF THE ARRAY FJAC.      

C                                                                       

C       R IS AN OUTPUT ARRAY OF LENGTH LR WHICH CONTAINS THE            

C         UPPER TRIANGULAR MATRIX PRODUCED BY THE QR FACTORIZATION      

C         OF THE FINAL APPROXIMATE JACOBIAN, STORED ROWWISE.            

C                                                                       

C       LR IS A POSITIVE INTEGER INPUT VARIABLE NOT LESS THAN           

C         (N*(N+1))/2.                                                  

C                                                                       

C       QTF IS AN OUTPUT ARRAY OF LENGTH N WHICH CONTAINS               

C         THE VECTOR (Q TRANSPOSE)*FVEC.                                

C                                                                       

C       WA1, WA2, WA3, AND WA4 ARE WORK ARRAYS OF LENGTH N.             

C                                                                       

C     SUBPROGRAMS CALLED                                                

C                                                                       

C       USER-SUPPLIED ...... FCN                                        

C                                                                       

C       MINPACK-SUPPLIED ... DOGLEG,SPMPAR,ENORM,FDJAC1,                

C                            QFORM,QRFAC,R1MPYQ,R1UPDT                  

C                                                                       

C       FORTRAN-SUPPLIED ... ABS,AMAX1,AMIN1,MIN0,MOD                   

C                                                                       

C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.         

C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE             

C                                                                       

C     **********                                                        

      DIMENSION IWA(1)                                                  

      LOGICAL JEVAL,SING                                                

C     REAL ACTRED,DELTA,EPSMCH,FNORM,FNORM1,ONE,PNORM,PRERED,P1,P5,     

C    *     P001,P0001,RATIO,SUM,TEMP,XNORM,ZERO                         

C     REAL SPMPAR,ENORM                                                 

      DATA ONE,P1,P5,P001,P0001,ZERO                                    

     *     /1.0D0,1.0D-1,5.0D-1,1.0D-3,1.0D-4,0.0D0/                    

C                                                                       

C     EPSMCH IS THE MACHINE PRECISION.                                  

C                                                                       

      EPSMCH = SPMPAR(1)                                                

C                                                                       

      INFO = 0                                                          

      IFLAG = 0                                                         

      NFEV = 0                                                          

C                                                                       

C     CHECK THE INPUT PARAMETERS FOR ERRORS.                            

C                                                                       

      IF (N .LE. 0 .OR. XTOL .LT. ZERO .OR. MAXFEV .LE. 0               

     *    .OR. ML .LT. 0 .OR. MU .LT. 0 .OR. FACTOR .LE. ZERO           

     *    .OR. LDFJAC .LT. N .OR. LR .LT. (N*(N + 1))/2) GO TO 300      

      IF (MODE .NE. 2) GO TO 20                                         

      DO 10 J = 1, N                                                    

         IF (DIAG(J) .LE. ZERO) GO TO 300                               

   10    CONTINUE                                                       

   20 CONTINUE                                                          

C                                                                       

C     EVALUATE THE FUNCTION AT THE STARTING POINT                       

C     AND CALCULATE ITS NORM.                                           

C                                                                       

      IFLAG = 1                                                         

      CALL FCN(N,X,FVEC,IFLAG)                                          

      NFEV = 1                                                          

      IF (IFLAG .LT. 0) GO TO 300                                       

      FNORM = ENORM(N,FVEC)                                             

C                                                                       

C     DETERMINE THE NUMBER OF CALLS TO FCN NEEDED TO COMPUTE            

C     THE JACOBIAN MATRIX.                                              

C                                                                       

      MSUM = MIN0(ML+MU+1,N)                                            

C                                                                       

C     INITIALIZE ITERATION COUNTER AND MONITORS.                        

C                                                                       

      ITER = 1                                                          

      NCSUC = 0                                                         

      NCFAIL = 0                                                        

      NSLOW1 = 0                                                        

      NSLOW2 = 0                                                        

C                                                                       

C     BEGINNING OF THE OUTER LOOP.                                      

C                                                                       

   30 CONTINUE                                                          

         JEVAL = .TRUE.                                                 

C                                                                       

C        CALCULATE THE JACOBIAN MATRIX.                                 

C                                                                       

         IFLAG = 2                                                      

         CALL FDJAC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,EPSFCN,WA1,   

     *               WA2)                                               

         NFEV = NFEV + MSUM                                             

         IF (IFLAG .LT. 0) GO TO 300                                    

C                                                                       

C        COMPUTE THE QR FACTORIZATION OF THE JACOBIAN.                  

C                                                                       

         CALL QRFAC(N,N,FJAC,LDFJAC,.FALSE.,IWA,1,WA1,WA2,WA3)          

C                                                                       

C        ON THE FIRST ITERATION AND IF MODE IS 1, SCALE ACCORDING       

C        TO THE NORMS OF THE COLUMNS OF THE INITIAL JACOBIAN.           

C                                                                       

         IF (ITER .NE. 1) GO TO 70                                      

         IF (MODE .EQ. 2) GO TO 50                                      

         DO 40 J = 1, N                                                 

            DIAG(J) = WA2(J)                                            

            IF (WA2(J) .EQ. ZERO) DIAG(J) = ONE                         

   40       CONTINUE                                                    

   50    CONTINUE                                                       

C                                                                       

C        ON THE FIRST ITERATION, CALCULATE THE NORM OF THE SCALED X     

C        AND INITIALIZE THE STEP BOUND DELTA.                           

C                                                                       

         DO 60 J = 1, N                                                 

            WA3(J) = DIAG(J)*X(J)                                       

   60       CONTINUE                                                    

         XNORM = ENORM(N,WA3)                                           

         DELTA = FACTOR*XNORM                                           

         IF (DELTA .EQ. ZERO) DELTA = FACTOR                            

   70    CONTINUE                                                       

C                                                                       

C        FORM (Q TRANSPOSE)*FVEC AND STORE IN QTF.                      

C                                                                       

         DO 80 I = 1, N                                                 

            QTF(I) = FVEC(I)                                            

   80       CONTINUE                                                    

         DO 120 J = 1, N                                                

            IF (FJAC(J,J) .EQ. ZERO) GO TO 110                          

            SUM = ZERO                                                  

            DO 90 I = J, N                                              

               SUM = SUM + FJAC(I,J)*QTF(I)                             

   90          CONTINUE                                                 

            TEMP = -SUM/FJAC(J,J)                                       

            DO 100 I = J, N                                             

               QTF(I) = QTF(I) + FJAC(I,J)*TEMP                         

  100          CONTINUE                                                 

  110       CONTINUE                                                    

  120       CONTINUE                                                    

C                                                                       

C        COPY THE TRIANGULAR FACTOR OF THE QR FACTORIZATION INTO R.     

C                                                                       

         SING = .FALSE.                                                 

         DO 150 J = 1, N                                                

            L = J                                                       

            JM1 = J - 1                                                 

            IF (JM1 .LT. 1) GO TO 140                                   

            DO 130 I = 1, JM1                                           

               R(L) = FJAC(I,J)                                         

               L = L + N - I                                            

  130          CONTINUE                                                 

  140       CONTINUE                                                    

            R(L) = WA1(J)                                               

            IF (WA1(J) .EQ. ZERO) SING = .TRUE.                         

  150       CONTINUE                                                    

C                                                                       

C        ACCUMULATE THE ORTHOGONAL FACTOR IN FJAC.                      

C                                                                       

         CALL QFORM(N,N,FJAC,LDFJAC,WA1)                                

C                                                                       

C        RESCALE IF NECESSARY.                                          

C                                                                       

         IF (MODE .EQ. 2) GO TO 170                                     

         DO 160 J = 1, N                                                

            DIAG(J) = MAX(DIAG(J),WA2(J))                               

  160       CONTINUE                                                    

  170    CONTINUE                                                       

C                                                                       

C        BEGINNING OF THE INNER LOOP.                                   

C                                                                       

  180    CONTINUE                                                       

C                                                                       

C           IF REQUESTED, CALL FCN TO ENABLE PRINTING OF ITERATES.      

C                                                                       

            IF (NPRINT .LE. 0) GO TO 190                                

            IFLAG = 0                                                   

            IF (MOD(ITER-1,NPRINT) .EQ. 0) CALL FCN(N,X,FVEC,IFLAG)     

            IF (IFLAG .LT. 0) GO TO 300                                 

  190       CONTINUE                                                    

C                                                                       

C           DETERMINE THE DIRECTION P.                                  

C                                                                       

            CALL DOGLEG(N,R,LR,DIAG,QTF,DELTA,WA1,WA2,WA3)              

C                                                                       

C           STORE THE DIRECTION P AND X + P. CALCULATE THE NORM OF P.   

C                                                                       

            DO 200 J = 1, N                                             

               WA1(J) = -WA1(J)                                         

               WA2(J) = X(J) + WA1(J)                                   

               WA3(J) = DIAG(J)*WA1(J)                                  

  200          CONTINUE                                                 

            PNORM = ENORM(N,WA3)                                        

C                                                                       

C           ON THE FIRST ITERATION, ADJUST THE INITIAL STEP BOUND.      

C                                                                       

            IF (ITER .EQ. 1) DELTA = MIN(DELTA,PNORM)                   

C                                                                       

C           EVALUATE THE FUNCTION AT X + P AND CALCULATE ITS NORM.      

C                                                                       

            IFLAG = 1                                                   

            CALL FCN(N,WA2,WA4,IFLAG)                                   

            NFEV = NFEV + 1                                             

            IF (IFLAG .LT. 0) GO TO 300                                 

            FNORM1 = ENORM(N,WA4)                                       

C                                                                       

C           COMPUTE THE SCALED ACTUAL REDUCTION.                        

C                                                                       

            ACTRED = -ONE                                               

            IF (FNORM1 .LT. FNORM) ACTRED = ONE - (FNORM1/FNORM)**2     

C                                                                       

C           COMPUTE THE SCALED PREDICTED REDUCTION.                     

C                                                                       

            L = 1                                                       

            DO 220 I = 1, N                                             

               SUM = ZERO                                               

               DO 210 J = I, N                                          

                  SUM = SUM + R(L)*WA1(J)                               

                  L = L + 1                                             

  210             CONTINUE                                              

               WA3(I) = QTF(I) + SUM                                    

  220          CONTINUE                                                 

            TEMP = ENORM(N,WA3)                                         

            PRERED = ZERO                                               

            IF (TEMP .LT. FNORM) PRERED = ONE - (TEMP/FNORM)**2         

C                                                                       

C           COMPUTE THE RATIO OF THE ACTUAL TO THE PREDICTED            

C           REDUCTION.                                                  

C                                                                       

            RATIO = ZERO                                                

            IF (PRERED .GT. ZERO) RATIO = ACTRED/PRERED                 

C                                                                       

C           UPDATE THE STEP BOUND.                                      

C                                                                       

            IF (RATIO .GE. P1) GO TO 230                                

               NCSUC = 0                                                

               NCFAIL = NCFAIL + 1                                      

               DELTA = P5*DELTA                                         

               GO TO 240                                                

  230       CONTINUE                                                    

               NCFAIL = 0                                               

               NCSUC = NCSUC + 1                                        

               IF (RATIO .GE. P5 .OR. NCSUC .GT. 1)                     

     *            DELTA = MAX(DELTA,PNORM/P5)                           

               IF (ABS(RATIO-ONE) .LE. P1) DELTA = PNORM/P5             

  240       CONTINUE                                                    

C                                                                       

C           TEST FOR SUCCESSFUL ITERATION.                              

C                                                                       

            IF (RATIO .LT. P0001) GO TO 260                             

C                                                                       

C           SUCCESSFUL ITERATION. UPDATE X, FVEC, AND THEIR NORMS.      

C                                                                       

            DO 250 J = 1, N                                             

               X(J) = WA2(J)                                            

               WA2(J) = DIAG(J)*X(J)                                    

               FVEC(J) = WA4(J)                                         

  250          CONTINUE                                                 

            XNORM = ENORM(N,WA2)                                        

            FNORM = FNORM1                                              

            ITER = ITER + 1                                             

  260       CONTINUE                                                    

C                                                                       

C           DETERMINE THE PROGRESS OF THE ITERATION.                    

C                                                                       

            NSLOW1 = NSLOW1 + 1                                         

            IF (ACTRED .GE. P001) NSLOW1 = 0                            

            IF (JEVAL) NSLOW2 = NSLOW2 + 1                              

            IF (ACTRED .GE. P1) NSLOW2 = 0                              

C                                                                       

C           TEST FOR CONVERGENCE.                                       

C                                                                       

            IF (DELTA .LE. XTOL*XNORM .OR. FNORM .EQ. ZERO) INFO = 1    

            IF (INFO .NE. 0) GO TO 300                                  

C                                                                       

C           TESTS FOR TERMINATION AND STRINGENT TOLERANCES.             

C                                                                       

            IF (NFEV .GE. MAXFEV) INFO = 2                              

            IF (P1*MAX(P1*DELTA,PNORM) .LE. EPSMCH*XNORM) INFO = 3      

            IF (NSLOW2 .EQ. 5) INFO = 4                                 

            IF (NSLOW1 .EQ. 10) INFO = 5                                

            IF (INFO .NE. 0) GO TO 300                                  

C                                                                       

C           CRITERION FOR RECALCULATING JACOBIAN APPROXIMATION          

C           BY FORWARD DIFFERENCES.                                     

C                                                                       

            IF (NCFAIL .EQ. 2) GO TO 290                                

C                                                                       

C           CALCULATE THE RANK ONE MODIFICATION TO THE JACOBIAN         

C           AND UPDATE QTF IF NECESSARY.                                

C                                                                       

            DO 280 J = 1, N                                             

               SUM = ZERO                                               

               DO 270 I = 1, N                                          

                  SUM = SUM + FJAC(I,J)*WA4(I)                          

  270             CONTINUE                                              

               WA2(J) = (SUM - WA3(J))/PNORM                            

               WA1(J) = DIAG(J)*((DIAG(J)*WA1(J))/PNORM)                

               IF (RATIO .GE. P0001) QTF(J) = SUM                       

  280          CONTINUE                                                 

C                                                                       

C           COMPUTE THE QR FACTORIZATION OF THE UPDATED JACOBIAN.       

C                                                                       

            CALL R1UPDT(N,N,R,LR,WA1,WA2,WA3,SING)                      

            CALL R1MPYQ(N,N,FJAC,LDFJAC,WA2,WA3)                        

            CALL R1MPYQ(1,N,QTF,1,WA2,WA3)                              

C                                                                       

C           END OF THE INNER LOOP.                                      

C                                                                       

            JEVAL = .FALSE.                                             

            GO TO 180                                                   

  290    CONTINUE                                                       

C                                                                       

C        END OF THE OUTER LOOP.                                         

C                                                                       

         GO TO 30                                                       

  300 CONTINUE                                                          

C                                                                       

C     TERMINATION, EITHER NORMAL OR USER IMPOSED.                       

C                                                                       

      IF (IFLAG .LT. 0) INFO = IFLAG                                    

      IFLAG = 0                                                         

      IF (NPRINT .GT. 0) CALL FCN(N,X,FVEC,IFLAG)                       

      RETURN                                                            

C                                                                       

C     LAST CARD OF SUBROUTINE HYBRD.                                    

C                                                                       

      END                                                               

      SUBROUTINE HYBRD1(FCN,N,X,FVEC,TOL,INFO,WA,LWA)                   

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

C     REAL TOL                                                          

      DIMENSION X(N),FVEC(N),WA(LWA)                                    

      EXTERNAL FCN                                                      

C     **********                                                        

C                                                                       

C     SUBROUTINE HYBRD1                                                 

C                                                                       

C     THE PURPOSE OF HYBRD1 IS TO FIND A ZERO OF A SYSTEM OF            

C     N NONLINEAR FUNCTIONS IN N VARIABLES BY A MODIFICATION            

C     OF THE POWELL HYBRID METHOD. THIS IS DONE BY USING THE            

C     MORE GENERAL NONLINEAR EQUATION SOLVER HYBRD. THE USER            

C     MUST PROVIDE A SUBROUTINE WHICH CALCULATES THE FUNCTIONS.         

C     THE JACOBIAN IS THEN CALCULATED BY A FORWARD-DIFFERENCE           

C     APPROXIMATION.                                                    

C                                                                       

C     THE SUBROUTINE STATEMENT IS                                       

C                                                                       

C       SUBROUTINE HYBRD1(FCN,N,X,FVEC,TOL,INFO,WA,LWA)                 

C                                                                       

C     WHERE                                                             

C                                                                       

C       FCN IS THE NAME OF THE USER-SUPPLIED SUBROUTINE WHICH           

C         CALCULATES THE FUNCTIONS. FCN MUST BE DECLARED                

C         IN AN EXTERNAL STATEMENT IN THE USER CALLING                  

C         PROGRAM, AND SHOULD BE WRITTEN AS FOLLOWS.                    

C                                                                       

C         SUBROUTINE FCN(N,X,FVEC,IFLAG)                                

C         INTEGER N,IFLAG                                               

C         REAL X(N),FVEC(N)                                             

C         ----------                                                    

C         CALCULATE THE FUNCTIONS AT X AND                              

C         RETURN THIS VECTOR IN FVEC.                                   

C         ---------                                                     

C         RETURN                                                        

C         END                                                           

C                                                                       

C         THE VALUE OF IFLAG SHOULD NOT BE CHANGED BY FCN UNLESS        

C         THE USER WANTS TO TERMINATE EXECUTION OF HYBRD1.              

C         IN THIS CASE SET IFLAG TO A NEGATIVE INTEGER.                 

C                                                                       

C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        

C         OF FUNCTIONS AND VARIABLES.                                   

C                                                                       

C       X IS AN ARRAY OF LENGTH N. ON INPUT X MUST CONTAIN              

C         AN INITIAL ESTIMATE OF THE SOLUTION VECTOR. ON OUTPUT X       

C         CONTAINS THE FINAL ESTIMATE OF THE SOLUTION VECTOR.           

C                                                                       

C       FVEC IS AN OUTPUT ARRAY OF LENGTH N WHICH CONTAINS              

C         THE FUNCTIONS EVALUATED AT THE OUTPUT X.                      

C                                                                       

C       TOL IS A NONNEGATIVE INPUT VARIABLE. TERMINATION OCCURS         

C         WHEN THE ALGORITHM ESTIMATES THAT THE RELATIVE ERROR          

C         BETWEEN X AND THE SOLUTION IS AT MOST TOL.                    

C                                                                       

C       INFO IS AN INTEGER OUTPUT VARIABLE. IF THE USER HAS             

C         TERMINATED EXECUTION, INFO IS SET TO THE (NEGATIVE)           

C         VALUE OF IFLAG. SEE DESCRIPTION OF FCN. OTHERWISE,            

C         INFO IS SET AS FOLLOWS.                                       

C                                                                       

C         INFO = 0   IMPROPER INPUT PARAMETERS.                         

C                                                                       

C         INFO = 1   ALGORITHM ESTIMATES THAT THE RELATIVE ERROR        

C                    BETWEEN X AND THE SOLUTION IS AT MOST TOL.         

C                                                                       

C         INFO = 2   NUMBER OF CALLS TO FCN HAS REACHED OR EXCEEDED     

C                    200*(N+1).                                         

C                                                                       

C         INFO = 3   TOL IS TOO SMALL. NO FURTHER IMPROVEMENT IN        

C                    THE APPROXIMATE SOLUTION X IS POSSIBLE.            

C                                                                       

C         INFO = 4   ITERATION IS NOT MAKING GOOD PROGRESS.             

C                                                                       

C       WA IS A WORK ARRAY OF LENGTH LWA.                               

C                                                                       

C       LWA IS A POSITIVE INTEGER INPUT VARIABLE NOT LESS THAN          

C         (N*(3*N+13))/2.                                               

C                                                                       

C     SUBPROGRAMS CALLED                                                

C                                                                       

C       USER-SUPPLIED ...... FCN                                        

C                                                                       

C       MINPACK-SUPPLIED ... HYBRD                                      

C                                                                       

C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.         

C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE             

C                                                                       

C     **********                                                        

      DATA FACTOR,ONE,ZERO /1.0D2,1.0D0,0.0D0/                          

      INFO = 0                                                          

C                                                                       

C     CHECK THE INPUT PARAMETERS FOR ERRORS.                            

C                                                                       

      IF (N .LE. 0 .OR. TOL .LT. ZERO .OR. LWA .LT. (N*(3*N + 13))/2)   

     *   GO TO 20                                                       

C                                                                       

C     CALL HYBRD.                                                       

C                                                                       

      MAXFEV = 200*(N + 1)                                              

      XTOL = TOL                                                        

      ML = N - 1                                                        

      MU = N - 1                                                        

      EPSFCN = ZERO                                                     

      MODE = 2                                                          

      DO 10 J = 1, N                                                    

         WA(J) = ONE                                                    

   10    CONTINUE                                                       

      NPRINT = 0                                                        

      LR = (N*(N + 1))/2                                                

      INDEX = 6*N + LR                                                  

      CALL HYBRD(FCN,N,X,FVEC,XTOL,MAXFEV,ML,MU,EPSFCN,WA(1),MODE,      

     *           FACTOR,NPRINT,INFO,NFEV,WA(INDEX+1),N,WA(6*N+1),LR,    

     *           WA(N+1),WA(2*N+1),WA(3*N+1),WA(4*N+1),WA(5*N+1))       

      IF (INFO .EQ. 5) INFO = 4                                         

   20 CONTINUE                                                          

      RETURN                                                            

C                                                                       

C     LAST CARD OF SUBROUTINE HYBRD1.                                   

C                                                                       

      END                                                               

      SUBROUTINE LMDIF(FCN,M,N,X,FVEC,FTOL,XTOL,GTOL,MAXFEV,EPSFCN,     

     *                 DIAG,MODE,FACTOR,NPRINT,INFO,NFEV,FJAC,LDFJAC,   

     *                 IPVT,QTF,WA1,WA2,WA3,WA4)                        

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      DIMENSION IPVT(N)                                                 

C     REAL FTOL,XTOL,GTOL,EPSFCN,FACTOR                                 

      DIMENSION X(N),FVEC(M),DIAG(N),FJAC(LDFJAC,N),QTF(N),WA1(N),      

     *     WA2(N),WA3(N),WA4(M)                                         

      EXTERNAL FCN                                                      

C     **********                                                        

C                                                                       

C     SUBROUTINE LMDIF                                                  

C                                                                       

C     THE PURPOSE OF LMDIF IS TO MINIMIZE THE SUM OF THE SQUARES OF     

C     M NONLINEAR FUNCTIONS IN N VARIABLES BY A MODIFICATION OF         

C     THE LEVENBERG-MARQUARDT ALGORITHM. THE USER MUST PROVIDE A        

C     SUBROUTINE WHICH CALCULATES THE FUNCTIONS. THE JACOBIAN IS        

C     THEN CALCULATED BY A FORWARD-DIFFERENCE APPROXIMATION.            

C                                                                       

C     THE SUBROUTINE STATEMENT IS                                       

C                                                                       

C       SUBROUTINE LMDIF(FCN,M,N,X,FVEC,FTOL,XTOL,GTOL,MAXFEV,EPSFCN,   

C                        DIAG,MODE,FACTOR,NPRINT,INFO,NFEV,FJAC,        

C                        LDFJAC,IPVT,QTF,WA1,WA2,WA3,WA4)               

C                                                                       

C     WHERE                                                             

C                                                                       

C       FCN IS THE NAME OF THE USER-SUPPLIED SUBROUTINE WHICH           

C         CALCULATES THE FUNCTIONS. FCN MUST BE DECLARED                

C         IN AN EXTERNAL STATEMENT IN THE USER CALLING                  

C         PROGRAM, AND SHOULD BE WRITTEN AS FOLLOWS.                    

C                                                                       

C         SUBROUTINE FCN(M,N,X,FVEC,IFLAG)                              

C         INTEGER M,N,IFLAG                                             

C         REAL X(N),FVEC(M)                                             

C         ----------                                                    

C         CALCULATE THE FUNCTIONS AT X AND                              

C         RETURN THIS VECTOR IN FVEC.                                   

C         ----------                                                    

C         RETURN                                                        

C         END                                                           

C                                                                       

C         THE VALUE OF IFLAG SHOULD NOT BE CHANGED BY FCN UNLESS        

C         THE USER WANTS TO TERMINATE EXECUTION OF LMDIF.               

C         IN THIS CASE SET IFLAG TO A NEGATIVE INTEGER.                 

C                                                                       

C       M IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        

C         OF FUNCTIONS.                                                 

C                                                                       

C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        

C         OF VARIABLES. N MUST NOT EXCEED M.                            

C                                                                       

C       X IS AN ARRAY OF LENGTH N. ON INPUT X MUST CONTAIN              

C         AN INITIAL ESTIMATE OF THE SOLUTION VECTOR. ON OUTPUT X       

C         CONTAINS THE FINAL ESTIMATE OF THE SOLUTION VECTOR.           

C                                                                       

C       FVEC IS AN OUTPUT ARRAY OF LENGTH M WHICH CONTAINS              

C         THE FUNCTIONS EVALUATED AT THE OUTPUT X.                      

C                                                                       

C       FTOL IS A NONNEGATIVE INPUT VARIABLE. TERMINATION               

C         OCCURS WHEN BOTH THE ACTUAL AND PREDICTED RELATIVE            

C         REDUCTIONS IN THE SUM OF SQUARES ARE AT MOST FTOL.            

C         THEREFORE, FTOL MEASURES THE RELATIVE ERROR DESIRED           

C         IN THE SUM OF SQUARES.                                        

C                                                                       

C       XTOL IS A NONNEGATIVE INPUT VARIABLE. TERMINATION               

C         OCCURS WHEN THE RELATIVE ERROR BETWEEN TWO CONSECUTIVE        

C         ITERATES IS AT MOST XTOL. THEREFORE, XTOL MEASURES THE        

C         RELATIVE ERROR DESIRED IN THE APPROXIMATE SOLUTION.           

C                                                                       

C       GTOL IS A NONNEGATIVE INPUT VARIABLE. TERMINATION               

C         OCCURS WHEN THE COSINE OF THE ANGLE BETWEEN FVEC AND          

C         ANY COLUMN OF THE JACOBIAN IS AT MOST GTOL IN ABSOLUTE        

C         VALUE. THEREFORE, GTOL MEASURES THE ORTHOGONALITY             

C         DESIRED BETWEEN THE FUNCTION VECTOR AND THE COLUMNS           

C         OF THE JACOBIAN.                                              

C                                                                       

C       MAXFEV IS A POSITIVE INTEGER INPUT VARIABLE. TERMINATION        

C         OCCURS WHEN THE NUMBER OF CALLS TO FCN IS AT LEAST            

C         MAXFEV BY THE END OF AN ITERATION.                            

C                                                                       

C       EPSFCN IS AN INPUT VARIABLE USED IN DETERMINING A SUITABLE      

C         STEP LENGTH FOR THE FORWARD-DIFFERENCE APPROXIMATION. THIS    

C         APPROXIMATION ASSUMES THAT THE RELATIVE ERRORS IN THE         

C         FUNCTIONS ARE OF THE ORDER OF EPSFCN. IF EPSFCN IS LESS       

C         THAN THE MACHINE PRECISION, IT IS ASSUMED THAT THE RELATIVE   

C         ERRORS IN THE FUNCTIONS ARE OF THE ORDER OF THE MACHINE       

C         PRECISION.                                                    

C                                                                       

C       DIAG IS AN ARRAY OF LENGTH N. IF MODE = 1 (SEE                  

C         BELOW), DIAG IS INTERNALLY SET. IF MODE = 2, DIAG             

C         MUST CONTAIN POSITIVE ENTRIES THAT SERVE AS                   

C         MULTIPLICATIVE SCALE FACTORS FOR THE VARIABLES.               

C                                                                       

C       MODE IS AN INTEGER INPUT VARIABLE. IF MODE = 1, THE             

C         VARIABLES WILL BE SCALED INTERNALLY. IF MODE = 2,             

C         THE SCALING IS SPECIFIED BY THE INPUT DIAG. OTHER             

C         VALUES OF MODE ARE EQUIVALENT TO MODE = 1.                    

C                                                                       

C       FACTOR IS A POSITIVE INPUT VARIABLE USED IN DETERMINING THE     

C         INITIAL STEP BOUND. THIS BOUND IS SET TO THE PRODUCT OF       

C         FACTOR AND THE EUCLIDEAN NORM OF DIAG*X IF NONZERO, OR ELSE   

C         TO FACTOR ITSELF. IN MOST CASES FACTOR SHOULD LIE IN THE      

C         INTERVAL (.1,100.). 100. IS A GENERALLY RECOMMENDED VALUE.    

C                                                                       

C       NPRINT IS AN INTEGER INPUT VARIABLE THAT ENABLES CONTROLLED     

C         PRINTING OF ITERATES IF IT IS POSITIVE. IN THIS CASE,         

C         FCN IS CALLED WITH IFLAG = 0 AT THE BEGINNING OF THE FIRST    

C         ITERATION AND EVERY NPRINT ITERATIONS THEREAFTER AND          

C         IMMEDIATELY PRIOR TO RETURN, WITH X AND FVEC AVAILABLE        

C         FOR PRINTING. IF NPRINT IS NOT POSITIVE, NO SPECIAL CALLS     

C         OF FCN WITH IFLAG = 0 ARE MADE.                               

C                                                                       

C       INFO IS AN INTEGER OUTPUT VARIABLE. IF THE USER HAS             

C         TERMINATED EXECUTION, INFO IS SET TO THE (NEGATIVE)           

C         VALUE OF IFLAG. SEE DESCRIPTION OF FCN. OTHERWISE,            

C         INFO IS SET AS FOLLOWS.                                       

C                                                                       

C         INFO = 0  IMPROPER INPUT PARAMETERS.                          

C                                                                       

C         INFO = 1  BOTH ACTUAL AND PREDICTED RELATIVE REDUCTIONS       

C                   IN THE SUM OF SQUARES ARE AT MOST FTOL.             

C                                                                       

C         INFO = 2  RELATIVE ERROR BETWEEN TWO CONSECUTIVE ITERATES     

C                   IS AT MOST XTOL.                                    

C                                                                       

C         INFO = 3  CONDITIONS FOR INFO = 1 AND INFO = 2 BOTH HOLD.     

C                                                                       

C         INFO = 4  THE COSINE OF THE ANGLE BETWEEN FVEC AND ANY        

C                   COLUMN OF THE JACOBIAN IS AT MOST GTOL IN           

C                   ABSOLUTE VALUE.                                     

C                                                                       

C         INFO = 5  NUMBER OF CALLS TO FCN HAS REACHED OR               

C                   EXCEEDED MAXFEV.                                    

C                                                                       

C         INFO = 6  FTOL IS TOO SMALL. NO FURTHER REDUCTION IN          

C                   THE SUM OF SQUARES IS POSSIBLE.                     

C                                                                       

C         INFO = 7  XTOL IS TOO SMALL. NO FURTHER IMPROVEMENT IN        

C                   THE APPROXIMATE SOLUTION X IS POSSIBLE.             

C                                                                       

C         INFO = 8  GTOL IS TOO SMALL. FVEC IS ORTHOGONAL TO THE        

C                   COLUMNS OF THE JACOBIAN TO MACHINE PRECISION.       

C                                                                       

C       NFEV IS AN INTEGER OUTPUT VARIABLE SET TO THE NUMBER OF         

C         CALLS TO FCN.                                                 

C                                                                       

C       FJAC IS AN OUTPUT M BY N ARRAY. THE UPPER N BY N SUBMATRIX      

C         OF FJAC CONTAINS AN UPPER TRIANGULAR MATRIX R WITH            

C         DIAGONAL ELEMENTS OF NONINCREASING MAGNITUDE SUCH THAT        

C                                                                       

C                T     T           T                                    

C               P *(JAC *JAC)*P = R *R,                                 

C                                                                       

C         WHERE P IS A PERMUTATION MATRIX AND JAC IS THE FINAL          

C         CALCULATED JACOBIAN. COLUMN J OF P IS COLUMN IPVT(J)          

C         (SEE BELOW) OF THE IDENTITY MATRIX. THE LOWER TRAPEZOIDAL     

C         PART OF FJAC CONTAINS INFORMATION GENERATED DURING            

C         THE COMPUTATION OF R.                                         

C                                                                       

C       LDFJAC IS A POSITIVE INTEGER INPUT VARIABLE NOT LESS THAN M     

C         WHICH SPECIFIES THE LEADING DIMENSION OF THE ARRAY FJAC.      

C                                                                       

C       IPVT IS AN INTEGER OUTPUT ARRAY OF LENGTH N. IPVT               

C         DEFINES A PERMUTATION MATRIX P SUCH THAT JAC*P = Q*R,         

C         WHERE JAC IS THE FINAL CALCULATED JACOBIAN, Q IS              

C         ORTHOGONAL (NOT STORED), AND R IS UPPER TRIANGULAR            

C         WITH DIAGONAL ELEMENTS OF NONINCREASING MAGNITUDE.            

C         COLUMN J OF P IS COLUMN IPVT(J) OF THE IDENTITY MATRIX.       

C                                                                       

C       QTF IS AN OUTPUT ARRAY OF LENGTH N WHICH CONTAINS               

C         THE FIRST N ELEMENTS OF THE VECTOR (Q TRANSPOSE)*FVEC.        

C                                                                       

C       WA1, WA2, AND WA3 ARE WORK ARRAYS OF LENGTH N.                  

C                                                                       

C       WA4 IS A WORK ARRAY OF LENGTH M.                                

C                                                                       

C     SUBPROGRAMS CALLED                                                

C                                                                       

C       USER-SUPPLIED ...... FCN                                        

C                                                                       

C       MINPACK-SUPPLIED ... SPMPAR,ENORM,FDJAC2,LMPAR,QRFAC            

C                                                                       

C       FORTRAN-SUPPLIED ... ABS,AMAX1,AMIN1,SQRT,MOD                   

C                                                                       

C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.         

C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE             

C                                                                       

C     **********                                                        

C     REAL ACTRED,DELTA,DIRDER,EPSMCH,FNORM,FNORM1,GNORM,ONE,PAR,       

C    *     PNORM,PRERED,P1,P5,P25,P75,P0001,RATIO,SUM,TEMP,TEMP1,       

C    *     TEMP2,XNORM,ZERO                                             

C     REAL SPMPAR,ENORM                                                 

      DATA ONE,P1,P5,P25,P75,P0001,ZERO                                 

     *     /1.0D0,1.0D-1,5.0D-1,2.5D-1,7.5D-1,1.0D-4,0.0D0/             

C                                                                       

C     EPSMCH IS THE MACHINE PRECISION.                                  

C                                                                       

      EPSMCH = SPMPAR(1)                                                

C                                                                       

      INFO = 0                                                          

      IFLAG = 0                                                         

      NFEV = 0                                                          

C                                                                       

C     CHECK THE INPUT PARAMETERS FOR ERRORS.                            

C                                                                       

      IF (N .LE. 0 .OR. M .LT. N .OR. LDFJAC .LT. M                     

     *    .OR. FTOL .LT. ZERO .OR. XTOL .LT. ZERO .OR. GTOL .LT. ZERO   

     *    .OR. MAXFEV .LE. 0 .OR. FACTOR .LE. ZERO) GO TO 300           

      IF (MODE .NE. 2) GO TO 20                                         

      DO 10 J = 1, N                                                    

         IF (DIAG(J) .LE. ZERO) GO TO 300                               

   10    CONTINUE                                                       

   20 CONTINUE                                                          

C                                                                       

C     EVALUATE THE FUNCTION AT THE STARTING POINT                       

C     AND CALCULATE ITS NORM.                                           

C                                                                       

      IFLAG = 1                                                         

      CALL FCN(M,N,X,FVEC,IFLAG)                                        

      NFEV = 1                                                          

      IF (IFLAG .LT. 0) GO TO 300                                       

      FNORM = ENORM(M,FVEC)                                             

C                                                                       

C     INITIALIZE LEVENBERG-MARQUARDT PARAMETER AND ITERATION COUNTER.   

C                                                                       

      PAR = ZERO                                                        

      ITER = 1                                                          

C                                                                       

C     BEGINNING OF THE OUTER LOOP.                                      

C                                                                       

   30 CONTINUE                                                          

C                                                                       

C        CALCULATE THE JACOBIAN MATRIX.                                 

C                                                                       

         IFLAG = 2                                                      

         CALL FDJAC2(FCN,M,N,X,FVEC,FJAC,LDFJAC,IFLAG,EPSFCN,WA4)       

         NFEV = NFEV + N                                                

         IF (IFLAG .LT. 0) GO TO 300                                    

C                                                                       

C        IF REQUESTED, CALL FCN TO ENABLE PRINTING OF ITERATES.         

C                                                                       

         IF (NPRINT .LE. 0) GO TO 40                                    

         IFLAG = 0                                                      

         IF (MOD(ITER-1,NPRINT) .EQ. 0) CALL FCN(M,N,X,FVEC,IFLAG)      

         IF (IFLAG .LT. 0) GO TO 300                                    

   40    CONTINUE                                                       

C                                                                       

C        COMPUTE THE QR FACTORIZATION OF THE JACOBIAN.                  

C                                                                       

         CALL QRFAC(M,N,FJAC,LDFJAC,.TRUE.,IPVT,N,WA1,WA2,WA3)          

C                                                                       

C        ON THE FIRST ITERATION AND IF MODE IS 1, SCALE ACCORDING       

C        TO THE NORMS OF THE COLUMNS OF THE INITIAL JACOBIAN.           

C                                                                       

         IF (ITER .NE. 1) GO TO 80                                      

         IF (MODE .EQ. 2) GO TO 60                                      

         DO 50 J = 1, N                                                 

            DIAG(J) = WA2(J)                                            

            IF (WA2(J) .EQ. ZERO) DIAG(J) = ONE                         

   50       CONTINUE                                                    

   60    CONTINUE                                                       

C                                                                       

C        ON THE FIRST ITERATION, CALCULATE THE NORM OF THE SCALED X     

C        AND INITIALIZE THE STEP BOUND DELTA.                           

C                                                                       

         DO 70 J = 1, N                                                 

            WA3(J) = DIAG(J)*X(J)                                       

   70       CONTINUE                                                    

         XNORM = ENORM(N,WA3)                                           

         DELTA = FACTOR*XNORM                                           

         IF (DELTA .EQ. ZERO) DELTA = FACTOR                            

   80    CONTINUE                                                       

C                                                                       

C        FORM (Q TRANSPOSE)*FVEC AND STORE THE FIRST N COMPONENTS IN    

C        QTF.                                                           

C                                                                       

         DO 90 I = 1, M                                                 

            WA4(I) = FVEC(I)                                            

   90       CONTINUE                                                    

         DO 130 J = 1, N                                                

            IF (FJAC(J,J) .EQ. ZERO) GO TO 120                          

            SUM = ZERO                                                  

            DO 100 I = J, M                                             

               SUM = SUM + FJAC(I,J)*WA4(I)                             

  100          CONTINUE                                                 

            TEMP = -SUM/FJAC(J,J)                                       

            DO 110 I = J, M                                             

               WA4(I) = WA4(I) + FJAC(I,J)*TEMP                         

  110          CONTINUE                                                 

  120       CONTINUE                                                    

            FJAC(J,J) = WA1(J)                                          

            QTF(J) = WA4(J)                                             

  130       CONTINUE                                                    

C                                                                       

C        COMPUTE THE NORM OF THE SCALED GRADIENT.                       

C                                                                       

         GNORM = ZERO                                                   

         IF (FNORM .EQ. ZERO) GO TO 170                                 

         DO 160 J = 1, N                                                

            L = IPVT(J)                                                 

            IF (WA2(L) .EQ. ZERO) GO TO 150                             

            SUM = ZERO                                                  

            DO 140 I = 1, J                                             

               SUM = SUM + FJAC(I,J)*(QTF(I)/FNORM)                     

  140          CONTINUE                                                 

            GNORM = MAX(GNORM,ABS(SUM/WA2(L)))                          

  150       CONTINUE                                                    

  160       CONTINUE                                                    

  170    CONTINUE                                                       

C                                                                       

C        TEST FOR CONVERGENCE OF THE GRADIENT NORM.                     

C                                                                       

         IF (GNORM .LE. GTOL) INFO = 4                                  

         IF (INFO .NE. 0) GO TO 300                                     

C                                                                       

C        RESCALE IF NECESSARY.                                          

C                                                                       

         IF (MODE .EQ. 2) GO TO 190                                     

         DO 180 J = 1, N                                                

            DIAG(J) = MAX(DIAG(J),WA2(J))                               

  180       CONTINUE                                                    

  190    CONTINUE                                                       

C                                                                       

C        BEGINNING OF THE INNER LOOP.                                   

C                                                                       

  200    CONTINUE                                                       

C                                                                       

C           DETERMINE THE LEVENBERG-MARQUARDT PARAMETER.                

C                                                                       

            CALL LMPAR(N,FJAC,LDFJAC,IPVT,DIAG,QTF,DELTA,PAR,WA1,WA2,   

     *                 WA3,WA4)                                         

C                                                                       

C           STORE THE DIRECTION P AND X + P. CALCULATE THE NORM OF P.   

C                                                                       

            DO 210 J = 1, N                                             

               WA1(J) = -WA1(J)                                         

               WA2(J) = X(J) + WA1(J)                                   

               WA3(J) = DIAG(J)*WA1(J)                                  

  210          CONTINUE                                                 

            PNORM = ENORM(N,WA3)                                        

C                                                                       

C           ON THE FIRST ITERATION, ADJUST THE INITIAL STEP BOUND.      

C                                                                       

            IF (ITER .EQ. 1) DELTA = MIN(DELTA,PNORM)                   

C                                                                       

C           EVALUATE THE FUNCTION AT X + P AND CALCULATE ITS NORM.      

C                                                                       

            IFLAG = 1                                                   

            CALL FCN(M,N,WA2,WA4,IFLAG)                                 

            NFEV = NFEV + 1                                             

            IF (IFLAG .LT. 0) GO TO 300                                 

            FNORM1 = ENORM(M,WA4)                                       

C                                                                       

C           COMPUTE THE SCALED ACTUAL REDUCTION.                        

C                                                                       

            ACTRED = -ONE                                               

            IF (P1*FNORM1 .LT. FNORM) ACTRED = ONE - (FNORM1/FNORM)**2  

C                                                                       

C           COMPUTE THE SCALED PREDICTED REDUCTION AND                  

C           THE SCALED DIRECTIONAL DERIVATIVE.                          

C                                                                       

            DO 230 J = 1, N                                             

               WA3(J) = ZERO                                            

               L = IPVT(J)                                              

               TEMP = WA1(L)                                            

               DO 220 I = 1, J                                          

                  WA3(I) = WA3(I) + FJAC(I,J)*TEMP                      

  220             CONTINUE                                              

  230          CONTINUE                                                 

            TEMP1 = ENORM(N,WA3)/FNORM                                  

            TEMP2 = (SQRT(PAR)*PNORM)/FNORM                             

            PRERED = TEMP1**2 + TEMP2**2/P5                             

            DIRDER = -(TEMP1**2 + TEMP2**2)                             

C                                                                       

C           COMPUTE THE RATIO OF THE ACTUAL TO THE PREDICTED            

C           REDUCTION.                                                  

C                                                                       

            RATIO = ZERO                                                

            IF (PRERED .NE. ZERO) RATIO = ACTRED/PRERED                 

C                                                                       

C           UPDATE THE STEP BOUND.                                      

C                                                                       

            IF (RATIO .GT. P25) GO TO 240                               

               IF (ACTRED .GE. ZERO) TEMP = P5                          

               IF (ACTRED .LT. ZERO)                                    

     *            TEMP = P5*DIRDER/(DIRDER + P5*ACTRED)                 

               IF (P1*FNORM1 .GE. FNORM .OR. TEMP .LT. P1) TEMP = P1    

               DELTA = TEMP*MIN(DELTA,PNORM/P1)                         

               PAR = PAR/TEMP                                           

               GO TO 260                                                

  240       CONTINUE                                                    

               IF (PAR .NE. ZERO .AND. RATIO .LT. P75) GO TO 250        

               DELTA = PNORM/P5                                         

               PAR = P5*PAR                                             

  250          CONTINUE                                                 

  260       CONTINUE                                                    

C                                                                       

C           TEST FOR SUCCESSFUL ITERATION.                              

C                                                                       

            IF (RATIO .LT. P0001) GO TO 290                             

C                                                                       

C           SUCCESSFUL ITERATION. UPDATE X, FVEC, AND THEIR NORMS.      

C                                                                       

            DO 270 J = 1, N                                             

               X(J) = WA2(J)                                            

               WA2(J) = DIAG(J)*X(J)                                    

  270          CONTINUE                                                 

            DO 280 I = 1, M                                             

               FVEC(I) = WA4(I)                                         

  280          CONTINUE                                                 

            XNORM = ENORM(N,WA2)                                        

            FNORM = FNORM1                                              

            ITER = ITER + 1                                             

  290       CONTINUE                                                    

C                                                                       

C           TESTS FOR CONVERGENCE.                                      

C                                                                       

            IF (ABS(ACTRED) .LE. FTOL .AND. PRERED .LE. FTOL            

     *          .AND. P5*RATIO .LE. ONE) INFO = 1                       

            IF (DELTA .LE. XTOL*XNORM) INFO = 2                         

            IF (ABS(ACTRED) .LE. FTOL .AND. PRERED .LE. FTOL            

     *          .AND. P5*RATIO .LE. ONE .AND. INFO .EQ. 2) INFO = 3     

            IF (INFO .NE. 0) GO TO 300                                  

C                                                                       

C           TESTS FOR TERMINATION AND STRINGENT TOLERANCES.             

C                                                                       

            IF (NFEV .GE. MAXFEV) INFO = 5                              

            IF (ABS(ACTRED) .LE. EPSMCH .AND. PRERED .LE. EPSMCH        

     *          .AND. P5*RATIO .LE. ONE) INFO = 6                       

            IF (DELTA .LE. EPSMCH*XNORM) INFO = 7                       

            IF (GNORM .LE. EPSMCH) INFO = 8                             

            IF (INFO .NE. 0) GO TO 300                                  

C                                                                       

C           END OF THE INNER LOOP. REPEAT IF ITERATION UNSUCCESSFUL.    

C                                                                       

            IF (RATIO .LT. P0001) GO TO 200                             

C                                                                       

C        END OF THE OUTER LOOP.                                         

C                                                                       

         GO TO 30                                                       

  300 CONTINUE                                                          

C                                                                       

C     TERMINATION, EITHER NORMAL OR USER IMPOSED.                       

C                                                                       

      IF (IFLAG .LT. 0) INFO = IFLAG                                    

      IFLAG = 0                                                         

      IF (NPRINT .GT. 0) CALL FCN(M,N,X,FVEC,IFLAG)                     

      RETURN                                                            

C                                                                       

C     LAST CARD OF SUBROUTINE LMDIF.                                    

C                                                                       

      END                                                               

      SUBROUTINE LMDIF1(FCN,M,N,X,FVEC,TOL,INFO,IWA,WA,LWA)             

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      DIMENSION IWA(N)                                                  

      DIMENSION X(N),FVEC(M),WA(LWA)                                    

      EXTERNAL FCN                                                      

C     **********                                                        

C                                                                       

C     SUBROUTINE LMDIF1                                                 

C                                                                       

C     THE PURPOSE OF LMDIF1 IS TO MINIMIZE THE SUM OF THE SQUARES OF    

C     M NONLINEAR FUNCTIONS IN N VARIABLES BY A MODIFICATION OF THE     

C     LEVENBERG-MARQUARDT ALGORITHM. THIS IS DONE BY USING THE MORE     

C     GENERAL LEAST-SQUARES SOLVER LMDIF. THE USER MUST PROVIDE A       

C     SUBROUTINE WHICH CALCULATES THE FUNCTIONS. THE JACOBIAN IS        

C     THEN CALCULATED BY A FORWARD-DIFFERENCE APPROXIMATION.            

C                                                                       

C     THE SUBROUTINE STATEMENT IS                                       

C                                                                       

C       SUBROUTINE LMDIF1(FCN,M,N,X,FVEC,TOL,INFO,IWA,WA,LWA)           

C                                                                       

C     WHERE                                                             

C                                                                       

C       FCN IS THE NAME OF THE USER-SUPPLIED SUBROUTINE WHICH           

C         CALCULATES THE FUNCTIONS. FCN MUST BE DECLARED                

C         IN AN EXTERNAL STATEMENT IN THE USER CALLING                  

C         PROGRAM, AND SHOULD BE WRITTEN AS FOLLOWS.                    

C                                                                       

C         SUBROUTINE FCN(M,N,X,FVEC,IFLAG)                              

C         INTEGER M,N,IFLAG                                             

C         REAL X(N),FVEC(M)                                             

C         ----------                                                    

C         CALCULATE THE FUNCTIONS AT X AND                              

C         RETURN THIS VECTOR IN FVEC.                                   

C         ----------                                                    

C         RETURN                                                        

C         END                                                           

C                                                                       

C         THE VALUE OF IFLAG SHOULD NOT BE CHANGED BY FCN UNLESS        

C         THE USER WANTS TO TERMINATE EXECUTION OF LMDIF1.              

C         IN THIS CASE SET IFLAG TO A NEGATIVE INTEGER.                 

C                                                                       

C       M IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        

C         OF FUNCTIONS.                                                 

C                                                                       

C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        

C         OF VARIABLES. N MUST NOT EXCEED M.                            

C                                                                       

C       X IS AN ARRAY OF LENGTH N. ON INPUT X MUST CONTAIN              

C         AN INITIAL ESTIMATE OF THE SOLUTION VECTOR. ON OUTPUT X       

C         CONTAINS THE FINAL ESTIMATE OF THE SOLUTION VECTOR.           

C                                                                       

C       FVEC IS AN OUTPUT ARRAY OF LENGTH M WHICH CONTAINS              

C         THE FUNCTIONS EVALUATED AT THE OUTPUT X.                      

C                                                                       

C       TOL IS A NONNEGATIVE INPUT VARIABLE. TERMINATION OCCURS         

C         WHEN THE ALGORITHM ESTIMATES EITHER THAT THE RELATIVE         

C         ERROR IN THE SUM OF SQUARES IS AT MOST TOL OR THAT            

C         THE RELATIVE ERROR BETWEEN X AND THE SOLUTION IS AT           

C         MOST TOL.                                                     

C                                                                       

C       INFO IS AN INTEGER OUTPUT VARIABLE. IF THE USER HAS             

C         TERMINATED EXECUTION, INFO IS SET TO THE (NEGATIVE)           

C         VALUE OF IFLAG. SEE DESCRIPTION OF FCN. OTHERWISE,            

C         INFO IS SET AS FOLLOWS.                                       

C                                                                       

C         INFO = 0  IMPROPER INPUT PARAMETERS.                          

C                                                                       

C         INFO = 1  ALGORITHM ESTIMATES THAT THE RELATIVE ERROR         

C                   IN THE SUM OF SQUARES IS AT MOST TOL.               

C                                                                       

C         INFO = 2  ALGORITHM ESTIMATES THAT THE RELATIVE ERROR         

C                   BETWEEN X AND THE SOLUTION IS AT MOST TOL.          

C                                                                       

C         INFO = 3  CONDITIONS FOR INFO = 1 AND INFO = 2 BOTH HOLD.     

C                                                                       

C         INFO = 4  FVEC IS ORTHOGONAL TO THE COLUMNS OF THE            

C                   JACOBIAN TO MACHINE PRECISION.                      

C                                                                       

C         INFO = 5  NUMBER OF CALLS TO FCN HAS REACHED OR               

C                   EXCEEDED 200*(N+1).                                 

C                                                                       

C         INFO = 6  TOL IS TOO SMALL. NO FURTHER REDUCTION IN           

C                   THE SUM OF SQUARES IS POSSIBLE.                     

C                                                                       

C         INFO = 7  TOL IS TOO SMALL. NO FURTHER IMPROVEMENT IN         

C                   THE APPROXIMATE SOLUTION X IS POSSIBLE.             

C                                                                       

C       IWA IS AN INTEGER WORK ARRAY OF LENGTH N.                       

C                                                                       

C       WA IS A WORK ARRAY OF LENGTH LWA.                               

C                                                                       

C       LWA IS A POSITIVE INTEGER INPUT VARIABLE NOT LESS THAN          

C         M*N+5*N+M.                                                    

C                                                                       

C     SUBPROGRAMS CALLED                                                

C                                                                       

C       USER-SUPPLIED ...... FCN                                        

C                                                                       

C       MINPACK-SUPPLIED ... LMDIF                                      

C                                                                       

C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.         

C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE             

C                                                                       

C     **********                                                        

      INTEGER MAXFEV,MODE,MP5N,NFEV,NPRINT                              

      REAL EPSFCN,FACTOR,FTOL,GTOL,XTOL,ZERO                            

      DATA FACTOR,ZERO /1.0D2,0.0D0/                                    

      INFO = 0                                                          

C                                                                       

C     CHECK THE INPUT PARAMETERS FOR ERRORS.                            

C                                                                       

      IF (N .LE. 0 .OR. M .LT. N .OR. TOL .LT. ZERO                     

     *    .OR. LWA .LT. M*N + 5*N + M) GO TO 10                         

C                                                                       

C     CALL LMDIF.                                                       

C                                                                       

      MAXFEV = 200*(N + 1)                                              

      FTOL = TOL                                                        

      XTOL = TOL                                                        

      GTOL = ZERO                                                       

      EPSFCN = ZERO                                                     

      MODE = 1                                                          

      NPRINT = 0                                                        

      MP5N = M + 5*N                                                    
c      CALL LMDIF(FCN,M,N,X,FVEC,FTOL,XTOL,GTOL,MAXFEV,EPSFCN,WA(1),     

c     *           MODE,FACTOR,NPRINT,INFO,NFEV,WA(MP5N+1),M,IWA,         

c     *           WA(N+1),WA(2*N+1),WA(3*N+1),WA(4*N+1),WA(5*N+1))       

      IF (INFO .EQ. 8) INFO = 4                                         

   10 CONTINUE                                                          

      RETURN                                                            

C                                                                       

C     LAST CARD OF SUBROUTINE LMDIF1.                                   

C                                                                       

      END                                                               

      SUBROUTINE DOGLEG(N,R,LR,DIAG,QTB,DELTA,X,WA1,WA2)                

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

C     REAL DELTA                                                        

      DIMENSION R(LR),DIAG(N),QTB(N),X(N),WA1(N),WA2(N)                 

C     **********                                                        

C                                                                       

C     SUBROUTINE DOGLEG                                                 

C                                                                       

C     GIVEN AN M BY N MATRIX A, AN N BY N NONSINGULAR DIAGONAL          

C     MATRIX D, AN M-VECTOR B, AND A POSITIVE NUMBER DELTA, THE         

C     PROBLEM IS TO DETERMINE THE CONVEX COMBINATION X OF THE           

C     GAUSS-NEWTON AND SCALED GRADIENT DIRECTIONS THAT MINIMIZES        

C     (A*X - B) IN THE LEAST SQUARES SENSE, SUBJECT TO THE              

C     RESTRICTION THAT THE EUCLIDEAN NORM OF D*X BE AT MOST DELTA.      

C                                                                       

C     THIS SUBROUTINE COMPLETES THE SOLUTION OF THE PROBLEM             

C     IF IT IS PROVIDED WITH THE NECESSARY INFORMATION FROM THE         

C     QR FACTORIZATION OF A. THAT IS, IF A = Q*R, WHERE Q HAS           

C     ORTHOGONAL COLUMNS AND R IS AN UPPER TRIANGULAR MATRIX,           

C     THEN DOGLEG EXPECTS THE FULL UPPER TRIANGLE OF R AND              

C     THE FIRST N COMPONENTS OF (Q TRANSPOSE)*B.                        

C                                                                       

C     THE SUBROUTINE STATEMENT IS                                       

C                                                                       

C       SUBROUTINE DOGLEG(N,R,LR,DIAG,QTB,DELTA,X,WA1,WA2)              

C                                                                       

C     WHERE                                                             

C                                                                       

C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE ORDER OF R.   

C                                                                       

C       R IS AN INPUT ARRAY OF LENGTH LR WHICH MUST CONTAIN THE UPPER   

C         TRIANGULAR MATRIX R STORED BY ROWS.                           

C                                                                       

C       LR IS A POSITIVE INTEGER INPUT VARIABLE NOT LESS THAN           

C         (N*(N+1))/2.                                                  

C                                                                       

C       DIAG IS AN INPUT ARRAY OF LENGTH N WHICH MUST CONTAIN THE       

C         DIAGONAL ELEMENTS OF THE MATRIX D.                            

C                                                                       

C       QTB IS AN INPUT ARRAY OF LENGTH N WHICH MUST CONTAIN THE FIRST  

C         N ELEMENTS OF THE VECTOR (Q TRANSPOSE)*B.                     

C                                                                       

C       DELTA IS A POSITIVE INPUT VARIABLE WHICH SPECIFIES AN UPPER     

C         BOUND ON THE EUCLIDEAN NORM OF D*X.                           

C                                                                       

C       X IS AN OUTPUT ARRAY OF LENGTH N WHICH CONTAINS THE DESIRED     

C         CONVEX COMBINATION OF THE GAUSS-NEWTON DIRECTION AND THE      

C         SCALED GRADIENT DIRECTION.                                    

C                                                                       

C       WA1 AND WA2 ARE WORK ARRAYS OF LENGTH N.                        

C                                                                       

C     SUBPROGRAMS CALLED                                                

C                                                                       

C       MINPACK-SUPPLIED ... SPMPAR,ENORM                               

C                                                                       

C       FORTRAN-SUPPLIED ... ABS,AMAX1,AMIN1,SQRT                       

C                                                                       

C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.         

C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE             

C                                                                       

C     **********                                                        

      DATA ONE,ZERO /1.0D0,0.0D0/                                       

C                                                                       

C     EPSMCH IS THE MACHINE PRECISION.                                  

C                                                                       

      EPSMCH = SPMPAR(1)                                                

C                                                                       

C     FIRST, CALCULATE THE GAUSS-NEWTON DIRECTION.                      

C                                                                       

      JJ = (N*(N + 1))/2 + 1                                            

      DO 50 K = 1, N                                                    

         J = N - K + 1                                                  

         JP1 = J + 1                                                    

         JJ = JJ - K                                                    

         L = JJ + 1                                                     

         SUM = ZERO                                                     

         IF (N .LT. JP1) GO TO 20                                       

         DO 10 I = JP1, N                                               

            SUM = SUM + R(L)*X(I)                                       

            L = L + 1                                                   

   10       CONTINUE                                                    

   20    CONTINUE                                                       

         TEMP = R(JJ)                                                   

         IF (TEMP .NE. ZERO) GO TO 40                                   

         L = J                                                          

         DO 30 I = 1, J                                                 

            TEMP = MAX(TEMP,ABS(R(L)))                                  

            L = L + N - I                                               

   30       CONTINUE                                                    

         TEMP = EPSMCH*TEMP                                             

         IF (TEMP .EQ. ZERO) TEMP = EPSMCH                              

   40    CONTINUE                                                       

         X(J) = (QTB(J) - SUM)/TEMP                                     

   50    CONTINUE                                                       

C                                                                       

C     TEST WHETHER THE GAUSS-NEWTON DIRECTION IS ACCEPTABLE.            

C                                                                       

      DO 60 J = 1, N                                                    

         WA1(J) = ZERO                                                  

         WA2(J) = DIAG(J)*X(J)                                          

   60    CONTINUE                                                       

      QNORM = ENORM(N,WA2)                                              

      IF (QNORM .LE. DELTA) GO TO 140                                   

C                                                                       

C     THE GAUSS-NEWTON DIRECTION IS NOT ACCEPTABLE.                     

C     NEXT, CALCULATE THE SCALED GRADIENT DIRECTION.                    

C                                                                       

      L = 1                                                             

      DO 80 J = 1, N                                                    

         TEMP = QTB(J)                                                  

         DO 70 I = J, N                                                 

            WA1(I) = WA1(I) + R(L)*TEMP                                 

            L = L + 1                                                   

   70       CONTINUE                                                    

         WA1(J) = WA1(J)/DIAG(J)                                        

   80    CONTINUE                                                       

C                                                                       

C     CALCULATE THE NORM OF THE SCALED GRADIENT AND TEST FOR            

C     THE SPECIAL CASE IN WHICH THE SCALED GRADIENT IS ZERO.            

C                                                                       

      GNORM = ENORM(N,WA1)                                              

      SGNORM = ZERO                                                     

      ALPHA = DELTA/QNORM                                               

      IF (GNORM .EQ. ZERO) GO TO 120                                    

C                                                                       

C     CALCULATE THE POINT ALONG THE SCALED GRADIENT                     

C     AT WHICH THE QUADRATIC IS MINIMIZED.                              

C                                                                       

      DO 90 J = 1, N                                                    

         WA1(J) = (WA1(J)/GNORM)/DIAG(J)                                

   90    CONTINUE                                                       

      L = 1                                                             

      DO 110 J = 1, N                                                   

         SUM = ZERO                                                     

         DO 100 I = J, N                                                

            SUM = SUM + R(L)*WA1(I)                                     

            L = L + 1                                                   

  100       CONTINUE                                                    

         WA2(J) = SUM                                                   

  110    CONTINUE                                                       

      TEMP = ENORM(N,WA2)                                               

      SGNORM = (GNORM/TEMP)/TEMP                                        

C                                                                       

C     TEST WHETHER THE SCALED GRADIENT DIRECTION IS ACCEPTABLE.         

C                                                                       

      ALPHA = ZERO                                                      

      IF (SGNORM .GE. DELTA) GO TO 120                                  

C                                                                       

C     THE SCALED GRADIENT DIRECTION IS NOT ACCEPTABLE.                  

C     FINALLY, CALCULATE THE POINT ALONG THE DOGLEG                     

C     AT WHICH THE QUADRATIC IS MINIMIZED.                              

C                                                                       

      BNORM = ENORM(N,QTB)                                              

      TEMP = (BNORM/GNORM)*(BNORM/QNORM)*(SGNORM/DELTA)                 

      TEMP = TEMP - (DELTA/QNORM)*(SGNORM/DELTA)**2                     

     *       + SQRT((TEMP-(DELTA/QNORM))**2                             

     *              +(ONE-(DELTA/QNORM)**2)*(ONE-(SGNORM/DELTA)**2))    

      ALPHA = ((DELTA/QNORM)*(ONE - (SGNORM/DELTA)**2))/TEMP            

  120 CONTINUE                                                          

C                                                                       

C     FORM APPROPRIATE CONVEX COMBINATION OF THE GAUSS-NEWTON           

C     DIRECTION AND THE SCALED GRADIENT DIRECTION.                      

C                                                                       

      TEMP = (ONE - ALPHA)*MIN(SGNORM,DELTA)                            

      DO 130 J = 1, N                                                   

         X(J) = TEMP*WA1(J) + ALPHA*X(J)                                

  130    CONTINUE                                                       

  140 CONTINUE                                                          

      RETURN                                                            

C                                                                       

C     LAST CARD OF SUBROUTINE DOGLEG.                                   

C                                                                       

      END                                                               

CDC   FUNCTION SPMPAR(I)                                                

      DOUBLE PRECISION FUNCTION SPMPAR(I)                               

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

C     **********                                                        

C                                                                       

C     FUNCTION SPMPAR                                                   

C                                                                       

C     THIS FUNCTION PROVIDES SINGLE PRECISION MACHINE PARAMETERS        

C     WHEN THE APPROPRIATE SET OF DATA STATEMENTS IS ACTIVATED (BY      

C     REMOVING THE C FROM COLUMN 1) AND ALL OTHER DATA STATEMENTS ARE   

C     RENDERED INACTIVE. MOST OF THE PARAMETER VALUES WERE OBTAINED     

C     FROM THE CORRESPONDING BELL LABORATORIES PORT LIBRARY FUNCTION.   

C                                                                       

C     THE FUNCTION STATEMENT IS                                         

C                                                                       

C       REAL FUNCTION SPMPAR(I)                                         

C                                                                       

C     WHERE                                                             

C                                                                       

C       I IS AN INTEGER INPUT VARIABLE SET TO 1, 2, OR 3 WHICH          

C         SELECTS THE DESIRED MACHINE PARAMETER. IF THE MACHINE HAS     

C         T BASE B DIGITS AND ITS SMALLEST AND LARGEST EXPONENTS ARE    

C         EMIN AND EMAX, RESPECTIVELY, THEN THESE PARAMETERS ARE        

C                                                                       

C         SPMPAR(1) = B**(1 - T), THE MACHINE PRECISION,                

C                                                                       

C         SPMPAR(2) = B**(EMIN - 1), THE SMALLEST MAGNITUDE,            

C                                                                       

C         SPMPAR(3) = B**EMAX*(1 - B**(-T)), THE LARGEST MAGNITUDE.     

C                                                                       

C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.         

C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE             

C                                                                       

C     **********                                                        

      INTEGER MCHEPS(4)                                                 

      INTEGER MINMAG(4)                                                 

      INTEGER MAXMAG(4)                                                 

      DIMENSION RMACH(3)                                                

      EQUIVALENCE (RMACH(1),MCHEPS(1))                                  

      EQUIVALENCE (RMACH(2),MINMAG(1))                                  

      EQUIVALENCE (RMACH(3),MAXMAG(1))                                  

C                                                                       

C     MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,                     

C     THE AMDAHL 470/V6, THE ICL 2900, THE ITEL AS/6,                   

C     THE XEROX SIGMA 5/7/9 AND THE SEL SYSTEMS 85/86.                  

C                                                                       

C     DATA RMACH(1) / Z3C100000 /                                       

C     DATA RMACH(2) / Z00100000 /                                       

C     DATA RMACH(3) / Z7FFFFFFF /                                       

C                                                                       

C     DOUBLE PRECISION MACHINE CONSTANTS FOR THE IBM 360/370 SERIES,    

C                                                                       

C     DATA RMACH(1) / Z3410000000000000 /                               

C     DATA RMACH(2) / Z0010000000000000 /                               

C     DATA RMACH(3) / Z7FFFFFFFFFFFFFFF /                               

C                                                                       

C     MACHINE CONSTANTS FOR THE HONEYWELL 600/6000 SERIES.              

C                                                                       

C     DATA RMACH(1) / O716400000000 /                                   

C     DATA RMACH(2) / O402400000000 /                                   

C     DATA RMACH(3) / O376777777777 /                                   

C                                                                       

C     MACHINE CONSTANTS FOR THE CDC 6000/7000 SERIES.                   

C                                                                       

C     DATA RMACH(1) / 16414000000000000000B /                           

C     DATA RMACH(2) / 00014000000000000000B /                           

C     DATA RMACH(3) / 37767777777777777777B /                           

C     THE FOLLOWING VERSION FOR FTN5 ON THE CDC7600.                    

CDC   DATA RMACH(1) / O"16414000000000000000" /                         

CDC   DATA RMACH(2) / O"00014000000000000000" /                         

CDC   DATA RMACH(3) / O"37767777777777777777" /                         

C                                                                       

C     MACHINE CONSTANTS FOR THE PDP-10 (KA OR KI PROCESSOR).            

C                                                                       

C     DATA RMACH(1) / "147400000000 /                                   

C     DATA RMACH(2) / "000400000000 /                                   

C     DATA RMACH(3) / "377777777777 /                                   

C                                                                       

C     MACHINE CONSTANTS FOR THE PDP-11 FORTRAN SUPPORTING               

C     32-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).                 

C                                                                       

C     DATA MCHEPS(1) /  889192448 /                                     

C     DATA MINMAG(1) /    8388608 /                                     

C     DATA MAXMAG(1) / 2147483647 /                                     

C                                                                       

C     DATA RMACH(1) / O06500000000 /                                    

C     DATA RMACH(2) / O00040000000 /                                    

C     DATA RMACH(3) / O17777777777 /                                    

C                                                                       

C     MACHINE CONSTANTS FOR THE PDP-11 FORTRAN SUPPORTING               

C     16-BIT INTEGERS (EXPRESSED IN INTEGER AND OCTAL).                 

C                                                                       

C     DATA MCHEPS(1),MCHEPS(2) / 13568,     0 /                         

C     DATA MINMAG(1),MINMAG(2) /   128,     0 /                         

C     DATA MAXMAG(1),MAXMAG(2) / 32767,    -1 /                         

C                                                                       

C     DATA MCHEPS(1),MCHEPS(2) / O032400, O000000 /                     

C     DATA MINMAG(1),MINMAG(2) / O000200, O000000 /                     

C     DATA MAXMAG(1),MAXMAG(2) / O077777, O177777 /                     

C                                                                       

C     MACHINE CONSTANTS FOR THE BURROUGHS 5700/6700/7700 SYSTEMS.       

C                                                                       

C     DATA RMACH(1) / O1301000000000000 /                               

C     DATA RMACH(2) / O1771000000000000 /                               

C     DATA RMACH(3) / O0777777777777777 /                               

C                                                                       

C     MACHINE CONSTANTS FOR THE BURROUGHS 1700 SYSTEM.                  

C                                                                       

C     DATA RMACH(1) / Z4EA800000 /                                      

C     DATA RMACH(2) / Z400800000 /                                      

C     DATA RMACH(3) / Z5FFFFFFFF /                                      

C                                                                       

C     MACHINE CONSTANTS FOR THE UNIVAC 1100 SERIES.                     

C                                                                       

C     DATA RMACH(1) / O147400000000 /                                   

C     DATA RMACH(2) / O000400000000 /                                   

C     DATA RMACH(3) / O377777777777 /                                   

C                                                                       

C     MACHINE CONSTANTS FOR THE DATA GENERAL ECLIPSE S/200.             

C                                                                       

C     NOTE - IT MAY BE APPROPRIATE TO INCLUDE THE FOLLOWING CARD -      

C     STATIC RMACH(3)                                                   

C                                                                       

C     DATA MINMAG/20K,0/,MAXMAG/77777K,177777K/                         

C     DATA MCHEPS/36020K,0/                                             

C                                                                       

C     MACHINE CONSTANTS FOR THE HARRIS 220.                             

C                                                                       

C     DATA MCHEPS(1) / '20000000, '00000353 /                           

C     DATA MINMAG(1) / '20000000, '00000201 /                           

C     DATA MAXMAG(1) / '37777777, '00000177 /                           

C                                                                       

C     MACHINE CONSTANTS FOR THE CRAY-1.                                 

C                                                                       

C     DATA RMACH(1) / 0377224000000000000000B /                         

C     DATA RMACH(2) / 0200034000000000000000B /                         

C     DATA RMACH(3) / 0577777777777777777776B /                         

C                                                                       

C     MACHINE CONSTANTS FOR THE PRIME 400.                              

C                                                                       

C     DATA MCHEPS(1) / :10000000153 /                                   

C     DATA MINMAG(1) / :10000000000 /                                   

C     DATA MAXMAG(1) / :17777777777 /                                   

C                                                                       

C     SPMPAR = RMACH(I)                                                 

C.....This version for the Liverpool Sun/Unix system, February 1993.    

C      IF ( I .EQ. 1 ) THEN                                              

C      SPMPAR = 1.1102230246252D-16                                      

C      ELSE IF ( I .EQ. 2 ) THEN                                         

C      SPMPAR = 2.22507385850721D-308                                     

C      ELSE IF ( I .EQ. 3 ) THEN                                         

C      SPMPAR = 1.7976931348623D+308                                     

C      ELSE                                                              

C      WRITE(*,*) 'WRONG ARGUMENT FOR SPMPAR'                            

C      END IF                                                            

C      RETURN                                                            

C.....This version for the Cardiff Unix system, March 2004.    

      IF ( I .EQ. 1 ) THEN                                              

      SPMPAR = 1.1102230246252D-16                                      

      ELSE IF ( I .EQ. 2 ) THEN                                         

      SPMPAR = 2.22507385850721D-308                                     

      ELSE IF ( I .EQ. 3 ) THEN                                         

      SPMPAR = 1.7976931348623D+308                                     

      ELSE                                                              

      WRITE(*,*) 'WRONG ARGUMENT FOR SPMPAR'                            

      END IF                                                            

      RETURN                                                            

C                                                                       

C     LAST CARD OF FUNCTION SPMPAR.                                     

C                                                                       

      END                                                               

CDC   FUNCTION ENORM(N,X)                                               

      DOUBLE PRECISION FUNCTION ENORM(N,X)                              

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      DIMENSION X(N)                                                    

C     **********                                                        

C                                                                       

C     FUNCTION ENORM                                                    

C                                                                       

C     GIVEN AN N-VECTOR X, THIS FUNCTION CALCULATES THE                 

C     EUCLIDEAN NORM OF X.                                              

C                                                                       

C     THE EUCLIDEAN NORM IS COMPUTED BY ACCUMULATING THE SUM OF         

C     SQUARES IN THREE DIFFERENT SUMS. THE SUMS OF SQUARES FOR THE      

C     SMALL AND LARGE COMPONENTS ARE SCALED SO THAT NO OVERFLOWS        

C     OCCUR. NON-DESTRUCTIVE UNDERFLOWS ARE PERMITTED. UNDERFLOWS       

C     AND OVERFLOWS DO NOT OCCUR IN THE COMPUTATION OF THE UNSCALED     

C     SUM OF SQUARES FOR THE INTERMEDIATE COMPONENTS.                   

C     THE DEFINITIONS OF SMALL, INTERMEDIATE AND LARGE COMPONENTS       

C     DEPEND ON TWO CONSTANTS, RDWARF AND RGIANT. THE MAIN              

C     RESTRICTIONS ON THESE CONSTANTS ARE THAT RDWARF**2 NOT            

C     UNDERFLOW AND RGIANT**2 NOT OVERFLOW. THE CONSTANTS               

C     GIVEN HERE ARE SUITABLE FOR EVERY KNOWN COMPUTER.                 

C                                                                       

C     THE FUNCTION STATEMENT IS                                         

C                                                                       

C       REAL FUNCTION ENORM(N,X)                                        

C                                                                       

C     WHERE                                                             

C                                                                       

C       N IS A POSITIVE INTEGER INPUT VARIABLE.                         

C                                                                       

C       X IS AN INPUT ARRAY OF LENGTH N.                                

C                                                                       

C     SUBPROGRAMS CALLED                                                

C                                                                       

C       FORTRAN-SUPPLIED ... ABS,SQRT                                   

C                                                                       

C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.         

C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE             

C                                                                       

C     **********                                                        

      DATA ONE,ZERO,RDWARF,RGIANT /1.0D0,0.0D0,3.834D-20,1.304D19/      

      S1 = ZERO                                                         

      S2 = ZERO                                                         

      S3 = ZERO                                                         

      X1MAX = ZERO                                                      

      X3MAX = ZERO                                                      

      FLOATN = N                                                        

      AGIANT = RGIANT/FLOATN                                            

      DO 90 I = 1, N                                                    

         XABS = ABS(X(I))                                               

         IF (XABS .GT. RDWARF .AND. XABS .LT. AGIANT) GO TO 70          

            IF (XABS .LE. RDWARF) GO TO 30                              

C                                                                       

C              SUM FOR LARGE COMPONENTS.                                

C                                                                       

               IF (XABS .LE. X1MAX) GO TO 10                            

                  S1 = ONE + S1*(X1MAX/XABS)**2                         

                  X1MAX = XABS                                          

                  GO TO 20                                              

   10          CONTINUE                                                 

                  S1 = S1 + (XABS/X1MAX)**2                             

   20          CONTINUE                                                 

               GO TO 60                                                 

   30       CONTINUE                                                    

C                                                                       

C              SUM FOR SMALL COMPONENTS.                                

C                                                                       

               IF (XABS .LE. X3MAX) GO TO 40                            

                  S3 = ONE + S3*(X3MAX/XABS)**2                         

                  X3MAX = XABS                                          

                  GO TO 50                                              

   40          CONTINUE                                                 

                  IF (XABS .NE. ZERO) S3 = S3 + (XABS/X3MAX)**2         

   50          CONTINUE                                                 

   60       CONTINUE                                                    

            GO TO 80                                                    

   70    CONTINUE                                                       

C                                                                       

C           SUM FOR INTERMEDIATE COMPONENTS.                            

C                                                                       

            S2 = S2 + XABS**2                                           

   80    CONTINUE                                                       

   90    CONTINUE                                                       

C                                                                       

C     CALCULATION OF NORM.                                              

C                                                                       

      IF (S1 .EQ. ZERO) GO TO 100                                       

         ENORM = X1MAX*SQRT(S1+(S2/X1MAX)/X1MAX)                        

         GO TO 130                                                      

  100 CONTINUE                                                          

         IF (S2 .EQ. ZERO) GO TO 110                                    

            IF (S2 .GE. X3MAX)                                          

     *         ENORM = SQRT(S2*(ONE+(X3MAX/S2)*(X3MAX*S3)))             

            IF (S2 .LT. X3MAX)                                          

     *         ENORM = SQRT(X3MAX*((S2/X3MAX)+(X3MAX*S3)))              

            GO TO 120                                                   

  110    CONTINUE                                                       

            ENORM = X3MAX*SQRT(S3)                                      

  120    CONTINUE                                                       

  130 CONTINUE                                                          

      RETURN                                                            

C                                                                       

C     LAST CARD OF FUNCTION ENORM.                                      

C                                                                       

      END                                                               

      SUBROUTINE FDJAC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,EPSFCN,    

     *                  WA1,WA2)                                        

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      DIMENSION X(N),FVEC(N),FJAC(LDFJAC,N),WA1(N),WA2(N)               

C     **********                                                        

C                                                                       

C     SUBROUTINE FDJAC1                                                 

C                                                                       

C     THIS SUBROUTINE COMPUTES A FORWARD-DIFFERENCE APPROXIMATION       

C     TO THE N BY N JACOBIAN MATRIX ASSOCIATED WITH A SPECIFIED         

C     PROBLEM OF N FUNCTIONS IN N VARIABLES. IF THE JACOBIAN HAS        

C     A BANDED FORM, THEN FUNCTION EVALUATIONS ARE SAVED BY ONLY        

C     APPROXIMATING THE NONZERO TERMS.                                  

C                                                                       

C     THE SUBROUTINE STATEMENT IS                                       

C                                                                       

C       SUBROUTINE FDJAC1(FCN,N,X,FVEC,FJAC,LDFJAC,IFLAG,ML,MU,EPSFCN,  

C                         WA1,WA2)                                      

C                                                                       

C     WHERE                                                             

C                                                                       

C       FCN IS THE NAME OF THE USER-SUPPLIED SUBROUTINE WHICH           

C         CALCULATES THE FUNCTIONS. FCN MUST BE DECLARED                

C         IN AN EXTERNAL STATEMENT IN THE USER CALLING                  

C         PROGRAM, AND SHOULD BE WRITTEN AS FOLLOWS.                    

C                                                                       

C         SUBROUTINE FCN(N,X,FVEC,IFLAG)                                

C         INTEGER N,IFLAG                                               

C         REAL X(N),FVEC(N)                                             

C         ----------                                                    

C         CALCULATE THE FUNCTIONS AT X AND                              

C         RETURN THIS VECTOR IN FVEC.                                   

C         ----------                                                    

C         RETURN                                                        

C         END                                                           

C                                                                       

C         THE VALUE OF IFLAG SHOULD NOT BE CHANGED BY FCN UNLESS        

C         THE USER WANTS TO TERMINATE EXECUTION OF FDJAC1.              

C         IN THIS CASE SET IFLAG TO A NEGATIVE INTEGER.                 

C                                                                       

C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        

C         OF FUNCTIONS AND VARIABLES.                                   

C                                                                       

C       X IS AN INPUT ARRAY OF LENGTH N.                                

C                                                                       

C       FVEC IS AN INPUT ARRAY OF LENGTH N WHICH MUST CONTAIN THE       

C         FUNCTIONS EVALUATED AT X.                                     

C                                                                       

C       FJAC IS AN OUTPUT N BY N ARRAY WHICH CONTAINS THE               

C         APPROXIMATION TO THE JACOBIAN MATRIX EVALUATED AT X.          

C                                                                       

C       LDFJAC IS A POSITIVE INTEGER INPUT VARIABLE NOT LESS THAN N     

C         WHICH SPECIFIES THE LEADING DIMENSION OF THE ARRAY FJAC.      

C                                                                       

C       IFLAG IS AN INTEGER VARIABLE WHICH CAN BE USED TO TERMINATE     

C         THE EXECUTION OF FDJAC1. SEE DESCRIPTION OF FCN.              

C                                                                       

C       ML IS A NONNEGATIVE INTEGER INPUT VARIABLE WHICH SPECIFIES      

C         THE NUMBER OF SUBDIAGONALS WITHIN THE BAND OF THE             

C         JACOBIAN MATRIX. IF THE JACOBIAN IS NOT BANDED, SET           

C         ML TO AT LEAST N - 1.                                         

C                                                                       

C       EPSFCN IS AN INPUT VARIABLE USED IN DETERMINING A SUITABLE      

C         STEP LENGTH FOR THE FORWARD-DIFFERENCE APPROXIMATION. THIS    

C         APPROXIMATION ASSUMES THAT THE RELATIVE ERRORS IN THE         

C         FUNCTIONS ARE OF THE ORDER OF EPSFCN. IF EPSFCN IS LESS       

C         THAN THE MACHINE PRECISION, IT IS ASSUMED THAT THE RELATIVE   

C         ERRORS IN THE FUNCTIONS ARE OF THE ORDER OF THE MACHINE       

C         PRECISION.                                                    

C                                                                       

C       MU IS A NONNEGATIVE INTEGER INPUT VARIABLE WHICH SPECIFIES      

C         THE NUMBER OF SUPERDIAGONALS WITHIN THE BAND OF THE           

C         JACOBIAN MATRIX. IF THE JACOBIAN IS NOT BANDED, SET           

C         MU TO AT LEAST N - 1.                                         

C                                                                       

C       WA1 AND WA2 ARE WORK ARRAYS OF LENGTH N. IF ML + MU + 1 IS AT   

C         LEAST N, THEN THE JACOBIAN IS CONSIDERED DENSE, AND WA2 IS    

C         NOT REFERENCED.                                               

C                                                                       

C     SUBPROGRAMS CALLED                                                

C                                                                       

C       MINPACK-SUPPLIED ... SPMPAR                                     

C                                                                       

C       FORTRAN-SUPPLIED ... ABS,AMAX1,SQRT                             

C                                                                       

C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.         

C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE             

C                                                                       

C     **********                                                        

      DATA ZERO /0.0D0/                                                 

C                                                                       

C     EPSMCH IS THE MACHINE PRECISION.                                  

C                                                                       

      EPSMCH = SPMPAR(1)                                                

C                                                                       

      EPS = SQRT(MAX(EPSFCN,EPSMCH))                                    

      MSUM = ML + MU + 1                                                

      IF (MSUM .LT. N) GO TO 40                                         

C                                                                       

C        COMPUTATION OF DENSE APPROXIMATE JACOBIAN.                     

C                                                                       

         DO 20 J = 1, N                                                 

            TEMP = X(J)                                                 

            H = EPS*ABS(TEMP)                                           

            IF (H .EQ. ZERO) H = EPS                                    

            X(J) = TEMP + H                                             

            CALL FCN(N,X,WA1,IFLAG)                                     

            IF (IFLAG .LT. 0) GO TO 30                                  

            X(J) = TEMP                                                 

            DO 10 I = 1, N                                              

               FJAC(I,J) = (WA1(I) - FVEC(I))/H                         

   10          CONTINUE                                                 

   20       CONTINUE                                                    

   30    CONTINUE                                                       

         GO TO 110                                                      

   40 CONTINUE                                                          

C                                                                       

C        COMPUTATION OF BANDED APPROXIMATE JACOBIAN.                    

C                                                                       

         DO 90 K = 1, MSUM                                              

            DO 60 J = K, N, MSUM                                        

               WA2(J) = X(J)                                            

               H = EPS*ABS(WA2(J))                                      

               IF (H .EQ. ZERO) H = EPS                                 

               X(J) = WA2(J) + H                                        

   60          CONTINUE                                                 

            CALL FCN(N,X,WA1,IFLAG)                                     

            IF (IFLAG .LT. 0) GO TO 100                                 

            DO 80 J = K, N, MSUM                                        

               X(J) = WA2(J)                                            

               H = EPS*ABS(WA2(J))                                      

               IF (H .EQ. ZERO) H = EPS                                 

               DO 70 I = 1, N                                           

                  FJAC(I,J) = ZERO                                      

                  IF (I .GE. J - MU .AND. I .LE. J + ML)                

     *               FJAC(I,J) = (WA1(I) - FVEC(I))/H                   

   70             CONTINUE                                              

   80          CONTINUE                                                 

   90       CONTINUE                                                    

  100    CONTINUE                                                       

  110 CONTINUE                                                          

      RETURN                                                            

C                                                                       

C     LAST CARD OF SUBROUTINE FDJAC1.                                   

C                                                                       

      END                                                               

      SUBROUTINE QFORM(M,N,Q,LDQ,WA)                                    

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      DIMENSION Q(LDQ,M),WA(M)                                          

C     **********                                                        

C                                                                       

C     SUBROUTINE QFORM                                                  

C                                                                       

C     THIS SUBROUTINE PROCEEDS FROM THE COMPUTED QR FACTORIZATION OF    

C     AN M BY N MATRIX A TO ACCUMULATE THE M BY M ORTHOGONAL MATRIX     

C     Q FROM ITS FACTORED FORM.                                         

C                                                                       

C     THE SUBROUTINE STATEMENT IS                                       

C                                                                       

C       SUBROUTINE QFORM(M,N,Q,LDQ,WA)                                  

C                                                                       

C     WHERE                                                             

C                                                                       

C       M IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        

C         OF ROWS OF A AND THE ORDER OF Q.                              

C                                                                       

C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        

C         OF COLUMNS OF A.                                              

C                                                                       

C       Q IS AN M BY M ARRAY. ON INPUT THE FULL LOWER TRAPEZOID IN      

C         THE FIRST MIN(M,N) COLUMNS OF Q CONTAINS THE FACTORED FORM.   

C         ON OUTPUT Q HAS BEEN ACCUMULATED INTO A SQUARE MATRIX.        

C                                                                       

C       LDQ IS A POSITIVE INTEGER INPUT VARIABLE NOT LESS THAN M        

C         WHICH SPECIFIES THE LEADING DIMENSION OF THE ARRAY Q.         

C                                                                       

C       WA IS A WORK ARRAY OF LENGTH M.                                 

C                                                                       

C     SUBPROGRAMS CALLED                                                

C                                                                       

C       FORTRAN-SUPPLIED ... MIN0                                       

C                                                                       

C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.         

C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE             

C                                                                       

C     **********                                                        

      DATA ONE,ZERO /1.0D0,0.0D0/                                       

C                                                                       

C     ZERO OUT UPPER TRIANGLE OF Q IN THE FIRST MIN(M,N) COLUMNS.       

C                                                                       

      MINMN = MIN0(M,N)                                                 

      IF (MINMN .LT. 2) GO TO 30                                        

      DO 20 J = 2, MINMN                                                

         JM1 = J - 1                                                    

         DO 10 I = 1, JM1                                               

            Q(I,J) = ZERO                                               

   10       CONTINUE                                                    

   20    CONTINUE                                                       

   30 CONTINUE                                                          

C                                                                       

C     INITIALIZE REMAINING COLUMNS TO THOSE OF THE IDENTITY MATRIX.     

C                                                                       

      NP1 = N + 1                                                       

      IF (M .LT. NP1) GO TO 60                                          

      DO 50 J = NP1, M                                                  

         DO 40 I = 1, M                                                 

            Q(I,J) = ZERO                                               

   40       CONTINUE                                                    

         Q(J,J) = ONE                                                   

   50    CONTINUE                                                       

   60 CONTINUE                                                          

C                                                                       

C     ACCUMULATE Q FROM ITS FACTORED FORM.                              

C                                                                       

      DO 120 L = 1, MINMN                                               

         K = MINMN - L + 1                                              

         DO 70 I = K, M                                                 

            WA(I) = Q(I,K)                                              

            Q(I,K) = ZERO                                               

   70       CONTINUE                                                    

         Q(K,K) = ONE                                                   

         IF (WA(K) .EQ. ZERO) GO TO 110                                 

         DO 100 J = K, M                                                

            SUM = ZERO                                                  

            DO 80 I = K, M                                              

               SUM = SUM + Q(I,J)*WA(I)                                 

   80          CONTINUE                                                 

            TEMP = SUM/WA(K)                                            

            DO 90 I = K, M                                              

               Q(I,J) = Q(I,J) - TEMP*WA(I)                             

   90          CONTINUE                                                 

  100       CONTINUE                                                    

  110    CONTINUE                                                       

  120    CONTINUE                                                       

      RETURN                                                            

C                                                                       

C     LAST CARD OF SUBROUTINE QFORM.                                    

C                                                                       

      END                                                               

      SUBROUTINE QRFAC(M,N,A,LDA,PIVOT,IPVT,LIPVT,RDIAG,ACNORM,WA)      

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      DIMENSION IPVT(LIPVT)                                             

      LOGICAL PIVOT                                                     

      DIMENSION A(LDA,N),RDIAG(N),ACNORM(N),WA(N)                       

C     **********                                                        

C                                                                       

C     SUBROUTINE QRFAC                                                  

C                                                                       

C     THIS SUBROUTINE USES HOUSEHOLDER TRANSFORMATIONS WITH COLUMN      

C     PIVOTING (OPTIONAL) TO COMPUTE A QR FACTORIZATION OF THE          

C     M BY N MATRIX A. THAT IS, QRFAC DETERMINES AN ORTHOGONAL          

C     MATRIX Q, A PERMUTATION MATRIX P, AND AN UPPER TRAPEZOIDAL        

C     MATRIX R WITH DIAGONAL ELEMENTS OF NONINCREASING MAGNITUDE,       

C     SUCH THAT A*P = Q*R. THE HOUSEHOLDER TRANSFORMATION FOR           

C     COLUMN K, K = 1,2,...,MIN(M,N), IS OF THE FORM                    

C                                                                       

C                           T                                           

C           I - (1/U(K))*U*U                                            

C                                                                       

C     WHERE U HAS ZEROS IN THE FIRST K-1 POSITIONS. THE FORM OF         

C     THIS TRANSFORMATION AND THE METHOD OF PIVOTING FIRST              

C     APPEARED IN THE CORRESPONDING LINPACK SUBROUTINE.                 

C                                                                       

C     THE SUBROUTINE STATEMENT IS                                       

C                                                                       

C       SUBROUTINE QRFAC(M,N,A,LDA,PIVOT,IPVT,LIPVT,RDIAG,ACNORM,WA)    

C                                                                       

C     WHERE                                                             

C                                                                       

C       M IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        

C         OF ROWS OF A.                                                 

C                                                                       

C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        

C         OF COLUMNS OF A.                                              

C                                                                       

C       A IS AN M BY N ARRAY. ON INPUT A CONTAINS THE MATRIX FOR        

C         WHICH THE QR FACTORIZATION IS TO BE COMPUTED. ON OUTPUT       

C         THE STRICT UPPER TRAPEZOIDAL PART OF A CONTAINS THE STRICT    

C         UPPER TRAPEZOIDAL PART OF R, AND THE LOWER TRAPEZOIDAL        

C         PART OF A CONTAINS A FACTORED FORM OF Q (THE NON-TRIVIAL      

C         ELEMENTS OF THE U VECTORS DESCRIBED ABOVE).                   

C                                                                       

C       LDA IS A POSITIVE INTEGER INPUT VARIABLE NOT LESS THAN M        

C         WHICH SPECIFIES THE LEADING DIMENSION OF THE ARRAY A.         

C                                                                       

C       PIVOT IS A LOGICAL INPUT VARIABLE. IF PIVOT IS SET TRUE,        

C         THEN COLUMN PIVOTING IS ENFORCED. IF PIVOT IS SET FALSE,      

C         THEN NO COLUMN PIVOTING IS DONE.                              

C                                                                       

C       IPVT IS AN INTEGER OUTPUT ARRAY OF LENGTH LIPVT. IPVT           

C         DEFINES THE PERMUTATION MATRIX P SUCH THAT A*P = Q*R.         

C         COLUMN J OF P IS COLUMN IPVT(J) OF THE IDENTITY MATRIX.       

C         IF PIVOT IS FALSE, IPVT IS NOT REFERENCED.                    

C                                                                       

C       LIPVT IS A POSITIVE INTEGER INPUT VARIABLE. IF PIVOT IS FALSE,  

C         THEN LIPVT MAY BE AS SMALL AS 1. IF PIVOT IS TRUE, THEN       

C         LIPVT MUST BE AT LEAST N.                                     

C                                                                       

C       RDIAG IS AN OUTPUT ARRAY OF LENGTH N WHICH CONTAINS THE         

C         DIAGONAL ELEMENTS OF R.                                       

C                                                                       

C       ACNORM IS AN OUTPUT ARRAY OF LENGTH N WHICH CONTAINS THE        

C         NORMS OF THE CORRESPONDING COLUMNS OF THE INPUT MATRIX A.     

C         IF THIS INFORMATION IS NOT NEEDED, THEN ACNORM CAN COINCIDE   

C         WITH RDIAG.                                                   

C                                                                       

C       WA IS A WORK ARRAY OF LENGTH N. IF PIVOT IS FALSE, THEN WA      

C         CAN COINCIDE WITH RDIAG.                                      

C                                                                       

C     SUBPROGRAMS CALLED                                                

C                                                                       

C       MINPACK-SUPPLIED ... SPMPAR,ENORM                               

C                                                                       

C       FORTRAN-SUPPLIED ... AMAX1,SQRT,MIN0                            

C                                                                       

C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.         

C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE             

C                                                                       

C     **********                                                        

      DATA ONE,P05,ZERO /1.0D0,5.0D-2,0.0D0/                            

C                                                                       

C     EPSMCH IS THE MACHINE PRECISION.                                  

C                                                                       

      EPSMCH = SPMPAR(1)                                                

C                                                                       

C     COMPUTE THE INITIAL COLUMN NORMS AND INITIALIZE SEVERAL ARRAYS.   

C                                                                       

      DO 10 J = 1, N                                                    

         ACNORM(J) = ENORM(M,A(1,J))                                    

         RDIAG(J) = ACNORM(J)                                           

         WA(J) = RDIAG(J)                                               

         IF (PIVOT) IPVT(J) = J                                         

   10    CONTINUE                                                       

C                                                                       

C     REDUCE A TO R WITH HOUSEHOLDER TRANSFORMATIONS.                   

C                                                                       

      MINMN = MIN0(M,N)                                                 

      DO 110 J = 1, MINMN                                               

         IF (.NOT.PIVOT) GO TO 40                                       

C                                                                       

C        BRING THE COLUMN OF LARGEST NORM INTO THE PIVOT POSITION.      

C                                                                       

         KMAX = J                                                       

         DO 20 K = J, N                                                 

            IF (RDIAG(K) .GT. RDIAG(KMAX)) KMAX = K                     

   20       CONTINUE                                                    

         IF (KMAX .EQ. J) GO TO 40                                      

         DO 30 I = 1, M                                                 

            TEMP = A(I,J)                                               

            A(I,J) = A(I,KMAX)                                          

            A(I,KMAX) = TEMP                                            

   30       CONTINUE                                                    

         RDIAG(KMAX) = RDIAG(J)                                         

         WA(KMAX) = WA(J)                                               

         K = IPVT(J)                                                    

         IPVT(J) = IPVT(KMAX)                                           

         IPVT(KMAX) = K                                                 

   40    CONTINUE                                                       

C                                                                       

C        COMPUTE THE HOUSEHOLDER TRANSFORMATION TO REDUCE THE           

C        J-TH COLUMN OF A TO A MULTIPLE OF THE J-TH UNIT VECTOR.        

C                                                                       

         AJNORM = ENORM(M-J+1,A(J,J))                                   

         IF (AJNORM .EQ. ZERO) GO TO 100                                

         IF (A(J,J) .LT. ZERO) AJNORM = -AJNORM                         

         DO 50 I = J, M                                                 

            A(I,J) = A(I,J)/AJNORM                                      

   50       CONTINUE                                                    

         A(J,J) = A(J,J) + ONE                                          

C                                                                       

C        APPLY THE TRANSFORMATION TO THE REMAINING COLUMNS              

C        AND UPDATE THE NORMS.                                          

C                                                                       

         JP1 = J + 1                                                    

         IF (N .LT. JP1) GO TO 100                                      

         DO 90 K = JP1, N                                               

            SUM = ZERO                                                  

            DO 60 I = J, M                                              

               SUM = SUM + A(I,J)*A(I,K)                                

   60          CONTINUE                                                 

            TEMP = SUM/A(J,J)                                           

            DO 70 I = J, M                                              

               A(I,K) = A(I,K) - TEMP*A(I,J)                            

   70          CONTINUE                                                 

            IF (.NOT.PIVOT .OR. RDIAG(K) .EQ. ZERO) GO TO 80            

            TEMP = A(J,K)/RDIAG(K)                                      

            RDIAG(K) = RDIAG(K)*SQRT(MAX(ZERO,ONE-TEMP**2))             

            IF (P05*(RDIAG(K)/WA(K))**2 .GT. EPSMCH) GO TO 80           

            RDIAG(K) = ENORM(M-J,A(JP1,K))                              

            WA(K) = RDIAG(K)                                            

   80       CONTINUE                                                    

   90       CONTINUE                                                    

  100    CONTINUE                                                       

         RDIAG(J) = -AJNORM                                             

  110    CONTINUE                                                       

      RETURN                                                            

C                                                                       

C     LAST CARD OF SUBROUTINE QRFAC.                                    

C                                                                       

      END                                                               

      SUBROUTINE R1MPYQ(M,N,A,LDA,V,W)                                  

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      DIMENSION A(LDA,N),V(N),W(N)                                      

C     **********                                                        

C                                                                       

C     SUBROUTINE R1MPYQ                                                 

C                                                                       

C     GIVEN AN M BY N MATRIX A, THIS SUBROUTINE COMPUTES A*Q WHERE      

C     Q IS THE PRODUCT OF 2*(N - 1) TRANSFORMATIONS                     

C                                                                       

C           GV(N-1)*...*GV(1)*GW(1)*...*GW(N-1)                         

C                                                                       

C     AND GV(I), GW(I) ARE GIVENS ROTATIONS IN THE (I,N) PLANE WHICH    

C     ELIMINATE ELEMENTS IN THE I-TH AND N-TH PLANES, RESPECTIVELY.     

C     Q ITSELF IS NOT GIVEN, RATHER THE INFORMATION TO RECOVER THE      

C     GV, GW ROTATIONS IS SUPPLIED.                                     

C                                                                       

C     THE SUBROUTINE STATEMENT IS                                       

C                                                                       

C       SUBROUTINE R1MPYQ(M,N,A,LDA,V,W)                                

C                                                                       

C     WHERE                                                             

C                                                                       

C       M IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        

C         OF ROWS OF A.                                                 

C                                                                       

C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        

C         OF COLUMNS OF A.                                              

C                                                                       

C       A IS AN M BY N ARRAY. ON INPUT A MUST CONTAIN THE MATRIX        

C         TO BE POSTMULTIPLIED BY THE ORTHOGONAL MATRIX Q               

C         DESCRIBED ABOVE. ON OUTPUT A*Q HAS REPLACED A.                

C                                                                       

C       LDA IS A POSITIVE INTEGER INPUT VARIABLE NOT LESS THAN M        

C         WHICH SPECIFIES THE LEADING DIMENSION OF THE ARRAY A.         

C                                                                       

C       V IS AN INPUT ARRAY OF LENGTH N. V(I) MUST CONTAIN THE          

C         INFORMATION NECESSARY TO RECOVER THE GIVENS ROTATION GV(I)    

C         DESCRIBED ABOVE.                                              

C                                                                       

C       W IS AN INPUT ARRAY OF LENGTH N. W(I) MUST CONTAIN THE          

C         INFORMATION NECESSARY TO RECOVER THE GIVENS ROTATION GW(I)    

C         DESCRIBED ABOVE.                                              

C                                                                       

C     SUBROUTINES CALLED                                                

C                                                                       

C       FORTRAN-SUPPLIED ... ABS,SQRT                                   

C                                                                       

C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.         

C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE             

C                                                                       

C     **********                                                        

      DATA ONE /1.0D0/                                                  

C                                                                       

C     APPLY THE FIRST SET OF GIVENS ROTATIONS TO A.                     

C                                                                       

      NM1 = N - 1                                                       

      IF (NM1 .LT. 1) GO TO 50                                          

      DO 20 NMJ = 1, NM1                                                

         J = N - NMJ                                                    

         IF (ABS(V(J)) .GT. ONE) COS = ONE/V(J)                         

         IF (ABS(V(J)) .GT. ONE) SIN = SQRT(ONE-COS**2)                 

         IF (ABS(V(J)) .LE. ONE) SIN = V(J)                             

         IF (ABS(V(J)) .LE. ONE) COS = SQRT(ONE-SIN**2)                 

         DO 10 I = 1, M                                                 

            TEMP = COS*A(I,J) - SIN*A(I,N)                              

            A(I,N) = SIN*A(I,J) + COS*A(I,N)                            

            A(I,J) = TEMP                                               

   10       CONTINUE                                                    

   20    CONTINUE                                                       

C                                                                       

C     APPLY THE SECOND SET OF GIVENS ROTATIONS TO A.                    

C                                                                       

      DO 40 J = 1, NM1                                                  

         IF (ABS(W(J)) .GT. ONE) COS = ONE/W(J)                         

         IF (ABS(W(J)) .GT. ONE) SIN = SQRT(ONE-COS**2)                 

         IF (ABS(W(J)) .LE. ONE) SIN = W(J)                             

         IF (ABS(W(J)) .LE. ONE) COS = SQRT(ONE-SIN**2)                 

         DO 30 I = 1, M                                                 

            TEMP = COS*A(I,J) + SIN*A(I,N)                              

            A(I,N) = -SIN*A(I,J) + COS*A(I,N)                           

            A(I,J) = TEMP                                               

   30       CONTINUE                                                    

   40    CONTINUE                                                       

   50 CONTINUE                                                          

      RETURN                                                            

C                                                                       

C     LAST CARD OF SUBROUTINE R1MPYQ.                                   

C                                                                       

      END                                                               

      SUBROUTINE R1UPDT(M,N,S,LS,U,V,W,SING)                            

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      LOGICAL SING                                                      

      DIMENSION S(LS),U(M),V(N),W(M)                                    

C     **********                                                        

C                                                                       

C     SUBROUTINE R1UPDT                                                 

C                                                                       

C     GIVEN AN M BY N LOWER TRAPEZOIDAL MATRIX S, AN M-VECTOR U,        

C     AND AN N-VECTOR V, THE PROBLEM IS TO DETERMINE AN                 

C     ORTHOGONAL MATRIX Q SUCH THAT                                     

C                                                                       

C                   T                                                   

C           (S + U*V )*Q                                                

C                                                                       

C     IS AGAIN LOWER TRAPEZOIDAL.                                       

C                                                                       

C     THIS SUBROUTINE DETERMINES Q AS THE PRODUCT OF 2*(N - 1)          

C     TRANSFORMATIONS                                                   

C                                                                       

C           GV(N-1)*...*GV(1)*GW(1)*...*GW(N-1)                         

C                                                                       

C     WHERE GV(I), GW(I) ARE GIVENS ROTATIONS IN THE (I,N) PLANE        

C     WHICH ELIMINATE ELEMENTS IN THE I-TH AND N-TH PLANES,             

C     RESPECTIVELY. Q ITSELF IS NOT ACCUMULATED, RATHER THE             

C     INFORMATION TO RECOVER THE GV, GW ROTATIONS IS RETURNED.          

C                                                                       

C     THE SUBROUTINE STATEMENT IS                                       

C                                                                       

C       SUBROUTINE R1UPDT(M,N,S,LS,U,V,W,SING)                          

C                                                                       

C     WHERE                                                             

C                                                                       
C       M IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        

C         OF ROWS OF S.                                                 

C                                                                       

C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        

C         OF COLUMNS OF S. N MUST NOT EXCEED M.                         

C                                                                       

C       S IS AN ARRAY OF LENGTH LS. ON INPUT S MUST CONTAIN THE LOWER   

C         TRAPEZOIDAL MATRIX S STORED BY COLUMNS. ON OUTPUT S CONTAINS  

C         THE LOWER TRAPEZOIDAL MATRIX PRODUCED AS DESCRIBED ABOVE.     

C                                                                       

C       LS IS A POSITIVE INTEGER INPUT VARIABLE NOT LESS THAN           

C         (N*(2*M-N+1))/2.                                              

C                                                                       

C       U IS AN INPUT ARRAY OF LENGTH M WHICH MUST CONTAIN THE          

C         VECTOR U.                                                     

C                                                                       

C       V IS AN ARRAY OF LENGTH N. ON INPUT V MUST CONTAIN THE VECTOR   

C         V. ON OUTPUT V(I) CONTAINS THE INFORMATION NECESSARY TO       

C         RECOVER THE GIVENS ROTATION GV(I) DESCRIBED ABOVE.            

C                                                                       

C       W IS AN OUTPUT ARRAY OF LENGTH M. W(I) CONTAINS INFORMATION     

C         NECESSARY TO RECOVER THE GIVENS ROTATION GW(I) DESCRIBED      

C         ABOVE.                                                        

C                                                                       

C       SING IS A LOGICAL OUTPUT VARIABLE. SING IS SET TRUE IF ANY      

C         OF THE DIAGONAL ELEMENTS OF THE OUTPUT S ARE ZERO. OTHERWISE  

C         SING IS SET FALSE.                                            

C                                                                       

C     SUBPROGRAMS CALLED                                                

C                                                                       

C       MINPACK-SUPPLIED ... SPMPAR                                     

C                                                                       

C       FORTRAN-SUPPLIED ... ABS,SQRT                                   

C                                                                       

C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.         

C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE,            

C     JOHN L. NAZARETH                                                  

C                                                                       

C     **********                                                        

      DATA ONE,P5,P25,ZERO /1.0D0,5.0D-1,2.5D-1,0.0D0/                  

C                                                                       

C     GIANT IS THE LARGEST MAGNITUDE.                                   

C                                                                       

      GIANT = SPMPAR(3)                                                 

C                                                                       

C     INITIALIZE THE DIAGONAL ELEMENT POINTER.                          

C                                                                       

      JJ = (N*(2*M - N + 1))/2 - (M - N)                                

C                                                                       

C     MOVE THE NONTRIVIAL PART OF THE LAST COLUMN OF S INTO W.          

C                                                                       

      L = JJ                                                            

      DO 10 I = N, M                                                    

         W(I) = S(L)                                                    

         L = L + 1                                                      

   10    CONTINUE                                                       

C                                                                       

C     ROTATE THE VECTOR V INTO A MULTIPLE OF THE N-TH UNIT VECTOR       

C     IN SUCH A WAY THAT A SPIKE IS INTRODUCED INTO W.                  

C                                                                       

      NM1 = N - 1                                                       

      IF (NM1 .LT. 1) GO TO 70                                          

      DO 60 NMJ = 1, NM1                                                

         J = N - NMJ                                                    

         JJ = JJ - (M - J + 1)                                          

         W(J) = ZERO                                                    

         IF (V(J) .EQ. ZERO) GO TO 50                                   

C                                                                       

C        DETERMINE A GIVENS ROTATION WHICH ELIMINATES THE               

C        J-TH ELEMENT OF V.                                             

C                                                                       

         IF (ABS(V(N)) .GE. ABS(V(J))) GO TO 20                         

            COTAN = V(N)/V(J)                                           

            SIN = P5/SQRT(P25+P25*COTAN**2)                             

            COS = SIN*COTAN                                             

            TAU = ONE                                                   

            IF (ABS(COS)*GIANT .GT. ONE) TAU = ONE/COS                  

            GO TO 30                                                    

   20    CONTINUE                                                       

            TAN = V(J)/V(N)                                             

            COS = P5/SQRT(P25+P25*TAN**2)                               

            SIN = COS*TAN                                               

            TAU = SIN                                                   

   30    CONTINUE                                                       

C                                                                       

C        APPLY THE TRANSFORMATION TO V AND STORE THE INFORMATION        

C        NECESSARY TO RECOVER THE GIVENS ROTATION.                      

C                                                                       

         V(N) = SIN*V(J) + COS*V(N)                                     

         V(J) = TAU                                                     

C                                                                       

C        APPLY THE TRANSFORMATION TO S AND EXTEND THE SPIKE IN W.       

C                                                                       

         L = JJ                                                         

         DO 40 I = J, M                                                 

            TEMP = COS*S(L) - SIN*W(I)                                  

            W(I) = SIN*S(L) + COS*W(I)                                  

            S(L) = TEMP                                                 

            L = L + 1                                                   

   40       CONTINUE                                                    

   50    CONTINUE                                                       

   60    CONTINUE                                                       

   70 CONTINUE                                                          

C                                                                       

C     ADD THE SPIKE FROM THE RANK 1 UPDATE TO W.                        

C                                                                       

      DO 80 I = 1, M                                                    

         W(I) = W(I) + V(N)*U(I)                                        

   80    CONTINUE                                                       

C                                                                       

C     ELIMINATE THE SPIKE.                                              

C                                                                       

      SING = .FALSE.                                                    

      IF (NM1 .LT. 1) GO TO 140                                         

      DO 130 J = 1, NM1                                                 

         IF (W(J) .EQ. ZERO) GO TO 120                                  

C                                                                       

C        DETERMINE A GIVENS ROTATION WHICH ELIMINATES THE               

C        J-TH ELEMENT OF THE SPIKE.                                     

C                                                                       

         IF (ABS(S(JJ)) .GE. ABS(W(J))) GO TO 90                        

            COTAN = S(JJ)/W(J)                                          

            SIN = P5/SQRT(P25+P25*COTAN**2)                             

            COS = SIN*COTAN                                             

            TAU = ONE                                                   

            IF (ABS(COS)*GIANT .GT. ONE) TAU = ONE/COS                  

            GO TO 100                                                   

   90    CONTINUE                                                       

            TAN = W(J)/S(JJ)                                            

            COS = P5/SQRT(P25+P25*TAN**2)                               

            SIN = COS*TAN                                               

            TAU = SIN                                                   

  100    CONTINUE                                                       

C                                                                       

C        APPLY THE TRANSFORMATION TO S AND REDUCE THE SPIKE IN W.       

C                                                                       

         L = JJ                                                         

         DO 110 I = J, M                                                

            TEMP = COS*S(L) + SIN*W(I)                                  

            W(I) = -SIN*S(L) + COS*W(I)                                 

            S(L) = TEMP                                                 

            L = L + 1                                                   

  110       CONTINUE                                                    

C                                                                       

C        STORE THE INFORMATION NECESSARY TO RECOVER THE                 

C        GIVENS ROTATION.                                               

C                                                                       

         W(J) = TAU                                                     

  120    CONTINUE                                                       

C                                                                       

C        TEST FOR ZERO DIAGONAL ELEMENTS IN THE OUTPUT S.               

C                                                                       

         IF (S(JJ) .EQ. ZERO) SING = .TRUE.                             

         JJ = JJ + (M - J + 1)                                          

  130    CONTINUE                                                       

  140 CONTINUE                                                          

C                                                                       

C     MOVE W BACK INTO THE LAST COLUMN OF THE OUTPUT S.                 

C                                                                       

      L = JJ                                                            

      DO 150 I = N, M                                                   

         S(L) = W(I)                                                    

         L = L + 1                                                      

  150    CONTINUE                                                       

      IF (S(JJ) .EQ. ZERO) SING = .TRUE.                                

      RETURN                                                            

C                                                                       

C     LAST CARD OF SUBROUTINE R1UPDT.                                   

C                                                                       

      END                                                               

      SUBROUTINE FDJAC2(FCN,M,N,X,FVEC,FJAC,LDFJAC,IFLAG,EPSFCN,WA)     

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      DIMENSION X(N),FVEC(M),FJAC(LDFJAC,N),WA(M)                       

C     **********                                                        

C                                                                       

C     SUBROUTINE FDJAC2                                                 

C                                                                       

C     THIS SUBROUTINE COMPUTES A FORWARD-DIFFERENCE APPROXIMATION       

C     TO THE M BY N JACOBIAN MATRIX ASSOCIATED WITH A SPECIFIED         

C     PROBLEM OF M FUNCTIONS IN N VARIABLES.                            

C                                                                       

C     THE SUBROUTINE STATEMENT IS                                       

C                                                                       

C       SUBROUTINE FDJAC2(FCN,M,N,X,FVEC,FJAC,LDFJAC,IFLAG,EPSFCN,WA)   

C                                                                       

C     WHERE                                                             

C                                                                       

C       FCN IS THE NAME OF THE USER-SUPPLIED SUBROUTINE WHICH           

C         CALCULATES THE FUNCTIONS. FCN MUST BE DECLARED                

C         IN AN EXTERNAL STATEMENT IN THE USER CALLING                  

C         PROGRAM, AND SHOULD BE WRITTEN AS FOLLOWS.                    

C                                                                       

C         SUBROUTINE FCN(M,N,X,FVEC,IFLAG)                              

C         INTEGER M,N,IFLAG                                             

C         REAL X(N),FVEC(M)                                             

C         ----------                                                    

C         CALCULATE THE FUNCTIONS AT X AND                              

C         RETURN THIS VECTOR IN FVEC.                                   

C         ----------                                                    

C         RETURN                                                        

C         END                                                           

C                                                                       

C         THE VALUE OF IFLAG SHOULD NOT BE CHANGED BY FCN UNLESS        

C         THE USER WANTS TO TERMINATE EXECUTION OF FDJAC2.              

C         IN THIS CASE SET IFLAG TO A NEGATIVE INTEGER.                 

C                                                                       

C       M IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        

C         OF FUNCTIONS.                                                 

C                                                                       

C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE NUMBER        

C         OF VARIABLES. N MUST NOT EXCEED M.                            

C                                                                       

C       X IS AN INPUT ARRAY OF LENGTH N.                                

C                                                                       

C       FVEC IS AN INPUT ARRAY OF LENGTH M WHICH MUST CONTAIN THE       

C         FUNCTIONS EVALUATED AT X.                                     

C                                                                       

C       FJAC IS AN OUTPUT M BY N ARRAY WHICH CONTAINS THE               

C         APPROXIMATION TO THE JACOBIAN MATRIX EVALUATED AT X.          

C                                                                       

C       LDFJAC IS A POSITIVE INTEGER INPUT VARIABLE NOT LESS THAN M     

C         WHICH SPECIFIES THE LEADING DIMENSION OF THE ARRAY FJAC.      

C                                                                       

C       IFLAG IS AN INTEGER VARIABLE WHICH CAN BE USED TO TERMINATE     

C         THE EXECUTION OF FDJAC2. SEE DESCRIPTION OF FCN.              

C                                                                       

C       EPSFCN IS AN INPUT VARIABLE USED IN DETERMINING A SUITABLE      

C         STEP LENGTH FOR THE FORWARD-DIFFERENCE APPROXIMATION. THIS    

C         APPROXIMATION ASSUMES THAT THE RELATIVE ERRORS IN THE         

C         FUNCTIONS ARE OF THE ORDER OF EPSFCN. IF EPSFCN IS LESS       

C         THAN THE MACHINE PRECISION, IT IS ASSUMED THAT THE RELATIVE   

C         ERRORS IN THE FUNCTIONS ARE OF THE ORDER OF THE MACHINE       

C         PRECISION.                                                    

C                                                                       

C       WA IS A WORK ARRAY OF LENGTH M.                                 

C                                                                       

C     SUBPROGRAMS CALLED                                                

C                                                                       

C       USER-SUPPLIED ...... FCN                                        

C                                                                       

C       MINPACK-SUPPLIED ... SPMPAR                                     

C                                                                       

C       FORTRAN-SUPPLIED ... ABS,AMAX1,SQRT                             

C                                                                       

C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.         

C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE             

C                                                                       

C     **********                                                        

      DATA ZERO /0.0D0/                                                 

C                                                                       

C     EPSMCH IS THE MACHINE PRECISION.                                  

C                                                                       

      EPSMCH = SPMPAR(1)                                                

C                                                                       

      EPS = SQRT(MAX(EPSFCN,EPSMCH))                                    

      DO 20 J = 1, N                                                    

         TEMP = X(J)                                                    

         H = EPS*ABS(TEMP)                                              

         IF (H .EQ. ZERO) H = EPS                                       

         X(J) = TEMP + H                                                

         CALL FCN(M,N,X,WA,IFLAG)                                       

         IF (IFLAG .LT. 0) GO TO 30                                     

         X(J) = TEMP                                                    

         DO 10 I = 1, M                                                 

            FJAC(I,J) = (WA(I) - FVEC(I))/H                             

   10       CONTINUE                                                    

   20    CONTINUE                                                       

   30 CONTINUE                                                          

      RETURN                                                            

C                                                                       

C     LAST CARD OF SUBROUTINE FDJAC2.                                   

C                                                                       

      END                                                               

      SUBROUTINE LMPAR(N,R,LDR,IPVT,DIAG,QTB,DELTA,PAR,X,SDIAG,WA1,     

     *                 WA2)                                             

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      INTEGER IPVT(N)                                                   

      DIMENSION R(LDR,N),DIAG(N),QTB(N),X(N),SDIAG(N),WA1(N),WA2(N)     

C     **********                                                        

C                                                                       

C     SUBROUTINE LMPAR                                                  

C                                                                       

C     GIVEN AN M BY N MATRIX A, AN N BY N NONSINGULAR DIAGONAL          

C     MATRIX D, AN M-VECTOR B, AND A POSITIVE NUMBER DELTA,             

C     THE PROBLEM IS TO DETERMINE A VALUE FOR THE PARAMETER             

C     PAR SUCH THAT IF X SOLVES THE SYSTEM                              

C                                                                       

C           A*X = B ,     SQRT(PAR)*D*X = 0 ,                           

C                                                                       

C     IN THE LEAST SQUARES SENSE, AND DXNORM IS THE EUCLIDEAN           

C     NORM OF D*X, THEN EITHER PAR IS ZERO AND                          

C                                                                       

C           (DXNORM-DELTA) .LE. 0.1*DELTA ,                             

C                                                                       

C     OR PAR IS POSITIVE AND                                            

C                                                                       

C           ABS(DXNORM-DELTA) .LE. 0.1*DELTA .                          

C                                                                       

C     THIS SUBROUTINE COMPLETES THE SOLUTION OF THE PROBLEM             

C     IF IT IS PROVIDED WITH THE NECESSARY INFORMATION FROM THE         

C     QR FACTORIZATION, WITH COLUMN PIVOTING, OF A. THAT IS, IF         

C     A*P = Q*R, WHERE P IS A PERMUTATION MATRIX, Q HAS ORTHOGONAL      

C     COLUMNS, AND R IS AN UPPER TRIANGULAR MATRIX WITH DIAGONAL        

C     ELEMENTS OF NONINCREASING MAGNITUDE, THEN LMPAR EXPECTS           

C     THE FULL UPPER TRIANGLE OF R, THE PERMUTATION MATRIX P,           

C     AND THE FIRST N COMPONENTS OF (Q TRANSPOSE)*B. ON OUTPUT          

C     LMPAR ALSO PROVIDES AN UPPER TRIANGULAR MATRIX S SUCH THAT        

C                                                                       

C            T   T                   T                                  

C           P *(A *A + PAR*D*D)*P = S *S .                              

C                                                                       

C     S IS EMPLOYED WITHIN LMPAR AND MAY BE OF SEPARATE INTEREST.       

C                                                                       

C     ONLY A FEW ITERATIONS ARE GENERALLY NEEDED FOR CONVERGENCE        

C     OF THE ALGORITHM. IF, HOWEVER, THE LIMIT OF 10 ITERATIONS         

C     IS REACHED, THEN THE OUTPUT PAR WILL CONTAIN THE BEST             

C     VALUE OBTAINED SO FAR.                                            

C                                                                       

C     THE SUBROUTINE STATEMENT IS                                       

C                                                                       

C       SUBROUTINE LMPAR(N,R,LDR,IPVT,DIAG,QTB,DELTA,PAR,X,SDIAG,       

C                        WA1,WA2)                                       

C                                                                       

C     WHERE                                                             

C                                                                       

C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE ORDER OF R.   

C                                                                       

C       R IS AN N BY N ARRAY. ON INPUT THE FULL UPPER TRIANGLE          

C         MUST CONTAIN THE FULL UPPER TRIANGLE OF THE MATRIX R.         

C         ON OUTPUT THE FULL UPPER TRIANGLE IS UNALTERED, AND THE       

C         STRICT LOWER TRIANGLE CONTAINS THE STRICT UPPER TRIANGLE      

C         (TRANSPOSED) OF THE UPPER TRIANGULAR MATRIX S.                

C                                                                       

C       LDR IS A POSITIVE INTEGER INPUT VARIABLE NOT LESS THAN N        

C         WHICH SPECIFIES THE LEADING DIMENSION OF THE ARRAY R.         

C                                                                       

C       IPVT IS AN INTEGER INPUT ARRAY OF LENGTH N WHICH DEFINES THE    

C         PERMUTATION MATRIX P SUCH THAT A*P = Q*R. COLUMN J OF P       

C         IS COLUMN IPVT(J) OF THE IDENTITY MATRIX.                     

C                                                                       

C       DIAG IS AN INPUT ARRAY OF LENGTH N WHICH MUST CONTAIN THE       

C         DIAGONAL ELEMENTS OF THE MATRIX D.                            

C                                                                       

C       QTB IS AN INPUT ARRAY OF LENGTH N WHICH MUST CONTAIN THE FIRST  

C         N ELEMENTS OF THE VECTOR (Q TRANSPOSE)*B.                     

C                                                                       

C       DELTA IS A POSITIVE INPUT VARIABLE WHICH SPECIFIES AN UPPER     

C         BOUND ON THE EUCLIDEAN NORM OF D*X.                           

C                                                                       

C       PAR IS A NONNEGATIVE VARIABLE. ON INPUT PAR CONTAINS AN         

C         INITIAL ESTIMATE OF THE LEVENBERG-MARQUARDT PARAMETER.        

C         ON OUTPUT PAR CONTAINS THE FINAL ESTIMATE.                    

C                                                                       

C       X IS AN OUTPUT ARRAY OF LENGTH N WHICH CONTAINS THE LEAST       

C         SQUARES SOLUTION OF THE SYSTEM A*X = B, SQRT(PAR)*D*X = 0,    

C         FOR THE OUTPUT PAR.                                           

C                                                                       

C       SDIAG IS AN OUTPUT ARRAY OF LENGTH N WHICH CONTAINS THE         

C         DIAGONAL ELEMENTS OF THE UPPER TRIANGULAR MATRIX S.           

C                                                                       

C       WA1 AND WA2 ARE WORK ARRAYS OF LENGTH N.                        

C                                                                       

C     SUBPROGRAMS CALLED                                                

C                                                                       

C       MINPACK-SUPPLIED ... SPMPAR,ENORM,QRSOLV                        

C                                                                       

C       FORTRAN-SUPPLIED ... ABS,AMAX1,AMIN1,SQRT                       

C                                                                       

C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.         

C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE             

C                                                                       

C     **********                                                        

      DATA P1,P001,ZERO /1.0D-1,1.0D-3,0.0D0/                           

C                                                                       

C     DWARF IS THE SMALLEST POSITIVE MAGNITUDE.                         

C                                                                       

      DWARF = SPMPAR(2)                                                 

C                                                                       

C     COMPUTE AND STORE IN X THE GAUSS-NEWTON DIRECTION. IF THE         

C     JACOBIAN IS RANK-DEFICIENT, OBTAIN A LEAST SQUARES SOLUTION.      

C                                                                       

      NSING = N                                                         

      DO 10 J = 1, N                                                    

         WA1(J) = QTB(J)                                                

         IF (R(J,J) .EQ. ZERO .AND. NSING .EQ. N) NSING = J - 1         

         IF (NSING .LT. N) WA1(J) = ZERO                                

   10    CONTINUE                                                       

      IF (NSING .LT. 1) GO TO 50                                        

      DO 40 K = 1, NSING                                                

         J = NSING - K + 1                                              

         WA1(J) = WA1(J)/R(J,J)                                         

         TEMP = WA1(J)                                                  

         JM1 = J - 1                                                    

         IF (JM1 .LT. 1) GO TO 30                                       

         DO 20 I = 1, JM1                                               

            WA1(I) = WA1(I) - R(I,J)*TEMP                               

   20       CONTINUE                                                    

   30    CONTINUE                                                       

   40    CONTINUE                                                       

   50 CONTINUE                                                          

      DO 60 J = 1, N                                                    

         L = IPVT(J)                                                    

         X(L) = WA1(J)                                                  

   60    CONTINUE                                                       

C                                                                       

C     INITIALIZE THE ITERATION COUNTER.                                 

C     EVALUATE THE FUNCTION AT THE ORIGIN, AND TEST                     

C     FOR ACCEPTANCE OF THE GAUSS-NEWTON DIRECTION.                     

C                                                                       

      ITER = 0                                                          

      DO 70 J = 1, N                                                    

         WA2(J) = DIAG(J)*X(J)                                          

   70    CONTINUE                                                       

      DXNORM = ENORM(N,WA2)                                             

      FP = DXNORM - DELTA                                               

      IF (FP .LE. P1*DELTA) GO TO 220                                   

C                                                                       

C     IF THE JACOBIAN IS NOT RANK DEFICIENT, THE NEWTON                 

C     STEP PROVIDES A LOWER BOUND, PARL, FOR THE ZERO OF                

C     THE FUNCTION. OTHERWISE SET THIS BOUND TO ZERO.                   

C                                                                       

      PARL = ZERO                                                       

      IF (NSING .LT. N) GO TO 120                                       

      DO 80 J = 1, N                                                    

         L = IPVT(J)                                                    

         WA1(J) = DIAG(L)*(WA2(L)/DXNORM)                               

   80    CONTINUE                                                       

      DO 110 J = 1, N                                                   

         SUM = ZERO                                                     

         JM1 = J - 1                                                    

         IF (JM1 .LT. 1) GO TO 100                                      

         DO 90 I = 1, JM1                                               

            SUM = SUM + R(I,J)*WA1(I)                                   

   90       CONTINUE                                                    

  100    CONTINUE                                                       

         WA1(J) = (WA1(J) - SUM)/R(J,J)                                 

  110    CONTINUE                                                       

      TEMP = ENORM(N,WA1)                                               

      PARL = ((FP/DELTA)/TEMP)/TEMP                                     

  120 CONTINUE                                                          

C                                                                       

C     CALCULATE AN UPPER BOUND, PARU, FOR THE ZERO OF THE FUNCTION.     

C                                                                       

      DO 140 J = 1, N                                                   

         SUM = ZERO                                                     

         DO 130 I = 1, J                                                

            SUM = SUM + R(I,J)*QTB(I)                                   

  130       CONTINUE                                                    

         L = IPVT(J)                                                    

         WA1(J) = SUM/DIAG(L)                                           

  140    CONTINUE                                                       

      GNORM = ENORM(N,WA1)                                              

      PARU = GNORM/DELTA                                                

      IF (PARU .EQ. ZERO) PARU = DWARF/MIN(DELTA,P1)                    

C                                                                       

C     IF THE INPUT PAR LIES OUTSIDE OF THE INTERVAL (PARL,PARU),        

C     SET PAR TO THE CLOSER ENDPOINT.                                   

C                                                                       

      PAR = MAX(PAR,PARL)                                               

      PAR = MIN(PAR,PARU)                                               

      IF (PAR .EQ. ZERO) PAR = GNORM/DXNORM                             

C                                                                       

C     BEGINNING OF AN ITERATION.                                        

C                                                                       

  150 CONTINUE                                                          

         ITER = ITER + 1                                                

C                                                                       

C        EVALUATE THE FUNCTION AT THE CURRENT VALUE OF PAR.             

C                                                                       

         IF (PAR .EQ. ZERO) PAR = MAX(DWARF,P001*PARU)                  

         TEMP = SQRT(PAR)                                               

         DO 160 J = 1, N                                                

            WA1(J) = TEMP*DIAG(J)                                       

  160       CONTINUE                                                    

         CALL QRSOLV(N,R,LDR,IPVT,WA1,QTB,X,SDIAG,WA2)                  

         DO 170 J = 1, N                                                

            WA2(J) = DIAG(J)*X(J)                                       

  170       CONTINUE                                                    

         DXNORM = ENORM(N,WA2)                                          

         TEMP = FP                                                      

         FP = DXNORM - DELTA                                            

C                                                                       

C        IF THE FUNCTION IS SMALL ENOUGH, ACCEPT THE CURRENT VALUE      

C        OF PAR. ALSO TEST FOR THE EXCEPTIONAL CASES WHERE PARL         

C        IS ZERO OR THE NUMBER OF ITERATIONS HAS REACHED 10.            

C                                                                       

         IF (ABS(FP) .LE. P1*DELTA                                      

     *       .OR. PARL .EQ. ZERO .AND. FP .LE. TEMP                     

     *            .AND. TEMP .LT. ZERO .OR. ITER .EQ. 10) GO TO 220     

C                                                                       

C        COMPUTE THE NEWTON CORRECTION.                                 

C                                                                       

         DO 180 J = 1, N                                                

            L = IPVT(J)                                                 

            WA1(J) = DIAG(L)*(WA2(L)/DXNORM)                            

  180       CONTINUE                                                    

         DO 210 J = 1, N                                                

            WA1(J) = WA1(J)/SDIAG(J)                                    

            TEMP = WA1(J)                                               

            JP1 = J + 1                                                 

            IF (N .LT. JP1) GO TO 200                                   

            DO 190 I = JP1, N                                           

               WA1(I) = WA1(I) - R(I,J)*TEMP                            

  190          CONTINUE                                                 

  200       CONTINUE                                                    

  210       CONTINUE                                                    

         TEMP = ENORM(N,WA1)                                            

         PARC = ((FP/DELTA)/TEMP)/TEMP                                  

C                                                                       

C        DEPENDING ON THE SIGN OF THE FUNCTION, UPDATE PARL OR PARU.    

C                                                                       

         IF (FP .GT. ZERO) PARL = MAX(PARL,PAR)                         

         IF (FP .LT. ZERO) PARU = MIN(PARU,PAR)                         

C                                                                       

C        COMPUTE AN IMPROVED ESTIMATE FOR PAR.                          

C                                                                       

         PAR = MAX(PARL,PAR+PARC)                                       

C                                                                       

C        END OF AN ITERATION.                                           

C                                                                       

         GO TO 150                                                      

  220 CONTINUE                                                          

C                                                                       

C     TERMINATION.                                                      

C                                                                       

      IF (ITER .EQ. 0) PAR = ZERO                                       

      RETURN                                                            

C                                                                       

C     LAST CARD OF SUBROUTINE LMPAR.                                    

C                                                                       

      END                                                               

      SUBROUTINE QRSOLV(N,R,LDR,IPVT,DIAG,QTB,X,SDIAG,WA)               

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      INTEGER IPVT(N)                                                   

      DIMENSION R(LDR,N),DIAG(N),QTB(N),X(N),SDIAG(N),WA(N)             

C     **********                                                        

C                                                                       

C     SUBROUTINE QRSOLV                                                 

C                                                                       

C     GIVEN AN M BY N MATRIX A, AN N BY N DIAGONAL MATRIX D,            

C     AND AN M-VECTOR B, THE PROBLEM IS TO DETERMINE AN X WHICH         

C     SOLVES THE SYSTEM                                                 

C                                                                       

C           A*X = B ,     D*X = 0 ,                                     

C                                                                       

C     IN THE LEAST SQUARES SENSE.                                       

C                                                                       

C     THIS SUBROUTINE COMPLETES THE SOLUTION OF THE PROBLEM             

C     IF IT IS PROVIDED WITH THE NECESSARY INFORMATION FROM THE         

C     QR FACTORIZATION, WITH COLUMN PIVOTING, OF A. THAT IS, IF         

C     A*P = Q*R, WHERE P IS A PERMUTATION MATRIX, Q HAS ORTHOGONAL      

C     COLUMNS, AND R IS AN UPPER TRIANGULAR MATRIX WITH DIAGONAL        

C     ELEMENTS OF NONINCREASING MAGNITUDE, THEN QRSOLV EXPECTS          

C     THE FULL UPPER TRIANGLE OF R, THE PERMUTATION MATRIX P,           

C     AND THE FIRST N COMPONENTS OF (Q TRANSPOSE)*B. THE SYSTEM         

C     A*X = B, D*X = 0, IS THEN EQUIVALENT TO                           

C                                                                       

C                  T       T                                            

C           R*Z = Q *B ,  P *D*P*Z = 0 ,                                

C                                                                       

C     WHERE X = P*Z. IF THIS SYSTEM DOES NOT HAVE FULL RANK,            

C     THEN A LEAST SQUARES SOLUTION IS OBTAINED. ON OUTPUT QRSOLV       

C     ALSO PROVIDES AN UPPER TRIANGULAR MATRIX S SUCH THAT              

C                                                                       

C            T   T               T                                      

C           P *(A *A + D*D)*P = S *S .                                  

C                                                                       

C     S IS COMPUTED WITHIN QRSOLV AND MAY BE OF SEPARATE INTEREST.      

C                                                                       

C     THE SUBROUTINE STATEMENT IS                                       

C                                                                       

C       SUBROUTINE QRSOLV(N,R,LDR,IPVT,DIAG,QTB,X,SDIAG,WA)             

C                                                                       

C     WHERE                                                             

C                                                                       

C       N IS A POSITIVE INTEGER INPUT VARIABLE SET TO THE ORDER OF R.   

C                                                                       

C       R IS AN N BY N ARRAY. ON INPUT THE FULL UPPER TRIANGLE          

C         MUST CONTAIN THE FULL UPPER TRIANGLE OF THE MATRIX R.         

C         ON OUTPUT THE FULL UPPER TRIANGLE IS UNALTERED, AND THE       

C         STRICT LOWER TRIANGLE CONTAINS THE STRICT UPPER TRIANGLE      

C         (TRANSPOSED) OF THE UPPER TRIANGULAR MATRIX S.                

C                                                                       

C       LDR IS A POSITIVE INTEGER INPUT VARIABLE NOT LESS THAN N        

C         WHICH SPECIFIES THE LEADING DIMENSION OF THE ARRAY R.         

C                                                                       

C       IPVT IS AN INTEGER INPUT ARRAY OF LENGTH N WHICH DEFINES THE    

C         PERMUTATION MATRIX P SUCH THAT A*P = Q*R. COLUMN J OF P       

C         IS COLUMN IPVT(J) OF THE IDENTITY MATRIX.                     

C                                                                       

C       DIAG IS AN INPUT ARRAY OF LENGTH N WHICH MUST CONTAIN THE       

C         DIAGONAL ELEMENTS OF THE MATRIX D.                            

C                                                                       

C       QTB IS AN INPUT ARRAY OF LENGTH N WHICH MUST CONTAIN THE FIRST  

C         N ELEMENTS OF THE VECTOR (Q TRANSPOSE)*B.                     

C                                                                       

C       X IS AN OUTPUT ARRAY OF LENGTH N WHICH CONTAINS THE LEAST       

C         SQUARES SOLUTION OF THE SYSTEM A*X = B, D*X = 0.              

C                                                                       

C       SDIAG IS AN OUTPUT ARRAY OF LENGTH N WHICH CONTAINS THE         

C         DIAGONAL ELEMENTS OF THE UPPER TRIANGULAR MATRIX S.           

C                                                                       

C       WA IS A WORK ARRAY OF LENGTH N.                                 

C                                                                       

C     SUBPROGRAMS CALLED                                                

C                                                                       

C       FORTRAN-SUPPLIED ... ABS,SQRT                                   

C                                                                       

C     ARGONNE NATIONAL LABORATORY. MINPACK PROJECT. MARCH 1980.         

C     BURTON S. GARBOW, KENNETH E. HILLSTROM, JORGE J. MORE             

C                                                                       

C     **********                                                        

      DATA P5,P25,ZERO /5.0D-1,2.5D-1,0.0D0/                            

C                                                                       

C     COPY R AND (Q TRANSPOSE)*B TO PRESERVE INPUT AND INITIALIZE S.    

C     IN PARTICULAR, SAVE THE DIAGONAL ELEMENTS OF R IN X.              

C                                                                       

      DO 20 J = 1, N                                                    

         DO 10 I = J, N                                                 

            R(I,J) = R(J,I)                                             

   10       CONTINUE                                                    

         X(J) = R(J,J)                                                  

         WA(J) = QTB(J)                                                 

   20    CONTINUE                                                       

C                                                                       

C     ELIMINATE THE DIAGONAL MATRIX D USING A GIVENS ROTATION.          

C                                                                       

      DO 100 J = 1, N                                                   

C                                                                       

C        PREPARE THE ROW OF D TO BE ELIMINATED, LOCATING THE            

C        DIAGONAL ELEMENT USING P FROM THE QR FACTORIZATION.            

C                                                                       

         L = IPVT(J)                                                    

         IF (DIAG(L) .EQ. ZERO) GO TO 90                                

         DO 30 K = J, N                                                 

            SDIAG(K) = ZERO                                             

   30       CONTINUE                                                    

         SDIAG(J) = DIAG(L)                                             

C                                                                       

C        THE TRANSFORMATIONS TO ELIMINATE THE ROW OF D                  

C        MODIFY ONLY A SINGLE ELEMENT OF (Q TRANSPOSE)*B                

C        BEYOND THE FIRST N, WHICH IS INITIALLY ZERO.                   

C                                                                       

         QTBPJ = ZERO                                                   

         DO 80 K = J, N                                                 

C                                                                       

C           DETERMINE A GIVENS ROTATION WHICH ELIMINATES THE            

C           APPROPRIATE ELEMENT IN THE CURRENT ROW OF D.                

C                                                                       

            IF (SDIAG(K) .EQ. ZERO) GO TO 70                            

            IF (ABS(R(K,K)) .GE. ABS(SDIAG(K))) GO TO 40                

               COTAN = R(K,K)/SDIAG(K)                                  

               SIN = P5/SQRT(P25+P25*COTAN**2)                          

               COS = SIN*COTAN                                          

               GO TO 50                                                 

   40       CONTINUE                                                    

               TAN = SDIAG(K)/R(K,K)                                    

               COS = P5/SQRT(P25+P25*TAN**2)                            

               SIN = COS*TAN                                            

   50       CONTINUE                                                    

C                                                                       

C           COMPUTE THE MODIFIED DIAGONAL ELEMENT OF R AND              

C           THE MODIFIED ELEMENT OF ((Q TRANSPOSE)*B,0).                

C                                                                       

            R(K,K) = COS*R(K,K) + SIN*SDIAG(K)                          

            TEMP = COS*WA(K) + SIN*QTBPJ                                

            QTBPJ = -SIN*WA(K) + COS*QTBPJ                              

            WA(K) = TEMP                                                

C                                                                       

C           ACCUMULATE THE TRANFORMATION IN THE ROW OF S.               

C                                                                       

            KP1 = K + 1                                                 

            IF (N .LT. KP1) GO TO 70                                    

            DO 60 I = KP1, N                                            

               TEMP = COS*R(I,K) + SIN*SDIAG(I)                         

               SDIAG(I) = -SIN*R(I,K) + COS*SDIAG(I)                    

               R(I,K) = TEMP                                            

   60          CONTINUE                                                 

   70       CONTINUE                                                    

   80       CONTINUE                                                    

   90    CONTINUE                                                       

C                                                                       

C        STORE THE DIAGONAL ELEMENT OF S AND RESTORE                    

C        THE CORRESPONDING DIAGONAL ELEMENT OF R.                       

C                                                                       

         SDIAG(J) = R(J,J)                                              

         R(J,J) = X(J)                                                  

  100    CONTINUE                                                       

C                                                                       

C     SOLVE THE TRIANGULAR SYSTEM FOR Z. IF THE SYSTEM IS               

C     SINGULAR, THEN OBTAIN A LEAST SQUARES SOLUTION.                   

C                                                                       

      NSING = N                                                         

      DO 110 J = 1, N                                                   

         IF (SDIAG(J) .EQ. ZERO .AND. NSING .EQ. N) NSING = J - 1       

         IF (NSING .LT. N) WA(J) = ZERO                                 

  110    CONTINUE                                                       

      IF (NSING .LT. 1) GO TO 150                                       

      DO 140 K = 1, NSING                                               

         J = NSING - K + 1                                              

         SUM = ZERO                                                     

         JP1 = J + 1                                                    

         IF (NSING .LT. JP1) GO TO 130                                  

         DO 120 I = JP1, NSING                                          

            SUM = SUM + R(I,J)*WA(I)                                    

  120       CONTINUE                                                    

  130    CONTINUE                                                       

         WA(J) = (WA(J) - SUM)/SDIAG(J)                                 

  140    CONTINUE                                                       

  150 CONTINUE                                                          

C                                                                       

C     PERMUTE THE COMPONENTS OF Z BACK TO COMPONENTS OF X.              

C                                                                       

      DO 160 J = 1, N                                                   

         L = IPVT(J)                                                    

         X(L) = WA(J)                                                   

  160    CONTINUE                                                       

      RETURN                                                            

C                                                                       

C     LAST CARD OF SUBROUTINE QRSOLV.                                   

C                                                                       

      END                                                               

CDC   FUNCTION XPN(X)                                                   

      DOUBLE PRECISION FUNCTION XPN(X)                                  

C=======================================================================

C                                                                       

C     FUNCTION XPN    IS USED TO ENSURE THAT THE MAGINTUDE OF THE       

C     ************    FUNCTION EXP IS NOT "TOO LARGE".                  

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      XMIN  = -75.0D0                                                   

      XMAX  =  75.0D0                                                   

      IF  ( X .LT. XMIN )  X = XMIN                                     

      IF  ( X .GT. XMAX )  X = XMAX                                     

      XPN   =  EXP(X)                                                   

      RETURN                                                            

      END                                                               

      DOUBLE PRECISION FUNCTION YLOG(X)                                 

C=======================================================================

C                                                                       

C     FUNCTION YLOG    IS USED TO ENSURE THAT THE ARGUMENT OF THE       

C     *************    LOG FUNCTION IS POSITIVE.                        

C                                                                       

C=======================================================================

      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               

      IF ( X .LT. 0.000001D0 ) X = 0.000001D0                           

      YLOG = LOG(X)                                                     

      RETURN                                                            

      END                                                               

      SUBROUTINE NEWRES                                                 

      IMPLICIT DOUBLE PRECISION ( A-H,O-Z )                             

      CHARACTER*8   NAME, IXOG                                          

      LOGICAL       LFIX2                                               

      COMMON/ALPHA/ NAME(300), IXOG(600)                                

      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),

     1          ERX(600,300), EE(300,300), XX(600,300)

      COMMON/HELEN/F(300),A(600),BA(300),JX(10),VX(10),                 

     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     

     2,PR,NPER,NSK,IJX,IPER,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR

     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         

      COMMON/BASDAT/ XB(600,300)                                         

      COMMON/KGROUP/KK(41)/JGROUP/JJ(41)                                

      COMMON/WOREQN/IBLOC,NCONV,NBLOC                                   

      COMMON/NEWCAL/NEN(100),NEX(100),NENH(100),NEXP,MXEH,IOPTT,IOPTB,  

     1IOPTC                                                             

      COMMON/SIMDIM/NAG,LPER,MAXE,MAXX,MAXP                             

      COMMON/FIX2/ LFIX2, NFIX2, LDOFIX, NFIXMX, NFIXEND                

      DO 10000 IC = 1,2                                                

      NV  =  KK(IC+1) - KK(IC)                                          

      DO 8000 IV = 1,NV                                                 

      LPRINT = 0                                                        

      KVAR = KK(IC) + IV                                                

      DO 6000 IY = 1,KAG                                                

      V  =  ER(KVAR,IY)                                                 

      IF ( ABS(V) .GT. 0.000001 ) THEN                                  

        LPRINT  =  1                                                    

      END IF                                                            

 6000 CONTINUE

      IF ( LPRINT .EQ. 1 ) THEN                                         

      WRITE(8,7000) IC,IV                                               

c      WRITE(8,7500) ( ER(KVAR,IY),IY=1,KAG )                            

      WRITE(8,7500) ( ER(KVAR,IY),IY=1,234 )                            

      END IF                                                            

 7000 FORMAT(2I2)                                                       

 7500 FORMAT(4F18.12)                                                    

 8000 CONTINUE                                                          

10000 CONTINUE                                                          

      RETURN                                                            

      END    

      SUBROUTINE FTPL                                                     
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
      CHARACTER*8   NAME, IXOG                                          
      LOGICAL       LFIX2                                               
      COMMON/ALPHA/ NAME(300), IXOG(600)                                
      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),
     1          DUMMY(1500,300)
      COMMON/HELEN/V(300),Z(600),BA(300),JX(10),VX(10),                 
     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,XX,P,TOL,TOLR,BR     
     2,PR,NPER,NSK,IJX,I,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR   
     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         
      COMMON/KGROUP/K01,K02,K03,K04,K05,K06,K07,K08,K09,K10,K11,K12,K13,
     1K14,K15,K16,K17,K18,K19,K20,K21,K22,K23,K24,K25,K26,K27,K28,K29,  
     2K30,K31,K32,K33,K34,K35,K36,K37,K38,K39,K40,K41                   
      COMMON/JGROUP/J01,J02,J03,J04,J05,J06,J07,J08,J09,J10,J11,J12,J13,
     1J14,J15,J16,J17,J18,J19,J20,J21,J22,J23,J24,J25,J26,J27,J28,J29,  
     2J30,J31,J32,J33,J34,J35,J36,J37,J38,J39,J40,J41                   
      COMMON/FIX2/ LFIX2, NFIX2, LDOFIX, NFIXMX, NFIXEND                
      COMMON/SHK/err(13,1000), zfiscal_outputgap, zfiscal_pinf
      double precision zerr(13,1000)
C-----------------------------------------------------------------------
                                                     
      PARAMETER( ZERO = 0.00D0 ,
     &           P03  = 0.03D0,
     &           P25  = 0.25D0,
     &           P30  = 0.30D0,
     &           P50  = 0.50D0,
     &           P75  = 0.75D0,
     &           ONE  = 1.00D0 )
      DV(I,J,K)=VV(I,J,K)-VV(I,J,K-1)                                   
      DL(I,J,K)=VV(I,J,K)/VV(I,J,K-1)      

      CONSTEPINF=0.78
      CONSTEBETA=0.16
      CONSTELAB=0.53
      CTREND=0.43
      CBETA=ONE/(1+CONSTEBETA/100.0)
      CGAMMA=CTREND/100.0+1
      CTOU=0.025
      CLANDAW=1.5
      CG=0.18
      CURVP=10
      CURVW=10
      CGY=0.52612
      CLANDAP=Z(10)
      CBETABAR=CBETA*CGAMMA**(-Z(2))
      CRK=(1.7**Z(18))*(CBETA**(-1))*(CGAMMA**Z(2))-(ONE-CTOU)
      CW=(Z(15)**Z(15)*(ONE-Z(15))**(1-Z(15))
     >/(CLANDAP*CRK**Z(15)))**(ONE/(ONE-Z(15)))
      CIKBAR=(ONE-(ONE-CTOU)/CGAMMA)
      CIK=(ONE-(ONE-CTOU)/CGAMMA)*CGAMMA
      CLK=((ONE-Z(15))/Z(15))*(CRK/CW)
      CKY=Z(10)*(CLK)**(Z(15)-ONE)
      CIY=CIK*CKY
      CCY=ONE-CG-CIK*CKY
      CRKKY=CRK*CKY
      CWHLC=(ONE/CLANDAW)*(ONE-Z(15))/Z(15)*CRK*CKY/CCY
      CWLY=ONE-CRK*CKY
      PARPI=ZERO

      if(i.eq.mag) then
      do 3110, k=1,13
      do 3111, j=1,234
         zerr(k,j)=err(k,j)
 3111 continue
 3110 continue

      else
      do 2110, k=1,13
      do 2111, j=1,234
         zerr(k,j)=0.0d0
 2111    continue
 2110 continue
      endif

      XNB0=564.4724849d0
      
C-----------------------------------------------------------------------
c.....shock_g : government spending fiscal response to output and inflation
C.... deviation from base
c      X(22,I)=
c!     >Z(22)*VV(21,22,-1)+Z(23)*zerr(5,i)+zerr(1,i)
c!     >VV(21,22,-1)+Z(31)*(VV(21,22,-1)-VV(21,22,-2))+zerr(1,i)
cc     >-Z(27)*(VV(1,8,0)-VV(21,8,0)-VV(21,26,0))
c     >-Z(27)*(VV(1,8,0)-VV(21,8,0))
c     >-Z(28)*(VV(1,5,0)-2.0d0)
c     >+zfiscal_outputgap+zfiscal_pinf
c.....shock_g : government spending data
      X(22,I)=
!     >Z(22)*VV(21,22,-1)+Z(23)*zerr(5,i)+zerr(1,i)
     >VV(21,22,-1)+Z(31)*(VV(21,22,-1)-VV(21,22,-2))+zerr(1,i)
CC-----------------------------------------------------------------------
c.....shock_1 : interest rate equation
      X(23,I)=
     >Z(32)*VV(21,23,-1)+zerr(2,i)
C-----------------------------------------------------------------------
c.....shock_2 : investment equation
      X(24,I)=
     >Z(33)*VV(21,24,-1)+zerr(3,i)
C-----------------------------------------------------------------------
c.....shock_3 : inflation equation
      X(25,I)=
     >Z(34)*VV(21,25,-1)+zerr(4,i)
C-----------------------------------------------------------------------
C.....shock_4 : output equation
      X(26,I)=
     >VV(21,26,-1)+Z(35)*(VV(21,26,-1)-VV(21,26,-2))+zerr(5,i)
C-----------------------------------------------------------------------
C.....shock_5 : rk equation
      X(27,I)=
     >Z(36)*VV(21,27,-1)+zerr(6,i)
C-----------------------------------------------------------------------
C.....shock_6 : w equation
      X(28,I)=
     >Z(37)*VV(21,28,-1)+zerr(7,i)
C-----------------------------------------------------------------------
C.....shock_NC6 : NC w equation
      X(29,I)=
     >Z(38)*VV(21,29,-1)+zerr(8,i)
C-----------------------------------------------------------------------
cC.....SHOCK_7: PREMIUM SHOCK
      X(30,I)=
     >Z(39)*VV(21,30,-1)+zerr(9,i)
c----------------------------------------------------------------------
cc.....shock_8: networth shock
      X(31,I)=
     >Z(40)*VV(21,31,-1)+zerr(10,i)	
c----------------------------------------------------------------------
cc.....shock_8: mzero shock
      X(32,I)=
     >Z(41)*VV(21,32,-1)+zerr(11,i)	

C-----------------------------------------------------------------------
c.....d(surplus) responds to government spending
      X(36,I)=
     >VV(21,36,-1)-VV(21,22,0)
     >+zerr(13,i)
cc.....surplus ARIMA(1,1,0): surplus data
c      X(36,I)=
c     >VV(21,36,-1)+Z(27)*(VV(21,36,-1)-VV(21,36,-2))
c     >+zerr(13,i)
c----------------------------------------------------------------------
cc.....NB
      X(37,I)=VV(21,37,0)
c----------------------------------------------------------------------
cc.....pibar
      X(38,I)=VV(21,38,0)
c----------------------------------------------------------------------
cc.....NBbase
      X(39,I)=VV(21,39,0)
C-----------------------------------------------------------------------
c.....r :
      IF ( LDOFIX .EQ. 1 ) THEN
      ER(1,I) = VV(21,1,0) -
     >( VV(21,38,0)+ONE/((ONE-Z(3)/CGAMMA)
     >/(Z(2)*(ONE+Z(3)/CGAMMA)))
     >*((Z(3)/CGAMMA)/(ONE+Z(3)/CGAMMA)*VV(1,7,-1)
     >+(ONE/(ONE+Z(3)/CGAMMA))*VV(21,18,0)
     >+((Z(2)-ONE)*CWHLC/(Z(2)*(ONE+Z(3)/CGAMMA)))
     >*(VV(1,9,0)-VV(21,19,0))+VV(21,23,0)-VV(1,7,0)) )
      END IF
      V(1) = VV(21,38,0)+ONE/((ONE-Z(3)/CGAMMA)
     >/(Z(2)*(ONE+Z(3)/CGAMMA)))
     >*((Z(3)/CGAMMA)/(ONE+Z(3)/CGAMMA)*VV(1,7,-1)
     >+(ONE/(ONE+Z(3)/CGAMMA))*VV(21,18,0)
     >+((Z(2)-ONE)*CWHLC/(Z(2)*(ONE+Z(3)/CGAMMA)))
     >*(VV(1,9,0)-VV(21,19,0))+VV(21,23,0)-VV(1,7,0))
     >+RR(1,1,0) 
C-----------------------------------------------------------------------
c.....inv :
      IF ( LDOFIX .EQ. 1 ) THEN
      ER(2,I) = VV(21,2,0) -
     > ( (ONE/(ONE+CBETABAR*CGAMMA))*(VV(1,2,-1)
     >+CBETABAR*CGAMMA*VV(21,15,0)
     >+(ONE/(CGAMMA*CGAMMA*Z(1)))*VV(1,3,0))+VV(21,24,0) )
      END IF
      V(2) = (ONE/(ONE+CBETABAR*CGAMMA))*(VV(1,2,-1)
     >+CBETABAR*CGAMMA*VV(21,15,0)
     >+(ONE/(CGAMMA*CGAMMA*Z(1)))*VV(1,3,0))+VV(21,24,0)
     >+RR(1,2,0)
      
C-----------------------------------------------------------------------
c.....pk :
      IF ( LDOFIX .EQ. 1 ) THEN
      ER(3,I) = VV(21,3,0) -
     > ( -VV(21,21,0)+(CRK/(CRK+(ONE-CTOU)))*VV(21,16,0)
     >+((ONE-CTOU)/(CRK+(ONE-CTOU)))*VV(21,20,0) )
      END IF
      V(3) = -VV(21,21,0)+(CRK/(CRK+(ONE-CTOU)))*VV(21,16,0)
     >+((ONE-CTOU)/(CRK+(ONE-CTOU)))*VV(21,20,0)
     >+RR(1,3,0)
C-----------------------------------------------------------------------
c.....kp :
      IF ( LDOFIX .EQ. 1 ) THEN
      ER(4,I) = VV(21,4,0) -
     > ( (ONE-CIKBAR)*VV(1,4,-1)+CIKBAR*VV(1,2,0)
     >+CIKBAR*CGAMMA*CGAMMA*Z(1)*VV(21,24,0) )
      END IF
      V(4) = (ONE-CIKBAR)*VV(1,4,-1)+CIKBAR*VV(1,2,0)
     >+CIKBAR*CGAMMA*CGAMMA*Z(1)*VV(21,24,0)
     >+RR(1,4,0)
C-----------------------------------------------------------------------
c.....pinf :
      IF ( LDOFIX .EQ. 1 ) THEN
      ER(5,I) = VV(21,5,0) -
     > ( (VV(1,1,0)
     >-Z(12)*(VV(1,1,-1))-Z(13)*(ONE-Z(12))*VV(1,8,0)
     >-Z(14)*(VV(1,8,0)-VV(1,8,-1))-VV(21,25,0))/(Z(11)*(ONE-Z(12))) )
      END IF
c      V(5) = (VV(1,1,0)-Z(12)*VV(1,1,-1)-Z(13)*(ONE-Z(12))*VV(1,8,0)
      V(5) = (VV(1,1,0)
     >-Z(12)*(VV(1,1,-1))-Z(13)*(ONE-Z(12))*VV(1,8,0)
     >-Z(14)*(VV(1,8,0)-VV(1,8,-1))-VV(21,25,0))/(Z(11)*(ONE-Z(12)))
     >+RR(1,5,0)
C-----------------------------------------------------------------------
c.....wNK :
      IF ( LDOFIX .EQ. 1 ) THEN
         ER(17,I) = VV(21,7,0) - 
     >( (ONE/(ONE+(ONE-Z(4))
     >*((ONE-CBETABAR*CGAMMA*Z(4))/((ONE+CBETABAR*CGAMMA)*Z(4)))
     >*(ONE/((CLANDAW-ONE)*CURVW+ONE))))
     >*( (ONE/(ONE+CBETABAR*CGAMMA))*VV(1,6,-1)
     >+((CBETABAR*CGAMMA)/(ONE+CBETABAR*CGAMMA))*VV(21,17,0)
     >+(Z(7)/(ONE+CBETABAR*CGAMMA))*VV(1,5,-1)
     >-((ONE+CBETABAR*CGAMMA*Z(7))/(ONE+CBETABAR*CGAMMA))*VV(1,5,0)
     >+((CBETABAR*CGAMMA)/(ONE+CBETABAR*CGAMMA))*VV(21,38,0)
     >+((ONE-Z(4))
     >*((ONE-CBETABAR*CGAMMA*Z(4))/((ONE+CBETABAR*CGAMMA)*Z(4)))
     >*(ONE/((CLANDAW-ONE)*CURVW+ONE)))
     >*(Z(5)*VV(1,9,0)+(ONE/(ONE-Z(3)/CGAMMA))*VV(1,7,0)
     >-((Z(3)/CGAMMA)/(ONE-Z(3)/CGAMMA))*VV(1,7,-1))
     >+VV(21,28,0) ) )
      END IF
      V(17) = (ONE/(ONE+(ONE-Z(4))
     >*((ONE-CBETABAR*CGAMMA*Z(4))/((ONE+CBETABAR*CGAMMA)*Z(4)))
     >*(ONE/((CLANDAW-ONE)*CURVW+ONE))))
     >*( (ONE/(ONE+CBETABAR*CGAMMA))*VV(1,6,-1)
     >+((CBETABAR*CGAMMA)/(ONE+CBETABAR*CGAMMA))*VV(21,17,0)
     >+(Z(7)/(ONE+CBETABAR*CGAMMA))*VV(1,5,-1)
     >-((ONE+CBETABAR*CGAMMA*Z(7))/(ONE+CBETABAR*CGAMMA))*VV(1,5,0)
     >+((CBETABAR*CGAMMA)/(ONE+CBETABAR*CGAMMA))*VV(21,38,0)
     >+((ONE-Z(4))
     >*((ONE-CBETABAR*CGAMMA*Z(4))/((ONE+CBETABAR*CGAMMA)*Z(4)))
     >*(ONE/((CLANDAW-ONE)*CURVW+ONE)))
     >*(Z(5)*VV(1,9,0)+(ONE/(ONE-Z(3)/CGAMMA))*VV(1,7,0)
     >-((Z(3)/CGAMMA)/(ONE-Z(3)/CGAMMA))*VV(1,7,-1))
     >+VV(21,28,0) ) + RR(1,17,0)
 
C-----------------------------------------------------------------------
c.....wNC :
      IF ( LDOFIX .EQ. 1 ) THEN
         ER(18,I) = VV(21,7,0) - 
     >( Z(5)*VV(1,9,0)+(ONE/(ONE-Z(3)/CGAMMA))*VV(1,7,0)
     >-((Z(3)/CGAMMA)/(ONE-Z(3)/CGAMMA))*VV(1,7,-1)
     >-(VV(1,5,0)-VV(21,38,-1))
     >+VV(21,29,0) )
      END IF
    
      V(18) = Z(5)*VV(1,9,0)+(ONE/(ONE-Z(3)/CGAMMA))*VV(1,7,0)
     >-((Z(3)/CGAMMA)/(ONE-Z(3)/CGAMMA))*VV(1,7,-1)
     >-(VV(1,5,0)-VV(21,38,-1))
     >+VV(21,29,0) + RR(1,18,0)

C-----------------------------------------------------------------------
c.....w :
      V(6) = Z(16)*VV(1,17,0)+(ONE-Z(16))*VV(1,18,0)
       
C-----------------------------------------------------------------------
c.....c :
      IF ( LDOFIX .EQ. 1 ) THEN
      ER(7,I) = VV(21,7,0) -
     > ( (VV(1,8,0)-CIY*VV(1,2,0)-VV(21,22,0)
     >-ONE*CRKKY*(ONE/(Z(9)/(ONE-Z(9))))*VV(1,10,0)
     >-0.01*VV(1,13,0))/CCY )
      END IF
      V(7) = (VV(1,8,0)-CIY*VV(1,2,0)-VV(21,22,0)
     >-ONE*CRKKY*(ONE/(Z(9)/(ONE-Z(9))))*VV(1,10,0)
     >-0.01*VV(1,13,0))/CCY
     >+RR(1,7,0)
C-----------------------------------------------------------------------
c.....y :
      IF ( LDOFIX .EQ. 1 ) THEN
      ER(8,I) = VV(21,8,0) -
     > ( Z(10)*(Z(15)*(VV(1,4,-1)
     >+(ONE/(Z(9)/(ONE-Z(9))))*VV(1,10,0))
     >+(ONE-Z(15))*VV(1,9,0)+VV(21,26,0)) )
      END IF
      V(8) = Z(10)*(Z(15)*(VV(1,4,-1)
     >+(ONE/(Z(9)/(ONE-Z(9))))*VV(1,10,0))
     >+(ONE-Z(15))*VV(1,9,0)+VV(21,26,0))
     > +RR(1,8,0)
C-----------------------------------------------------------------------
c.....lab :
      IF ( LDOFIX .EQ. 1 ) THEN
      ER(9,I) = VV(21,9,0) -
     > ( VV(1,10,0)-VV(1,6,0)+(VV(1,4,-1)
     >+(ONE/(Z(9)/(ONE-Z(9))))*VV(1,10,0)) )
      END IF
      V(9) = VV(1,10,0)-VV(1,6,0)+(VV(1,4,-1)
     >+(ONE/(Z(9)/(ONE-Z(9))))*VV(1,10,0))
     > +RR(1,9,0)
C-----------------------------------------------------------------------
c.....RK :
      IF ( LDOFIX .EQ. 1 ) THEN
      ER(10,I) = VV(21,10,0) -
     > ( Z(17)*((((VV(1,5,0)-VV(21,27,0))/
     >(ONE/(ONE+CBETABAR*CGAMMA*Z(8)))
     >-(CBETABAR*CGAMMA*VV(21,38,0)+Z(8)*VV(1,5,-1)))/
     >(((ONE-Z(6))*(ONE-CBETABAR*CGAMMA*Z(6))/Z(6))
     >/((Z(10)-ONE)*CURVP+ONE))
     >-(ONE-Z(15))*VV(1,6,0)+VV(21,26,0))/Z(15))
     >+(ONE-Z(17))*((1-Z(15))/Z(15)*(-VV(1,6,0))+VV(21,26,0)/Z(15)) )
      END IF
      V(10) = Z(17)*((((VV(1,5,0)-VV(21,27,0))/
     >(ONE/(ONE+CBETABAR*CGAMMA*Z(8)))
     >-(CBETABAR*CGAMMA*VV(21,38,0)+Z(8)*VV(1,5,-1)))/
     >(((ONE-Z(6))*(ONE-CBETABAR*CGAMMA*Z(6))/Z(6))
     >/((Z(10)-ONE)*CURVP+ONE))
     >-(ONE-Z(15))*VV(1,6,0)+VV(21,26,0))/Z(15))
     >+(ONE-Z(17))*((1-Z(15))/Z(15)*(-VV(1,6,0))+VV(21,26,0)/Z(15))
     >+RR(1,10,0)
C-----------------------------------------------------------------------
c..... s:
      IF ( LDOFIX .EQ. 1 ) THEN
      ER(11,I) = VV(21,11,0) - 
     > ( -Z(18)*(VV(1,13,0)-VV(1,3,0)-VV(1,4,0))-Z(21)*VV(1,15,0)
     >+VV(21,30,0) )
      END IF
      V(11) = -Z(18)*(VV(1,13,0)-VV(1,3,0)-VV(1,4,0))-Z(21)*VV(1,15,0)
     >+VV(21,30,0) 
     >+RR(1,11,0)
C      V(11)=ZERO
C-----------------------------------------------------------------------
c..... cy:
c..... cy:
      IF ( LDOFIX .EQ. 1 ) THEN
      ER(12,I) = VV(21,12,0) -
     > ( VV(1,11,-1)+(VV(1,1,-1)-VV(21,38,-1)) )
      END IF
      V(12) = VV(1,11,-1)+(VV(1,1,-1)-VV(21,38,-1))
     > +RR(1,12,0)
c      V(12)=ZERO
C-----------------------------------------------------------------------
c..... n:
      IF ( LDOFIX .EQ. 1 ) THEN
      ER(13,I) = VV(21,13,0) -
     > ( 1.7*VV(1,14,0)-0.7*VV(21,21,-1)+0.99*VV(1,13,-1)
     >+VV(21,31,0) )
      END IF
      V(13) = 1.7*VV(1,14,0)-0.7*VV(21,21,-1)+0.99*VV(1,13,-1)
     >+VV(21,31,0)
     >+RR(1,13,0)
c      V(13)=ZERO
C-----------------------------------------------------------------------
c..... yk:
      IF ( LDOFIX .EQ. 1 ) THEN
      ER(14,I) = VV(21,12,0) -
     > ( (CRK/(CRK+(ONE-CTOU)))*(VV(1,8,0)-VV(1,4,0))
     >+((ONE-CTOU)/(CRK+(ONE-CTOU)))*VV(1,3,0)-VV(1,3,-1) )
      END IF
      V(14)=(CRK/(CRK+(ONE-CTOU)))*(VV(1,8,0)-VV(1,4,0))
     >+((ONE-CTOU)/(CRK+(ONE-CTOU)))*VV(1,3,0)-VV(1,3,-1)
     >+RR(1,14,0)
C-----------------------------------------------------------------------
c..... MZERO:
      IF ( LDOFIX .EQ. 1 ) THEN
      ER(15,I) = VV(21,34,0) - 
     > ( VV(1,15,-1)+Z(20)*(VV(1,16,0)-VV(1,16,-1))+VV(21,32,0) ) 
      END IF
      V(15) = VV(1,15,-1)+Z(20)*(VV(1,16,0)-VV(1,16,-1))+VV(21,32,0)
     >+RR(1,15,0)
C-----------------------------------------------------------------------
c..... MTWO:
      IF ( LDOFIX .EQ. 1 ) THEN
      ER(16,I) = VV(21,35,0) - 
     > ( (ONE+0.245928-0.073148)*VV(1,4,0)+0.073148*VV(1,15,0)
     >-0.245928*VV(1,13,0) )
      END IF
      V(16) = (ONE+0.245928-0.073148)*VV(1,4,0)+0.073148*VV(1,15,0)
     >-0.245928*VV(1,13,0)
     >+RR(1,16,0)
      

      RETURN                                                            
      END              
      SUBROUTINE CALCFX( FX )
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)                               
      COMMON/L2/E(300,300),   X(600,300),  ER(300,300),
     1          ERX(600,300), EE(300,300), XX(600,300)
      COMMON/HELEN/F(300),Z(600),BA(300),JX(10),VX(10),                 
     1 KAG,MAG,NDOG,LEXOG,IPAR,IRAND,B,NSTART,ITMX,PX,P,TOL,TOLR,BR     
     2,PR,NPER,NSK,IJX,I   ,IDATE,NEGP,T,IOPT,IDYN,ITR,NARG,BSTEP,MAXITR
     3,NXGP,NCG,IDYN1,IRANDX,IB,IRHO                                         
      COMMON/KGROUP/KK(41)/JGROUP/JJ(41)                                
      COMMON/WOREQN/IBLOC,NCONV,NBLOC                                   
      COMMON/NEWCAL/NEN(100),NEX(100),NENH(100),NEXP,MXEH,IOPTT,IOPTB,  
     1IOPTC                                                             
      COMMON/SIMDIM/NAG,LPER,MAXE,MAXX,MAXP                             
      COMMON/POWELL/R(200000),DIR(200000),L(200000),ICTRL                     

      KX(I,J)=600*(J-1)+I                                               
      KVV(IG,J,K)=KX(KK(IG)+J,I+K)                                      

C-----------------------------------------------------------------------

      J = 0
      ZERO   = 0.00D0
      P25    = 0.25D0
      P33    = 1.00D0 / 3.00D0
      P50    = 0.50D0
      ONE    = 1.00D0
      TWO    = 2.00D0
      THREE  = 3.00D0
      FOUR   = 4.00D0

C.....Terminal conditions for model
      RPINFTERM = x(5,KAG-0)
      RINVTERM = x(2,KAG-0)+(ONE/(ONE-Z(15)))*X(26,KAG-0)
      RRKTERM = x(10,KAG-0)
      RWTERM = x(6,KAG-0)+(ONE/(ONE-Z(15)))*X(26,KAG-0)
      RCTERM = x(7,KAG-0)+(ONE/(ONE-Z(15)))*X(26,KAG-0)
      RLABTERM = x(9,KAG-0)
      RPKTERM = x(3,KAG-0)+(ONE/(ONE-Z(15)))*X(26,KAG-0)
      RCYTERM = x(12,KAG-0)
c      RPINFTERM = x(5,kag)
c      RINVTERM = x(2,kag)
c      RRKTERM = x(10,kag)
c      RWTERM = x(6,kag)
c      RCTERM = x(7,kag)
c      RLABTERM = x(9,kag)
c      RPKTERM = x(3,kag)
c      RCYTERM = x(12,kag)


      MAGM1  = MAG - 1
      MAGM2  = MAG - 2
      MAGM3  = MAG - 3
      MAGM4  = MAG - 4
      KAGM1  = KAG - 1
      KAGM2  = KAG - 2
      KAGM3  = KAG - 3
      KAGM4  = KAG - 4
      KAGM5  = KAG - 5
      KAGM6  = KAG - 6

C-----------------------------------------------------------------------

C.....E(t)[pinf(t+1)]
      DO 2000 I = MAG,KAG
      J = J + 1
      IF ( I .GT. KAG-0 ) THEN
      L(J) = KVV(21,14,0)
      R(J) = RPINFTERM - VV(21,14,0)
      ELSE
      L(J) = KVV(21,14,0)
      R(J) = VV(1,5,1) - VV(21,14,0)
      END IF
 2000  CONTINUE

C.....E(t)[inv(t+1)]
      DO 4000 I = MAG,KAG
      J = J + 1
      IF ( I .EQ. KAG-0 ) THEN
      L(J) = KVV(21,15,0)
      R(J) = RINVTERM - VV(21,15,0)
      ELSE
      L(J) = KVV(21,15,0)
      R(J) = VV(1,2,1) - VV(21,15,0)
      END IF
 4000  CONTINUE
C.....E(t)[rk(t+1)]
      DO 4010 I = MAG,KAG
      J = J + 1
      IF ( I .EQ. KAG-0 ) THEN
      L(J) = KVV(21,16,0)
      R(J) = RRKTERM - VV(21,16,0)
      ELSE
      L(J) = KVV(21,16,0)
      R(J) = VV(1,10,1) - VV(21,16,0)
      END IF
 4010  CONTINUE
C.....E(t)[w(t+1)]
      DO 4011 I = MAG,KAG
      J = J + 1
      IF ( I .EQ. KAG-0 ) THEN
      L(J) = KVV(21,17,0)
      R(J) = RWTERM - VV(21,17,0)
      ELSE
      L(J) = KVV(21,17,0)
      R(J) = VV(1,6,1) - VV(21,17,0)
      END IF
 4011  CONTINUE
C.....E(t)[c(t+1)]
      DO 4012 I = MAG,KAG
      J = J + 1
      IF ( I .EQ. KAG-0 ) THEN
      L(J) = KVV(21,18,0)
      R(J) = RCTERM - VV(21,18,0)
      ELSE
      L(J) = KVV(21,18,0)
      R(J) = VV(1,7,1) - VV(21,18,0)
      END IF
 4012  CONTINUE
C.....E(t)[lab(t+1)]
      DO 4013 I = MAG,KAG
      J = J + 1
      IF ( I .EQ. KAG-0 ) THEN
      L(J) = KVV(21,19,0)
      R(J) = RLABTERM - VV(21,19,0)
      ELSE
      L(J) = KVV(21,19,0)
      R(J) = VV(1,9,1) - VV(21,19,0)
      END IF
 4013  CONTINUE
      
C.....E(t)[pk(t+1)]
      DO 4014 I = MAG,KAG
      J = J + 1
      IF ( I .EQ. KAG-0 ) THEN
      L(J) = KVV(21,20,0)
      R(J) = RPKTERM - VV(21,20,0)
      ELSE
      L(J) = KVV(21,20,0)
      R(J) = VV(1,3,1) - VV(21,20,0)
      END IF
 4014  CONTINUE
      
C.....E(t)[cy(t+1)]
      DO 4015 I = MAG,KAG
      J = J + 1
      IF ( I .EQ. KAG-0 ) THEN
c      IF ( I .GE. MAG ) THEN
      L(J) = KVV(21,21,0)
      R(J) = RCYTERM - VV(21,21,0)
      ELSE
      L(J) = KVV(21,21,0)
      R(J) = VV(1,12,1) - VV(21,21,0)
      END IF
 4015  CONTINUE
      
C-----------------------------------------------------------------------
C.....Pre-forecast period :
      I = MAG - 1

C.....E(t)[pinf(t+1)] 
      J = J + 1
      L(J) = KVV(21,14,0)
      R(J) = VV(1,5,1) - VV(21,14,0)

C.....E(t)[inv(t+1)]
      J = J + 1
      L(J) = KVV(21,15,0)
      R(J) = VV(1,2,1) - VV(21,15,0)

C.....E(t)[rk(t+1)]
      J = J + 1
      L(J) = KVV(21,16,0)
      R(J) = VV(1,10,1) - VV(21,16,0)

C.....E(t)[w(t+1)]
      J = J + 1
      L(J) = KVV(21,17,0)
      R(J) = VV(1,6,1) - VV(21,17,0)

C.....E(t)[c(t+1)]
      J = J + 1
      L(J) = KVV(21,18,0)
      R(J) = VV(1,7,1) - VV(21,18,0)

C.....E(t)[lab(t+1)]
      J = J + 1
      L(J) = KVV(21,19,0)
      R(J) = VV(1,9,1) - VV(21,19,0)

C.....E(t)[pk(t+1)]
      J = J + 1
      L(J) = KVV(21,20,0)
      R(J) = VV(1,3,1) - VV(21,20,0)

C.....E(t)[cy(t+1)]
      J = J + 1
      L(J) = KVV(21,21,0)
      R(J) = VV(1,12,1) - VV(21,21,0)



      RETURN
      END


      function gen_errname(i)
      implicit none
      integer i,i1,j,k
      character*10 digit
      character*18 gen_errname
      digit='0123456789'
      gen_errname(1:9)='tmp/errnc'
      gen_errname(14:18)='.data'
      i1=i
      do 10, j=13,10,-1
         k = i1 - int(i1/10)*10 + 1
         i1 = int(i1/10)
         gen_errname(j:j) = digit(k:k)
 10   continue
      return
      end

      function gen_xname(i)
      implicit none
      integer i,i1,j,k
      character*10 digit
      character*18 gen_xname
      digit='0123456789'
      gen_xname(1:9)='tmp/exonc'
      gen_xname(14:18)='.data'
      i1=i
      do 10, j=13,10,-1
         k = i1 - int(i1/10)*10 + 1
         i1 = int(i1/10)
         gen_xname(j:j) = digit(k:k)
 10   continue
      return
      end
