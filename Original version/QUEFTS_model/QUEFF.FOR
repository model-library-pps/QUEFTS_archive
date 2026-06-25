*-------------------------------------------------------------------------*
* Copyright 2013. Wageningen University, Plant Production Systems group,  *
* P.O. Box 430, 6700 AK Wageningen, The Netherlands.                      *
* You may not use this work except in compliance with the Licence.        *
* You may obtain a copy of the Licence at:                                *
*                                                                         *
* http://models.pps.wur.nl/content/licence-agreement                      *
*                                                                         *
* Unless required by applicable law or agreed to in writing, software     *
* distributed under the Licence is distributed on an "AS IS" basis,       *
* WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.*
*-------------------------------------------------------------------------*
      IMPLICIT REAL(A-H,J-Z), INTEGER*2(I)
      CHARACTER*1 DUMMY
      ISC=0
      IFL=1

      WRITE (ISC,4010)
      WRITE (ISC,1090)
      READ (ISC,3010) CC
      IF (CC .NE. 1.) GO TO 20
      IC=IFL
      WRITE (IC,4010)
      GO TO 30
   20 IC=ISC

   30 CONTINUE

      WRITE(ISC,2010)
      READ (ISC,3010) UU
      IF (UU .NE. 1.) GO TO 70

C     reading chemical soil data      
   40 WRITE (ISC,2020)
      READ(ISC,3010) PH
      WRITE (ISC,2030)
      READ (ISC,3010) ORGC
      WRITE (ISC,2040)
      READ (ISC,3010) POLSEN
      WRITE (ISC,2050)
      READ (ISC,3010) KEXC

      WRITE (ISC,2060) PH,ORGC,POLSEN,KEXC
      WRITE (ISC,2070)
      READ (ISC,3020) IX
      IF (IX .EQ. 1) GO TO 40
      IF (IC .NE. ISC) WRITE (IFL,2060) PH,ORGC,POLSEN,KEXC

C     base supply of N, P and K to maize as based on chemical soil data
C     (KG HA-1)
      FN= AMAX1(0.,AMIN1(1.,0.25*(PH - 3.)))
      NBASE= FN*6.8*ORGC
      FP=AMAX1(0.,AMIN1(1., 1.- 0.5*(PH - 6.)*(PH - 6.)))
      PBASE= FP*0.35*ORGC + 0.5*POLSEN
      FK= AMAX1(0.,0.625*(3.4 - 0.40*PH))
      KBASE= FK*400.*KEXC/(2. + 0.9*ORGC)

      GO TO 75
   70 CONTINUE

C     reading base soil supply of nutrients to maize
      WRITE (ISC,1010)
      READ (ISC,3010) NBASE
      WRITE (ISC,1020)
      READ (ISC,3010) PBASE
      WRITE (ISC,1030)
      READ (ISC,3010) KBASE

   75 CONTINUE

C     reading yield maximum
      YLDMAX= 15000.
      WRITE (ISC,4030)
      READ (ISC,3020) IMAX
      IF (IMAX .NE. 1) GO TO 85
      WRITE (ISC,4035)
      READ (ISC,3010) YLDMAX
   85 CONTINUE
      IF (IC .NE. ISC) WRITE (IC,4080) YLDMAX
      WRITE (IC,1080) NBASE,PBASE,KBASE

      CALL NUTRIENT(IC,ISC,NBASE,PBASE,KBASE,YLDMAX)

   90 WRITE (ISC,4020)
      READ (ISC,3020) IFF
      IF (IFF .EQ. 0) GO TO 900

C     reading amounts of fertilizer nutrients applied and recovery
C     fractions 
  100 WRITE (ISC,4040)
      READ (ISC,3010) FERTN
      WRITE (ISC,4050)
      READ (ISC,3010) RECN
      WRITE (ISC,4041) 
      READ (ISC,3010) FERTP
      WRITE (ISC,4051)
      READ (ISC,3010) RECP
      WRITE (ISC,4042)
      READ (ISC,3010) FERTK
      WRITE (ISC,4052) 
      READ (ISC,3010) RECK
      WRITE (ISC,4060) FERTN,FERTP,FERTK
      WRITE (ISC,4070) RECN,RECP,RECK

      WRITE (ISC,2080)
      READ (ISC,3020) IX
      IF (IX .EQ. 1) GO TO 90
      IF (IC .NE. ISC) WRITE (IC,4060) FERTN,FERTP,FERTK
      IF (IC .NE. ISC) WRITE (IC,4070) RECN,RECP,RECK

      NSUPF= NBASE + FERTN*RECN
      PSUPF= PBASE + FERTP*RECP
      KSUPF= KBASE + FERTK*RECK

      WRITE (IC,1081) NSUPF,PSUPF,KSUPF
      CALL NUTRIENT(IC,ISC,NSUPF,PSUPF,KSUPF,YLDMAX)

      WRITE (ISC,5050)
      READ (ISC,3010) YY
      IF (YY .EQ. 1.) GO TO 100

  900 WRITE (ISC,1000)
      READ (ISC,3020) ISTART
      IF (ISTART .EQ. 0) GO TO 999
      GO TO 30
  999 CONTINUE
      CLOSE (IFL)
      STOP

C ****** FORMAT ******************************

 1000 FORMAT(15X,' A next run, yes (1) or no (0) ? ',$)
 1010 FORMAT(//,15X,' uptake of nitrogen in kg N/ha     ',$)
 1020 FORMAT(/,15X,' uptake of phosphorus in kg P/ha   ',$)
 1030 FORMAT(/,15X,' uptake of potassium in kg K/ha    ',$) 
 1080 FORMAT(//,8X,' Base supply by unfertilized'
     1             ' soil, in kg N, P,and K :'
     1       //,15X,' Nbase=',F7.2,'  Pbase=',F7.2,'  Kbase=',F7.2)
 1081 FORMAT(//,8X,' Nutrient supply by soil and applied'
     1             ' fertilizers, in kg N, P, and K :'
     1      //,15X,' Nsupf=',F7.2,' Psupf=',F7.2,' Ksupf=',F7.2)
 1090 FORMAT(//,15X,'Output to : 1. file FOR001.DAT'/
     1         ,15X,'            2. screen         '/
     1         ,15X,'            - - - >  ',$)
 
 2010 FORMAT(//,5X,'Uptake of N, P and K by maize from unfertilized ', 
     1              'soil can be based on:'/
     1         ,10X,'                   1.chemical soil data '/ 
     1         ,10X,'                   2.data on potential uptake for '
     1              'maize'/,
     1        /,14X,'  Which type of data do you have ? ',$)
 2020 FORMAT(//,15X,' pH-H2O value =  ',$)
 2030 FORMAT(/,15X,' organic carbon in g/kg   ',$)
 2040 FORMAT(/,15X,' P-Olsen in mg/kg   ',$)
 2050 FORMAT(/,15X,' exchangeable K in mmol/kg   ',$)
 2060 FORMAT(//,15X,' pH=',F6.2,'  org.C=',F6.2,'  P-Olsen=',F6.2,
     1 '  exch. K=',F6.2) 
 2070 FORMAT(//,15X,' do you want to change these soil data :'//
     1         ,15X,'      yes (1)  or  no (0)  ?    ',$)
 2080 FORMAT(//,15X,' do you want to change these data :' //
     1         ,15X,'      yes (1)  or  no (0)  ?   ',$)
 
 3010 FORMAT(F8.0)
 3020 FORMAT(I6)
 3030 FORMAT(A1)

 4010 FORMAT (//' a system for Quantitative Evaluation of'
     1       ' the Fertility of Tropical Soils  '////
     1          ,10X,' ********     Q U E F T S     ********'//
     1          ,10X,///' Maize yields on non-fertilized soils are'
     1           ' calculated from :'//
     1          ,10X, ' a. chemical soil data (pH, org. C,'
     1                ' P-Olsen, exch. K)'/
     1          ,10X, ' b. data on N-, P-, and K-uptake by maize' 
     1                ' from unfertilized soil.'//)

 4020 FORMAT(//,15X,'do you want to apply fertilizer nutrients :',//
     1         ,15X,'       yes (1) or no (0) ?     ',$)
 4030 FORMAT(//,10X,'do you want to enter the maximum attainable'
     1        /,10X,' grain yield at optimum nutrient supply :'
     1        /,15X,'       yes (1) or no (0) ?     ',$)
 4035 FORMAT(//,15X,'Maximum grain yield of maize '
     1              'in kg/ha  :   ',$)
 4040 FORMAT(//,8X,'amount of fertilizer nitrogen added'
     1 ' in kg N per hectare :  ',$)
 4041 FORMAT(//,8X,'amount of fertilizer phosphorus added'
     1 ' in kg P per hectare :  ',$)
 4042 FORMAT(//,8X,'amount of fertilizer potassium added'
     1 ' in kg K per hectare :  ',$)
 4050 FORMAT(//,8X,'uptake fraction of the applied nitrogen '
     1             'if other nutrients',/
     1         ,8X,' are supplied sufficiently :  ',$)
 4051 FORMAT(//,8X,'uptake fraction of the applied phosphorus '
     1             'if other nutrients',/
     1         ,8X,' are supplied sufficiently :  ',$)
 4052 FORMAT(//,8X,'uptake fraction of the applied potassium '
     1             'if other nutrients',/
     1         ,8X,' are supplied sufficiently :  ',$)
 4060 FORMAT(//,15X,'FERTN= ',F6.1,' FERTP= ',F6.1,' FERTK= ',F6.1)
 4070 FORMAT(//,15X,'RECN= ',F6.2,'  RECP= ',F6.2,'  RECK= ',F6.2)
 4080 FORMAT(//,9X,'Maximum grain yield of maize is',F8.0,' kg/ha.')
 5000 FORMAT (/,15X,' to continue press <Return>',$)
 5050 FORMAT(//,14X,' do you want to calculate yields that '
     1              'result from'
     1        /,14X,' other fertilizer applications :',/
     1         ,14X,'                 yes (1)  or no (0) ?  ',$)

      END


C    ****************************************
      SUBROUTINE NUTRIENT(IC,ISC,NSUP,PSUP,KSUP,YLDMAX)
C    ****************************************
C     calculates actual nutrient uptake by maize
C     and yield of maize crop

      IMPLICIT REAL(A-H,J-Z), INTEGER*2(I)
      CHARACTER*1 DUMMY

C     nutrient uptake at zero grain yield
      NZERO=5.
      PZERO= 0.4
      KZERO= 2.

C     yield/nutrient uptake ratios for max. nutrient accumulation (A),
C     and max. nutrient dilution (D) 
      NRATA= 30.
      NRATD= 70.
      PRATA= 200.
      PRATD= 600.
      KRATA= 30.
      KRATD= 120.

C     actual uptake of nitrogen by maize (KG HA-1)
      IF (NSUP .GT. NZERO +PRATA*(PSUP - PZERO)/NRATD) GO TO 81
      NPUPT= NSUP
      GO TO 83
   81 IF (NSUP .LT. (NZERO + (PSUP - PZERO)*(2.*PRATD/NRATA
     1 - PRATA/NRATD))) GO TO 82
      NPUPT= NZERO + PRATD*(PSUP - PZERO)/NRATA
      GO TO 83
   82 NPUPT= NSUP - 0.25*(NSUP - NZERO - PRATA*(PSUP - PZERO)
     1 /NRATD)**2./((PRATD/NRATA - PRATA/NRATD)*(PSUP - PZERO))
   83 CONTINUE
      IF (NSUP .GT. NZERO + KRATA*(KSUP - KZERO)/NRATD) GO TO 84
      NKUPT=NSUP
      GO TO 86
   84 IF(NSUP .LT. (NZERO +(KSUP - KZERO)*(2.*KRATD/NRATA
     1 - KRATA/NRATD))) GO TO 85
      NKUPT= NZERO + KRATD*(KSUP - KZERO)/NRATA
      GO TO 86
   85 NKUPT= NSUP - 0.25*(NSUP - NZERO - KRATA*(KSUP - KZERO)
     1 /NRATD)**2./((KRATD/NRATA - KRATA/NRATD)*(KSUP - KZERO))
   86 CONTINUE
      NPUPT= AMAX1(0., NPUPT)
      NKUPT= AMAX1(0., NKUPT)
      NUPT= AMIN1(NPUPT, NKUPT)

C     actual uptake of phosphorus by maize (KG HA-1)
      IF (PSUP .GT. PZERO + NRATA*(NSUP - NZERO)/PRATD) GO TO 87
      PNUPT= PSUP
      GO TO 89
   87 IF (PSUP .LT. (PZERO + (NSUP - NZERO)*(2.* NRATD/PRATA 
     1 - NRATA/PRATD))) GO TO 88
      PNUPT= PZERO + NRATD*(NSUP - NZERO)/PRATA
      GO TO 89
   88 PNUPT= PSUP - 0.25*(PSUP - PZERO - NRATA*(NSUP - NZERO)
     1 /PRATD)**2./((NRATD/PRATA - NRATA/PRATD)*(NSUP - NZERO))
   89 CONTINUE
      IF (PSUP .GT. PZERO + KRATA*(KSUP - KZERO)/PRATD) GO TO 90
      PKUPT= PSUP
      GO TO 92
   90 IF (PSUP .LT. (PZERO + (KSUP - KZERO)*(2.*KRATD/PRATA
     1 - KRATA/PRATD))) GO TO 91
      PKUPT= PZERO + KRATD*(KSUP - KZERO)/PRATA
      GO TO 92
   91 PKUPT= PSUP - 0.25*(PSUP - PZERO - KRATA*(KSUP - KZERO)
     1 /PRATD)**2./((KRATD/PRATA - KRATA/PRATD)*(KSUP - KZERO))
   92 CONTINUE
      PNUPT= AMAX1(0., PNUPT)
      PKUPT= AMAX1(0., PKUPT)
      PUPT= AMIN1(PNUPT, PKUPT)

C     actual uptake of potassium by maize (KG HA-1)
      IF (KSUP .GT. KZERO + NRATA *(NSUP - NZERO)/KRATD) GO TO 93
      KNUPT= KSUP
      GO TO 95
   93 IF (KSUP .LT. (KZERO +(NSUP - NZERO)*(2.*NRATD/KRATA
     1 - NRATA/KRATD))) GO TO 94
      KNUPT= KZERO + NRATD*(NSUP - NZERO)/KRATA
      GO TO 95
   94 KNUPT= KSUP - 0.25*(KSUP - KZERO - NRATA*(NSUP - NZERO)
     1 /KRATD)**2./((NRATD/KRATA - NRATA/KRATD)*(NSUP - NZERO))
   95 CONTINUE
      IF (KSUP .GT. KZERO + PRATA *(PSUP - PZERO)/KRATD) GO TO 96
      KPUPT= KSUP
      GO TO 98
   96 IF (KSUP .LT. (KZERO +(PSUP - PZERO)*(2.*PRATD/KRATA
     1 - PRATA/KRATD))) GO TO 97
      KPUPT= KZERO + PRATD*(PSUP - PZERO)/KRATA
      GO TO 98
   97 KPUPT= KSUP - 0.25*(KSUP - KZERO - PRATA*(PSUP - PZERO)
     1 /KRATD)**2./((PRATD/KRATA - PRATA/KRATD)*(PSUP - PZERO))
   98 CONTINUE
      KNUPT= AMAX1(0., KNUPT)
      KPUPT= AMAX1(0., KPUPT)
      KUPT= AMIN1(KNUPT, KPUPT)
 
      WRITE (IC,1085) 
      WRITE (IC,1086) NPUPT,NKUPT,NUPT
      WRITE (IC,1087) PNUPT,PKUPT,PUPT
      WRITE (IC,1088) KNUPT,KPUPT,KUPT

      IF (IC .NE. ISC) GO TO 100
      WRITE (ISC,5000) 
      READ (ISC,3030) DUMMY
  100 CONTINUE      

C     N-, P-, and K-limited yields of maize (airdry) 
C     at accumulated and diluted nutrient concentrations (KG HA-1) 
      YNA= NRATA * AMAX1(0.,NUPT - NZERO)
      YND= NRATD * AMAX1(0.,NUPT - NZERO)
      YPA= PRATA * AMAX1(0.,PUPT - PZERO)
      YPD= PRATD * AMAX1(0.,PUPT - PZERO)
      YKA= KRATA * AMAX1(0.,KUPT - KZERO)
      YKD= KRATD * AMAX1(0.,KUPT - KZERO)

      WRITE(IC,1050) YNA,YPA,YKA
      WRITE(IC,1060) YND,YPD,YKD

      
C     estimation of yield (EYNP, EYPN) from N- and P-limited yields (KG HA-1)
      LYD= AMIN1(YND,YPD,YKD,YLDMAX)
      IF (YND .GT. YPA) GO TO 110 
      EYNP= YND
      IF (YND .GT. LYD) EYNP= LYD
      GO TO 150
 110  IF (YNA .LT. LYD .AND. YND .GT. LYD) GO TO 120
      IF (YNA .LT. LYD .AND. YND .EQ. LYD) GO TO 115
      EYNP= LYD 
      GO TO 150
 115  LYD= AMIN1(YPD,YKD,YLDMAX)
 120  EYNP= YPA +2.*(LYD - YPA)*(NUPT - NZERO - YPA/NRATD)
     1 /(LYD/NRATA - YPA/NRATD) -(LYD - YPA)*(NUPT - NZERO
     1 - YPA/NRATD)**2./(LYD/NRATA - YPA/NRATD)**2.
      IF (YPA .GT. LYD) EYNP= LYD     
 150  CONTINUE
      LYD= AMIN1(YND,YPD,YKD,YLDMAX)
      IF (YPD .GT. YNA) GO TO 160
      EYPN= YPD
      IF (YPD .GT. LYD) EYPN= LYD
      GO TO 200
 160  IF (YPA .LT. LYD .AND. YPD .GT. LYD) GO TO 170
      IF (YPA .LT. LYD .AND. YPD .EQ. LYD) GO TO 165
      EYPN= LYD
      GO TO 200
 165  LYD= AMIN1(YND,YKD,YLDMAX)
 170  EYPN= YNA + 2.*(LYD - YNA)*(PUPT - PZERO - YNA/PRATD)
     1 /(LYD/PRATA - YNA/PRATD) -(LYD - YNA)*(PUPT - PZERO
     1 - YNA/PRATD)**2./(LYD/PRATA - YNA/PRATD)**2.
      IF (YNA .GT. LYD) EYPN= LYD
 200  CONTINUE

C     estimation of yield (EYNK, EYKN) from N- and K-limited yields (KG HA-1)
      LYD= AMIN1(YND,YPD,YKD,YLDMAX)
      IF (YND .GT. YKA) GO TO 210 
      EYNK= YND
      IF (YND .GT. LYD) EYNK= LYD
      GO TO 250
  210 IF (YNA .LT. LYD .AND. YND .GT. LYD) GO TO 220
      IF (YNA .LT. LYD .AND. YND .EQ. LYD) GO TO 215
      EYNK= LYD
      GO TO 250
  215 LYD= AMIN1(YPD,YKD,YLDMAX)
  220 EYNK= YKA + 2.*(LYD - YKA)*(NUPT - NZERO - YKA/NRATD)
     1 /(LYD/NRATA - YKA/NRATD) - (LYD - YKA)*(NUPT - NZERO 
     1 - YKA/NRATD)**2./(LYD/NRATA - YKA/NRATD)**2.
      IF (YKA .GT. LYD) EYNK= LYD
  250 CONTINUE
      LYD= AMIN1(YND,YPD,YKD,YLDMAX)
      IF (YKD .GT. YNA) GO TO 260
      EYKN= YKD
      IF (YKD .GT. LYD) EYKN= LYD
      GO TO 300
  260 IF (YKA .LT. LYD .AND. YKD .GT. LYD) GO TO 270
      IF (YKA .LT. LYD .AND. YKD .EQ. LYD) GO TO 265
      EYKN= LYD
      GO TO 300
  265 LYD= AMIN1(YND,YPD,YLDMAX)
  270 EYKN= YNA + 2.*(LYD - YNA)*(KUPT - KZERO - YNA/KRATD)
     1 /(LYD/KRATA - YNA/KRATD) - (LYD - YNA)*(KUPT - KZERO
     1 -YNA/KRATD)**2./(LYD/KRATA - YNA/KRATD)**2.
      IF (YNA .GT. LYD) EYKN= LYD
  300 CONTINUE

C     estimation of yield (EYPK, EYKP) from P- and K-limited yields (KG HA-1)
      LYD= AMIN1(YND,YPD,YKD,YLDMAX)
      IF (YPD .GT. YKA) GO TO 310
      EYPK= YPD
      IF (YPD .GT. LYD) EYPK= LYD
      GO TO 350
  310 IF (YPA .LT. LYD .AND. YPD .GT. LYD) GO TO 320
      IF (YPA .LT. LYD .AND. YPD .EQ. LYD) GO TO 315
      EYPK= LYD
      GO TO 350
  315 LYD= AMIN1(YND,YKD,YLDMAX)
  320 EYPK= YKA + 2.*(LYD - YKA)*(PUPT - PZERO - YKA/PRATD)
     1 /(LYD/PRATA - YKA/PRATD) - (LYD - YKA)*(PUPT - PZERO
     1 - YKA/PRATD)**2./(LYD/PRATA - YKA/PRATD)**2.
      IF (YKA .GT. LYD) EYPK= LYD
  350 CONTINUE 
      LYD= AMIN1(YND,YPD,YKD,YLDMAX)
      IF (YKD .GT. YPA) GO TO 360
      EYKP= YKD
      IF (YKD .GT. LYD) EYKP= LYD
      GO TO 400
  360 IF (YKA .LT. LYD .AND. YKD .GT. LYD) GO TO 370
      IF (YKA .LT. LYD .AND. YKD .EQ. LYD) GO TO 365
      EYKP= LYD
      GO TO 400
  365 LYD= AMIN1(YND,YPD,YLDMAX)
  370 EYKP= YPA + 2.*(LYD - YPA)*(KUPT - KZERO - YPA/KRATD)
     1 /(LYD/KRATA - YPA/KRATD) - (LYD - YPA)*(KUPT - KZERO
     1 - YPA/KRATD)**2./(LYD/KRATA - YPA/KRATD)**2.
      IF (YPA .GT. LYD) EYKP= LYD 
  400 CONTINUE
  
C     estimation of yield (YE) from N-, P-, and K-limited yields
      LYD= AMIN1(YND,YPD,YKD,YLDMAX)
      XX1= (EYNP + EYPN + EYNK + EYKN + EYPK + EYKP)/6.
      YE= AMIN1(LYD,XX1)
      WRITE (IC,6000)
      WRITE (IC,6010) EYNP,EYNK,EYPK,EYPN,EYKN,EYKP
      WRITE (IC,6020) YLDMAX,EYNP,EYPN,EYNK,EYKN,EYPK,EYKP,
     1   YLDMAX,XX1
      
      WRITE (IC,1040) YE

      IF (IC .NE. ISC) GO TO 500
      WRITE (ISC,5000)
      READ (ISC,3030) DUMMY
  500 CONTINUE

      RETURN


C ******* FORMAT ***************************************

 1040 FORMAT(/,15X,'The yield of the Maize crop is '
     1         ,F8.0, ' kg/ha.'
     1      /,15X,'******************************************')
 1050 FORMAT(//,5X,'Yields at maximum Accumulation and at maximum' 
     1             ' Dilution of nutrients N, P,'/
     1         ,5X,' or K, in  kg/ha :'//
     1        ,15X,' YNA= ',F7.0,' YPA= ',F7.0,' YKA= ',F7.0)
 1060 FORMAT(/,15X,' YND= ',F7.0,' YPD= ',F7.0,' YKD= ',F7.0)
 1085 FORMAT(//,8X,' Actual nutrient uptake by maize, in kg/ha N, P,'
     1              ' and K :')
 1086 FORMAT(/,15X,' Nupt= AMIN(',F7.2,', ',F7.2,')= ',F7.2)
 1087 FORMAT(/,15X,' Pupt= AMIN(',F7.2,', ',F7.2,')= ',F7.2)
 1088 FORMAT(/,15X,' Kupt= AMIN(',F7.2,', ',F7.2,')= ',F7.2)
 3010 FORMAT(F8.0)
 3020 FORMAT(I6)
 3030 FORMAT(A1)
 5000 FORMAT(/,15X,' to continue press <Return>',$)
 6000 FORMAT(//,5X,' Yields calculated for combinations of'
     1             ' two nutrients : ')
 6010 FORMAT( /,15X,' YNP= ',F7.0,' YNK= ',F7.0,' YPK= ',F7.0,
     1       //,15X,' YPN= ',F7.0,' YKN= ',F7.0,' YKP= ',F7.0)
 6020 FORMAT(//,3X,'YE= AMIN1(',F6.0,', (',F6.0,' + ',F6.0,
     1      ' + ',F6.0,' + ',F6.0,' + ',F6.0,' + ',F6.0,')/6)'//
     1         ,5X,'  = AMIN1(',F6.0,',',F6.0,')'/ ) 
       END