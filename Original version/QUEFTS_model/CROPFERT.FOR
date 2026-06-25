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
$INCLUDE:'CROPFERT.CMN'
      DIMENSION UPTB(30)
      DATA UPTB/0.,0.,40.,0.4,80.,0.7,120.,1.,240.,1.6,360.,2.,
     1 1000.,2.,16*0./
      ISC=5
      IFL=1
      INUT=8
 
      WRITE (ISC,4010)
      WRITE (ISC,1090)
      READ (ISC,3010) CC
      IF (CC .NE. 1.) GO TO 20
      IC=IFL
      WRITE (IC,4010)
      GO TO 30
   20 IC=ISC

   30 CONTINUE

C     reading crop data
      OPEN (UNIT=INUT, STATUS= 'OLD', FILE='NUTRIDAT.DAT')
      WRITE (ISC,4000)
      READ (ISC,3010) CROP
      ICROP=CROP
      CALL NUTR(ICROP,ISC)

C     changing the crop growth duration
      WRITE (ISC,4001) DUROPT
      READ (ISC,3020) IX
      IF (IX .EQ. 0) GO TO 35
      WRITE (ISC,4002)
      READ (ISC,3020) ICGDUR
      DUROPT= FLOAT(ICGDUR)
   35 CONTINUE

      IF (IC .NE. ISC) WRITE (IFL,1050) CROPNAM
      WRITE(ISC,2010) CROPNAM
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

C     base supply of N, P and  K as based on chemical soil data
C     (kg ha-1)
      FN= AMAX1(0.,AMIN1(1.,0.25*(PH - 3.)))
      NBASE= FN*6.8*ORGC
      FP=AMAX1(0.,AMIN1(1., 1.- 0.5*(PH - 6.)*(PH - 6.)))
      PBASE= FP*0.35*ORGC + 0.5*POLSEN
      FK= AMAX1(0.,0.625*(3.4 - 0.40*PH))
      KBASE= FK*400.*KEXC/(2. + 0.9*ORGC)
      WRITE (ISC,1080) NBASE,PBASE,KBASE
      GO TO 74

   70 CONTINUE
      IF (UU .NE. 2.) GO TO 80

C     reading base soil supply of nutrients to maize
   72 WRITE (ISC,1010)
      READ (ISC,3010) NBASE
      WRITE (ISC,1020)
      READ (ISC,3010) PBASE
      WRITE (ISC,1030)
      READ (ISC,3010) KBASE
      WRITE (ISC,1080) NBASE,PBASE,KBASE
      WRITE (ISC,2080)
      READ (ISC,3020) IX
      IF (IX .EQ. 1) GO TO 72
  74  IF (IC .NE. ISC) WRITE (IFL,1080) NBASE,PBASE,KBASE

C     nutrient supply of soil in dependence of crop growth duration
      NBAS= NBASE * AFGEN(UPTB,DUROPT)
      PBAS= PBASE * AFGEN(UPTB,DUROPT)
      KBAS= KBASE * AFGEN(UPTB,DUROPT)
      WRITE (ISC,1085) CROPNAM,NBAS,PBAS,KBAS
      WRITE (ISC,2090)
      READ (ISC,3010) BB
      IF (BB .NE. 1.) GO TO 75
      NBAS=NBASE
      PBAS=PBASE
      KBAS=KBASE
   75 CONTINUE
      IF (IC .NE. ISC) WRITE (IFL,1085) CROPNAM,NBAS,PBAS,KBAS
      GO TO 85

   80 CONTINUE
C     reading soil supply of nutrients to specified crop species
      WRITE (ISC,1010)
      READ (ISC,3010) NBAS
      WRITE (ISC,1020)
      READ (ISC,3010) PBAS
      WRITE (ISC,1030)
      READ (ISC,3010) KBAS
      WRITE (ISC,1085) CROPNAM,NBAS,PBAS,KBAS
      WRITE (ISC,2080)
      READ (ISC,3020) IX
      IF (IX .EQ. 1) GO TO 80
      IF (IC .NE. ISC) WRITE (IFL,1085) CROPNAM,NBAS,PBAS,KBAS

   85 CONTINUE
      CALL NUTRIENT(IC,ISC,NBAS,PBAS,KBAS,0.)

   90 WRITE (ISC,4020)
      READ (ISC,3020) IFF
      IF (IFF .EQ. 0) GO TO 900

C     reading amounts of fertilizer nutrients applied and recovery fractions
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

      NSUPF= NBAS + FERTN*RECN
      PSUPF= PBAS + FERTP*RECP
      KSUPF= KBAS + FERTK*RECK

      WRITE (IC,1081) NSUPF,PSUPF,KSUPF
      CALL NUTRIENT(IC,ISC,NSUPF,PSUPF,KSUPF,1.)

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
 1050 FORMAT(//,1X,'*************************************'
     1             '*************************'/
     1         ,5X,15A1,/
     1         ,1X,'*************************************'
     1             '*************************')
 1080 FORMAT(//,8X,' Base supply to maize by unfertilized'
     1             ' soil, in kg N, P,and K :'
     1       //,15X,' Nbase=',F7.2,'  Pbase=',F7.2,'  Kbase=',F7.2)
 1081 FORMAT(//,8X,' Nutrient supply by soil and applied'
     1             ' fertilizers, in kg N, P, and K :'
     1      //,15X,' Nsupf=',F7.2,' Psupf=',F7.2,' Ksupf=',F7.2)
 1085 FORMAT(//,9X,'Nutrient supply to ',15A1,' by '
     1       /,10X,'unfertilized soil, in kg N, P, and K:'
     1       //,15X,' Nbas=',F7.2,'  Pbas=',F7.2,'  Kbas=',F7.2)
 1090 FORMAT(//,15X,'Output to : 1. file FOR001.DAT'/
     1         ,15X,'            2. screen         '/
     1         ,15X,'            - - - >  ',$)
 
 2010 FORMAT(//,5X,'Crop uptake of N, P and K from unfertilized ', 
     1              'soil can be based on:'/
     1         ,10X,'                   1.chemical soil data '/ 
     1         ,10X,'                   2.data on potential uptake for '
     1              'maize'/
     1         ,10X,'                   3.data on potential uptake for '
     1              ,15A1,/,
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
 2090 FORMAT(//, 7X,'Crop yields can be calculated for these '
     1          'N-, P-, and K-supply data ( = 0)'
     1    /,10X,'or for the base supply of nutrients to maize ( = 1)'
     1   //,15X,' Which data set do you prefer ? ',$)
 
 3010 FORMAT(F8.0)
 3020 FORMAT(I6)
 3030 FORMAT(A1)

 4000 FORMAT (10X,//
     110X,' 1. = Barley           16. = Pigeonpea     '/,
     210X,' 2. = Cassave          17. = Potato        '/,
     310X,' 3. = Chickpea         18. = Rapeseed      '/,
     410X,' 4. = Chillies         19. = Rice          '/,
     510X,' 5. = Cotton           20. = Sesame        '/,
     610X,' 6. = Cowpea           21. = Sorghum       '/,
     710X,' 7. = Field bean       22. = Soyabean      '/,
     810X,' 8. = Groundnut        23. = Sugarbeet     '/,
     910X,' 9. = Jute             24. = Sugarcane     '/,
     110X,'10. = Kenaf            25. = Sunflower     '/,
     210X,'11. = Lentil           26. = Sweet potato  '/,
     310X,'12. = Maize            27. = Tobacco       '/,
     410X,'13. = Millet,bulrush   28. = Wheat         '/,
     510X,'14. = Mungbean  '/,
     610X,'15. = Onion '//,
     7 5X,'For which crop type you want to calculate yields for' 
     8    ' specified'/,
     9 5X,' soil fertility and fertilizer applications ?'//,
     1  1H+,50X,' Crop =  ',$)
 4001 FORMAT (//,15X,'do you want to change the length of the '
     1               'growing period :',
     1         /,15X,' (standard = ',F5.0,' days) ',
     1        //,25X,'      yes (1)  or   no (0)  ?   ',$)
 4002 FORMAT (//,15X,'specify the length of the growing period '
     1               'in days : ',$)            
 4010 FORMAT (//' a system for Quantitative Evaluation of'
     1       ' the Fertility of Tropical Soils  '////
     1          ,10X,' ********     Q U E F T S     ********'//
     1          ,10X,///' Crop yields on non-fertilized soils are'
     1           ' calculated from :'//
     1          ,10X, ' a. chemical soil data (pH, org. C,'
     1                ' P-Olsen, exch. K)'/
     1          ,10X, ' b. data on N-, P-, and K-uptake '
     1                'from unfertilized soil.'//
     1          ,1X,'For specified applications of fertilizer nutrients'
     1              ' the resulting'/
     1          ,1X,' yield increases may be calculated.'//)
 4020 FORMAT(//,15X,'do you want to apply fertilizer nutrients :',//
     1         ,15X,'       yes (1) or no (0) ?     ',$)
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
 4060 FORMAT(///,15X,'FERTN= ',F6.1,' FERTP= ',F6.1,' FERTK= ',F6.1)
 4070 FORMAT(/,15X,'RECN= ',F6.2,'  RECP= ',F6.2,'  RECK= ',F6.2)
 5000 FORMAT (/,15X,' to continue press <Return>',$)
 5050 FORMAT(//,14X,' do you want to calculate yields that '
     1              'result from'
     1        /,14X,' other fertilizer applications :',/
     1         ,14X,'                 yes (1)  or no (0) ?  ',$)

      END


C    ****************************************
      SUBROUTINE NUTRIENT(IC,ISC,NSUP,PSUP,KSUP,FF)
C    ****************************************
C     calculates nutrient uptake, fertilizer requirements and crop 
C     yields 

$INCLUDE:'CROPFERT.CMN'
        NFERT0= 0.
        PFERTO= 0.
        KFERTO= 0.
 
C       balanced nutrient conc., nutrient uptake at zero yield
        NCBALVE= (NCMAXVE + 2.* NCMINVE)/3.
        NCBALSO= (NCMAXSO + 2.* NCMINSO)/3.
        PCBALVE= (PCMAXVE + 2.* PCMINVE)/3.
        PCBALSO= (PCMAXSO + 2.* PCMINSO)/3.
        KCBALVE= (KCMAXVE + 2.* KCMINVE)/3.
        KCBALSO= (KCMAXSO + 2.* KCMINSO)/3.
        NZERO= YZERO * NCBALVE * (1.- NFIX)
        PZERO= YZERO * PCBALVE
        KZERO= YZERO * KCBALVE

C       nutrient requirements for potential yields at
C       accumulated conc.
        NREQO= (NCMAXVE * (LVOPT + STOPT) + NCMAXSO * SOOPT) *
     1  (1.- NFIX)
        PREQO= PCMAXVE * (LVOPT + STOPT) + PCMAXSO * SOOPT
        KREQO= KCMAXVE * (LVOPT + STOPT) + KCMAXSO * SOOPT

C     ********** nutrient requirements at accumulated and diluted 
C       nutrient conc.
        NREQA= (NCMAXVE *(LVOPT + STOPT) + NCMAXSO * SOOPT) *
     1  (1.- NFIX)
        NREQD= (NCMINVE *(LVOPT + STOPT) + NCMINSO * SOOPT) *
     1  (1.- NFIX)
        PREQA= PCMAXVE *(LVOPT + STOPT) + PCMAXSO * SOOPT 
        PREQD= PCMINVE *(LVOPT + STOPT) + PCMINSO * SOOPT
        KREQA= KCMAXVE *(LVOPT + STOPT) + KCMAXSO * SOOPT
        KREQD= KCMINVE *(LVOPT + STOPT) + KCMINSO * SOOPT

C       fertilizer requirements
        IF (FF .NE. 1. .OR. RECN .EQ. 0.0) RECN=0.5
        IF (FF .NE. 1. .OR. RECP .EQ. 0.0) RECP=0.1
        IF (FF .NE. 1. .OR. RECK .EQ. 0.0) RECK=0.5
        IF (FF .NE. 1.) NBAS= NSUP
        IF (FF .NE. 1.) PBAS= PSUP
        IF (FF .NE. 1.) KBAS= KSUP
        NFERTO= AMAX1(0.,(NREQO- NBAS)/RECN)
        PFERTO= AMAX1(0.,(PREQO - PBAS)/RECP)
        KFERTO= AMAX1(0.,(KREQO - KBAS)/RECK)

C       plant organ
        IF (SOOPT .LE. 200.) GO TO 120
        YLDO= SOOPT
        IORGAN=3
        GO TO 200
  120   IF (STOPT .LE. 200.) GO TO 130
        YLDO= STOPT
        IORGAN=2
        GO TO 200
  130   YLDO= AMAX1(5.,LVOPT)
        IORGAN=1
  200   CONTINUE
        IF (IORGAN .EQ. 3) GO TO 210
        NZERO= 0.
        PZERO= 0.
        KZERO =0.
  210   CONTINUE

C       yield /nutrient uptake ratios at accumulated 
C       and diluted N-, P-, and K-conc.
        YNRATD= YLDO/AMAX1(0.1,NREQD - NZERO)
        YNRATA= YLDO/AMAX1(0.1,NREQA - NZERO)
        YPRATD= YLDO/AMAX1(0.1,PREQD - PZERO)
        YPRATA= YLDO/AMAX1(0.1,PREQA - PZERO)
        YKRATD= YLDO/AMAX1(0.1,KREQD - KZERO) 
        YKRATA= YLDO/AMAX1(0.1,KREQA - KZERO)

C       crop's nitrogen uptake from soil
        IF (NSUP .GT. NZERO +YPRATA*(PSUP - PZERO)/YNRATD) GO TO 281
        NPUPT= NSUP
        GO TO 283
  281   IF (NSUP .LT. (NZERO + (PSUP - PZERO)*(2.*YPRATD/YNRATA
     1  - YPRATA/YNRATD))) GO TO 282
        NPUPT= NZERO + YPRATD*(PSUP - PZERO)/YNRATA
        GO TO 283
  282   NPUPT= NSUP - 0.25*(NSUP - NZERO - YPRATA*(PSUP - PZERO)
     1  /YNRATD)**2./((YPRATD/YNRATA - YPRATA/YNRATD)*(PSUP - PZERO))
  283   CONTINUE
        IF (NSUP .GT. NZERO + YKRATA*(KSUP - KZERO)/YNRATD) GO TO 284
        NKUPT=NSUP
        GO TO 286
  284   IF(NSUP .LT. (NZERO +(KSUP - KZERO)*(2.*YKRATD/YNRATA
     1  - YKRATA/YNRATD))) GO TO 285
        NKUPT= NZERO + YKRATD*(KSUP - KZERO)/YNRATA
        GO TO 286
  285   NKUPT= NSUP - 0.25*(NSUP - NZERO - YKRATA*(KSUP - KZERO)
     1  /YNRATD)**2./((YKRATD/YNRATA - YKRATA/YNRATD)*(KSUP - KZERO))
  286   CONTINUE
        NPUPT= AMAX1(0.,NPUPT)
        NKUPT= AMAX1(0.,NKUPT)
        NUPT= AMIN1(NPUPT, NKUPT)

C       crop's phosphorus uptake from soil
        IF (PSUP .GT. PZERO + YNRATA*(NSUP - NZERO)/YPRATD) GO TO 287
        PNUPT= PSUP
        GO TO 289
  287   IF (PSUP .LT. (PZERO + (NSUP - NZERO)*(2.* YNRATD/YPRATA 
     1  - YNRATA/YPRATD))) GO TO 288
        PNUPT= PZERO + YNRATD*(NSUP - NZERO)/YPRATA
        GO TO 289
  288   PNUPT= PSUP - 0.25*(PSUP - PZERO - YNRATA*(NSUP - NZERO)
     1  /YPRATD)**2./((YNRATD/YPRATA - YNRATA/YPRATD)*(NSUP - NZERO))
  289   CONTINUE
        IF (PSUP .GT. PZERO + YKRATA*(KSUP - KZERO)/YPRATD) GO TO 290
        PKUPT= PSUP
        GO TO 292
  290   IF (PSUP .LT. (PZERO + (KSUP - KZERO)*(2.*YKRATD/YPRATA
     1  - YKRATA/YPRATD))) GO TO 291
        PKUPT= PZERO + YKRATD*(KSUP - KZERO)/YPRATA
        GO TO 292
  291   PKUPT= PSUP - 0.25*(PSUP - PZERO - YKRATA*(KSUP - KZERO)
     1  /YPRATD)**2./((YKRATD/YPRATA - YKRATA/YPRATD)*(KSUP - KZERO))
  292   CONTINUE
        PNUPT= AMAX1(0.,PNUPT)
        PKUPT= AMAX1(0.,PKUPT)
        PUPT= AMIN1(PNUPT, PKUPT)

C       crop's potassium uptake from soil
        IF (KSUP .GT. KZERO + YNRATA *(NSUP - NZERO)/YKRATD) GO TO 293
        KNUPT= KSUP
        GO TO 295
  293   IF (KSUP .LT. (KZERO +(NSUP - NZERO)*(2.*YNRATD/YKRATA
     1  - YNRATA/YKRATD))) GO TO 294
        KNUPT= KZERO + YNRATD*(NSUP - NZERO)/YKRATA
        GO TO 295
  294   KNUPT= KSUP - 0.25*(KSUP - KZERO - YNRATA*(NSUP - NZERO)
     1  /YKRATD)**2./((YNRATD/YKRATA - YNRATA/YKRATD)*(NSUP - NZERO))
  295   CONTINUE
        IF (KSUP .GT. KZERO + YPRATA *(PSUP - PZERO)/YKRATD) GO TO 296
        KPUPT= KSUP
        GO TO 298
  296   IF (KSUP .LT. (KZERO +(PSUP - PZERO)*(2.*YPRATD/YKRATA
     1  - YPRATA/YKRATD))) GO TO 297
        KPUPT= KZERO + YPRATD*(PSUP - PZERO)/YKRATA
        GO TO 298
  297   KPUPT= KSUP - 0.25*(KSUP - KZERO - YPRATA*(PSUP - PZERO)
     1  /YKRATD)**2./((YPRATD/YKRATA - YPRATA/YKRATD)*(PSUP - PZERO))
  298   CONTINUE
        KNUPT= AMAX1(0.,KNUPT)
        KPUPT= AMAX1(0.,KPUPT)
        KUPT= AMIN1(KNUPT, KPUPT)

        WRITE (IC,1085)  
        WRITE (IC,1086) NPUPT,NKUPT,NUPT
        WRITE (IC,1087) PNUPT,PKUPT,PUPT
        WRITE (IC,1088) KNUPT,KPUPT,KUPT
        IF (IC .NE. ISC) GO TO 300
        WRITE (ISC,5000)
        READ (ISC,3030) DUMMY
  300   CONTINUE

C       N-,P-, and K-limited yields at accumulated 
C       and diluted nutrient conc.
        YNA= YNRATA * AMAX1(0.,NUPT - NZERO)
        YND= YNRATD * AMAX1(0.,NUPT - NZERO)
        YPA= YPRATA * AMAX1(0.,PUPT - PZERO)
        YPD= YPRATD * AMAX1(0.,PUPT - PZERO)
        YKA= YKRATA * AMAX1(0.,KUPT - KZERO)
        YKD= YKRATD * AMAX1(0.,KUPT - KZERO)
      WRITE(IC,1050) YNA,YPA,YKA
      WRITE(IC,1060) YND,YPD,YKD
      
C     estimation of yield (EYNP, EYPN) from N- and P-limited yields 
      LYD= AMIN1(YND,YPD,YKD,YLDO)
      IF (YND .GT. YPA) GO TO 310 
      EYNP= YND
      IF (YND .GT. LYD) EYNP= LYD
      GO TO 350
 310  IF (YNA .LT. LYD .AND. YND .GT. LYD) GO TO 320
      IF (YNA .LT. LYD .AND. YND .EQ. LYD) GO TO 315
      EYNP= LYD 
      GO TO 350
 315  LYD= AMIN1(YPD,YKD,YLDO)
 320  EYNP= YPA +2.*(LYD - YPA)*(NUPT - NZERO - YPA/YNRATD)
     1 /(LYD/YNRATA - YPA/YNRATD) -(LYD - YPA)*(NUPT - NZERO
     1 - YPA/YNRATD)**2./(LYD/YNRATA - YPA/YNRATD)**2.
      IF (YPA .GT. LYD) EYNP= LYD     
 350  CONTINUE
      LYD= AMIN1(YND,YPD,YKD,YLDO)
      IF (YPD .GT. YNA) GO TO 360
      EYPN= YPD
      IF (YPD .GT. LYD) EYPN= LYD
      GO TO 400
 360  IF (YPA .LT. LYD .AND. YPD .GT. LYD) GO TO 370
      IF (YPA .LT. LYD .AND. YPD .EQ. LYD) GO TO 365
      EYPN= LYD
      GO TO 400
 365  LYD= AMIN1(YND,YKD,YLDO)
 370  EYPN= YNA + 2.*(LYD - YNA)*(PUPT - PZERO - YNA/YPRATD)
     1 /(LYD/YPRATA - YNA/YPRATD) -(LYD - YNA)*(PUPT - PZERO
     1 - YNA/YPRATD)**2./(LYD/YPRATA - YNA/YPRATD)**2.
      IF (YNA .GT. LYD) EYPN= LYD
 400  CONTINUE

C     estimation of yield (EYNK, EYKN) from N- and K-limited yields 
      LYD= AMIN1(YND,YPD,YKD,YLDO)
      IF (YND .GT. YKA) GO TO 410 
      EYNK= YND
      IF (YND .GT. LYD) EYNK= LYD
      GO TO 450
  410 IF (YNA .LT. LYD .AND. YND .GT. LYD) GO TO 420
      IF (YNA .LT. LYD .AND. YND .EQ. LYD) GO TO 415
      EYNK= LYD
      GO TO 450
  415 LYD= AMIN1(YPD,YKD,YLDO)
  420 EYNK= YKA + 2.*(LYD - YKA)*(NUPT - NZERO - YKA/YNRATD)
     1 /(LYD/YNRATA - YKA/YNRATD) - (LYD - YKA)*(NUPT - NZERO 
     1 - YKA/YNRATD)**2./(LYD/YNRATA - YKA/YNRATD)**2.
      IF (YKA .GT. LYD) EYNK= LYD
  450 CONTINUE
      LYD= AMIN1(YND,YPD,YKD,YLDO)
      IF (YKD .GT. YNA) GO TO 460
      EYKN= YKD
      IF (YKD .GT. LYD) EYKN= LYD
      GO TO 500
  460 IF (YKA .LT. LYD .AND. YKD .GT. LYD) GO TO 470
      IF (YKA .LT. LYD .AND. YKD .EQ. LYD) GO TO 465
      EYKN= LYD
      GO TO 500
  465 LYD= AMIN1(YND,YPD,YLDO)
  470 EYKN= YNA + 2.*(LYD - YNA)*(KUPT - KZERO - YNA/YKRATD)
     1 /(LYD/YKRATA - YNA/YKRATD) - (LYD - YNA)*(KUPT - KZERO
     1 -YNA/YKRATD)**2./(LYD/YKRATA - YNA/YKRATD)**2.
      IF (YNA .GT. LYD) EYKN= LYD
  500 CONTINUE

C     estimation of yield (EYPK, EYKP) from P- and K-limited yields
      LYD= AMIN1(YND,YPD,YKD,YLDO)
      IF (YPD .GT. YKA) GO TO 510
      EYPK= YPD
      IF (YPD .GT. LYD) EYPK= LYD
      GO TO 550
  510 IF (YPA .LT. LYD .AND. YPD .GT. LYD) GO TO 520
      IF (YPA .LT. LYD .AND. YPD .EQ. LYD) GO TO 515
      EYPK= LYD
      GO TO 550
  515 LYD= AMIN1(YND,YKD,YLDO)
  520 EYPK= YKA + 2.*(LYD - YKA)*(PUPT - PZERO - YKA/YPRATD)
     1 /(LYD/YPRATA - YKA/YPRATD) - (LYD - YKA)*(PUPT - PZERO
     1 - YKA/YPRATD)**2./(LYD/YPRATA - YKA/YPRATD)**2.
      IF (YKA .GT. LYD) EYPK= LYD
  550 CONTINUE 
      LYD= AMIN1(YND,YPD,YKD,YLDO)
      IF (YKD .GT. YPA) GO TO 560
      EYKP= YKD
      IF (YKD .GT. LYD) EYKP= LYD
      GO TO 600
  560 IF (YKA .LT. LYD .AND. YKD .GT. LYD) GO TO 570
      IF (YKA .LT. LYD .AND. YKD .EQ. LYD) GO TO 565
      EYKP= LYD
      GO TO 600
  565 LYD= AMIN1(YND,YPD,YLDO)
  570 EYKP= YPA + 2.*(LYD - YPA)*(KUPT - KZERO - YPA/YKRATD)
     1 /(LYD/YKRATA - YPA/YKRATD) - (LYD - YPA)*(KUPT - KZERO
     1 - YPA/YKRATD)**2./(LYD/YKRATA - YPA/YKRATD)**2.
      IF (YPA .GT. LYD) EYKP= LYD 
  600 CONTINUE
  

C       estimation of yield (YE) from N-, P-, and K-limited yields 
        LYD= AMIN1(YND,YPD,YKD,YLDO)
        XX1= (EYNP + EYPN + EYNK + EYKN + EYPK + EYKP)/6.
        YE= AMIN1(LYD,XX1)
        WRITE (IC,6000)
        WRITE (IC,6010) EYNP,EYNK,EYPK,EYPN,EYKN,EYKP
        WRITE (IC,6020) YLDO,EYNP,EYPN,EYNK,EYKN,EYPK,EYKP, 
     1  YLDO,XX1
        WRITE (IC,1040) CROPNAM,YE

C       nutrient-limited yields of plant organs
        IF (IORGAN .NE. 3) GO TO 620
        SO= YE
        LV= (LVOPT/(LVOPT + STOPT) * ((STOPT + LVOPT - YZERO) *
     1  YE/SOOPT + YZERO))
        IF (YE .LE. 0.) LV= LVOPT/(LVOPT + STOPT) *AMIN1(YZERO,
     1  NUPT/(NCBALVE * (1. - NFIX)), PUPT/PCBALVE, KUPT/KCBALVE)
        ST= (STOPT/LVOPT) * LV
        GO TO 700
  620   IF (IORGAN .NE. 2) GO TO 630
        ST= YE
        LV= (LVOPT/STOPT) * ST
        SO= (SOOPT/STOPT) * ST
        GO TO 700
  630   LV= YE
        ST= (STOPT/LVOPT) * LV
        SO= (SOOPT/LVOPT) * LV
  700   CONTINUE
        RAT= SO/AMAX1(1.,LV + ST)
        HI= SO/AMAX1(1.,LV + ST + SO)
        
        IF (IC .NE. ISC) GO TO 750
        WRITE (ISC,5000)
        READ (ISC,3030) DUMMY
  750   CONTINUE
 
        IF (FF .NE. 1.) GO TO 800
        LVFERT= LV
        STFERT= ST
        SOFERT= SO
        RATFERT=RAT
        HIFERT=HI
        GO TO 900
  800   CONTINUE
        LVFERT= 0.
        STFERT= 0.
        SOFERT= 0.
        RATFERT= 0.
        HIFERT= 0.
        FERTN= 0.
        FERTP= 0.
        FERTK= 0.
        LVBAS= LV
        STBAS= ST
        SOBAS= SO
        RATBAS= RAT
        HIBAS= HI
  900   CONTINUE
        RATOPT= SOOPT/(LVOPT + STOPT)
        HIOPT= SOOPT/(LVOPT + STOPT + SOOPT)

C       output table 
        WRITE (IC,1000) NBAS,PBAS,KBAS,RECN,RECP,RECK,
     1  LVOPT,LVBAS,LVFERT,STOPT,STBAS,STFERT,SOOPT,SOBAS,SOFERT,RATOPT,
     2  RATBAS,RATFERT,HIOPT,HIBAS,HIFERT,NFERTO,FERTN,PFERTO,FERTP,
     3  KFERTO,FERTK
	RETURN

C ********** FORMAT **************************************************
 1000 FORMAT(/,' SUMMARY CROP PRODUCTION AND NUTRIENT REQUIREMENTS',
     1   1X,'Nbas=',F4.0,' Pbas=',F4.0,' Kbas=',F4.0,/,1X,49('='),
     2   1X,'recN=',F4.2,' recP=',F4.2,' recK=',F4.2,/,16X,'| Potential',
     3   7X,'| Nutrient limited| Fert. limited',/,16X,
     4   3('| Crop production '),/,1X,69('_'),/,' Leaves',9X,3('|',5X,
     5   F7.0,5X),/,' Stems',10X,3('|',5X,F7.0,5X),/,' Storage organ  ',
     6   3('|',5X,F7.0,5X),/,' Ratio SO/straw ',3('|',7X,F7.2,3X),/,
     7   ' Harvest index  ',3('|',7X,F7.2,3X),/,' Fertilizer N   |',6X,
     8   F7.1,4X,'|',10X,'-',6X,'|',6X,F7.1,/,' Fertilizer P   |',
     9   6X,F7.1,4X,'|',10X,'-',6X,'|',6X,F7.1,/,' Fertilizer K   |'
     1   ,6X,F7.1,4X,'|',10X,'-',6X,'|',6X,F7.1)    
 1040 FORMAT(/,5X,' The yield of ',15A1,' is '
     1         ,F8.0, ' kg/ha.'/)
 1050 FORMAT(///,5X,'Yields at maximum Accumulation and at maximum' 
     1             ' Dilution of nutrients N, P,'/
     1         ,5X,' or K, in  kg/ha :'//
     1        ,15X,' YNA= ',F7.0,' YPA= ',F7.0,' YKA= ',F7.0)
 1060 FORMAT(/,15X,' YND= ',F7.0,' YPD= ',F7.0,' YKD= ',F7.0)
 1085 FORMAT(//, 8X,' Actual nutrient uptake in kg/ha N, P,'
     1              ' and K :')
 1086 FORMAT(/,15X,' Nupt= AMIN(',F7.2,', ',F7.2,')= ',F7.2)
 1087 FORMAT(/,15X,' Pupt= AMIN(',F7.2,', ',F7.2,')= ',F7.2)
 1088 FORMAT(/,15X,' Kupt= AMIN(',F7.2,', ',F7.2,')= ',F7.2)
 3010 FORMAT(F8.0)
 3020 FORMAT(I6)
 3030 FORMAT(A1)
 5000 FORMAT(/,15X,' to continue press <Return>',$)
 6000 FORMAT(//,5X,' Yields calculated for combinations of'
     1             ' two nutrients :')
 6010 FORMAT(/,15X,' YNP= ',F7.0,' YNK= ',F7.0,' YPK= ',F7.0,
     1      //,15X,' YPN= ',F7.0,' YKN= ',F7.0,' YKP= ',F7.0)
 6020 FORMAT(///,3X,'YE= AMIN1(',F6.0,', (',F6.0,' + ',F6.0,
     1             ' + ',F6.0,' + ',F6.0,
     1             ' + ',F6.0,' + ',F6.0,')/6)'//
     1          ,5X,'  = AMIN1(',F6.0,',',F6.0,')' ) 
 6030 FORMAT(6F8.0)
 6040 FORMAT(3F8.0,2F8.2)
      END

CC****************************************************************
        SUBROUTINE NUTR(ICROP,ISC)
CC****************************************************************
C       reading crop data from file NUTRIDAT.DAT

$INCLUDE:'CROPFERT.CMN'
        ISKIP= 4*(ICROP -1)
        CALL SKIP(INUT,ISKIP)
        READ (INUT,2000,ERR=999) CROPNAM
        READ (INUT,2010,ERR=999) NCMINSO,NCMINVE,NCMAXSO,NCMAXVE,
     1  PCMINSO,PCMINVE,PCMAXSO,PCMAXVE
        READ (INUT,2020,ERR=999) KCMINSO,KCMINVE,KCMAXSO,KCMAXVE
        READ (INUT,2030,ERR=999) YZERO,NFIX,LVOPT,STOPT,SOOPT,DUROPT
        
        REWIND INUT
        RETURN
  999   WRITE (ISC,1000) ICROP
        STOP

C       formats
 1000   FORMAT(' CANNOT READ PLANT DATA FOR CROP ',I3)
 2000   FORMAT(15A1)
 2010   FORMAT(8F7.0)
 2020   FORMAT(4F7.0)
 2030   FORMAT(6F7.0)
        END

C********************************************************************
        SUBROUTINE SKIP(IUNIT,ISKIP)
C********************************************************************
C       skips lines in sequential file

        IMPLICIT REAL(A-H,J-Z), INTEGER*2(I)
        CHARACTER*1 DUMMY
        IS=0
   10   IF (IS .EQ. ISKIP) RETURN
        READ (IUNIT,2000,ERR=999,END=999) DUMMY
        IS=IS + 1
        GO TO 10
  999   RETURN
 2000   FORMAT (A1)
        END


CC**********************************************************************
	REAL FUNCTION AFGEN(TABLE,X)
CC**********************************************************************
C	linear interpolation in tables

	IMPLICIT REAL(A-H,J-Z),INTEGER*2(I)
	DIMENSION TABLE(30)
	IF(TABLE(1).GE.X)  GOTO 40
	DO 10 I=3,29,2
	IF(TABLE(I).GE.X) GOTO 30
	IF(TABLE(I).LT.TABLE(I-2)) GOTO 20
10	CONTINUE
C	table fully filled, argument larger then last X in table
        AFGEN=TABLE(30)
        RETURN
C	table partly filled, argument larger then last X in table
20	AFGEN=TABLE(I-1) 
        RETURN
C	argument between first and last X in table, interpolation
30      SLOPE=(TABLE(I+1)-TABLE(I-1))/(TABLE(I)-TABLE(I-2))
	AFGEN= TABLE(I-1) + (X-TABLE(I-2))*SLOPE
        RETURN
C	argument less or equal to first X in table
40	AFGEN=TABLE(2)
        RETURN
	END

