$PROBLEM - DOXYCYCLINE 11450707
;NM7.3 

$INPUT ID TIME TAD AMT CMT MDV EVID DV DVID TRT PER FED FFM SEX
;ID TNOM TAD TAFD=TIME AMT DV DVID CMT MDVM2=MDV MDVM3 EVIDM2=EVID EVIDM3 BLOQ PER TRT TAB FED STUDY SUBJECT SEX AGE RACE WEIGHT BMI SMOKER PKIGNORE

$DATA ..\nmdata_28.csv IGNORE=C

$SUBROUTINES  ADVAN5 TRANS1

$MODEL ;SPECIFIES COMPARTMENTS FOR THE DIFFERENTIAL EQUATIONS

 COMP=(DEPOT DEFDOSE INITIALOFF)
 COMP=(CENTRAL DEFOBS NOOFF)
 COMP=(MET)    ;3
 COMP=(P1)
 COMP=(T1)
 COMP=(T2) 

$PK
 ; UNITS L; HOURS; MICROGRAMS
 IF (NEWIND.LE.1) DOSE=0
 IF (AMT.GT.0) DOSE=( AMT/1000 ) 
;--------------------------------------------------------
 LLOQ = 10 ; LLOQ has been set to the most recent assay. i.e. MYP-P2-991
 
 ;LLOQ=50 ; MICROG/L
 ;IF (STUDY.EQ.10) THEN
 ;  LLOQ=100
 ;ENDIF

 ;IF (STUDY.EQ.20) THEN
 ;  LLOQ=25
 ;ENDIF
 
 ;IF (STUDY.EQ.70) THEN
 ;  LLOQ=25
 ;ENDIF

 ;IF (STUDY.EQ.80) THEN
 ;  LLOQ=10
 ;ENDIF
;-------------------------------------------------------- 
  IF (FED.EQ.0) COVFED2=1
  ;IF (FED.EQ.1) COVFED2=(1+THETA(19))
	IF (FED.EQ.1) COVFED2=(1+THETA(14))
;--------------------------------------------------------	
	IF (SEX.EQ.1) COVSEX = 1
  ;IF (SEX.EQ.0) COVSEX = (1+THETA(20))
	IF (SEX.EQ.0) COVSEX = (1+THETA(15))
;-------------------------------------------------------- 
	IF (FED.EQ.0) COVFED=1
  IF (FED.EQ.1.AND.TRT.EQ.2) COVFED=(1+THETA(10))
  IF (FED.EQ.1.AND.TRT.EQ.3) COVFED=(1+THETA(10))
  IF (FED.EQ.1.AND.TRT.EQ.1) COVFED=(1+THETA(11))
;--------------------------------------------------------
 ;FFM has been simulated within the dataset
 
 ;IF (SEX.EQ.0) FFM=(9270*WEIGHT)/(8780 + (244*BMI))
 ;IF (SEX.EQ.1) FFM=(9270*WEIGHT)/(6680 + (216*BMI))
;--------------------------------------------------------
 ; COVSTDF FIXED to the average of the eight studies.
 COVSTDF = 1.157
 ; THETA(14) = 0.516   ;COVSTDF1
 ;  THETA(15) = -0.146   ;COVSTDF2
 ;IF (STUDY.EQ.10) COVSTDF=1
 ;IF (STUDY.EQ.20) COVSTDF=(1 + THETA(14))
 ;IF (STUDY.EQ.30) COVSTDF=1
 ;IF (STUDY.EQ.40) COVSTDF=(1 + THETA(15))
 ;IF (STUDY.EQ.50) COVSTDF=1
 ;IF (STUDY.EQ.60) COVSTDF=(1 + THETA(15))
 ;IF (STUDY.EQ.70) COVSTDF=(1 + THETA(14))
 ;IF (STUDY.EQ.80) COVSTDF=(1 + THETA(14)) 
;--------------------------------------------------------
 ; COVSTDKTR FIXED to the average of the eight studies. 
 COVSTDKTR = 0.894
 ; THETA(16) = -0.213   ;COVSTDKTR
 ;IF (STUDY.EQ.10) COVSTDKTR=1
 ;IF (STUDY.EQ.20) COVSTDKTR=1
 ;IF (STUDY.EQ.30) COVSTDKTR=(1 + THETA(16))
 ;IF (STUDY.EQ.40) COVSTDKTR=(1 + THETA(16))
 ;IF (STUDY.EQ.50) COVSTDKTR=(1 + THETA(16))
 ;IF (STUDY.EQ.60) COVSTDKTR=(1 + THETA(16))
 ;IF (STUDY.EQ.70) COVSTDKTR=1
 ;IF (STUDY.EQ.80) COVSTDKTR=1 
;--------------------------------------------------------
 ; COVSTDCL FIXED to the average of the eight studies. 
 COVSTDCL = 0.789
 ; THETA(17) = -0.241   ;COVSTDCL
 ;IF (STUDY.EQ.10) COVSTDCL=1
 ;IF (STUDY.EQ.20) COVSTDCL=(1 + THETA(17))
 ;IF (STUDY.EQ.30) COVSTDCL=(1 + THETA(17))
 ;IF (STUDY.EQ.40) COVSTDCL=(1 + THETA(17))
 ;IF (STUDY.EQ.50) COVSTDCL=(1 + THETA(17))
 ;IF (STUDY.EQ.60) COVSTDCL=(1 + THETA(17))
 ;IF (STUDY.EQ.70) COVSTDCL=(1 + THETA(17))
 ;IF (STUDY.EQ.80) COVSTDCL=(1 + THETA(17)) 
;--------------------------------------------------------
 ; COVSTDV FIXED to the average of the eight studies.
 COVSTDV = 0.786
 ;THETA(18) = -0.245   ;COVSTDV
 ;IF (STUDY.EQ.10) COVSTDV=1
 ;IF (STUDY.EQ.20) COVSTDV=(1 + THETA(18))
 ;IF (STUDY.EQ.30) COVSTDV=(1 + THETA(18))
 ;IF (STUDY.EQ.40) COVSTDV=(1 + THETA(18))
 ;IF (STUDY.EQ.50) COVSTDV=(1 + THETA(18))
 ;IF (STUDY.EQ.60) COVSTDV=(1 + THETA(18))
 ;IF (STUDY.EQ.70) COVSTDV=(1 + THETA(18))
 ;IF (STUDY.EQ.80) COVSTDV=(1 + THETA(18)) 
;--------------------------------------------------------
 BSVCL=ETA(1)
 
 IF (PER.EQ.1) THEN
  BOVCL=ETA(5)
 ELSE
  BOVCL=ETA(6)
 ENDIF
 
 PPVCL=BSVCL + BOVCL
;-------------------------------------------------------- 
 BSVKTR=ETA(2)
 
 IF (PER.EQ.1) THEN
  BOVKTR=ETA(7)
 ELSE
  BOVKTR=ETA(8)
 ENDIF
 
 PPVKTR=BSVKTR + BOVKTR 
;-------------------------------------------------------- 
 TVF=1 

 IF(TRT.EQ.1) THEN
 TVF=THETA(8)
 ENDIF

 IF(TRT.EQ.3) THEN
 TVF=THETA(9)
 ENDIF
 
 F1=TVF
;-------------------------------------------------------- 
 KTR=THETA(7) * EXP(PPVKTR) * COVFED * COVSTDKTR

 V=THETA(3)*(FFM/70) * EXP(ETA(4)) * COVSTDF * COVSTDV * COVFED2
 CL=THETA(4)*((FFM/70)**0.75) * EXP(PPVCL) * COVSTDF * COVSTDCL * COVFED2 * COVSEX

 VP1=THETA(5)*(FFM/70) * EXP(ETA(3)) * COVSTDF * COVFED2
 CLP1=THETA(6)*((FFM/70)**0.75) * COVSTDF * COVFED2
 
  IF (TRT.EQ.2) TLAG=0
  IF (TRT.EQ.1) TLAG=THETA(12)  
  IF (TRT.EQ.3) TLAG=THETA(12)
 
 IF (FED.EQ.0) FTLAG=0
 IF (FED.EQ.1) FTLAG=THETA(13)
 
 ALAG1=TLAG + FTLAG

;--------------------------------------------------------  
 S2=V
;--------------------------------------------------------  
 K15=KTR
 K56=KTR
 K62=KTR
 
 K24=CLP1/V
 K42=CLP1/VP1
 
 K23=CL/V
;----------------------------------------------------------------- 
$THETA
	(0,0.196,)   ; RUV_PROP
	(0.01,19.8,) ; RUV_ADD
	(0.01,55.2,) ; V
	(0.01,4.63,) ; CL
	(0.01,49.8,) ; VP1
	(0.01,11.3,) ; CLP1
	(0.001,2.,) ;KTR 
	(0.01,0.863,) ;F1XC
	(0.01,0.978,) ;F1CAP 
  (-0.99,-0.209,5)   ;COVFED
  (-0.99,-0.549,5)   ;COVFED2
	(0.001,0.115,) ; ALAG1  
	(0.001,0.203,) ; FTLAG2
;  (-0.99,0.516,5)   ;COVSTDF1
;  (-0.99,-0.146,5)   ;COVSTDF2
;  (-0.99,-0.213,5)   ;COVSTDKTR
;  (-0.99,-0.241,5)   ;COVSTDCL
;	(-0.99,-0.245,5)   ;COVSTDV
  (-0.99,0.105,5)   ;COVFEDF
  (-0.99,0.144,5)   ;COVSEX	
;-----------------------------------------------------------------       
$OMEGA BLOCK(4)
0.0373 ;BSV_CL ;STARTS AT 60% CV ON CL
0.0229 0.0796 ;BSV_KTR ;STARTS AT 60% CV ON V ; 10% COVARIANCE (R) OF V WITH CL
0.0106 -0.01 0.0229 ;PPV_VP1
0.0522 0.0936 -0.00506 0.141 ;PPV_V

$OMEGA BLOCK(1)
0.0183 ; BOVCL1
$OMEGA BLOCK(1) SAME ; BOVCL2

$OMEGA BLOCK(1)
0.0738 ; BOVKTR1
$OMEGA BLOCK(1) SAME ; BOVKTR2
;-----------------------------------------------------------------             
$SIGMA
1. FIX ;RUV_FIX
;-----------------------------------------------------------------       
$ERROR 
 IPRED=F 
 PROP=THETA(1)*F
 ADD=THETA(2) ; ADDITIVE PART 
 SD=SQRT(PROP*PROP + ADD*ADD) ; STANDARD DEVIATION USING PROPORTIONAL
 
 Y=F+SD*ERR(1)
 
 SIM = IREP
 
 IF (ICALL.EQ.4) THEN
  IF (AMT.GT.0.OR.Y.LT.LLOQ) THEN
   MDVX=1
  ELSE
   MDVX=0
  ENDIF
 ENDIF 
 
;------------------------------------------------------------------  
$SIMULATION (1234567)  ONLYSIM SUBPROBLEMS=1000

;$ESTIMATION METHOD=1 INTERACTION MAXEVALS=9990 POSTHOC NOABORT PRINT=1 NSIG=3 SIGL=9

;$COVARIANCE  UNCONDITIONAL SIGL=12 PRINT=E

$TABLE SIM ID TIME TAD AMT CMT MDV MDVX EVID DV DVID TRT PER FED FFM SEX
 IPRED V CL KTR CWRES ETA1 ETA2 ETA3 ETA4 FFM DOSE NPDE=PDERR
 NOPRINT ONEHEADER FILE=*.fit
