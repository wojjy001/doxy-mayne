#R script for generating a simulation dataset for use with NONMEM
#Then process the results of the NONMEM simulation

#---------------------------------------------------------------------------------------------------------------------------------------------
#Remove all current objects in the workspace
	rm(list=ls(all=TRUE))

#Load package libraries
	library(deSolve)	#Differential equation solver
	library(ggplot2)	#Plotting
	library(plyr)		#Split and rearrange data, ddply function
	library(grid)		#Plotting
	library(reshape)    #data wrangling functions
  library(doBy)       #data ordering functions
	library(compiler)	#Compile repeatedly-called functions
	library(tidyr)
  library(Rcpp)
  library(rbenchmark)

#Set the working directory - to the parent directory where you do the modelling/VPC
master.dir<-"C:/Users/hopam002/Documents/Shiny_APP/Test"
setwd(master.dir)

#Source  functions Need to have this loaded as it runs most of the processing of the NM output
	source("functions_utility_06Feb15.R")
  source("Twocomp_Tranists_RCppfunctions.R")
  sourceCpp("Twocomp_Transits_Cppfunctions.cpp")
	
#---------------------------------------------------------------------------------------------------------------------------------------------
#Step 1.  Create a parameter dataframe with ID and covariate values for the MPC formulation
	
#Define a population
	n <- 1000
	ID <- seq(from = 1, to = n, by = 1)
	FFM <- rlnorm(n, meanlog = log(55.49), sdlog =0.09)     #Log normal distribution for fat free mass (kg); 55.49 kg is the mean of the assessed studies.	
	#SEX <- rbinom(n, size = 1, prob = 0.50)             #Binomial distribution for sex  1 is male, 0 is female  ; 50% females
	SEX <- 1 # Males
  FED <- 0 																						#Fasted simulation  
	
	#Check demographics
	hist(FFM)	
	table(SEX)
	table(TRT)

NMdata.df <- data.frame(ID,FFM,SEX,FED)
head(NMdata.df)

NMdata.df$TRT <- 1
NMdata.df$TRT[NMdata.df$ID > 6] <- 2

head(NMdata.df)

#---------------------------------------------------------------------------------------------------------------------------------------------	
#Step 2.  Make a time sequence and specify the dose information
	
#Define IV bolus doses
  #T_doseIV <- c(0, 24, 28)    #user defined times
  #T_doseIV <-data.frame(T_doseIV)
  #T_doseIV$CMT <- 2    # CMT 2 = blood
  #T_doseIV$AMT <- 9    # enter dose amount
  #T_doseIV$RATE <- "."    # dose rate = "." for bolus
  #T_doseIV$MDV <- 1    # missing DV 
  #T_doseIV$DVID <- 0    #  Dosing DVID
  #T_doseIV$DV <- "."    #  Get a DV
  #T_doseIV <- rename(T_doseIV,c("T_doseIV" = "TIME"))
  #T_doseIV 
  
  #Define INF Infusion doses
  #T_doseINF <- c(0)    #user defined times
  #T_doseINF <-data.frame(T_doseINF)
  #T_doseINF$CMT <- 2    # CMT 2 = blood
  #T_doseINF$AMT <- 9    # enter dose amount
  #T_doseINF$RATE <- 18    # enter dose rate
  #T_doseINF$MDV <- 1    # missing DV 
  #T_doseINF$DVID <- 0    #  Dosing DV
  #T_doseINF$DV <- "."    #  Get a DV
  #T_doseINF <- rename(T_doseINF,c("T_doseINF" = "TIME"))
  #T_doseINF 
  
#Define Extravascular doses
  #T_doseEX <- c(seq(from = 24, to = 1152, by = 24))   #user defined times
	T_doseEX <- c(0)
  T_doseEX <-data.frame(T_doseEX)
  T_doseEX$CMT <- 1    # CMT 1 = gut
  T_doseEX$AMT <- 120000    # enter dose amount (microg)
  #T_doseEX$RATE <- "."    # dose rate = "." for extravasc or could represent zero order absorption if model requires
  T_doseEX$MDV <- 1    # missing DV 
  T_doseEX$DVID <- 0    #  Dosing DV
  T_doseEX$DV <- NA    #  Get a DV
  T_doseEX <- rename(T_doseEX,c("T_doseEX" = "TIME"))
  T_doseEX 
  
#Define TIME sequence for sampling
  T_sample <- c(0,0.25,0.5,1,1.5,2,2.5,3,3.5,4,6,8,12,16,24,36,48,72,96) #user defined times
  
	#Tlast = 240
  #T_sample <- seq(from = 0, to = Tlast, by = 2)    #intense sampling
  #T_sample <- c(seq(from = 0, to = 2, by = 0.5),seq(from = 0, to = Tlast, by = 2))    # intense early sampling followed by less intense
  #T_sample <- c(seq(from = 0, to = 1176, by = 24),seq(from = 2, to = 1154, by = 24))   # sequence for peak and trough
  T_sample <-data.frame(T_sample)
  T_sample$CMT <- 2    # CMT 2 = blood
  T_sample$AMT <- NA    # no dose given
  #T_sample$RATE <- "."    # dose rate = "." for samplinfg times
  T_sample$MDV <- 0    # not missing DV 
  T_sample$DVID <- 1    #  sampling DV
  T_sample$DV <- NA    #  Get a DV
  T_sample <- rename(T_sample,c("T_sample" = "TIME"))
  head(T_sample)
  #T_sample

  
#Bind dose data
#Only bind the doses that are required in the dataset 
#Time.df <- rbind(T_sample,T_doseIV,T_doseINF,T_doseEX)
Time.df <- rbind(T_sample,T_doseEX)
head(Time.df)
tail(Time.df)
#Time.df

#---------------------------------------------------------------------------------------------------------------------------------------------	
#Step 3.  Combine Time data with subject data
NMdata.df <- expand.grid.df(Time.df,NMdata.df)    #very handy function from reshape
head(NMdata.df)

#re-order by ID and time
NMdata.df <- orderBy(~ID+TIME-AMT, data=NMdata.df)   #"-" means decending to make the first dose appear prior to first sample
head(NMdata.df)

NMdata.df$TRT <- 1
head(NMdata.df)

#re-sequence columns
NMdata.df <- NMdata.df[c("ID", "TIME", "AMT", "CMT", "MDV", "DV", "DVID", "TRT", "FED", "FFM", "SEX")]
#---------------------------------------------------------------------------------------------------------------------------------------------
df.para <- NMdata.df

#Define average between study variability
COVSTDF <- 1.157
COVSTDKTR <- 0.894
COVSTDCL <- 0.789
COVSTDV <- 0.786

#Define theta values of the PK parameters in the model
CLtheta  <- 4.63  
Vtheta  <- 55.2  
CLP1theta   <- 11.3   
VP1theta  <- 49.8
KTRtheta <- 2
F1theta  <- 1

#Define population values of PK parameters in the model
CLpop  <- CLtheta * COVSTDCL 
Vpop  <- Vtheta * COVSTDV  
CLP1pop   <- CLP1theta  
VP1pop  <- VP1theta
KTRpop <- KTRtheta * COVSTDKTR
F1pop  <- F1theta * COVSTDF

#Define Between subject variability (Population Variability)
# FOR JESS - what about a correlation matrix???
# FOR JESS - what about a BOV (Currently PPVCL = BSVCL + BOVCL)???
df.ETA <- data.frame(ID)
df.ETA$ID <- seq(from = 1, to = n, by = 1)
df.ETA$PPVCL         <- rnorm(n, mean = 0, sd = 0.328)
df.ETA$PPVKTR       <- rnorm(n, mean = 0, sd = 0.554)
df.ETA$PPVVP1    <- rnorm(n, mean = 0, sd = 0.151)
df.ETA$PPVV      <- rnorm(n, mean = 0, sd = 0.376)
df.para <- merge(df.para,df.ETA,by="ID")

#Define residual error (Specified later in the reactive function!)
PROPRES <- 0.196
ADDRES <- 19.8

#Define structural covariate parameters
F1MPC <- 0.863 #Relative bioavailability of Doryx MPC compared to Doryx tablet
COVFED <- -0.209 #Effect of food on KTR for the Doryx tablet and capsules
COVFED2 <- -0.549 #A food effect on KTR for Doryx MPC
#These lag effects need to be added
ALAG1 <- 0.115 #A lag on KTR for the Doryx MPC and Doryx Capsule formulations (h)
FTLAG2 <- 0.203 #A lag on KTR for the fed status

#Define covariate parameters
COVFEDF <- 0.105 #Effect of food on CL,V,VP1,CLP1 (relative bioavailability)
COVSEX <- 0.144 #Effect of female sex on CL

#set individual PK parameters
df.para$CL <- CLpop*exp(df.para$PPVCL)*((df.para$FFM/70)**0.75)  #need to add DDCL later based on the dosing regimen
df.para$V2 <- Vpop*exp(df.para$PPVV)*(df.para$FFM/70) 
df.para$Q  <- CLP1pop*((df.para$FFM/70)**0.75)
df.para$V3 <- VP1pop*exp(df.para$PPVVP1)*(df.para$FFM/70)
df.para$KTR <- KTRpop*exp(df.para$PPVKTR)  #this is for FASTED
df.para$F1 <- F1pop*F1MPC      #this is for FASTED

#KTR and F1 (fed versus fasted)
if(FED==2) {df.para$KTR <- KTRpop*exp(df.para$PPVKTR)*COVFED2;
df.para$CL <- CLpop*exp(df.para$PPVCL)*((df.para$FFM/70)**0.75)*COVFEDF;
df.para$V2 <- Vpop*exp(df.para$PPVV)*(df.para$FFM/70)*COVFEDF;
df.para$Q  <- CLP1pop*((df.para$FFM/70)**0.75)*COVFEDF;
df.para$V3 <- VP1pop*exp(df.para$PPVVP1)*(df.para$FFM/70)*COVFEDF} #FED
#remove duplicate time-line.
df.para <- subset(df.para, (TIME==0 & AMT==0)==F) #remove the row that has a TIME=0 and AMT=0
df.para$AMT[is.na(df.para$AMT)==T] <- 0
#-------------
#ADD doryx tablet simulation
df.para2 <- df.para
df.para2$AMT[df.para2$AMT == 120000] <- 100000
df.para2$TRT <- 2
df.para2$ID <- df.para$ID + 1000

#adjust parameters for the doryx tablet
df.para2$F1[df.para2$TRT == 2] <- F1pop

#Bind to one data frame
df.para3 <- rbind(df.para,df.para2)

#re-order by ID and time
df.para3 <- orderBy(~ID+TIME-AMT, data=df.para3)   #"-" means decending to make the first dose appear prior to first sample
head(df.para3)

dfadvan <- df.para3

#Apply the ADVAN function for each ID in df
simdfRcpp <- ddply(dfadvan, .(ID), TwoCompTwoTransit)
system.time(simdfRcpp <- ddply(dfadvan, .(ID), TwoCompTwoTransit))

head(simdfRcpp)
tail(simdfRcpp)

#-------------------------------------------------------------------------------------
simdfRcpp$TRTf <- as.factor(simdfRcpp$TRT)
levels(simdfRcpp$TRTf) <- c("DORYX MPC","DORYX")




plotobj <- NULL
titletext <- expression(atop('2 compartment with 2-tranist absoprtion compartments',
                             atop(italic("Graph showing amounts in gut, central, and peripheral compartments"))))
plotobj  <- ggplot(data=simdfRcpp)
plotobj <-  plotobj + geom_point(aes(x=TIME, y=A2), size=3, colour="red") #central
plotobj <-  plotobj + geom_point(aes(x=TIME, y=A3), size=3, colour="blue") #peripheral 
plotobj <-  plotobj + geom_point(aes(x=TIME, y=A1), size=3, colour="blue") #Absorption
plotobj <-  plotobj + ggtitle(titletext)
plotobj <-  plotobj + scale_y_continuous("Amount in Compartment")
#plotobj <-  plotobj + facet_wrap(~ID)
plotobj



plotobj <- NULL
titletext <- expression(atop("Simulated Doxycycline Concentrations",
                             atop("Solid line is the median of 1000 simulated subjects"))) 
plotobj  <- ggplot(data=simdfRcpp)
plotobj <- plotobj + stat_summary(aes(x=TIME, y=IPRED, colour = TRTf), fun.y=median, geom="line", size=1)
plotobj <- plotobj + ggtitle(titletext)
plotobj <- plotobj + scale_y_log10("Doxycycline\n plasma concentration (ug/L)\n")
plotobj <- plotobj +  scale_x_continuous("\nTime after first dose (hours)")
plotobj
plotobj2 <- plotobj + stat_summary(aes(x=TIME, y=IPRED, fill = TRTf), geom="ribbon", fun.ymin="CI90lo", fun.ymax="CI90hi", alpha=0.5)
plotobj2






