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

#Source  functions Need to have this loaded as it runs most of the processing of the NM output
	source("F:/Hopkins/Doxycycline/functions_utility_08Dec15.R")
  source("Twocomp_Transits_Rfunctions.R")
  source("Twocomp_Tranists_RCppfunctions.R")
  sourceCpp("Twocomp_Transits_Cppfunctions.cpp")
	
#Set the working directory - to the parent directory where you do the modelling/VPC
	master.dir<-"F:/Hopkins/Doxycycline/Simulation/Shiny/Test"
	setwd(master.dir)
	
#---------------------------------------------------------------------------------------------------------------------------------------------
#Step 1.  Create a parameter dataframe with ID covariate values
	
#Define a population
	n <- 12
	ID <- seq(from = 1, to = n, by = 1)
	FFM <- rlnorm(n, meanlog = log(55.49), sdlog =0.09)     #Log normal distribution for fat free mass (kg); 55.49 kg is the mean of the assessed studies.	
	SEX <- rbinom(n, size = 1, prob = 0.50)             #Binomial distribution for sex  1 is male, 0 is female  ; 50% females
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
	T_doseEX <- c(0,336)
  T_doseEX <-data.frame(T_doseEX)
  T_doseEX$CMT <- 1    # CMT 1 = gut
  T_doseEX$AMT <- 120000    # enter dose amount (microg)
  #T_doseEX$RATE <- "."    # dose rate = "." for extravasc or could represent zero order absorption if model requires
  T_doseEX$MDV <- 1    # missing DV 
  T_doseEX$DVID <- NA    #  Dosing DV
  T_doseEX$DV <- NA    #  Get a DV
  T_doseEX <- rename(T_doseEX,c("T_doseEX" = "TIME"))
  T_doseEX 
  
#Define TIME sequence for sampling
  T_sample <- c(0,0.25,0.5,1,1.5,2,2.5,3,3.5,4,6,8,12,16,24,36,48,72,96,
	336,336.25,336.5,337,337.5,338,338.5,339,339.5,340,342,344,348,352,360,372,384,408,432) #user defined times
  
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
  T_sample

  
#Bind dose data
#Only bind the doses that are required in the dataset 
#Time.df <- rbind(T_sample,T_doseIV,T_doseINF,T_doseEX)
Time.df <- rbind(T_sample,T_doseEX)
Time.df


#---------------------------------------------------------------------------------------------------------------------------------------------	
#Step 3.  Combine Time data with subject data


NMdata.df <- expand.grid.df(Time.df,NMdata.df)    #very handy function from reshape
head(NMdata.df)

#re-order by ID and time
NMdata.df <- orderBy(~ID+TIME-AMT, data=NMdata.df)   #"-" means decending to make the first dose appear prior to first sample
head(NMdata.df)

#re-sequence columns
NMdata.df <- NMdata.df[c("ID", "TIME", "AMT", "CMT", "MDV", "DV", "DVID", "TRT", "FED", "FFM", "SEX")]

#Re-name ID to CID to comment out header in NM
NMdata.df <- rename(NMdata.df,c("ID" = "CID"))
head(NMdata.df)

NMdata.df$PER <- 1
NMdata.df$PER[NMdata.df$TIME >= 336] <- 2

NMdata.df$TRT[NMdata.df$CID > 6 & NMdata.df$PER==2] <- 1
NMdata.df$TRT[NMdata.df$CID < 7 & NMdata.df$PER==2] <- 2

NMdata.df.PER1 <- subset(NMdata.df, (PER == 1)==T, stringsAsFactors=F) #subset period 1 data
NMdata.df.PER2 <- subset(NMdata.df, (PER == 2)==T, stringsAsFactors=F) #subset period 2 data

NMdata.df.PER1$TAD <- NMdata.df.PER1$TIME
NMdata.df.PER2$TAD <- NMdata.df.PER2$TIME -336

head(NMdata.df)

NMdata.df2 <- rbind(NMdata.df.PER1,NMdata.df.PER2)
NMdata.df2

#re-order by ID and time
NMdata.df2 <- orderBy(~CID+TIME-AMT, data=NMdata.df2)   #"-" means decending to make the first dose appear prior to first sample
head(NMdata.df2)

NMdata.df2$FFM <- round(NMdata.df2$FFM, digits=2)

NMdata.df2$EVID <- 0
NMdata.df2$EVID[NMdata.df2$TIME == 0 & NMdata.df2$AMT > 0] <- 1
NMdata.df2$EVID[NMdata.df2$TIME == 336 & NMdata.df2$AMT > 0] <- 4

#re-sequence columns
NMdata.df2 <- NMdata.df2[c("CID", "TIME", "TAD", "AMT", "CMT", "MDV", "EVID", "DV", "DVID", "TRT", "PER", "FED", "FFM", "SEX")]

#---------------------------------------------------------------------------------------------------------------------------------------------
filenameout <- "nmdata_12.csv"
write.csv(NMdata.df2, file=filenameout, row.names=F, quote=F, na = ".")
#---------------------------------------------------------------------------------------------------------------------------------------------
#Run in NONMEM

runname = "SIM_12"     #Model file name, do not include the ".ctl" extension
wfn.dir <- "C:/nm73ifort_Com/wfn7/bin/wfn"  #modify as needed

wfn.line <- paste("call ",wfn.dir,sep="")
nmgo.line <- paste("call nmgo", runname, sep=" ")
batch.file <- rbind(wfn.line,nmgo.line)
writeLines(batch.file, "wfntemp.bat")

system("wfntemp", invisible=F, show.output.on.console=T)    #run NONMEM job

#Make sure control is returned to R
plot(1:10,1:10)

file.remove("wfntemp.bat")   #deletes batch file that runs the NONMEM job 


#---------------------------------------------------------------------------------------------------------------------------------------------
#Insert whatever code below that you need to post-process the NONMEM output eg
#Process output from NONMEM to draw some plots of the simulated data
#---------------------------------------------------------------------------------------------------------------------------------------------

#Use custom ggplot2 theme
    theme_bw2 <- theme_set(theme_bw(base_size = 16))  
    theme_bw2 <- theme_update(plot.margin = unit(c(1.1,1.1,3,1.1), "lines"),
    axis.title.x=element_text(size = 16, vjust = 0),
    axis.title.y=element_text(size = 16, vjust = 0, angle = 90),
    strip.text.x=element_text(size = 14),
    strip.text.y=element_text(size = 14, angle = 90))

    
#Process the simulated *.fit file. 
#Run name - Change this to the RUN you want to process
    #runname <- "Caff_sim"        #already set up in NONMEM run code
  
#Process the fit file - Comment this out if you have already generated the csv; this will save time!
    
	#processSIMdata(paste(runname,".ctl",sep=""))    # from the FUNCTION UTILITY

#Read the simulated data
  sim.data <- read.csv(paste(runname,".nm7/",runname,".fit.csv",sep=""), stringsAsFactors=F, na.strings=".")
 length (sim.data$DV)
 
#Factor covariate values (for example, SEX)
    sim.data$SEXf <- as.factor(sim.data$SEX)
    levels(sim.data$SEXf) <- c("Male","Female")
		
    sim.data$FEDf <- as.factor(sim.data$FED)
    levels(sim.data$FEDf) <- c("FASTED","FED")

    sim.data$TRTf <- as.factor(sim.data$TRT)
    levels(sim.data$TRTf) <- c("DORYX XC","DORYX")

    sim.data$PERf <- as.factor(sim.data$PER)
    levels(sim.data$PERf) <- c("PERIOD 1","PERIOD 2")

	#sim.data <- subset(sim.data, (MDVX == 1)==F, stringsAsFactors=F) #subset out data simulated BLOQ		
	sim.data$DV[sim.data$MDVX == 1] <- NA	#replace BLOQ data with NA
	sim.data <- subset(sim.data, (AMT > 0)==F, stringsAsFactors=F)
		
#Set working dir back to be inside the simulation directory to save plots
setwd(paste(master.dir,"/",runname,".nm7",sep="")) 

#Function for calculating 5th and 95th percentiles for plotting concentrations
    CIlow <- function(x) quantile(x, probs = 0.05)
    CIhi <- function(x) quantile(x, probs = 0.95)


#---------------------------------------------------------------------------------------------------------------------------------------------    
#Generate a plot of the sim.data
    plotobj <- NULL
    plotobj <- ggplot(data = sim.data)
    #plotobj <- plotobj + stat_summary(aes(x = TIME, y = DV), fun.ymin = CIlow, fun.ymax = CIhi, geom = "ribbon", fill = "blue", alpha = 0.2)
    plotobj <- plotobj + stat_summary(aes(x = TAD, y = IPRED), fun.ymin = CIlow, fun.ymax = CIhi, geom = "ribbon", fill = "red", alpha = 0.3)
    plotobj <- plotobj + stat_summary(aes(x = TAD, y = IPRED), fun.y = median, geom = "line", size = 1, colour = "red")
    
    plotobj <- plotobj + geom_hline(yintercept=10, linetype = 2, colour = "darkgreen")
    
    plotobj <- plotobj + scale_y_log10("Concentration (mg/L) \n")
    
    #logscaleticks <- sort(c(seq(from=-3,to=3,by=1),seq(from=log10(0.001),to=log10(1000),by=1)))
    #plotobj <- plotobj +  scale_y_log10("Concentration (mg/L) \n", breaks=10^logscaleticks)


    plotobj <- plotobj + scale_x_continuous("\nTime (hours)")
    print(plotobj)
    
    ggsave("conc_time.png", width=20, height=16, units=c("cm"))

#Facet for SEX
    plotobj1 <- plotobj + facet_grid(SEXf~.)
    plotobj1
    ggsave("conc_time_SEX.png", width=20, height=16, units=c("cm"))
#Facet for PER
    plotobj2 <- plotobj + facet_grid(PERf~.)
    plotobj2		
    ggsave("conc_time_PER.png", width=20, height=16, units=c("cm"))
#Facet for TRT
    plotobj3 <- plotobj + facet_grid(TRTf~.)
    plotobj3			
    ggsave("conc_time_TRT.png", width=20, height=16, units=c("cm"))
	

#######################################################################
#######################################################################
    #NCA functions
    AUC0trapz2 <- function(x, y)
    { # computes the integral of y with respect to x using trapezoidal integration - does not handle missing data
      n <- length(x)
      idx <- 2:n
      AUC0t <- (as.double( (x[idx] - x[idx-1]) %*% (y[idx] + y[idx-1])) / 2)
      data.frame(AUC0t,n)
    }
    
    
    AUCinfinity2 <- function(x, y, ntail)
    { # computes an AUC to infinity - does not handle missing data
      
      AUC0t <- AUC0trapz2(x,y)
      AUC0t <- as.numeric(AUC0t[1])  #AUC only
      
      taildatax <- tail(x,ntail)
      taildatay <- tail(y,ntail)
      tailfit <- lm(log(taildatay) ~ taildatax)
      
      k <- -1*tailfit$coefficients["taildatax"]
      thalfz <- log(2)/k
      R2 <- summary(tailfit)["r.squared"]
      
      Clast <- tail(y,1)
      AUCexp <- Clast/k
      AUCinf <- AUC0t+AUCexp
      
      result <- data.frame(AUCinf,k,thalfz,R2)
      #exclude bad extrapolation
      result$AUCinf[result$k < 0 |  result$r.squared < 0.75] <- NA
      result$thalfz[result$k < 0 |  result$r.squared < 0.75] <- NA
      
      result
    }
    
    
    Cmax <- function(x, y)
    { # computes the Cmax
      cmax <- max(y, na.rm=T)
      tindex <- which(y==cmax)
      tmax <- x[tindex]
      unique(cmax)[1] #as there can be 2 or more equal Cmax's, chose the first
    }
    
    
    Cmin <- function(x, y)
    { # computes the Cmin
      cmin <- min(y, na.rm=T)
      tindex <- which(y==cmin)
      tmax <- x[tindex]
      unique(cmin)[1] #as there can be 2 or more equal Cmin's, chose the first
    }
    
    
    tmax <- function(x, y)
    { # computes the time of Cmax
      cmax <- max(y,na.rm=T)
      tindex <- which(y==cmax)
      tmax <- x[tindex]
      head(tmax, n=1)   #as there can be 2 or more equal Cmax's, choose the first
    }
    
    
    NCAcalc <- function(x, y, ntail)
    {
      #Wrapper for various NCA functions
      AUC0t <- AUC0trapz2(x,y)
      AUCinf <- AUCinfinity2(x, y, ntail)
      Cmax <- Cmax(x,y)
      tmax <- tmax(x,y)
      Cmin <- Cmin(x, y)
      
      NCAResult <- data.frame(AUC0t,AUCinf,Cmax,tmax,Cmin)
    }
    
    
    #Trim extreme values for AUC and Cmax  #GOLD - trimmed boxplots
    trim_extreme <- function(x,lower=0.025,upper=0.975)
      #Sets values outside the lower and upper percentiles to NA to remove outliers from boxplots
    {
      lowerlimit <- quantile(x, probs=lower, na.rm=T, names=F) 
      upperlimit <- quantile(x, probs=upper, na.rm=T, names=F)
      x[x < lowerlimit] <- NA
      x[x > upperlimit] <- NA
      x
    }
    

#Function to count numbers in a boxplot and return as a coardinate,label pair
boxplot.give.n <- function(x)
{
  return(c(y = median(x), label = length(x)))
}
		
 #A function to run the BE aov for 1 replicate.  df is as per EXP.data
 runaov <- function(df, USE="AUC0t")
 {
     #debug
     #df <- EXP.data[EXP.data$REP==1,]
     #USE <- "AUC"
     #uselog <- T
     
     #BE aov for study replicate i
     df$metric <- df[,USE]
     result <- aov(log(metric)~TRTf, data=df) 
     result2 <- summary(result)
       
     #This function avoids changing the contrasts for the aov 
        aovtable <- model.tables(result,"means", se=T)
    
        #Extracting aov results
        sigma2 <- result2[[1]][["Mean Sq"]][2]    #careful, hard coded depends on number of aov terms
        Xt <- aovtable$tables$TRTf["Test"]
        Xr <- aovtable$tables$TRTf["Reference"]
        n1 <- (length(df$TRTf[df$TRTf=="Test"]))/2      #number of people starting with test
        n2 <- (length(df$TRTf[df$TRTf=="Reference"]))/2 #number of people starting with REFERENCE
        tval <- qt(0.95,(n1+n2-1))                      #degree of freedom number of participants in study minus 1
        A <- 0.5 #0.5 for 2x2 cross-over, 1 for parallel

        pointestimate <- exp(Xt-Xr)
        pointestimate

        CI90lovalue <- exp((Xt-Xr)-tval*sqrt(A*sigma2*(1/n1+1/n2)))
        CI90lovalue

        CI90hivalue <- exp((Xt-Xr)+tval*sqrt(A*sigma2*(1/n1+1/n2)))
        CI90hivalue
        
        #Bioequivalence test 1=bioequivalent
        bioflag <- 0
        if (CI90lovalue > 0.8 & CI90hivalue < 1.25) bioflag <- 1

        BEresultsi <- data.frame("Metric"=USE,"pointestimate"=pointestimate,"lowerCI"=CI90lovalue,"upperCI"=CI90hivalue,"BE"=bioflag)
    
 }  
 
############################################################################################
############################################################################################
###############################################################################	
#---------------------------------------------------------------------------------------------------------------------------------------------
#Only keep important data for Bioequivalence assessment
	BEdata <- subset(sim.data, select=c(SIM,ID,TIME,TAD,AMT,DV,IPRED,FFM,FED,PER,TRT,SEX,FEDf,PERf,TRTf,SEXf))
	BEdata <- subset(BEdata, (AMT > 0)==F, stringsAsFactors=F)
	
#Set up a BEdata for the Doryx XC and Standard formulation
	BEdata <- subset(BEdata, select=c(SIM,ID,TAD,DV,IPRED,TRT,PER,SEX,FED))
	
	BEdata <- subset(BEdata, is.na(DV)==F)   #cant have missing DV

	AUCdata <- NULL
  Cmaxdata <- NULL
  NCAdata <- NULL
  NCAresult <- NULL

  AUCdata <- ddply(BEdata, .(SIM,ID,TRT,PER,SEX,FED), function(d) AUCtrapz2(d$TAD,d$DV))
  Cmaxdata <- ddply(BEdata, .(SIM,ID,TRT,PER,SEX,FED), function(d) Cmax(d$TAD,d$DV))
  Tmaxdata <- ddply(BEdata, .(SIM,ID,TRT,PER,SEX,FED), function(d) tmax(d$TAD,d$DV))
  
  NCA.BEdata <- cbind(AUCdata,"CMAX"=Cmaxdata$V1, "TMAX"=Tmaxdata$V1)
	
  NCA.BEdata$SEXf <- as.factor(NCA.BEdata$SEX)
  levels(NCA.BEdata$SEXf) <- c("Male","Female")
		
  NCA.BEdata$FEDf <- as.factor(NCA.BEdata$FED)
  levels(NCA.BEdata$FEDf) <- c("FASTED","FED")

    NCA.BEdata$TRTf <- as.factor(NCA.BEdata$TRT)
    levels(NCA.BEdata$TRTf) <- c("DORYX XC","DORYX")

    NCA.BEdata$PERf <- as.factor(NCA.BEdata$PER)
    levels(NCA.BEdata$PERf) <- c("PERIOD 1","PERIOD 2")
	
#TRIM Extreme values of Cmax, AUC0t, TMAX
	 #NCA.BEdata$AUC0t <- trim_extreme(NCA.BEdata$AUC0t)
	 #NCA.BEdata$CMAX <- trim_extreme(NCA.BEdata$CMAX)
	 #NCA.BEdata$TMAX <- trim_extreme(NCA.BEdata$TMAX)	 
	
		 logscaleticks <- logscaleticks <- sort(c(seq(from=1000,to=100000,by=10000)))	
	
     plotobj <- NULL
		 plotobj <-  ggplot(data=NCA.BEdata)
     plotobj <- plotobj + geom_boxplot(aes(x = TRTf, y = AUC0t, colour=TRTf), notch=F)
     plotobj <- plotobj + stat_summary(aes_string(x = "TRTf", y = "AUC0t"), data=NCA.BEdata, fun.data = boxplot.give.n, geom = "text", size = 6, colour="BLACK") 
     plotobj <- plotobj + scale_x_discrete("Data Source")
     #plotobj <- plotobj + scale_y_log10("AUC (ng/ml * h)", breaks=10^logscaleticks2, labels=comma)  #, lim=c(0,35)
     plotobj <- plotobj + scale_y_log10("AUC (ng/ml * h)", breaks=logscaleticks)
		 plotobj <- plotobj + scale_colour_brewer("Data Source", palette="Set1")
     plotobj  <- plotobj + ggtitle("Simulation of population doxycycline AUC by formulation (Single 120mg Dose)\n")
     plotobj
 
		 to.png(plotobj,"SIM_AUC_TRTf")
		 
     plotobj <- NULL
		 plotobj <-  ggplot(data=NCA.BEdata)
     plotobj <- plotobj + geom_boxplot(aes(x = TRTf, y = AUC0t, colour=TRTf), notch=F)
     plotobj <- plotobj + stat_summary(aes_string(x = "TRTf", y = "AUC0t"), data=NCA.BEdata, fun.data = boxplot.give.n, geom = "text", size = 6, colour="BLACK") 
     plotobj <- plotobj + scale_x_discrete("Data Source")
     #plotobj <- plotobj + scale_y_log10("AUC (ng/ml * h)", breaks=10^logscaleticks2, labels=comma)  #, lim=c(0,35)
     plotobj <- plotobj + scale_y_log10("AUC (ng/ml * h)", breaks=logscaleticks)
		 plotobj <- plotobj + scale_colour_brewer("Data Source", palette="Set1")
     plotobj  <- plotobj + ggtitle("Simulation of population doxycycline AUC by formulation (Single 120mg Dose)\n")
     plotobj <- plotobj <- plotobj + facet_wrap(~SEXf, ncol=2, scales="free_y")		 
     plotobj
 
		 to.png(plotobj,"SIM_AUC_TRTf_SEXf")

     plotobj <- NULL
		 plotobj <-  ggplot(data=NCA.BEdata)
     plotobj <- plotobj + geom_boxplot(aes(x = TRTf, y = AUC0t, colour=TRTf), notch=F)
     plotobj <- plotobj + stat_summary(aes_string(x = "TRTf", y = "AUC0t"), data=NCA.BEdata, fun.data = boxplot.give.n, geom = "text", size = 6, colour="BLACK") 
     plotobj <- plotobj + scale_x_discrete("Data Source")
     #plotobj <- plotobj + scale_y_log10("AUC (ng/ml * h)", breaks=10^logscaleticks2, labels=comma)  #, lim=c(0,35)
     plotobj <- plotobj + scale_y_log10("AUC (ng/ml * h)", breaks=logscaleticks)
		 plotobj <- plotobj + scale_colour_brewer("Data Source", palette="Set1")
     plotobj  <- plotobj + ggtitle("Simulation of population doxycycline AUC by formulation (Single 120mg Dose)\n")
     plotobj <- plotobj <- plotobj + facet_wrap(~PERf, ncol=2, scales="free_y")		 
     plotobj
 
		 to.png(plotobj,"SIM_AUC_TRTf_PERf")

     plotobj <- NULL
		 plotobj <-  ggplot(data=NCA.BEdata)
     plotobj <- plotobj + geom_boxplot(aes(x = TRTf, y = AUC0t, colour=TRTf), notch=F)
     plotobj <- plotobj + stat_summary(aes_string(x = "TRTf", y = "AUC0t"), data=NCA.BEdata, fun.data = boxplot.give.n, geom = "text", size = 6, colour="BLACK") 
     plotobj <- plotobj + scale_x_discrete("Data Source")
     #plotobj <- plotobj + scale_y_log10("AUC (ng/ml * h)", breaks=10^logscaleticks2, labels=comma)  #, lim=c(0,35)
     plotobj <- plotobj + scale_y_log10("AUC (ng/ml * h)", breaks=logscaleticks)
		 plotobj <- plotobj + scale_colour_brewer("Data Source", palette="Set1")
     plotobj  <- plotobj + ggtitle("Simulation of population doxycycline AUC by formulation (Single 120mg Dose)\n")
     plotobj <- plotobj <- plotobj + facet_wrap(~FEDf, ncol=2, scales="free_y")		 
     plotobj
 
		 to.png(plotobj,"SIM_AUC_TRTf_FEDf")		 
		 
	 
		 logscaleticks <- logscaleticks <- sort(c(seq(from=500,to=100000,by=1000)))	
		 
     plotobj <- NULL
		 plotobj <-  ggplot(data=NCA.BEdata)
     plotobj <- plotobj + geom_boxplot(aes(x = TRTf, y = CMAX, colour=TRTf), notch=F)
     plotobj <- plotobj + stat_summary(aes_string(x = "TRTf", y = "CMAX"), data=NCA.BEdata, fun.data = boxplot.give.n, geom = "text", size = 6, colour="BLACK") 
     plotobj <- plotobj + scale_x_discrete("Data Source")
     plotobj <- plotobj + scale_y_log10("Cmax (ug/L)\n", breaks=logscaleticks) #, breaks=10^logscaleticks2, labels=comma) #"Cmax (ng/ml)", breaks=10^logscaleticks2, labels=comma)  #, lim=c(0,35)
     plotobj <- plotobj + scale_colour_brewer("Data Source", palette="Set1")
     plotobj  <- plotobj + ggtitle("Simulation of population doxycycline Cmax by formulation (Single 120mg Dose)\n")
     plotobj
  
     to.png(plotobj,"SIM_Cmax_TRTf")
		 
     plotobj <- NULL
		 plotobj <-  ggplot(data=NCA.BEdata)
     plotobj <- plotobj + geom_boxplot(aes(x = TRTf, y = CMAX, colour=TRTf), notch=F)
     plotobj <- plotobj + stat_summary(aes_string(x = "TRTf", y = "CMAX"), data=NCA.BEdata, fun.data = boxplot.give.n, geom = "text", size = 6, colour="BLACK") 
     plotobj <- plotobj + scale_x_discrete("Data Source")
     plotobj <- plotobj + scale_y_log10("Cmax (ug/L)\n", breaks=logscaleticks) #, breaks=10^logscaleticks2, labels=comma) #"Cmax (ng/ml)", breaks=10^logscaleticks2, labels=comma)  #, lim=c(0,35)
     plotobj <- plotobj + scale_colour_brewer("Data Source", palette="Set1")
     plotobj  <- plotobj + ggtitle("Simulation of population doxycycline Cmax by formulation (Single 120mg Dose)\n")
     plotobj <- plotobj <- plotobj + facet_wrap(~FEDf, ncol=2, scales="free_y")		 
     plotobj
  
     to.png(plotobj,"SIM_Cmax_TRTf_FEDf")

     plotobj <- NULL
		 plotobj <-  ggplot(data=NCA.BEdata)
     plotobj <- plotobj + geom_boxplot(aes(x = TRTf, y = CMAX, colour=TRTf), notch=F)
     plotobj <- plotobj + stat_summary(aes_string(x = "TRTf", y = "CMAX"), data=NCA.BEdata, fun.data = boxplot.give.n, geom = "text", size = 6, colour="BLACK") 
     plotobj <- plotobj + scale_x_discrete("Data Source")
     plotobj <- plotobj + scale_y_log10("Cmax (ug/L)\n", breaks=logscaleticks) #, breaks=10^logscaleticks2, labels=comma) #"Cmax (ng/ml)", breaks=10^logscaleticks2, labels=comma)  #, lim=c(0,35)
     plotobj <- plotobj + scale_colour_brewer("Data Source", palette="Set1")
     plotobj  <- plotobj + ggtitle("Simulation of population doxycycline Cmax by formulation (Single 120mg Dose)\n")
     plotobj <- plotobj <- plotobj + facet_wrap(~PERf, ncol=2, scales="free_y")		 
     plotobj
  
     to.png(plotobj,"SIM_Cmax_TRTf_PERf")

     plotobj <- NULL
		 plotobj <-  ggplot(data=NCA.BEdata)
     plotobj <- plotobj + geom_boxplot(aes(x = TRTf, y = CMAX, colour=TRTf), notch=F)
     plotobj <- plotobj + stat_summary(aes_string(x = "TRTf", y = "CMAX"), data=NCA.BEdata, fun.data = boxplot.give.n, geom = "text", size = 6, colour="BLACK") 
     plotobj <- plotobj + scale_x_discrete("Data Source")
     plotobj <- plotobj + scale_y_log10("Cmax (ug/L)\n", breaks=logscaleticks) #, breaks=10^logscaleticks2, labels=comma) #"Cmax (ng/ml)", breaks=10^logscaleticks2, labels=comma)  #, lim=c(0,35)
     plotobj <- plotobj + scale_colour_brewer("Data Source", palette="Set1")
     plotobj  <- plotobj + ggtitle("Simulation of population doxycycline Cmax by formulation (Single 120mg Dose)\n")
     plotobj <- plotobj <- plotobj + facet_wrap(~SEXf, ncol=2, scales="free_y")		 
     plotobj
  
     to.png(plotobj,"SIM_Cmax_TRTf_SEXf")			 

		 logscaleticks <- logscaleticks <- sort(c(seq(from=0.5,to=20,by=0.5)))		 

		 plotobj <- NULL
     plotobj <-  ggplot(data=NCA.BEdata)
     plotobj <- plotobj + geom_boxplot(aes(x = TRTf, y = TMAX, colour=TRTf), notch=F)
     plotobj <- plotobj + stat_summary(aes_string(x = "TRTf", y = "TMAX"), data=NCA.BEdata, fun.data = boxplot.give.n, geom = "text", size = 6, colour="BLACK") 
     plotobj <- plotobj + scale_x_discrete("Data Source")
     plotobj <- plotobj + scale_y_log10("Tmax (h)\n", breaks=logscaleticks) #, breaks=10^logscaleticks2, labels=comma) #"Cmax (ng/ml)", breaks=10^logscaleticks2, labels=comma)  #, lim=c(0,35)
     plotobj <- plotobj + scale_colour_brewer("Data Source", palette="Set1")
     plotobj  <- plotobj + ggtitle("Simulation of population doxycycline Tmax by formulation (Single 120mg Dose)\n") 
     plotobj
  
     to.png(plotobj,"SIM_Tmax_TRTf")

		 plotobj <- NULL
     plotobj <-  ggplot(data=NCA.BEdata)
     plotobj <- plotobj + geom_boxplot(aes(x = TRTf, y = TMAX, colour=TRTf), notch=F)
     plotobj <- plotobj + stat_summary(aes_string(x = "TRTf", y = "TMAX"), data=NCA.BEdata, fun.data = boxplot.give.n, geom = "text", size = 6, colour="BLACK") 
     plotobj <- plotobj + scale_x_discrete("Data Source")
     plotobj <- plotobj + scale_y_log10("Tmax (h)\n", breaks=logscaleticks) #, breaks=10^logscaleticks2, labels=comma) #"Cmax (ng/ml)", breaks=10^logscaleticks2, labels=comma)  #, lim=c(0,35)
     plotobj <- plotobj + scale_colour_brewer("Data Source", palette="Set1")
     plotobj  <- plotobj + ggtitle("Simulation of population doxycycline Tmax by formulation (Single 120mg Dose)\n")
     plotobj <- plotobj <- plotobj + facet_wrap(~SEXf, ncol=2, scales="free_y")			 
     plotobj
  
     to.png(plotobj,"SIM_Tmax_TRTf_SEXf")

		 plotobj <- NULL
     plotobj <-  ggplot(data=NCA.BEdata)
     plotobj <- plotobj + geom_boxplot(aes(x = TRTf, y = TMAX, colour=TRTf), notch=F)
     plotobj <- plotobj + stat_summary(aes_string(x = "TRTf", y = "TMAX"), data=NCA.BEdata, fun.data = boxplot.give.n, geom = "text", size = 6, colour="BLACK") 
     plotobj <- plotobj + scale_x_discrete("Data Source")
     plotobj <- plotobj + scale_y_log10("Tmax (h)\n", breaks=logscaleticks) #, breaks=10^logscaleticks2, labels=comma) #"Cmax (ng/ml)", breaks=10^logscaleticks2, labels=comma)  #, lim=c(0,35)
     plotobj <- plotobj + scale_colour_brewer("Data Source", palette="Set1")
     plotobj  <- plotobj + ggtitle("Simulation of population doxycycline Tmax by formulation (Single 120mg Dose)\n")
     plotobj <- plotobj <- plotobj + facet_wrap(~FEDf, ncol=2, scales="free_y")			 
     plotobj
  
     to.png(plotobj,"SIM_Tmax_TRTf_FEDf")

		 plotobj <- NULL
     plotobj <-  ggplot(data=NCA.BEdata)
     plotobj <- plotobj + geom_boxplot(aes(x = TRTf, y = TMAX, colour=TRTf), notch=F)
     plotobj <- plotobj + stat_summary(aes_string(x = "TRTf", y = "TMAX"), data=NCA.BEdata, fun.data = boxplot.give.n, geom = "text", size = 6, colour="BLACK") 
     plotobj <- plotobj + scale_x_discrete("Data Source")
     plotobj <- plotobj + scale_y_log10("Tmax (h)\n", breaks=logscaleticks) #, breaks=10^logscaleticks2, labels=comma) #"Cmax (ng/ml)", breaks=10^logscaleticks2, labels=comma)  #, lim=c(0,35)
     plotobj <- plotobj + scale_colour_brewer("Data Source", palette="Set1")
     plotobj  <- plotobj + ggtitle("Simulation of population doxycycline Tmax by formulation (Single 120mg Dose)\n")
     plotobj <- plotobj <- plotobj + facet_wrap(~PERf, ncol=2, scales="free_y")			 
     plotobj
  
     to.png(plotobj,"SIM_Tmax_TRTf_PERf")		 

########B.E
 library(dplyr)     
     
     
     
 BEdata <- BEdata 
 BEdata <- subset(BEdata, is.na(DV)==F)   #cant have missing DV     

 NCAresult <- NULL      
 NCAresult <- ddply(BEdata, .(SIM,ID,TRT,PER,SEX,FED), function(df) NCAcalc(df$TAD,df$DV,ntail=3))
 
 NCAresult2 <- data.frame("REP"=NCAresult$SIM,
                           "ID"=NCAresult$ID,
                           "AUCinf"=NCAresult$AUCinf,
                           "AUC0t"=NCAresult$AUC0t,
                           "CMAX"=NCAresult$Cmax,
                           "TMAX"=NCAresult$tmax,
                           "TRT"=NCAresult$TRT, stringsAsFactors=F)
     
 NCAresult2$TRTf[NCAresult2$TRT == 1] <- "Test"
 NCAresult2$TRTf[NCAresult2$TRT == 2] <- "Reference"
 
 NCAresult2$AUCinf[is.na(NCAresult2$AUCinf)] <- NCAresult2$AUC0t[is.na(NCAresult2$AUCinf)] # replace uncalculable AUCinf (NA's) with AUCot
 
 #Calculate power for AUC
 BEresultsAUC <- ddply(NCAresult2, .(REP), function(df) runaov(df,USE="AUC0t"))
 BEresultsAUC  #result for the 2 studies
 
 BEresultsAUCinf <- ddply(NCAresult2, .(REP), function(df) runaov(df,USE="AUCinf"))
 BEresultsAUCinf  #result for the 2 studies
 
 BEresultsTMAX <- ddply(NCAresult2, .(REP), function(df) runaov(df,USE="TMAX"))
 BEresultsTMAX  #result for the 2 studies
 
 BEresultsCMAX <- ddply(NCAresult2, .(REP), function(df) runaov(df,USE="CMAX"))
 BEresultsCMAX  #result for the 2 studies
 
 BEresultsAUC <- data.frame("REP"=BEresultsAUC$REP,
                          "Metric"=BEresultsAUC$Metric,
                          "pointestimate"=BEresultsAUC$pointestimate,
                          "lowerCI"=BEresultsAUC$lowerCI,
                          "upperCI"=BEresultsAUC$upperCI,
                          "AUC_BE"=BEresultsAUC$BE, stringsAsFactors=F)

 BEresultsAUCinf <- data.frame("REP"=BEresultsAUCinf$REP,
                            "Metric"=BEresultsAUCinf$Metric,
                            "pointestimate"=BEresultsAUCinf$pointestimate,
                            "lowerCI"=BEresultsAUCinf$lowerCI,
                            "upperCI"=BEresultsAUCinf$upperCI,
                            "AUCinf_BE"=BEresultsAUCinf$BE, stringsAsFactors=F)
 
 BEresultsCMAX <- data.frame("REP"=BEresultsCMAX$REP,
                            "Metric"=BEresultsCMAX$Metric,
                            "pointestimate"=BEresultsCMAX$pointestimate,
                            "lowerCI"=BEresultsCMAX$lowerCI,
                            "upperCI"=BEresultsCMAX$upperCI,
                            "CMAX_BE"=BEresultsCMAX$BE, stringsAsFactors=F)

 BEresultsTMAX <- data.frame("REP"=BEresultsTMAX$REP,
                            "Metric"=BEresultsTMAX$Metric,
                            "pointestimate"=BEresultsTMAX$pointestimate,
                            "lowerCI"=BEresultsTMAX$lowerCI,
                            "upperCI"=BEresultsTMAX$upperCI,
                            "TMAX_BE"=BEresultsTMAX$BE, stringsAsFactors=F)  
 
 BEresultsAOV <- cbind(BEresultsAUC,BEresultsAUCinf,BEresultsTMAX,BEresultsCMAX)
 BEresultsAOV
 
 BEresultsAOV$BE0t  <- ifelse(BEresultsAOV$AUC_BE == 1 & BEresultsAOV$CMAX_BE == 1,1,0)
 BEresultsAOV$BEinf  <- ifelse(BEresultsAOV$AUCinf_BE == 1 & BEresultsAOV$CMAX_BE == 1,1,0) 
 
 write.csv(BEresultsAOV, file="BEresults_ANOVA.csv", na=".", row.names=F, quote=F)
 
 #Use the replicated studies to work out the power of the study design
 powerAUC <- sum(BEresultsAUC$AUC_BE)/length(BEresultsAUC$AUC_BE)*100  #as BE is 0 or 1
 powerAUC 
 
 powerAUCinf <- sum(BEresultsAUCinf$AUCinf_BE)/length(BEresultsAUCinf$AUCinf_BE)*100  #as BE is 0 or 1
 powerAUCinf
 
 powerTMAX <- sum(BEresultsTMAX$TMAX_BE)/length(BEresultsTMAX$TMAX_BE)*100  #as BE is 0 or 1
 powerTMAX
 
 powerCMAX <- sum(BEresultsCMAX$CMAX_BE)/length(BEresultsCMAX$CMAX_BE)*100  #as BE is 0 or 1
 powerCMAX

 powerBE0t <- sum(BEresultsAOV$BE0t)/length(BEresultsAOV$BE0t)*100  #as BE is 0 or 1
 powerBE0t

 powerBEinf <- sum(BEresultsAOV$BEinf)/length(BEresultsAOV$BEinf)*100  #as BE is 0 or 1
 powerBEinf 
 
 BEpowerresultAOV <- cbind(powerAUC,powerAUCinf,powerTMAX,powerCMAX,powerBE0t,powerBEinf)
 BEpowerresultAOV
 
 write.csv(BEpowerresultAOV, file="POWERresults_ANOVA.csv", na=".", row.names=F, quote=F)

#################################
#################################
       
			 #Balthasar ----- ASH
 BEdataAUC  <- data.frame("REP"=NCAresult2$REP,
                          "ID"=NCAresult2$ID,
                          "AUC0t"=NCAresult2$AUC0t,
                          "TRTf"=NCAresult2$TRTf, stringsAsFactors=F)	 
			 
 BEdataAUClong <- spread(BEdataAUC, TRTf, AUC0t, fill = NA, convert = FALSE)
 
 BEdataAUClong  <- data.frame("REP"= BEdataAUClong$REP,
                             "ID"= BEdataAUClong$ID,
                             "AUC0ttest"= BEdataAUClong$Test,
                             "AUC0treference"= BEdataAUClong$Reference, stringsAsFactors=F)	
 #BEdataAUClong <- rename(BEdataAUClong,c("Test" = "AUCtest", "Reference" = "AUCreference"))
	
 #Calculate the difference of the log AUC
 BEdataAUClong$AUCdiff <- log(BEdataAUClong$AUC0ttest)-log(BEdataAUClong$AUC0treference)    #is equivalent to ln(AUC1/AUC2)
 BEdataAUCresults <- ddply(BEdataAUClong,~REP,summarise,meandiff=mean(AUCdiff),sddiff=sd(AUCdiff),nsubj=length(AUCdiff))
 
#Find the t-value and calculate the 90% confidence interval
 BEdataAUCresults$tval <- qt(0.95,(BEdataAUCresults$nsubj-1))  #1.645 for large n 
 
 BEdataAUCresults$lo90CI <- exp(BEdataAUCresults$meandiff - BEdataAUCresults$tval*BEdataAUCresults$sddiff/(sqrt(BEdataAUCresults$nsubj)))
 BEdataAUCresults$hi90CI <- exp(BEdataAUCresults$meandiff + BEdataAUCresults$tval*BEdataAUCresults$sddiff/(sqrt(BEdataAUCresults$nsubj)))
 BEdataAUCresults$bioflag <- 0
 BEdataAUCresults$bioflag[BEdataAUCresults$lo90CI > 0.8 & BEdataAUCresults$hi90CI < 1.25] <- 1
 BEdataAUCresults
 
 percent.passed <- sum(BEdataAUCresults$bioflag)/length(BEdataAUCresults$bioflag)*100
 percent.passed 	
 
 #####AHMAD --  ALL NCA BE comparisons
 BEdatalong  <- data.frame("REP"=NCAresult2$REP,
                          "ID"=NCAresult2$ID,
                          "AUC0t"=NCAresult2$AUC0t,
                          "AUCinf"=NCAresult2$AUCinf,
                          "TMAX"=NCAresult2$TMAX,
                          "CMAX"=NCAresult2$CMAX,
                          "TRTf"=NCAresult2$TRTf, stringsAsFactors=F)	 
 
 BEdatalong <- reshape(BEdatalong,idvar=c("ID","REP"),timevar="TRTf",direction="wide")
 #BEdatalong <- spread(NCAresult2, TRTf, c(AUC0t,AUCinf,TMAX,CMAX), fill = NA, convert = FALSE)
 
 ratio <- BEdatalong %>%
   group_by(ID,REP) %>%
   summarise(AUC_R = (AUC0t.Test/AUC0t.Reference),
             AUCinf_R = (AUCinf.Test/AUCinf.Reference),
             TMAX_R = (TMAX.Test/TMAX.Reference),
             CMAX_R = (CMAX.Test/CMAX.Reference))
 head(ratio)
 
 ratio <- ratio %>%
   group_by(ID,REP) %>%
   mutate(logAUC_R = log(AUC_R),
          logAUCinf_R = log(AUCinf_R),
          logTMAX_R = log(TMAX_R),
          logCMAX_R = log(CMAX_R))
 head(ratio)
 
 Fresult <- ratio %>%
   group_by(REP) %>%
   summarise (logmeanAUC = mean(logAUC_R, na.rm=T),
              logmeanAUCinf = mean(logAUCinf_R, na.rm=T),
              logmeanTMAX = mean(logTMAX_R, na.rm=T),
              logmeanCMAX = mean(logCMAX_R, na.rm=T))
 head(Fresult)
 
 Fresult <- ratio %>%
   group_by(REP) %>%
   summarise(logmeanAUC = mean(logAUC_R, na.rm=T),
          crittAUC    = qt(.95, (length(na.omit(logAUC_R)))-1),
          sem_logAUC = sd(logAUC_R, na.rm=T)/sqrt(length(na.omit(logAUC_R))),
          logmeanAUCinf = mean(logAUCinf_R, na.rm=T),
          crittAUCinf    = qt(.95, (length(na.omit(logAUCinf_R)))-1),
          sem_logAUCinf = sd(logAUCinf_R, na.rm=T)/sqrt(length(na.omit(logAUCinf_R))),
          logmeanTMAX = mean(logTMAX_R, na.rm=T),
          crittTMAX    = qt(.95, (length(na.omit(logTMAX_R)))-1),
          sem_logTMAX = sd(logTMAX_R, na.rm=T)/sqrt(length(na.omit(logTMAX_R))),
          logmeanCMAX = mean(logCMAX_R, na.rm=T),
          crittCMAX    = qt(.95, (length(na.omit(logCMAX_R)))-1),
          sem_logCMAX = sd(logCMAX_R, na.rm=T)/sqrt(length(na.omit(logCMAX_R))))
 head(Fresult)
 
 # Calculate 90% CI for each exposure metrics for each study
 Fresult <- Fresult %>%
   group_by(REP) %>%
   mutate(log90lo_AUC = logmeanAUC - crittAUC*sem_logAUC,
          log90hi_AUC = logmeanAUC + crittAUC*sem_logAUC,
          log90lo_AUCinf = logmeanAUCinf - crittAUCinf*sem_logAUCinf,
          log90hi_AUCinf = logmeanAUCinf + crittAUCinf*sem_logAUCinf,
          log90lo_TMAX = logmeanTMAX - crittTMAX*sem_logTMAX,
          log90hi_TMAX = logmeanTMAX + crittTMAX*sem_logTMAX,
          log90lo_CMAX = logmeanCMAX - crittCMAX*sem_logCMAX,
          log90hi_CMAX = logmeanCMAX + crittCMAX*sem_logCMAX)
 head(Fresult)
 
 #Take exponents of the geometric means and 90% CI
 Fresult2 <- Fresult %>%
   group_by(REP) %>%
   summarise(Gmean_AUC = exp(logmeanAUC),
             CI90lo_AUC = exp(log90lo_AUC),
             CI90hi_AUC = exp(log90hi_AUC),
             Gmean_AUCinf = exp(logmeanAUCinf),
             CI90lo_AUCinf = exp(log90lo_AUCinf),
             CI90hi_AUCinf = exp(log90hi_AUCinf),
             Gmean_TMAX = exp(logmeanTMAX),
             CI90lo_TMAX = exp(log90lo_TMAX),
             CI90hi_TMAX = exp(log90hi_TMAX),
             Gmean_CMAX = exp(logmeanCMAX),
             CI90lo_CMAX = exp(log90lo_CMAX),
             CI90hi_CMAX = exp(log90hi_CMAX))
 head(Fresult2)
 
 #calculate the % of BE studies
 # AUCinf
 Fresult2$P_AUCinf <- ifelse(Fresult2$CI90lo_AUCinf < 0.8 | Fresult2$CI90hi_AUCinf > 1.25,0,1)
 #AUC
 Fresult2$P_AUC <- ifelse(Fresult2$CI90lo_AUC < 0.8 | Fresult2$CI90hi_AUC > 1.25,0,1)
 #CMAX
 Fresult2$P_CMAX <- ifelse(Fresult2$CI90lo_CMAX < 0.8 | Fresult2$CI90hi_CMAX > 1.25,0,1)
 #TMAX
 Fresult2$P_TMAX <- ifelse(Fresult2$CI90lo_TMAX < 0.8 | Fresult2$CI90hi_TMAX > 1.25,0,1)
 #BE0t
 Fresult2$P_BE0t <- ifelse(Fresult2$P_AUC == 1 & Fresult2$P_CMAX == 1,1,0)
 #BEinf
 Fresult2$P_BEinf <- ifelse(Fresult2$P_AUCinf == 1 & Fresult2$P_CMAX == 1,1,0)
 
 head(Fresult2)
 
 write.csv(Fresult2, file="BEresults_BALTHASAR.csv", na=".", row.names=F, quote=F) 
 
 Percent_AUCinf = sum(Fresult2$P_AUCinf)/length(Fresult2$REP)*100
 Percent_AUC    = sum(Fresult2$P_AUC)/length(Fresult2$REP)*100
 Percent_CMAX   = sum(Fresult2$P_CMAX)/length(Fresult2$REP)*100
 Percent_TMAX   = sum(Fresult2$P_TMAX)/length(Fresult2$REP)*100
 Percent_BE0t   = sum(Fresult2$P_BE0t)/length(Fresult2$REP)*100
 Percent_BEinf   = sum(Fresult2$P_BEinf)/length(Fresult2$REP)*100
 
 BEresult <- cbind(Percent_AUCinf,Percent_AUC,Percent_CMAX,Percent_TMAX,Percent_BE0t,Percent_BEinf)
 BEresult 

 write.csv(BEresult, file="POWERresults_BALTHASAR.csv", na=".", row.names=F, quote=F)  
 
 #---------------------------------------------------------------------------------------------------------------
 #Compare with ANOVA
 #See Concordet_Statistics_in_bioequivalence.ppt Slide48 and thereafter
 #Estimate the mean for each formulation (xbarTtest, xbarRef)
 #Estimate the between (parallel) or within (crossover) subjects variance. (sigma^2)
 
 #Make sure factors are used for predictors
 EXP.data  <- data.frame("REP"=NCAresult2$REP,
                          "ID"=NCAresult2$ID,
                          "AUC0t"=NCAresult2$AUC0t,
                         "AUCinf"=NCAresult2$AUCinf,
                         "TMAX"=NCAresult2$TMAX,
                         "CMAX"=NCAresult2$CMAX,
                          "TRTf"=NCAresult2$TRTf, stringsAsFactors=F)	 
 
 EXP.data <- subset( EXP.data, (REP == 1)==T, stringsAsFactors=F)
 EXP.data$TRT <- as.factor(EXP.data$TRTf)
 
 #The aov
 result <- aov(log(AUC0t) ~ TRTf, data=EXP.data)
 result
 result2 <- summary(result)
 result2
 #The residual variance (the variance of the residuals!) appears in the anova table as the "residual mean square".
 #Thus the residual standard error (the standard deviation of the residuals) is sqrt(residual mean square)
 
 aovtable <- model.tables(result,"means", se=T)
 aovtable
 
 #Extracting aov data by hand
 Xt <- aovtable$tables$TRTf["Test"]
 Xt
 Xr <- aovtable$tables$TRTf["Reference"]
 Xr
 SE <- aovtable$se$TRTf[1,1]
 SE
 nsubj <- aovtable$n
 nsubj
 tval <- qt(0.95,(nsubj-1))
 
 pointestimateAUC <- exp(Xt-Xr)
 pointestimateAUC
 
 CI90lovalueAUC <- exp((Xt-Xr)-tval*SE)
 CI90lovalueAUC
 
 CI90hivalueAUC <- exp((Xt-Xr)+tval*SE)
 CI90hivalueAUC
 
 #The aov
 result <- aov(log(AUCinf) ~ TRTf, data=EXP.data)
 result
 result2 <- summary(result)
 result2
 #The residual variance (the variance of the residuals!) appears in the anova table as the "residual mean square".
 #Thus the residual standard error (the standard deviation of the residuals) is sqrt(residual mean square)
 
 aovtable <- model.tables(result,"means", se=T)
 aovtable
 
 #Extracting aov data by hand
 Xt <- aovtable$tables$TRTf["Test"]
 Xt
 Xr <- aovtable$tables$TRTf["Reference"]
 Xr
 SE <- aovtable$se$TRTf[1,1]
 SE
 nsubj <- aovtable$n
 nsubj
 tval <- qt(0.95,(nsubj-1))
 
 pointestimateAUCinf <- exp(Xt-Xr)
 pointestimateAUCinf
 
 CI90lovalueAUCinf <- exp((Xt-Xr)-tval*SE)
 CI90lovalueAUCinf
 
 CI90hivalueAUCinf <- exp((Xt-Xr)+tval*SE)
 CI90hivalueAUCinf

 #The aov
 result <- aov(log(TMAX) ~ TRTf, data=EXP.data)
 result
 result2 <- summary(result)
 result2
 #The residual variance (the variance of the residuals!) appears in the anova table as the "residual mean square".
 #Thus the residual standard error (the standard deviation of the residuals) is sqrt(residual mean square)
 
 aovtable <- model.tables(result,"means", se=T)
 aovtable
 
 #Extracting aov data by hand
 Xt <- aovtable$tables$TRTf["Test"]
 Xt
 Xr <- aovtable$tables$TRTf["Reference"]
 Xr
 SE <- aovtable$se$TRTf[1,1]
 SE
 nsubj <- aovtable$n
 nsubj
 tval <- qt(0.95,(nsubj-1))
 
 pointestimateTMAX <- exp(Xt-Xr)
 pointestimateTMAX
 
 CI90lovalueTMAX <- exp((Xt-Xr)-tval*SE)
 CI90lovalueTMAX
 
 CI90hivalueTMAX <- exp((Xt-Xr)+tval*SE)
 CI90hivalueTMAX

 #The aov
 result <- aov(log(CMAX) ~ TRTf, data=EXP.data)
 result
 result2 <- summary(result)
 result2
 #The residual variance (the variance of the residuals!) appears in the anova table as the "residual mean square".
 #Thus the residual standard error (the standard deviation of the residuals) is sqrt(residual mean square)
 
 aovtable <- model.tables(result,"means", se=T)
 aovtable
 
 #Extracting aov data by hand
 Xt <- aovtable$tables$TRTf["Test"]
 Xt
 Xr <- aovtable$tables$TRTf["Reference"]
 Xr
 SE <- aovtable$se$TRTf[1,1]
 SE
 nsubj <- aovtable$n
 nsubj
 tval <- qt(0.95,(nsubj-1))
 
 pointestimateCMAX <- exp(Xt-Xr)
 pointestimateCMAX
 
 CI90lovalueCMAX <- exp((Xt-Xr)-tval*SE)
 CI90lovalueCMAX
 
 CI90hivalueCMAX <- exp((Xt-Xr)+tval*SE)
 CI90hivalueCMAX

 AOVresult <- cbind(pointestimateAUC,CI90lovalueAUC,CI90hivalueAUC,
                    pointestimateAUCinf,CI90lovalueAUCinf,CI90hivalueAUCinf,
                    pointestimateTMAX,CI90lovalueTMAX,CI90hivalueTMAX,
                    pointestimateCMAX,CI90lovalueCMAX,CI90hivalueCMAX)
 
 