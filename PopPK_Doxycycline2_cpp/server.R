#Load package libraries
library(shiny)        #Creating the app 
library(ggplot2)      #Plotting
library(plyr)          #Split and rearrange data, ddply function
library(reshape2)
library(scales)
library(grid)

#ADVAN-style analytical solution for itraconazole model
source("Twocomp_Tranists_RCppfunctions.R")
sourceCpp("Twocomp_Transits_Cppfunctions.cpp")

#-------------------------------------------------------------------------------------  
#Customize ggplot2 theme - R 2.15.1
theme_bw2 <- theme_set(theme_bw(20))  
theme_bw2 <- theme_update(plot.margin = unit(c(1.5,1.5,3,1.5), "lines"),
                          axis.title.x=element_text(size = 16, vjust = 0),
                          axis.title.y=element_text(size = 16, vjust = 1, angle = 90))  

#--------------------------------------------------------------- 
# This part of the code is used set up multiple cores to run the application over
	#cl <- makePSOCKcluster(detectCores())
	#clusterEvalQ(cl, list(library(foreach)))
#Registers the parallel backend with the foreach package (automatically loaded when
#doParallel is loaded)  
	#registerDoParallel(cl)    
#---------------------------------------------------------------
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

#Set number of individuals to simulate
nID  <- 1000   
ID   <- 1:nID

#Define Between subject variability (Population Variability)
# FOR JESS - what about a correlation matrix???
# FOR JESS - what about a BOV (Currently PPVCL = BSVCL + BOVCL)???
PPVCL         <- rnorm(nID, mean = 0, sd = 0.328)
PPVKTR       <- rnorm(nID, mean = 0, sd = 0.554)
PPVVP1    <- rnorm(nID, mean = 0, sd = 0.151)
PPVV      <- rnorm(nID, mean = 0, sd = 0.376)

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

shinyServer(function(input,output){
  
  SIM.data <- reactive({
    
    input$calculatePK  #Make the execution of the reactive function occur only after clicking Simulate button.

  isolate({           #Isolate the rest of the reactive function, so it doesnt execute when all the sliders are changed          
    SELECT     <- input$SELECT
    
  #The first Formulation: Doryx MPC
    if (SELECT=="1") {
      SPO_DOSE   <- input$SPO_DOSE
      SPO_DAYS   <- input$SPO_DAYS
      FREQ       <- input$FREQ
      FED_STATUS <- input$FED_STATUS   
      #set individual PK parameters
      df.para    <- data.frame(ID)
      df.para$CL <- CLpop*exp(PPVCL)  #need to add DDCL later based on the dosing regimen
      df.para$V <- Vpop*exp(PPVV)
      df.para$CLP1  <- CLP1pop
      df.para$VP1 <- VP1pop*exp(PPVVP1)
      df.para$KTR <- KTRpop*exp(PPVKTR)  #this is for FASTED
      df.para$F1 <- F1pop*F1MPC      #this is for FASTED
      #KTR and F1 (fed versus fasted)
      if(FED_STATUS==2) {df.para$KTR <- KTRpop*exp(PPVKTR)*COVFED2;
      df.para$CL <- CLpop*exp(PPVCL)*COVFEDF;
      df.para$V <- Vpop*exp(PPVV)*COVFEDF;
      df.para$CLP1  <- CLP1pop*COVFEDF;
      df.para$VP1 <- VP1pop*exp(PPVVP1)*COVFEDF} #FED

      #Make a data frame
#set dosing times
if(FREQ==1) { dosetimes <- c(seq(0,(SPO_DAYS-1)*24,by=24)) }  
if(FREQ==2) { dosetimes <- c(seq(0,(SPO_DAYS-0.5)*24,by=12)) } 

#Now define finer sample times for after a dose to capture Cmax
doseseq <- c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,7,8,9,10)   
#Use the outer product but with addition to expand this doseseq for all dosetimes
PKtimes <- outer(dosetimes,doseseq,"+") 

#Now define a background sequence with a wider time interval to fill the gaps between doses
tlast        <- SPO_DAYS*24
sampletimes  <- sort(unique(c(seq(0,tlast,1),PKtimes)))
if(SPO_DAYS >= 5){ sampletimes  <- sort(unique(c(seq(0,tlast,2),PKtimes)))}

      df           <- expand.grid("ID"=ID,"TIME"=sampletimes,"AMT"=0)
      doserows     <- subset(df, TIME%in%dosetimes)
      doserows$AMT <- SPO_DOSE*1000 #to get conc in (ng/ml)
      #Add back dose information
      df <- rbind(df,doserows)
      df <- df[order(df$ID,df$TIME),]                # arrange df by TIME (ascending) and by AMT (descending)
      df <- subset(df, (TIME==0 & AMT==0)==F)        #remove the row that has a TIME=0 and AMT=0
      #For the accumulation
      df$DAY    <- floor(df$TIME/24)+1 
      df$DOSE   <- SPO_DOSE
      df$DDKSPO <- DDKSPO 
      if(FREQ==1){df$DDOSEF <- df$DOSE/DRUGF}    #single dose
      if(FREQ==2){df$DDOSEF <- df$DOSE*2/DRUGF}  #twice daily dose
      df$DDCLSS <- exp(-1*df$DDKSPO*df$DDOSEF)
      df$DDCL   <- ((1-df$DDCLSS)*exp(-KSS*(df$DAY-1)))+df$DDCLSS
      df$DDF    <- ((1-DDFSS)*exp(-KSS*(df$DAY-1)))+DDFSS
      
      dfADVAN <- join(df,df.para,by="ID")
      
      #add DDCL and DDF on central clearance, bioavailability, respectively
      dfADVAN$CL <- dfADVAN$CL*dfADVAN$DDCL 
      dfADVAN$F1 <- dfADVAN$F1*dfADVAN$DDF  
      
      #Calculate micro-rate constants:
      dfADVAN$k20 <- dfADVAN$CL/dfADVAN$V2
      dfADVAN$k23 <- dfADVAN$Q/dfADVAN$V2
      dfADVAN$k32 <- dfADVAN$Q/dfADVAN$V3
      
      #Specify residual errors
      if(SPO_DAYS==1 & FREQ==1 & FED_STATUS==1) {PROPRES <- PROPRES_S_Fasted; ADDRES <- ADDRES_S_Fasted}
      if(SPO_DAYS==1 & FREQ==1 & FED_STATUS==2) {PROPRES <- PROPRES_S_Fed; ADDRES <- ADDRES_S_Fed}
      if(SPO_DAYS==1 & FREQ==2) {PROPRES <- PROPRES_M; ADDRES <- ADDRES_M}
      if(SPO_DAYS>=2) {PROPRES <- PROPRES_M; ADDRES <- ADDRES_M}
           
   } #end of select ==1
  
#start SELECT ==2 ==> Lozanoc alone  
   if (SELECT=="2") {
      LOZ_DOSE   <- input$LOZ_DOSE
      LOZ_DAYS   <- input$LOZ_DAYS
      FREQ       <- input$FREQ
      FED_STATUS <- input$FED_STATUS   
      #set individual PK parameters
      df.para     <- data.frame(ID)
      df.para$CL  <- CLpop*exp(BSVCL)  #need to add DDCL later based on the dosing regimen
      df.para$V2  <- V2pop*exp(BSVV2)
      df.para$Q   <- Qpop
      df.para$V3  <- V3pop
        df.para$KTR <- KTRpop*exp(BSVKTR)  #this is for FASTED
        df.para$F1  <- F1pop*DRUGF*exp(FVAR*ETASCALE)      #this is for FASTED
      #KTR and F1 (fed)
      if(FED_STATUS==2) {df.para$KTR <- KTRpop*exp(BSVKTR)*FEDKTR; df.para$F1 <- F1pop*DRUGF*exp(FVAR*ETASCALE)*FEDF} #Fed

      #Make a data frame
      #set dosing times
      if(FREQ==1) { dosetimes <- c(seq(0,(LOZ_DAYS-1)*24,by=24)) }  
      if(FREQ==2) { dosetimes <- c(seq(0,(LOZ_DAYS-0.5)*24,by=12)) } 
      
      #Now define finer sample times for after a dose to capture Cmax
      doseseq <- c(0,0.5,1,1.5,2,2.5,3,3.5,4,4.5,5,5.5,6,6.5,7,8,9,10)   
      #Use the outer product but with addition to expand this doseseq for all dosetimes
      PKtimes <- outer(dosetimes,doseseq,"+") 

      #Now define a background sequence with a wider time interval to fill the gaps between doses
      tlast        <- LOZ_DAYS*24
      sampletimes  <- sort(unique(c(seq(0,tlast,1),PKtimes)))
      if(LOZ_DAYS >= 5){ sampletimes  <- sort(unique(c(seq(0,tlast,2),PKtimes)))}
      df           <- expand.grid("ID"=ID,"TIME"=sampletimes,"AMT"=0)
      doserows     <- subset(df, TIME%in%dosetimes)
      doserows$AMT <- LOZ_DOSE*1000    #to get conc in (ng/ml)
      #Add back dose information
      df <- rbind(df,doserows)
      df <- df[order(df$ID,df$TIME),]                # arrange df by TIME (ascending) and by AMT (descending)
      df <- subset(df, (TIME==0 & AMT==0)==F)      
        #For the accumulation
      df$DAY    <- floor(df$TIME/24)+1 
      df$DOSE   <- LOZ_DOSE
      df$DDKLOZ <- DDKLOZ 
      if(FREQ==1){df$DDOSEF <- df$DOSE}    #single dose
      if(FREQ==2){df$DDOSEF <- df$DOSE*2}  #twice daily dose
      df$DDCLSS <- exp(-1*df$DDKLOZ*df$DDOSEF)
      df$DDCL   <- ((1-df$DDCLSS)*exp(-KSS*(df$DAY-1)))+df$DDCLSS
      df$DDF    <- ((1-DDFSS)*exp(-KSS*(df$DAY-1)))+DDFSS
      
      dfADVAN <- join(df,df.para,by="ID")
      
      #add DDCL and DDF on central clearance, bioavailability, respectively
      dfADVAN$CL <- dfADVAN$CL*dfADVAN$DDCL 
      dfADVAN$F1 <- dfADVAN$F1*dfADVAN$DDF  
      
      #Calculate micro-rate constants:
      dfADVAN$k20 <- dfADVAN$CL/dfADVAN$V2
      dfADVAN$k23 <- dfADVAN$Q/dfADVAN$V2
      dfADVAN$k32 <- dfADVAN$Q/dfADVAN$V3
      
      #Specify residual errors
      if(LOZ_DAYS==1 & FREQ==1 & FED_STATUS==1) {PROPRES <- PROPRES_S_Fasted; ADDRES <- ADDRES_S_Fasted}
      if(LOZ_DAYS==1 & FREQ==1 & FED_STATUS==2) {PROPRES <- PROPRES_S_Fed; ADDRES <- ADDRES_S_Fed}
      if(LOZ_DAYS==1 & FREQ==2) {PROPRES <- PROPRES_M; ADDRES <- ADDRES_M}
      if(LOZ_DAYS>=2) {PROPRES <- PROPRES_M; ADDRES <- ADDRES_M}
           
   } #end of select ==2

   SIM.data <- ddply(dfADVAN, .(ID), TwoCompOralFourTranistRCpp) 
   #Calculate IPRED
    SIM.data$IPRED <- SIM.data$A2/SIM.data$V2
    SIM.data$IPRED <- ifelse(SIM.data$IPRED <= 0.05, 0.05, SIM.data$IPRED)  #constrain the prediction to be above 1/10th of LLOQ
    SIM.data$logIPRED <- log(SIM.data$IPRED)
    #add residual error 
    SIM.data$W <- sqrt(PROPRES**2+ADDRES**2/exp(SIM.data$logIPRED)**2)
    SIM.data$W<-ifelse(SIM.data$W >= 1,0.1,SIM.data$W)   #To get rid of huge residual errors that doesn't represent model predictions!
    SIM.data$logDV <- SIM.data$logIPRED+SIM.data$W 
    SIM.data$DV <- exp(SIM.data$logDV)          #DV in ng/ml
       
  }) #end of isolate
  
SIM.data <- as.data.frame(SIM.data)

}) #end of reactive function
  
#Generate a plot for the data
output$plotCONC <- renderPlot({
  
  plotobj <- ggplot(SIM.data())
  
 titletext <- expression(atop("Simulated Doxycycline Concentrations",
                               atop("Red line is the median of 1000 simulated subjects"))) 
  
  plotobj <- plotobj + stat_summary(aes(x=TIME, y=DV), fun.y=median, geom="line", colour="red", size=1)
  plotobj <- plotobj + ggtitle(titletext) 
  plotobj <- plotobj + scale_y_continuous("Doxycycline\n plasma concentration (ug/L)\n", labels=comma)
  plotobj <- plotobj +  scale_x_continuous("\nTime after first dose (hours)")
   
if(input$CIDOXY==TRUE) {
  plotobj <- plotobj + stat_summary(aes(x=TIME, y=DV), geom="ribbon", fun.ymin="CI90lo", fun.ymax="CI90hi", alpha=0.5)
   } #end of CIDOXY

print(plotobj) 
}) #end of render plot

#For downloading simulated data
output$downloadsimdata <- downloadHandler(
  filename = function() {
    "SIM.data.doxycycline.csv"
  },
  
  content = function(file) {
    write.csv(SIM.data(),row.names=F, file)        
  }
)   #Close downloadHandler

  
}) #end shiny server


