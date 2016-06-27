library(Rcpp)
sourceCpp("Twocomp_Transits_Cppfunctions.cpp")
#---------------------------------------------------------------------------------
# 2 compartment-2transit absorption model via ADVAN-style equations: RCppfunctions
#---------------------------------------------------------------------------------
TwoCompTwoTransit <- function(inputDataFrame){
#Accepts a NONMEM style data frame for 1 subject with columns for TIME, AMT,MDV, CL, V2, Q, V3, KTR & F1
#Returns a dataframe with populated columns for A1, A4, A5, A2, A3 and IPRED

  #Calculate micro-rate constants
  inputDataFrame$k20 <- inputDataFrame$CL/inputDataFrame$V2
  inputDataFrame$k23 <- inputDataFrame$Q/inputDataFrame$V2
  inputDataFrame$k32 <- inputDataFrame$Q/inputDataFrame$V3
  inputDataFrame$k30 <- 0
  
  #Add columns for amounts for marginal speed gain!
  inputDataFrame$A1 <- 0
  inputDataFrame$A4 <- 0
  inputDataFrame$A5 <- 0  
  inputDataFrame$A2 <- 0
  inputDataFrame$A3 <- 0
  
  #set initial values in the compartments
  inputDataFrame$A1[inputDataFrame$TIME==0] <- inputDataFrame$AMT[inputDataFrame$TIME==0]*inputDataFrame$F1[1]   # Amount in the absorption (GUT) compartment at time zero.
  inputDataFrame$A4[inputDataFrame$TIME==0] <- 0                   # Amount in the first transit
  inputDataFrame$A5[inputDataFrame$TIME==0] <- 0                   # Amount in the 2nd transit
  inputDataFrame$A2[inputDataFrame$TIME==0] <- 0                   # Amount in the central compartment at time zero.
  inputDataFrame$A3[inputDataFrame$TIME==0] <- 0                   # Amount in the peripheral compartment at time zero.
  
  #Process
  TwoCompTwoTransitCpp( inputDataFrame )
  
  #Calculate IPRED for the central compartment
  inputDataFrame$IPRED <- inputDataFrame$A2/inputDataFrame$V2
  
  #subset extra columns
  inputDataFrame <- subset(inputDataFrame, select=-c(k20,k23,k32,k30))
  
  #Return output
  inputDataFrame
}
