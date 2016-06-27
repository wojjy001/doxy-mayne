
#--------------------------------------------------------------------------------
# 2 compartment-2transit absorption model via ADVAN-style equations: R Function
#--------------------------------------------------------------------------------
TwoCompTwoTransitR <- function(d){
#Accepts a NONMEM style data frame for 1 subject with columns for TIME, AMT,MDV, CL, V2, Q, V3, KTR & F1
#Returns a dataframe with populated columns for A1, A4, A5, A2, A3 and IPRED

  #set initial values in the compartments
  d$A1[d$TIME==0] <- d$AMT[d$TIME==0]*d$F1[1]   # Amount in the absorption (GUT) compartment at time zero.
  d$A4[d$TIME==0] <- 0                   # Amount in the first transit
  d$A5[d$TIME==0] <- 0                   # Amount in the 2nd transit
  d$A2[d$TIME==0] <- 0                   # Amount in the central compartment at time zero.
  d$A3[d$TIME==0] <- 0                   # Amount in the peripheral compartment at time zero.
  #This loop calculates micro-rate constants based on individual's PK parameter values.
  #It uses these values to calculate macro-rate constants (Lambda1/lambda2).
  #Rate constants(micro- and macro), along other parameters, are used in the equations to calculate drug amounts in each compartment.
  #The loop advances the solution from one time interval to the next.
  #It also calculates the concentration in the central compartment.
  
  for(i in 2:nrow(d))
  {
   
    k20 <- d$CL[i]/d$V2[i]
    k23 <- d$Q[i]/d$V2[i]
    k32 <- d$Q[i]/d$V3[i]
   	KTR <- d$KTR[i]
	k30 <- 0
    E2 <- k20+k23
    E3 <- k32+k30
    
    #calculate hybrid rate constants
    lambda1 = 0.5*((E2+E3)+sqrt((E2+E3)^2-4*(E2*E3-k23*k32)))
    lambda2 = 0.5*((E2+E3)-sqrt((E2+E3)^2-4*(E2*E3-k23*k32)))
            
    t <- d$TIME[i]-d$TIME[i-1]
	
    A4last <- d$A4[i-1]   #1st transit
    A5last <- d$A5[i-1]   #2nd transit
    A2last <- d$A2[i-1]   #central compartment
    A3last <- d$A3[i-1]   #peripheral compartment
    A1last <- d$A1[i-1]   #Gut compartment
	
    #Transit compartments
    d$A4[i] = A4last*exp(-t*KTR)+KTR*A1last*t*exp(-t*KTR)
    d$A5[i] = A5last*exp(-t*KTR)+KTR*A4last*t*exp(-t*KTR)+0.5*KTR**2*A1last*t**2*exp(-t*KTR)
     
    A2term3 = (exp(-t*lambda1)*((A2last*E3+A3last*k32)-A2last*lambda1)-exp(-t*lambda2)*((A2last*E3+A3last*k32)-A2last*lambda2))/(lambda2-lambda1)
    
    A2term2 = KTR*E3*(A5last*(exp(-t*KTR)/((lambda1-KTR)*(lambda2-KTR))+exp(-t*lambda1)/((KTR-lambda1)*(lambda2-lambda1))+exp(-t*lambda2)/((KTR-lambda2)*(lambda1-lambda2)))
    +A4last*KTR*(exp(-t*KTR)*(-lambda1-lambda2+2*KTR)/((lambda1-KTR)**2*(KTR-lambda2)**2)
                 -exp(-t*lambda1)/((lambda1-lambda2)*(lambda1-KTR)**2)
                 +exp(-t*lambda2)/((lambda1-lambda2)*(lambda2-KTR)**2)
                 -exp(-t*KTR)*t/((lambda1-KTR)*(KTR-lambda2)))
    +A1last*KTR**2*((exp(-t*KTR)*(-lambda1**2-lambda1*lambda2+3*lambda1*KTR-lambda2**2+3*lambda2*KTR-3*KTR**2))/((lambda1-KTR)**3*(KTR-lambda2)**3)
                    -exp(-t*KTR)*t**2/(2*(lambda1-KTR)*(KTR-lambda2))+exp(-t*KTR)*t*(-lambda1-lambda2+2*KTR)/((lambda1-KTR)**2*(KTR-lambda2)**2)
                    +exp(-t*lambda1)/((lambda1-lambda2)*(lambda1-KTR)**3)-exp(-t*lambda2)/((lambda1-lambda2)*(lambda2-KTR)**3)))
    
    A2term1 = KTR*(A5last*(exp(-t*KTR)*KTR/((lambda1-KTR)*(KTR-lambda2))+exp(-t*lambda2)*lambda2/((lambda1-lambda2)*(lambda2-KTR))-exp(-t*lambda1)*lambda1/((lambda1-lambda2)*(lambda1-KTR)))
    +A4last*KTR*(exp(-t*KTR)*(lambda1*lambda2-KTR**2)/((lambda1-KTR)**2*(KTR-lambda2)**2)+exp(-t*lambda1)*lambda1/((lambda1-lambda2)*(lambda1-KTR)**2)-exp(-t*lambda2)*lambda2/((lambda1-lambda2)*(lambda2-KTR)**2)+exp(-t*KTR)*t*KTR/((lambda1-KTR)*(KTR-lambda2)))
    +A1last*KTR**2*(exp(-t*KTR)*(lambda1**2*lambda2+lambda1*lambda2**2-3*lambda1*lambda2*KTR+KTR**3)/((lambda1-KTR)**3*(KTR-lambda2)**3)
                    +exp(-t*KTR)*t*(lambda1*lambda2-KTR**2)/((lambda1-KTR)**2*(KTR-lambda2)**2)+exp(-t*KTR)*KTR*t**2/(2*(lambda1-KTR)*(KTR-lambda2))
                    -exp(-t*lambda1)*lambda1/((lambda1-lambda2)*(lambda1-KTR)**3)+exp(-t*lambda2)*lambda2/((lambda1-lambda2)*(lambda2-KTR)**3)))
    
    d$A2[i] = A2term1+A2term2+A2term3
    
    A3term1 = (exp(-t*lambda1)*((A3last*E2+k23*A2last)-A3last*lambda1)-exp(-t*lambda2)*((A3last*E2+k23*A2last)-A3last*lambda2))/(lambda2-lambda1)
    
    A3term2 = KTR*k23*(A5last*(exp(-t*KTR)/((lambda1-KTR)*(lambda2-KTR))+exp(-t*lambda1)/((KTR-lambda1)*(lambda2-lambda1))+exp(-t*lambda2)/((KTR-lambda2)*(lambda1-lambda2)))
                       +A4last*KTR*(exp(-t*KTR)*(-lambda1-lambda2+2*KTR)/((lambda1-KTR)**2*(KTR-lambda2)**2)-exp(-t*lambda1)/((lambda1-lambda2)*(lambda1-KTR)**2)+exp(-t*lambda2)/((lambda1-lambda2)*(lambda2-KTR)**2)-exp(-t*KTR)*t/((lambda1-KTR)*(KTR-lambda2)))
                       +A1last*KTR**2*((exp(-t*KTR)*(-lambda1**2-lambda1*lambda2+3*lambda1*KTR-lambda2**2+3*lambda2*KTR-3*KTR**2))/((lambda1-KTR)**3*(KTR-lambda2)**3)
                                       -exp(-t*KTR)*t**2/(2*(lambda1-KTR)*(KTR-lambda2))+exp(-t*KTR)*t*(-lambda1-lambda2+2*KTR)/((lambda1-KTR)**2*(KTR-lambda2)**2)
                                       +exp(-t*lambda1)/((lambda1-lambda2)*(lambda1-KTR)**3)-exp(-t*lambda2)/((lambda1-lambda2)*(lambda2-KTR)**3)))
    d$A3[i] = A3term1+A3term2
    
    A1last = A1last*exp(-t*KTR)
    d$A1[i] = A1last+d$AMT[i]*d$F1[i]
      
  d$IPRED[i] <- d$A2[i]/d$V2[i]    #Concentration in the central compartment
    
  }
  d
}
