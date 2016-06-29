# server.R script for PrototypeAPAP3
# Reactive objects (i.e., those dependent on widget input) are written here
# ------------------------------------------------------------------------------
# Define the "server" part of the Shiny application
shinyServer(function(input,output,session) {
	###########
	##_INPUT_##
	###########
	# Create a reactive input expression that takes widget values and makes a data frame ready for Bayesian estimation of individual pharmacokinetic parameters (Rinput.data)
	Rinput.data <- reactive({
		# Call in reactive widget input from ui.R
		WT <- input$WT  #Patient's weight (kg)
		AMT <- input$AMT*1000  #Estimated amount ingested (mg)
		PROD <- input$PROD  #Product category ingested
		if (input$SDAC == TRUE) SDAC <- 1	#If the patient received single-dose activated charcoal then SDAC = 1
		if (input$SDAC == FALSE) SDAC <- 0  #If the patient did not receive single-dose activated charcoal, then SDAC = 0
		PAC1 <- input$PAC1  #First plasma acetaminophen concentration (mg/L)
		TIME1 <- input$TIME1  #TIme for first plasma acetaminophen concentration (hours)
		PAC2 <- NA  #Make NA unless the appropriate "selectInput" option has been chosen
		TIME2 <- 0  #When PAC2 is NA, just make it correspond to TIME = 0
		if (input$NPAC > 1) {
			PAC2 <- input$PAC2  #Second plasma acetaminophen concentration (mg/L)
			TIME2 <- input$TIME2  #Time for second plasma acetaminophen concentration (hours)
		}
		# Slot sampling times into TIME sequence
		TIME <- sort(unique(c(TIME.base,TIME1,TIME2)))
		# Create a sequence of PAC values for corresponding times
		PAC <- rep(NA,times = length(TIME))
		PAC[TIME == TIME1] <- PAC1  #Input with PAC1 when TIME = TIME1
		PAC[TIME == TIME2] <- PAC2  #Input with PAC2 when TIME = TIME2
		# Collate into a data frame
		input.data <- data.frame(TIME,  #Time sequence
														AMT = c(AMT,rep(0,times = length(TIME)-1)),  #AMT input at time = 0, then no further doses at subsequent times
														PAC,  #Patient's plasma acetaminophen concentrations (mg/L)
														WT,  #Patient's weight (kg)
														SDAC,  #Single-dose activated charcoal status (0 = No, 1 = Yes)
														PROD  #Product category ingested
		)
		input.data
	})  #Brackets closing "Rinput.data"

	# Use individual parameter estimates in Rbayes.data to simulate a concentration-time profile for the individual
	Rconc.data <- reactive({
		input.data <- Rinput.data()  #Read in reactive "input.data"
		bayes.data <- Rbayes.data()  #Read in reactive "bayes.data"
		# Update ERR_X values in model code - previously set to zero, but now we have individual parameter values for CL, V, KA and F
		parameter.list <- list(ERR_CL = bayes.data$ETA1,ERR_V = bayes.data$ETA2,ERR_KA = bayes.data$ETA3,ERR_F = bayes.data$ETA4)
		# Update the covariate values in model code - dependent on the individual input
		covariate.list <- list(PROD = input.data$PROD[1],WT = input.data$WT[1],SDAC = input.data$SDAC[1])
		# Update the omega values in model code - omega here are NOT between-subject variability but the precision of the parameters
		omega.list <- list(ETA_CL = 0,ETA_V = 0,ETA_KA = 0,ETA_F = 0)	#Simulating only the individual based on the Bayes estimates - no need for variability
		# Formally update the model parameters
		update.parameters <- mod %>% param(parameter.list) %>% param(covariate.list) %>% omat(dmat(omega.list))
		# Input dataset for differential equation solver
		input.conc.data <- expand.ev(ID = 1,amt = input.data$AMT[1])
		# Run differential equation solver
		conc.data <- update.parameters %>% data_set(input.conc.data,evid = 1,cmt = 1) %>% mrgsim(tgrid = TIME.tgrid)
		conc.data <- as.data.frame(conc.data)
	})  # Brackets closing "Rconc.data"

	############
	##_OUTPUT_##
	############


  #############
  ##_SESSION_##
  #############
  # Close the R session when Chrome closes
  session$onSessionEnded(function() {
    stopApp()
  })
})  #Brackets closing "shinyServer" function
