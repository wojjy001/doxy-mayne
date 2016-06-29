# server.R script for DoxyApp
# Reactive objects (i.e., those dependent on widget input) are written here
# ------------------------------------------------------------------------------
# Define the "server" part of the Shiny application
shinyServer(function(input,output,session) {
	###########
	##_INPUT_##
	###########
	# Simulate a population that will compare fed versus fasted status
	Rfed.data <- reactive({
		# Simulate concentration-time profiles for the population
			input.fed.data <- expand.ev(ID = 1:nsim,amt = 120*1000,evid = 1,cmt = 1,time = 0)
				# n individuals
				# amt in microg
				# evid = 1; dosing event
				# cmt = 1; dose goes into compartment 1 = depot
				# time = 0; dose at time = 0
			fed.data <- mod %>% data_set(input.fed.data) %>% mrgsim(tgrid = TIME.tgrid)
			fed.data <- as.data.frame(fed.data)	#Convert to a data frame so that it is more useful for me!
	})	#Brackets closing "Rfed.data"

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
	# Plot simulation results of fed versus fasted
	output$Rfed.plot <- renderPlot({
		# Read in the reactive data frame for fed.data
		fed.data <- Rfed.data()

		# Plot
		plotobj1 <- ggplot()
		plotobj1 <- plotobj1 + geom_line(aes(x = time,y = IPRE),data = fed.data[fed.data$ID == 1,],colour = "red")	#Population typical individual
		# Plot 90% or 95% prediction intervals depending on what value was selected
		if (input$PI == 2) plotobj1 <- plotobj1 + stat_summary(aes(x = time,y = IPRE),data = fed.data[fed.data$ID != 1,],geom = "ribbon",fun.ymin = "CI90lo",fun.ymax = "CI90hi",fill = "red",alpha = 0.3)	#90% prediction intervals
		if (input$PI == 3) plotobj1 <- plotobj1 + stat_summary(aes(x = time,y = IPRE),data = fed.data[fed.data$ID != 1,],geom = "ribbon",fun.ymin = "CI95lo",fun.ymax = "CI95hi",fill = "red",alpha = 0.3)	#95% prediction intervals
		# Plot horizontal line representing LLOQ
		plotobj1 <- plotobj1 + geom_hline(aes(yintercept = 10),linetype = "dashed")
		plotobj1 <- plotobj1 + scale_x_continuous("\nTime (hours)")
		# Plot on linear or log-scale depending on input
		if (input$LOGS == FALSE) plotobj1 <- plotobj1 + scale_y_continuous("Doxycycline Concentration (microg/L)\n")
		if (input$LOGS == TRUE) plotobj1 <- plotobj1 + scale_y_log10("Doxycycline Concentration (microg/L)\n",breaks = c(10,100,1000))
		print(plotobj1)

	})	#Brackets closing "renderPlot"

  #############
  ##_SESSION_##
  #############
  # Close the R session when Chrome closes
  session$onSessionEnded(function() {
    stopApp()
  })
})  #Brackets closing "shinyServer" function
