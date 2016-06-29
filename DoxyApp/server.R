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
			input.fed.data <- expand.ev(ID = 1:n,amt = 120*1000,evid = 1,cmt = 1,time = 0,
				FED = c(0,1),
				TRT = c(1,3,1,3))
				# n individuals
				# amt in microg
				# evid = 1; dosing event
				# cmt = 1; dose goes into compartment 1 = depot
				# time = 0; dose at time = 0
			fed.data <- mod %>% data_set(input.fed.data) %>% mrgsim(tgrid = TIME.tgrid)
			fed.data <- as.data.frame(fed.data)	#Convert to a data frame so that it is more useful for me!
			fed.data
	})	#Brackets closing "Rfed.data"

	############
	##_OUTPUT_##
	############
	# Plot simulation results of fed versus fasted
	output$Rfed.plot <- renderPlot({
		# Read in the reactive data frame for fed.data
		fed.data <- Rfed.data()

		# Plot
		plotobj1 <- ggplot(fed.data)
			# Fasted
			plotobj1 <- plotobj1 + stat_summary(aes(x = time,y = IPRE),data = fed.data[fed.data$FED == 0,],geom = "line",fun.y = median,colour = "red")	#Population typical individual
			# Plot 90% or 95% prediction intervals depending on what value was selected
			if (input$PI == 2) plotobj1 <- plotobj1 + stat_summary(aes(x = time,y = IPRE),data = fed.data[fed.data$FED == 0,],geom = "ribbon",fun.ymin = "CI90lo",fun.ymax = "CI90hi",fill = "red",alpha = 0.3)	#90% prediction intervals
			if (input$PI == 3) plotobj1 <- plotobj1 + stat_summary(aes(x = time,y = IPRE),data = fed.data[fed.data$FED == 0,],geom = "ribbon",fun.ymin = "CI95lo",fun.ymax = "CI95hi",fill = "red",alpha = 0.3)	#95% prediction intervals
			# Fed
			plotobj1 <- plotobj1 + stat_summary(aes(x = time,y = IPRE),data = fed.data[fed.data$FED == 1,],geom = "line",fun.y = median,colour = "blue")	#Population typical individual
			# Plot 90% or 95% prediction intervals depending on what value was selected
			if (input$PI == 2) plotobj1 <- plotobj1 + stat_summary(aes(x = time,y = IPRE),data = fed.data[fed.data$FED == 1,],geom = "ribbon",fun.ymin = "CI90lo",fun.ymax = "CI90hi",fill = "blue",alpha = 0.3)	#90% prediction intervals
			if (input$PI == 3) plotobj1 <- plotobj1 + stat_summary(aes(x = time,y = IPRE),data = fed.data[fed.data$FED == 1,],geom = "ribbon",fun.ymin = "CI95lo",fun.ymax = "CI95hi",fill = "blue",alpha = 0.3)	#95% prediction intervals
		# Plot horizontal line representing LLOQ
		plotobj1 <- plotobj1 + geom_hline(aes(yintercept = 10),linetype = "dashed")
		plotobj1 <- plotobj1 + scale_x_continuous("\nTime (hours)")
		# Plot on linear or log-scale depending on input
		if (input$LOGS == FALSE) plotobj1 <- plotobj1 + scale_y_continuous("Doxycycline Concentration (microg/L)\n")
		if (input$LOGS == TRUE) plotobj1 <- plotobj1 + scale_y_log10("Doxycycline Concentration (microg/L)\n",breaks = c(10,100,1000),lim = c(1,3000))
		# Facet for formulation (Doryx MPC or Doryx tablet) if "Stratify by secondary factor" is selected
	if (input$FACET == TRUE) plotobj1 <- plotobj1 + facet_wrap(~TRT)
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
