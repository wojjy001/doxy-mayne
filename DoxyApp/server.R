# server.R script for DoxyApp
# Reactive objects (i.e., those dependent on widget input) are written here
# ------------------------------------------------------------------------------
# Define the "server" part of the Shiny application
shinyServer(function(input,output,session) {
	###########
	##_INPUT_##
	###########
	# Create a reactive summary function for calculating the median and prediction intervals
	Rsummary.function <- reactive({
		# Specify percentile probabilities given input
		if (input$PI == 2) {
			input.CIlo <- 0.05	#5th percentile
			input.CIhi <- 0.95	#95th percentile
		}
		if (input$PI == 3) {
			input.CIlo <- 0.025	#2.5th percentile
			input.CIhi <- 0.975	#97.5th percentile
		}
		# Summary function for median and prediction intervals
		summary.function <- function(x) {
			median <- median(x)
			summary <- c("Median" = median)
			if (input$PI > 1) {
				CIlo <- quantile(x,probs = input.CIlo)
				CIhi <- quantile(x,probs = input.CIhi)
				summary <- c("Median" = median,CIlo,CIhi)
				names(summary)[c(2,3)] <- c("CIlo","CIhi")
				summary
			}
			summary
		}
	})	#Brackets closing "Rsummary.function"

	# Simulate a population of fed and fasted individuals administered Doryx MPC
	RdoryxMPC.data <- reactive({
		# Simulate concentration-time profiles for the population
		# Specify dosing input
		if (input$DOSE_DORYXMPC1 == 1) DOSE_DORYXMPC1 <- 120	#mg
		if (input$DOSE_DORYXMPC1 == 2) DOSE_DORYXMPC1 <- 240	#mg
		# Specify number of additional doses from input
		if (input$NUMDOSE_DORYXMPC1 == 1) NUMDOSE_DORYXMPC1 <- 0	#No additional doses
		if (input$NUMDOSE_DORYXMPC1 == 2) NUMDOSE_DORYXMPC1 <- 96/24-1	#Number of additional doses for 24-hourly dosing
		# Create input data frame for mrgsim
		input.doryxMPC.data <- expand.ev(ID = 1:n,	# n individuals
			amt = DOSE_DORYXMPC1*1000,	# amt in microg
			evid = 1,	# evid = 1; dosing event
			cmt = 1,	# cmt = 1; dose goes into compartment 1 = depot
			time = 0,	# time = 0; begin dosing at time = 0
			TRT = 1,	# Doryx MPC
			FED = c(0,1),	# Fasted and Fed status
			ii = 24,	# Dosing interval
			addl = NUMDOSE_DORYXMPC1)	#Number of additional doses
		doryxMPC.data <- mod %>% data_set(input.doryxMPC.data) %>% mrgsim(tgrid = TIME.tgrid,add = 96)
		doryxMPC.data <- as.data.frame(doryxMPC.data)	#Convert to a data frame so that it is more useful for me!
	})	#Brackets closing "RdoryxMPC.data"

	RdoryxMPC.summary <- reactive({
		# Read in the necessary reactive expressions
		doryxMPC.data <- RdoryxMPC.data()
		summary.function <- Rsummary.function()
		# Calculate the median and prediction intervals for calculations at each time-point
		doryxMPC.summary <- ddply(doryxMPC.data, .(time,FED), function(doryxMPC.data) summary.function(doryxMPC.data$IPRE))
	})	#Brackets closing "RdoryxMPC.summary"

	# Simulate a population of fed individuals administered Doryx MPClet
	RdoryxTAB.data <- reactive({
		# Simulate concentration-time profiles for the population
		if (input$ADD_DORYXTAB1 == TRUE) {
			# Specify dosing input
			if (input$DOSE_DORYXTAB1 == 1) DOSE_DORYXTAB1 <- 100	#mg
			if (input$DOSE_DORYXTAB1 == 2) DOSE_DORYXTAB1 <- 200	#mg
			# Specify number of doses and frequency from input
			if (input$NUMDOSE_DORYXTAB1 == 1) NUMDOSE_DORYXTAB1 <- 0	#No additional doses
			if (input$NUMDOSE_DORYXTAB1 == 2) NUMDOSE_DORYXTAB1 <- 96/24-1	#Number of additional doses for 24-hourly dosing
			# Create input data frame for mrgsim
			input.doryxTAB.data <- expand.ev(ID = 1:n,	# n individuals
				amt = DOSE_DORYXTAB1*1000,	# amt in microg
				evid = 1,	# evid = 1; dosing event
				cmt = 1,	# cmt = 1; dose goes into compartment 1 = depot
				time = 0,	# time = 0; begin dosing at time = 0
				TRT = 2,	# Doryx TAB
				FED = c(0,1),	# Fasted and Fed status
				ii = 24,	# Dosing interval
				addl = NUMDOSE_DORYXTAB1)	#Number of additional doses
			doryxTAB.data <- mod %>% data_set(input.doryxTAB.data) %>% mrgsim(tgrid = TIME.tgrid,add = 96)
			doryxTAB.data <- as.data.frame(doryxTAB.data)	#Convert to a data frame so that it is more useful for me!
		}
	})	#Brackets closing "RdoryxTAB.data"

	RdoryxTAB.summary <- reactive({
		# Read in the necessary reactive expressions
		doryxTAB.data <- RdoryxTAB.data()
		summary.function <- Rsummary.function()
		# Calculate the median and prediction intervals for calculations at each time-point
		doryxTAB.summary <- ddply(doryxTAB.data, .(time,FED), function(doryxTAB.data) summary.function(doryxTAB.data$IPRE))
	})	#Brackets closing "RdoryxTAB.summary"

	############
	##_OUTPUT_##
	############
	# Plot simulation results of fed versus fasted for Doryx MPC
	output$RdoryxMPC.plot <- renderPlot({
		# Read in the reactive data frame for summary
		doryxMPC.summary <- RdoryxMPC.summary()

		# Plot
		plotobj1 <- ggplot(doryxMPC.summary)
		# Fasted
			plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),data = doryxMPC.summary[doryxMPC.summary$FED == 0,],colour = "red")
			if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxMPC.summary[doryxMPC.summary$FED == 0,],fill = "red",alpha = 0.3)
		# Fed
			plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),data = doryxMPC.summary[doryxMPC.summary$FED == 1,],colour = "blue")
			if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxMPC.summary[doryxMPC.summary$FED == 1,],fill = "blue",alpha = 0.3)
		# Plot horizontal line representing LLOQ
		plotobj1 <- plotobj1 + geom_hline(aes(yintercept = 10),linetype = "dashed")
		plotobj1 <- plotobj1 + scale_x_continuous("\nTime (hours)")
		# Plot on linear or log-scale depending on input
		if (input$LOGS == FALSE) plotobj1 <- plotobj1 + scale_y_continuous("Doxycycline Concentration (microg/L)\n")
		if (input$LOGS == TRUE) plotobj1 <- plotobj1 + scale_y_log10("Doxycycline Concentration (microg/L)\n",breaks = c(10,100,1000),lim = c(1,NA))
		print(plotobj1)
	})	#Brackets closing "renderPlot"

	output$RdoryxMPC.table <- renderTable({
		# Read in the necessary reactive expressions
		doryxMPC.data <- RdoryxMPC.data()
		doryxMPC.data96 <- subset(doryxMPC.data,time == 96)
		summary.function <- Rsummary.function()
		# Only summarise AUC, Tmax and Cmax for single dose simulations
		# Summarise AUC at t = 96 hours
		if (input$NUMDOSE_DORYXMPC1 == 1) {
			AUC.table <- ddply(doryxMPC.data96, .(FED), function(doryxMPC.data96) summary.function(doryxMPC.data96$AUC))
			AUC.table$Variable <- "AUC (microg*h/L)"
			# Summarise Cmax (value will be found at time = 96)
			Cmax.table <- ddply(doryxMPC.data96, .(FED), function(doryxMPC.data96) summary.function(doryxMPC.data96$Cmax))
			Cmax.table$Variable <- "Cmax (microg/L)"
			# Summarise Tmax (value will be found at time = 96)
			Tmax.table <- ddply(doryxMPC.data96, .(FED), function(doryxMPC.data96) summary.function(doryxMPC.data96$Tmax))
			Tmax.table$Variable <- "Tmax (h)"
			doryxMPC.table <- rbind(AUC.table,Cmax.table,Tmax.table)
		}
		if (input$NUMDOSE_DORYXMPC1 == 2) {
			doryxMPC.table <- ""
		}
		doryxMPC.table
	})	#Brackets closing "renderText"

	# Plot simulation results of fed versus fasted for Doryx Tablet
	output$RdoryxTAB.plot <- renderPlot({
		if (input$ADD_DORYXTAB1 == TRUE) {
			# Read in the reactive data frame for fed.summary
			doryxTAB.summary <- RdoryxTAB.summary()

			# Plot
			plotobj1 <- ggplot(doryxTAB.summary)
			# Fasted
				plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),data = doryxTAB.summary[doryxTAB.summary$FED == 0,],colour = "red")
				if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxTAB.summary[doryxTAB.summary$FED == 0,],fill = "red",alpha = 0.3)
			# Fed
				plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),data = doryxTAB.summary[doryxTAB.summary$FED == 1,],colour = "blue")
				if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxTAB.summary[doryxTAB.summary$FED == 1,],fill = "blue",alpha = 0.3)
			# Plot horizontal line representing LLOQ
			plotobj1 <- plotobj1 + geom_hline(aes(yintercept = 10),linetype = "dashed")
			plotobj1 <- plotobj1 + scale_x_continuous("\nTime (hours)")
			# Plot on linear or log-scale depending on input
			if (input$LOGS == FALSE) plotobj1 <- plotobj1 + scale_y_continuous("Doxycycline Concentration (microg/L)\n")
			if (input$LOGS == TRUE) plotobj1 <- plotobj1 + scale_y_log10("Doxycycline Concentration (microg/L)\n",breaks = c(10,100,1000),lim = c(1,NA))
			print(plotobj1)
		}
	})	#Brackets closing "renderPlot"

	output$RdoryxTAB.table <- renderTable({
		# Read in the necessary reactive expressions
		doryxTAB.data <- RdoryxTAB.data()
		doryxTAB.data96 <- subset(doryxTAB.data,time == 96)
		summary.function <- Rsummary.function()
		# Only summarise AUC, Tmax and Cmax for single dose simulations
		# Summarise AUC at t = 96 hours
		if (input$NUMDOSE_DORYXTAB1 == 1) {
			AUC.table <- ddply(doryxTAB.data96, .(FED), function(doryxTAB.data96) summary.function(doryxTAB.data96$AUC))
			AUC.table$Variable <- "AUC (microg*h/L)"
			# Summarise Cmax (value will be found at time = 96)
			Cmax.table <- ddply(doryxTAB.data96, .(FED), function(doryxTAB.data96) summary.function(doryxTAB.data96$Cmax))
			Cmax.table$Variable <- "Cmax (microg/L)"
			# Summarise Tmax (value will be found at time = 96)
			Tmax.table <- ddply(doryxTAB.data96, .(FED), function(doryxTAB.data96) summary.function(doryxTAB.data96$Tmax))
			Tmax.table$Variable <- "Tmax (h)"
			doryxTAB.table <- rbind(AUC.table,Cmax.table,Tmax.table)
		}
		if (input$NUMDOSE_DORYXTAB1 == 2) {
			doryxTAB.table <- ""
		}
		doryxTAB.table
	})	#Brackets closing "renderText"

  #############
  ##_SESSION_##
  #############
  # Close the R session when Chrome closes
  session$onSessionEnded(function() {
    stopApp()
  })
})  #Brackets closing "shinyServer" function
