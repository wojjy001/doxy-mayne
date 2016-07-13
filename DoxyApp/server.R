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

	# Simulate a population of fed/ fasted individuals administered Doryx MPC
	#------------------------------------------------------------------------
	RdoryxMPC.data <- reactive({
		# Simulate concentration-time profiles for the population
		# Specify dosing input
	  if (input$DOSE1 != 3) {
  		if (input$DOSE1 == 1) DOSE_DORYXMPC1 <- 120	#mg
  		if (input$DOSE1 == 2) DOSE_DORYXMPC1 <- 240	#mg
  		# Create input data frame for mrgsim
  		input.doryxMPC.data <- data.frame(
  			ID = 1:n,	# n individuals
  			amt = DOSE_DORYXMPC1*1000,	# amt in microg
  			evid = 1,	# evid = 1; dosing event
  			cmt = 1,	# cmt = 1; dose goes into compartment 1 = depot
  			time = 0,	# time = 0; begin dosing at time = 0
  			TRT = 1,	# Doryx MPC
  			FED = rbinom(n,size = 1,prob = 0.5), # Simulations comparing Fasted and Fed status
  			SEX = rbinom(n,size = 1,prob = 0.5),
  			FFM = rlnorm(n,meanlog = log(55.49),sd = 0.09)
  		)
	  }
		if (input$DOSE1 == 3) {
			dose.times <- c(0,24,48,72,96,120,144)
		  # Create input data frame for mrgsim
		  input.doryxMPC.data <- data.frame(
		    ID = 1:n,	# n individuals
				time = 0,
		    amt = 240*1000,	# amt in microg
		    evid = 1,	# evid = 1; dosing event
		    cmt = 1,	# cmt = 1; dose goes into compartment 1 = depot
		    TRT = 1,	# Doryx MPC
		    FED = rbinom(n,size = 1,prob = 0.5), # Simulations comparing Fasted and Fed status
		    SEX = rbinom(n,size = 1,prob = 0.5),
		    FFM = rlnorm(n,meanlog = log(55.49),sd = 0.09)
		  )
			input.doryxMPC.data <- lapply(input.doryxMPC.data,rep.int,times = length(time.multiple))
			input.doryxMPC.data <- as.data.frame(input.doryxMPC.data)
			input.doryxMPC.data <- input.doryxMPC.data[with(input.doryxMPC.data, order(input.doryxMPC.data$ID)),]
			input.doryxMPC.data$time <- time.multiple
			input.doryxMPC.data$evid[!c(input.doryxMPC.data$time %in% dose.times)] <- 0
			input.doryxMPC.data$amt[input.doryxMPC.data$time > 0] <- 120*1000
			input.doryxMPC.data
		}
		doryxMPC.data <- mod %>% data_set(input.doryxMPC.data) %>% mrgsim()
		doryxMPC.data <- as.data.frame(doryxMPC.data)	#Convert to a data frame so that it is more useful for me!
	})	#Brackets closing "RdoryxMPC.data"

	RdoryxMPC.summary <- reactive({
		# Read in the necessary reactive expressions
		doryxMPC.data <- RdoryxMPC.data()
		summary.function <- Rsummary.function()
		# Calculate the median and prediction intervals for calculations at each time-point
		doryxMPC.summary <- ddply(doryxMPC.data, .(time,FED), function(doryxMPC.data) summary.function(doryxMPC.data$IPRE))
	})	#Brackets closing "RdoryxMPC.summary"

	# Simulate a population of fed/ fasted individuals administered Doryx Tablet
	#---------------------------------------------------------------------------
	RdoryxTAB.data <- reactive({
  # Simulate concentration-time profiles for the population
  	# Specify dosing input
	  if (input$DOSE1 != 3) {	  
    	if (input$DOSE1 == 1) DOSE_DORYXTAB1 <- 100	#mg
    	if (input$DOSE1 == 2) DOSE_DORYXTAB1 <- 200	#mg
    	# Create input data frame for mrgsim
    	input.doryxTAB.data <- data.frame(
    	  ID = 1:n,	# n individuals
    		amt = DOSE_DORYXTAB1*1000,	# amt in microg
    		evid = 1,	# evid = 1; dosing event
    		cmt = 1,	# cmt = 1; dose goes into compartment 1 = depot
    		time = 0,	# time = 0; begin dosing at time = 0
    		TRT = 2,	# Doryx TAB
    		FED = rbinom(n,size = 1,prob = 0.5), # Simulations comparing Fasted and Fed status
    		SEX = rbinom(n,size = 1,prob = 0.5),
    		FFM = rlnorm(n,meanlog = log(55.49),sd = 0.09),
    		ii = 24	# Dosing interval
    		)
	  }
	  if (input$DOSE1 == 3) {
	    dose.times <- c(0,24,48,72,96,120,144)
	    # Create input data frame for mrgsim
	    input.doryxTAB.data <- data.frame(
	      ID = 1:n,	# n individuals
	      time = 0,
	      amt = 200*1000,	# amt in microg
	      evid = 1,	# evid = 1; dosing event
	      cmt = 1,	# cmt = 1; dose goes into compartment 1 = depot
	      TRT = 2,	# Doryx MPC
	      FED = rbinom(n,size = 1,prob = 0.5), # Simulations comparing Fasted and Fed status
	      SEX = rbinom(n,size = 1,prob = 0.5),
	      FFM = rlnorm(n,meanlog = log(55.49),sd = 0.09)
	    )
	    input.doryxTAB.data <- lapply(input.doryxTAB.data,rep.int,times = length(time.multiple))
	    input.doryxTAB.data <- as.data.frame(input.doryxTAB.data)
	    input.doryxTAB.data <- input.doryxTAB.data[with(input.doryxTAB.data, order(input.doryxTAB.data$ID)),]
	    input.doryxTAB.data$time <- time.multiple
	    input.doryxTAB.data$evid[!c(input.doryxTAB.data$time %in% dose.times)] <- 0
	    input.doryxTAB.data$amt[input.doryxTAB.data$time > 0] <- 100*1000
	    input.doryxTAB.data
	  }
  	doryxTAB.data <- mod %>% data_set(input.doryxTAB.data) %>% mrgsim()
  	doryxTAB.data <- as.data.frame(doryxTAB.data)	#Convert to a data frame so that it is more useful for me!
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
			doryxTAB.table <- NA
		}
		doryxTAB.table
	})	#Brackets closing "renderText"
<<<<<<< HEAD
	
	# Plot simulation results of Doryx MPC versus Doryx for Fasted
	output$RformFasted.plot <- renderPlot({
	  # Read in the reactive data frames for summary
	  doryxMPC.summary <- RdoryxMPC.summary()
	  doryxTAB.summary <- RdoryxTAB.summary()
	  
	  # Plot
	  plotobj1 <- ggplot(doryxMPC.summary)
	  # MPC
	  plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),data = doryxMPC.summary[doryxMPC.summary$FED == 0,],colour = "red")
	  if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxMPC.summary[doryxMPC.summary$FED == 0,],fill = "red",alpha = 0.3)
	  # Doryx
	  plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),data = doryxTAB.summary[doryxTAB.summary$FED == 0,],colour = "blue")
	  if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxTAB.summary[doryxTAB.summary$FED == 0,],fill = "blue",alpha = 0.3)
	  # Plot horizontal line representing LLOQ
	  plotobj1 <- plotobj1 + geom_hline(aes(yintercept = 10),linetype = "dashed")
	  plotobj1 <- plotobj1 + scale_x_continuous("\nTime (hours)")
	  # Plot on linear or log-scale depending on input
	  if (input$LOGS == FALSE) plotobj1 <- plotobj1 + scale_y_continuous("Doxycycline Concentration (microg/L)\n")
	  if (input$LOGS == TRUE) plotobj1 <- plotobj1 + scale_y_log10("Doxycycline Concentration (microg/L)\n",breaks = c(10,100,1000),lim = c(1,NA))
	  print(plotobj1)
	})	#Brackets closing "renderPlot"	
	
	# Plot simulation results of Doryx MPC versus Doryx for Fed
	output$RformFed.plot <- renderPlot({
	  # Read in the reactive data frames for summary
	  doryxMPC.summary <- RdoryxMPC.summary()
	  doryxTAB.summary <- RdoryxTAB.summary()
	  
	  # Plot
	  plotobj1 <- ggplot(doryxMPC.summary)
	  # MPC
	  plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),data = doryxMPC.summary[doryxMPC.summary$FED == 1,],colour = "red")
	  if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxMPC.summary[doryxMPC.summary$FED == 1,],fill = "red",alpha = 0.3)
	  # Doryx
	  plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),data = doryxTAB.summary[doryxTAB.summary$FED == 1,],colour = "blue")
	  if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxTAB.summary[doryxTAB.summary$FED == 1,],fill = "blue",alpha = 0.3)
	  # Plot horizontal line representing LLOQ
	  plotobj1 <- plotobj1 + geom_hline(aes(yintercept = 10),linetype = "dashed")
	  plotobj1 <- plotobj1 + scale_x_continuous("\nTime (hours)")
	  # Plot on linear or log-scale depending on input
	  if (input$LOGS == FALSE) plotobj1 <- plotobj1 + scale_y_continuous("Doxycycline Concentration (microg/L)\n")
	  if (input$LOGS == TRUE) plotobj1 <- plotobj1 + scale_y_log10("Doxycycline Concentration (microg/L)\n",breaks = c(10,100,1000),lim = c(1,NA))
	  print(plotobj1)
	})	#Brackets closing "renderPlot"	
	
	
=======
>>>>>>> parent of 5070e57... Add RformFasted.plot to server

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
			doryxTAB.table <- NA
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
