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

	################
	##_FED_STATUS_##
	################
	# Simulate a population of fed/fasted individuals administered Doryx MPC
		RdoryxMPC.data1 <- reactive({
			# Simulate concentration-time profiles for the population
			# Specify dosing input
			  if (input$DOSE1 == 1) {	# Single dose scenario : A single 120 mg MPC dose
			    DOSE_DORYXMPC1 <- 120	#mg
		  		# Create input data frame for mrgsim
		  		input.doryxMPC.data1 <- data.frame(
		  			ID = 1:n,	# n individuals
		  			amt = DOSE_DORYXMPC1*1000,	# amt in microg
		  			evid = 1,	# evid = 1; dosing event
		  			cmt = 1,	# cmt = 1; dose goes into compartment 1 = depot
		  			time = 0,	# time = 0; begin dosing at time = 0
		  			TRT = 1,	# Doryx MPC
		  			FED = rbinom(n,size = 1,prob = 0.5),	# Randomly assign fed or fasted status
		  			SEX = rbinom(n,size = 1,prob = 0.5),	# Randomly assign male or female status
		  			FFM = rlnorm(n,meanlog = log(55.49),sd = 0.09)	# Randomly generate value for fat free mass (kg)
		  		)
			  }
		  
				if (input$DOSE1 != 1) {	# Multiple dose scenario 
					# Specify the dosing times for the clinical scenario
				  if (input$DOSE1 == 2) dose.times <- c(0,12,24,48,72,96,120,144) #120 mg every 12 hours on the first day, followed by six 120 mg doses at 24 hour intervals
				  if (input$DOSE1 == 3) dose.times <- dose.times <- seq(from = 0,to = 156,by = 12) #120 mg every 12 hours for 7 days
				  # Create input data frame for mrgsim
					  input.doryxMPC.data1 <- data.frame(
					    ID = 1:n,	# n individuals
							time = 0,	# Begin dosing at time = 0
					    amt = 120*1000,	# amt in microg
					    evid = 1,	# evid = 1; dosing event
					    cmt = 1,	# cmt = 1; dose goes into compartment 1 = depot
					    TRT = 1,	# Doryx MPC
							FED = rbinom(n,size = 1,prob = 0.5),	# Randomly assign fed or fasted status
			  			SEX = rbinom(n,size = 1,prob = 0.5),	# Randomly assign male or female status
			  			FFM = rlnorm(n,meanlog = log(55.49),sd = 0.09)	# Randomly generate value for fat free mass (kg)
					  )
					# Multiple input.doryxMPC.data by the length of sample times
						input.doryxMPC.data1 <- lapply(input.doryxMPC.data1,rep.int,times = length(time.multiple))
						input.doryxMPC.data1 <- as.data.frame(input.doryxMPC.data1)	# Convert to a data frame
						input.doryxMPC.data1 <- input.doryxMPC.data1[with(input.doryxMPC.data1, order(input.doryxMPC.data1$ID)),]	# Sort by ID
					# Add a time column
						input.doryxMPC.data1$time <- time.multiple
					# For times that aren't dosing times, make evid = 0
						input.doryxMPC.data1$evid[!c(input.doryxMPC.data1$time %in% dose.times)] <- 0
					# Return the resulting data frame
						input.doryxMPC.data1
				}
		  # Simulate
				doryxMPC.data1 <- mod %>% data_set(input.doryxMPC.data1) %>% mrgsim(tgrid = TIME.tgrid)
				doryxMPC.data1 <- as.data.frame(doryxMPC.data1)	#Convert to a data frame so that it is more useful for me!
		
				})	#Brackets closing "RdoryxMPC.data1"

	RdoryxMPC.summary1 <- reactive({
		# Read in the necessary reactive expressions
			doryxMPC.data1 <- RdoryxMPC.data1()
			summary.function <- Rsummary.function()
		# Calculate the median and prediction intervals for calculations at each time-point
			doryxMPC.summary1 <- ddply(doryxMPC.data1, .(time,FED), function(doryxMPC.data1) summary.function(doryxMPC.data1$IPRE))
	})	# Brackets closing "RdoryxMPC.summary1"

	# Simulate a population of fed/fasted individuals administered Doryx Tablet
		RdoryxTAB.data1 <- reactive({
	  # Simulate concentration-time profiles for the population
	  	# Specify dosing input
		  if (input$DOSE1 == 1) {	# Single dose scenario: A single 100 mg dose of Doryx Tablet
		  		DOSE_DORYXTAB1 <- 100	#mg
		  		# Create input data frame for mrgsim
			  		input.doryxTAB.data1 <- data.frame(
			  			ID = 1:n,	# n individuals
			  			amt = DOSE_DORYXTAB1*1000,	# amt in microg
			  			evid = 1,	# evid = 1; dosing event
			  			cmt = 1,	# cmt = 1; dose goes into compartment 1 = depot
			  			time = 0,	# time = 0; begin dosing at time = 0
			  			TRT = 2,	# Doryx tablet
							FED = rbinom(n,size = 1,prob = 0.5),	# Randomly assign fed or fasted status
			  			SEX = rbinom(n,size = 1,prob = 0.5),	# Randomly assign male or female status
			  			FFM = rlnorm(n,meanlog = log(55.49),sd = 0.09)	# Randomly generate value for fat free mass (kg)
			  		)
			  }
				if (input$DOSE1 != 1) {	# Multiple dose scenario
					# Specify the dosing times for the clinical scenario
				  if (input$DOSE1 == 2) dose.times <- c(0,12,24,48,72,96,120,144) #Doryx TAB: 100 mg every 12 hours on the first day, followed by six 100 mg doses at 24 hour intervals
				  if (input$DOSE1 == 3) dose.times <- seq(from = 0,to = 156,by = 12) #Doryx TAB: 100 mg every 12 hours for 7 days
				  # Create input data frame for mrgsim
					  input.doryxTAB.data1 <- data.frame(
					    ID = 1:n,	# n individuals
							time = 0,
					    amt = 100*1000,	# amt in microg
					    evid = 1,	# evid = 1; dosing event
					    cmt = 1,	# cmt = 1; dose goes into compartment 1 = depot
					    TRT = 2,	# Doryx tablet
							FED = rbinom(n,size = 1,prob = 0.5),	# Randomly assign fed or fasted status
			  			SEX = rbinom(n,size = 1,prob = 0.5),	# Randomly assign male or female status
			  			FFM = rlnorm(n,meanlog = log(55.49),sd = 0.09)	# Randomly generate value for fat free mass (kg)
					  )
					# Multiple input.doryxMPC.data1 by the length of sample times
						input.doryxTAB.data1 <- lapply(input.doryxTAB.data1,rep.int,times = length(time.multiple))
						input.doryxTAB.data1 <- as.data.frame(input.doryxTAB.data1)	# Convert to a data frame
						input.doryxTAB.data1 <- input.doryxTAB.data1[with(input.doryxTAB.data1, order(input.doryxTAB.data1$ID)),]	# Sort by ID
					# Add a time column
						input.doryxTAB.data1$time <- time.multiple
					# For times that aren't dosing times, make evid = 0
						input.doryxTAB.data1$evid[!c(input.doryxTAB.data1$time %in% dose.times)] <- 0
					# Return the resulting data1 frame
						input.doryxTAB.data1
				}
			# Simulate
		  	doryxTAB.data1 <- mod %>% data_set(input.doryxTAB.data1) %>% mrgsim(tgrid = TIME.tgrid)
		  	doryxTAB.data1 <- as.data.frame(doryxTAB.data1)	#Convert to a data frame so that it is more useful for me!
		})	#Brackets closing "RdoryxTAB.data1"

	RdoryxTAB.summary1 <- reactive({
		# Read in the necessary reactive expressions
			doryxTAB.data1 <- RdoryxTAB.data1()
			summary.function <- Rsummary.function()
		# Calculate the median and prediction intervals for calculations at each time-point
			doryxTAB.summary1 <- ddply(doryxTAB.data1, .(time,FED), function(doryxTAB.data1) summary.function(doryxTAB.data1$IPRE))
	})	#Brackets closing "RdoryxTAB.summary1"

	# #################
	# ##_FORM_STATUS_##
	# #################

	# Simulate a population of fed/fasted individuals administered Doryx MPC
	RdoryxMPC.data2 <- reactive({
	  # Simulate concentration-time profiles for the population
	  # Specify dosing input
	  if (input$DOSE2 == 1) {	# Single dose scenario : A single 120 mg MPC dose
	    DOSE_DORYXMPC2 <- 120	#mg
	    # Create input data frame for mrgsim
	    input.doryxMPC.data2 <- data.frame(
	      ID = 1:n,	# n individuals
	      amt = DOSE_DORYXMPC2*1000,	# amt in microg
	      evid = 1,	# evid = 1; dosing event
	      cmt = 1,	# cmt = 1; dose goes into compartment 1 = depot
	      time = 0,	# time = 0; begin dosing at time = 0
	      TRT = 1,	# Doryx MPC
	      FED = rbinom(n,size = 1,prob = 0.5),	# Randomly assign fed or fasted status
	      SEX = rbinom(n,size = 1,prob = 0.5),	# Randomly assign male or female status
	      FFM = rlnorm(n,meanlog = log(55.49),sd = 0.09)	# Randomly generate value for fat free mass (kg)
	    )
	  }
	  
	  if (input$DOSE2 != 1) {	# Multiple dose scenario 
	    # Specify the dosing times for the clinical scenario
	    if (input$DOSE2 == 2) dose.times <- c(0,12,24,48,72,96,120,144) #120 mg every 12 hours on the first day, followed by six 120 mg doses at 24 hour intervals
	    if (input$DOSE2 == 3) dose.times <- dose.times <- seq(from = 0,to = 156,by = 12) #120 mg every 12 hours for 7 days
	    # Create input data frame for mrgsim
	    input.doryxMPC.data2 <- data.frame(
	      ID = 1:n,	# n individuals
	      time = 0,	# Begin dosing at time = 0
	      amt = 120*1000,	# amt in microg
	      evid = 1,	# evid = 1; dosing event
	      cmt = 1,	# cmt = 1; dose goes into compartment 1 = depot
	      TRT = 1,	# Doryx MPC
	      FED = rbinom(n,size = 1,prob = 0.5),	# Randomly assign fed or fasted status
	      SEX = rbinom(n,size = 1,prob = 0.5),	# Randomly assign male or female status
	      FFM = rlnorm(n,meanlog = log(55.49),sd = 0.09)	# Randomly generate value for fat free mass (kg)
	    )
	    # Multiple input.doryxMPC.data by the length of sample times
	    input.doryxMPC.data2 <- lapply(input.doryxMPC.data2,rep.int,times = length(time.multiple))
	    input.doryxMPC.data2 <- as.data.frame(input.doryxMPC.data2)	# Convert to a data frame
	    input.doryxMPC.data2 <- input.doryxMPC.data2[with(input.doryxMPC.data2, order(input.doryxMPC.data2$ID)),]	# Sort by ID
	    # Add a time column
	    input.doryxMPC.data2$time <- time.multiple
	    # For times that aren't dosing times, make evid = 0
	    input.doryxMPC.data2$evid[!c(input.doryxMPC.data2$time %in% dose.times)] <- 0
	    # Return the resulting data frame
	    input.doryxMPC.data2
	  }
	  # Simulate
	  doryxMPC.data2 <- mod %>% data_set(input.doryxMPC.data2) %>% mrgsim(tgrid = TIME.tgrid)
	  doryxMPC.data2 <- as.data.frame(doryxMPC.data2)	#Convert to a data frame so that it is more useful for me!
	  
	})	#Brackets closing "RdoryxMPC.data2"
	
	RdoryxMPC.summary2 <- reactive({
	  # Read in the necessary reactive expressions
	  doryxMPC.data2 <- RdoryxMPC.data2()
	  summary.function <- Rsummary.function()
	  # Calculate the median and prediction intervals for calculations at each time-point
	  doryxMPC.summary2 <- ddply(doryxMPC.data2, .(time,FED), function(doryxMPC.data2) summary.function(doryxMPC.data2$IPRE))
	})	# Brackets closing "RdoryxMPC.summary2"
	
	# Simulate a population of fed/fasted individuals administered Doryx Tablet
	RdoryxTAB.data2 <- reactive({
	  # Simulate concentration-time profiles for the population
	  # Specify dosing input
	  if (input$DOSE2 == 1) {	# Single dose scenario: A single 100 mg dose of Doryx Tablet
	    DOSE_DORYXTAB2 <- 100	#mg
	    # Create input data frame for mrgsim
	    input.doryxTAB.data2 <- data.frame(
	      ID = 1:n,	# n individuals
	      amt = DOSE_DORYXTAB2*1000,	# amt in microg
	      evid = 1,	# evid = 1; dosing event
	      cmt = 1,	# cmt = 1; dose goes into compartment 1 = depot
	      time = 0,	# time = 0; begin dosing at time = 0
	      TRT = 2,	# Doryx tablet
	      FED = rbinom(n,size = 1,prob = 0.5),	# Randomly assign fed or fasted status
	      SEX = rbinom(n,size = 1,prob = 0.5),	# Randomly assign male or female status
	      FFM = rlnorm(n,meanlog = log(55.49),sd = 0.09)	# Randomly generate value for fat free mass (kg)
	    )
	  }
	  if (input$DOSE2 != 1) {	# Multiple dose scenario
	    # Specify the dosing times for the clinical scenario
	    if (input$DOSE2 == 2) dose.times <- c(0,12,24,48,72,96,120,144) #Doryx TAB: 100 mg every 12 hours on the first day, followed by six 100 mg doses at 24 hour intervals
	    if (input$DOSE2 == 3) dose.times <- seq(from = 0,to = 156,by = 12) #Doryx TAB: 100 mg every 12 hours for 7 days
	    # Create input data frame for mrgsim
	    input.doryxTAB.data2 <- data.frame(
	      ID = 1:n,	# n individuals
	      time = 0,
	      amt = 100*1000,	# amt in microg
	      evid = 1,	# evid = 1; dosing event
	      cmt = 1,	# cmt = 1; dose goes into compartment 1 = depot
	      TRT = 2,	# Doryx tablet
	      FED = rbinom(n,size = 1,prob = 0.5),	# Randomly assign fed or fasted status
	      SEX = rbinom(n,size = 1,prob = 0.5),	# Randomly assign male or female status
	      FFM = rlnorm(n,meanlog = log(55.49),sd = 0.09)	# Randomly generate value for fat free mass (kg)
	    )
	    # Multiple input.doryxMPC.data2 by the length of sample times
	    input.doryxTAB.data2 <- lapply(input.doryxTAB.data2,rep.int,times = length(time.multiple))
	    input.doryxTAB.data2 <- as.data.frame(input.doryxTAB.data2)	# Convert to a data frame
	    input.doryxTAB.data2 <- input.doryxTAB.data2[with(input.doryxTAB.data2, order(input.doryxTAB.data2$ID)),]	# Sort by ID
	    # Add a time column
	    input.doryxTAB.data2$time <- time.multiple
	    # For times that aren't dosing times, make evid = 0
	    input.doryxTAB.data2$evid[!c(input.doryxTAB.data2$time %in% dose.times)] <- 0
	    # Return the resulting data2 frame
	    input.doryxTAB.data2
	  }
	  # Simulate
	  doryxTAB.data2 <- mod %>% data_set(input.doryxTAB.data2) %>% mrgsim(tgrid = TIME.tgrid)
	  doryxTAB.data2 <- as.data.frame(doryxTAB.data2)	#Convert to a data frame so that it is more useful for me!
	})	#Brackets closing "RdoryxTAB.data2"
	
	RdoryxTAB.summary2 <- reactive({
	  # Read in the necessary reactive expressions
	  doryxTAB.data2 <- RdoryxTAB.data2()
	  summary.function <- Rsummary.function()
	  # Calculate the median and prediction intervals for calculations at each time-point
	  doryxTAB.summary2 <- ddply(doryxTAB.data2, .(time,FED), function(doryxTAB.data2) summary.function(doryxTAB.data2$IPRE))
	})	#Brackets closing "RdoryxTAB.summary2"

	################
	##_SEX_STATUS_##
	################
	# Simulate a population of male/female individuals administered Doryx MPC (fasted)
	RdoryxMPC.data3 <- reactive({
	  # Simulate concentration-time profiles for the population
	  # Specify dosing input
	  if (input$DOSE3 == 1) {	# Single dose scenario : A single 120 mg MPC dose (fasted)
	    DOSE_DORYXMPC3 <- 120	#mg
	    # Create input data frame for mrgsim
	    input.doryxMPC.data3 <- data.frame(
	      ID = 1:n,	# n individuals
	      amt = DOSE_DORYXMPC3*1000,	# amt in microg
	      evid = 1,	# evid = 1; dosing event
	      cmt = 1,	# cmt = 1; dose goes into compartment 1 = depot
	      time = 0,	# time = 0; begin dosing at time = 0
	      TRT = 1,	# Doryx MPC
	      FED = 0,	# Fasted status
	      SEX = rbinom(n,size = 1,prob = 0.5),	# Randomly assign male or female status
	      FFM = rlnorm(n,meanlog = log(55.49),sd = 0.09)	# Randomly generate value for fat free mass (kg)
	    )
	  }
	  
	  if (input$DOSE3 != 1) {	# Multiple dose scenario 
	    # Specify the dosing times for the clinical scenario
	    if (input$DOSE3 == 2) dose.times <- c(0,12,24,48,72,96,120,144) #120 mg every 12 hours on the first day, followed by six 120 mg doses at 24 hour intervals (fasted)
	    if (input$DOSE3 == 3) dose.times <- dose.times <- seq(from = 0,to = 156,by = 12) #120 mg every 12 hours for 7 days (fasted)
	    # Create input data frame for mrgsim
	    input.doryxMPC.data3 <- data.frame(
	      ID = 1:n,	# n individuals
	      time = 0,	# Begin dosing at time = 0
	      amt = 120*1000,	# amt in microg
	      evid = 1,	# evid = 1; dosing event
	      cmt = 1,	# cmt = 1; dose goes into compartment 1 = depot
	      TRT = 1,	# Doryx MPC
	      FED = 0,	# Randomly assign fed or fasted status
	      SEX = rbinom(n,size = 1,prob = 0.5),	# Randomly assign male or female status
	      FFM = rlnorm(n,meanlog = log(55.49),sd = 0.09)	# Randomly generate value for fat free mass (kg)
	    )
	    # Multiple input.doryxMPC.data by the length of sample times
	    input.doryxMPC.data3 <- lapply(input.doryxMPC.data3,rep.int,times = length(time.multiple))
	    input.doryxMPC.data3 <- as.data.frame(input.doryxMPC.data3)	# Convert to a data frame
	    input.doryxMPC.data3 <- input.doryxMPC.data3[with(input.doryxMPC.data3, order(input.doryxMPC.data3$ID)),]	# Sort by ID
	    # Add a time column
	    input.doryxMPC.data3$time <- time.multiple
	    # For times that aren't dosing times, make evid = 0
	    input.doryxMPC.data3$evid[!c(input.doryxMPC.data3$time %in% dose.times)] <- 0
	    # Return the resulting data frame
	    input.doryxMPC.data3
	  }
	  # Simulate
	  doryxMPC.data3 <- mod %>% data_set(input.doryxMPC.data3) %>% mrgsim(tgrid = TIME.tgrid)
	  doryxMPC.data3 <- as.data.frame(doryxMPC.data3)	#Convert to a data frame so that it is more useful for me!
	  
	})	#Brackets closing "RdoryxMPC.data3"
	
	RdoryxMPC.summary3 <- reactive({
	  # Read in the necessary reactive expressions
	  doryxMPC.data3 <- RdoryxMPC.data3()
	  summary.function <- Rsummary.function()
	  # Calculate the median and prediction intervals for calculations at each time-point
	  doryxMPC.summary3 <- ddply(doryxMPC.data3, .(time,SEX), function(doryxMPC.data3) summary.function(doryxMPC.data3$IPRE))
	})	# Brackets closing "RdoryxMPC.summary3"
	
	# Simulate a population of male/female individuals administered Doryx Tablet (fasted)
	RdoryxTAB.data3 <- reactive({
	  # Simulate concentration-time profiles for the population
	  # Specify dosing input
	  if (input$DOSE3 == 1) {	# Single dose scenario: A single 100 mg dose of Doryx Tablet
	    DOSE_DORYXTAB3 <- 100	#mg
	    # Create input data frame for mrgsim
	    input.doryxTAB.data3 <- data.frame(
	      ID = 1:n,	# n individuals
	      amt = DOSE_DORYXTAB3*1000,	# amt in microg
	      evid = 1,	# evid = 1; dosing event
	      cmt = 1,	# cmt = 1; dose goes into compartment 1 = depot
	      time = 0,	# time = 0; begin dosing at time = 0
	      TRT = 2,	# Doryx tablet
	      FED = 0,	# Fasted status
	      SEX = rbinom(n,size = 1,prob = 0.5),	# Randomly assign male or female status
	      FFM = rlnorm(n,meanlog = log(55.49),sd = 0.09)	# Randomly generate value for fat free mass (kg)
	    )
	  }
	  if (input$DOSE3 != 1) {	# Multiple dose scenario
	    # Specify the dosing times for the clinical scenario
	    if (input$DOSE3 == 2) dose.times <- c(0,12,24,48,72,96,120,144) #Doryx TAB: 100 mg every 12 hours on the first day, followed by six 100 mg doses at 24 hour intervals
	    if (input$DOSE3 == 3) dose.times <- seq(from = 0,to = 156,by = 12) #Doryx TAB: 100 mg every 12 hours for 7 days
	    # Create input data frame for mrgsim
	    input.doryxTAB.data3 <- data.frame(
	      ID = 1:n,	# n individuals
	      time = 0,
	      amt = 100*1000,	# amt in microg
	      evid = 1,	# evid = 1; dosing event
	      cmt = 1,	# cmt = 1; dose goes into compartment 1 = depot
	      TRT = 2,	# Doryx tablet
	      FED = 0,	# Fasted status
	      SEX = rbinom(n,size = 1,prob = 0.5),	# Randomly assign male or female status
	      FFM = rlnorm(n,meanlog = log(55.49),sd = 0.09)	# Randomly generate value for fat free mass (kg)
	    )
	    # Multiple input.doryxMPC.data3 by the length of sample times
	    input.doryxTAB.data3 <- lapply(input.doryxTAB.data3,rep.int,times = length(time.multiple))
	    input.doryxTAB.data3 <- as.data.frame(input.doryxTAB.data3)	# Convert to a data frame
	    input.doryxTAB.data3 <- input.doryxTAB.data3[with(input.doryxTAB.data3, order(input.doryxTAB.data3$ID)),]	# Sort by ID
	    # Add a time column
	    input.doryxTAB.data3$time <- time.multiple
	    # For times that aren't dosing times, make evid = 0
	    input.doryxTAB.data3$evid[!c(input.doryxTAB.data3$time %in% dose.times)] <- 0
	    # Return the resulting data3 frame
	    input.doryxTAB.data3
	  }
	  # Simulate
	  doryxTAB.data3 <- mod %>% data_set(input.doryxTAB.data3) %>% mrgsim(tgrid = TIME.tgrid)
	  doryxTAB.data3 <- as.data.frame(doryxTAB.data3)	#Convert to a data frame so that it is more useful for me!
	})	#Brackets closing "RdoryxTAB.data3"
	
	RdoryxTAB.summary3 <- reactive({
	  # Read in the necessary reactive expressions
	  doryxTAB.data3 <- RdoryxTAB.data3()
	  summary.function <- Rsummary.function()
	  # Calculate the median and prediction intervals for calculations at each time-point
	  doryxTAB.summary3 <- ddply(doryxTAB.data3, .(time,SEX), function(doryxTAB.data3) summary.function(doryxTAB.data3$IPRE))
	})	#Brackets closing "RdoryxTAB.summary3"
	

	############
	##_OUTPUT_##
	############

	################
	##_FED_STATUS_##
	################
	# Plot simulation results of fed versus fasted for Doryx MPC
		output$RdoryxMPC.plot1 <- renderPlot({
			# Read in the reactive data frame for summary
				doryxMPC.summary1 <- RdoryxMPC.summary1()

			# Plot
				plotobj1 <- ggplot(doryxMPC.summary1)
			# Fasted
				plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),data = doryxMPC.summary1[doryxMPC.summary1$FED == 0,],colour = "red")
				if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxMPC.summary1[doryxMPC.summary1$FED == 0,],fill = "red",alpha = 0.3)
			# Fed
				plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),data = doryxMPC.summary1[doryxMPC.summary1$FED == 1,],colour = "blue")
				if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxMPC.summary1[doryxMPC.summary1$FED == 1,],fill = "blue",alpha = 0.3)
			# Plot horizontal line representing LLOQ
				plotobj1 <- plotobj1 + geom_hline(aes(yintercept = 10),linetype = "dashed")
				plotobj1 <- plotobj1 + scale_x_continuous("\nTime (hours)")
			# Plot on linear or log-scale depending on input
				if (input$LOGS == FALSE) plotobj1 <- plotobj1 + scale_y_continuous("Doxycycline Concentration (microg/L)\n")
				if (input$LOGS == TRUE) plotobj1 <- plotobj1 + scale_y_log10("Doxycycline Concentration (microg/L)\n",breaks = c(10,100,1000),lim = c(1,NA))
				print(plotobj1)
		})	#Brackets closing "renderPlot"

	# Plot simulation results of fed versus fasted for Doryx Tablet
		output$RdoryxTAB.plot1 <- renderPlot({
			# Read in the reactive data frame for fed.summary
				doryxTAB.summary1 <- RdoryxTAB.summary1()

			# Plot
				plotobj2 <- ggplot(doryxTAB.summary1)
			# Fasted
				plotobj2 <- plotobj2 + geom_line(aes(x = time,y = Median),data = doryxTAB.summary1[doryxTAB.summary1$FED == 0,],colour = "red")
				if (input$PI > 1) plotobj2 <- plotobj2 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxTAB.summary1[doryxTAB.summary1$FED == 0,],fill = "red",alpha = 0.3)
			# Fed
				plotobj2 <- plotobj2 + geom_line(aes(x = time,y = Median),data = doryxTAB.summary1[doryxTAB.summary1$FED == 1,],colour = "blue")
				if (input$PI > 1) plotobj2 <- plotobj2 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxTAB.summary1[doryxTAB.summary1$FED == 1,],fill = "blue",alpha = 0.3)
			# Plot horizontal line representing LLOQ
				plotobj2 <- plotobj2 + geom_hline(aes(yintercept = 10),linetype = "dashed")
				plotobj2 <- plotobj2 + scale_x_continuous("\nTime (hours)")
			# Plot on linear or log-scale depending on input
				if (input$LOGS == FALSE) plotobj2 <- plotobj2 + scale_y_continuous("Doxycycline Concentration (microg/L)\n")
				if (input$LOGS == TRUE) plotobj2 <- plotobj2 + scale_y_log10("Doxycycline Concentration (microg/L)\n",breaks = c(10,100,1000),lim = c(1,NA))
				print(plotobj2)
	})	#Brackets closing "renderPlot"

	# Summary table of fed versus fasted for Doryx MPC
		output$RdoryxMPC.table1 <- renderTable({
		  # Read in the necessary reactive expressions
			  doryxMPC.data1 <- RdoryxMPC.data1()
			  summary.function <- Rsummary.function()
			  if (input$DOSE1 == 1) {
					# Summarise at t = 96 hours for single dose scenarios
						doryxMPC.data96 <- subset(doryxMPC.data1,time == 96)
						# Summarise AUC
					    AUC.table <- ddply(doryxMPC.data96, .(FED), function(doryxMPC.data96) summary.function(doryxMPC.data96$AUC))
					    AUC.table$Variable <- "AUC(0-96 h) (microg*h/L)"
				    # Summarise Cmax (value will be found at time = 96)
					    Cmax.table <- ddply(doryxMPC.data96, .(FED), function(doryxMPC.data96) summary.function(doryxMPC.data96$Cmax))
					    Cmax.table$Variable <- "Cmax (microg/L)"
				    # Summarise Tmax (value will be found at time = 96)
					    Tmax.table <- ddply(doryxMPC.data96, .(FED), function(doryxMPC.data96) summary.function(doryxMPC.data96$Tmax))
					    Tmax.table$Variable <- "Tmax (h)"
					# Return data frame
						doryxMPC.table1 <- rbind(AUC.table,Cmax.table,Tmax.table)
						doryxMPC.table1$FED[doryxMPC.table1$FED== 0] <- "Fasted"
						doryxMPC.table1$FED[doryxMPC.table1$FED== 1] <- "Fed"
						if (input$PI == 1) {doryxMPC.table1 <- data.frame(Status = doryxMPC.table1$FED,Median = doryxMPC.table1$Median,Variable = doryxMPC.table1$Variable)
						}
						if (input$PI > 1) {doryxMPC.table1 <- data.frame(Status = doryxMPC.table1$FED,Median = doryxMPC.table1$Median,CIlo = doryxMPC.table1$CIlo,CIhi = doryxMPC.table1$CIhi,Variable = doryxMPC.table1$Variable)
						}
			  }
			  
			  if (input$DOSE1 != 1) {
					# Summarise at t = 240 for multiple dose scenario
				  	doryxMPC.data240 <- subset(doryxMPC.data1,time == 240)
						# Summarise AUC
					    AUC.table <- ddply(doryxMPC.data240, .(FED), function(doryxMPC.data240) summary.function(doryxMPC.data240$AUC))
					    AUC.table$Variable <- "AUC(0-240 h) (microg*h/L)"
				    # Summarise Cmax (value will be found at time = 240)
					    Cmax.table <- ddply(doryxMPC.data240, .(FED), function(doryxMPC.data240) summary.function(doryxMPC.data240$Cmax))
					    Cmax.table$Variable <- "Cmax (microg/L)"
				    # Summarise Tmax (value will be found at time = 240)
					    Tmax.table <- ddply(doryxMPC.data240, .(FED), function(doryxMPC.data240) summary.function(doryxMPC.data240$Tmax))
					    Tmax.table$Variable <- "Tmax (h)"
					# Return data frame
				    doryxMPC.table1 <- rbind(AUC.table,Cmax.table,Tmax.table)
				    #Name Fasted and Fed Values
				    doryxMPC.table1$FED[doryxMPC.table1$FED== 0] <- "Fasted"
				    doryxMPC.table1$FED[doryxMPC.table1$FED== 1] <- "Fed"
				    if (input$PI == 1) {doryxMPC.table1 <- data.frame(Status = doryxMPC.table1$FED,Median = doryxMPC.table1$Median,Variable = doryxMPC.table1$Variable)
				    } #close if
				    if (input$PI > 1) {doryxMPC.table1 <- data.frame(Status = doryxMPC.table1$FED,Median = doryxMPC.table1$Median,CIlo = doryxMPC.table1$CIlo,CIhi = doryxMPC.table1$CIhi,Variable = doryxMPC.table1$Variable)
				    } #close if
			  } #close if
			  doryxMPC.table1
		})	#Brackets closing "renderText"

	# Summary table of fed versus fasted for Doryx TAB
		output$RdoryxTAB.table1 <- renderTable({
		  # Read in the necessary reactive expressions
		  doryxTAB.data1 <- RdoryxTAB.data1()
		  summary.function <- Rsummary.function()
		  if (input$DOSE1 == 1) {
		    # Summarise at t = 96 hours for single dose scenarios
		    doryxTAB.data96 <- subset(doryxTAB.data1,time == 96)
		    # Summarise AUC
		    AUC.table <- ddply(doryxTAB.data96, .(FED), function(doryxTAB.data96) summary.function(doryxTAB.data96$AUC))
		    AUC.table$Variable <- "AUC(0-96 h) (microg*h/L)"
		    # Summarise Cmax (value will be found at time = 96)
		    Cmax.table <- ddply(doryxTAB.data96, .(FED), function(doryxTAB.data96) summary.function(doryxTAB.data96$Cmax))
		    Cmax.table$Variable <- "Cmax (microg/L)"
		    # Summarise Tmax (value will be found at time = 96)
		    Tmax.table <- ddply(doryxTAB.data96, .(FED), function(doryxTAB.data96) summary.function(doryxTAB.data96$Tmax))
		    Tmax.table$Variable <- "Tmax (h)"
		    # Return data frame
		    doryxTAB.table1 <- rbind(AUC.table,Cmax.table,Tmax.table)
		    doryxTAB.table1$FED[doryxTAB.table1$FED== 0] <- "Fasted"
		    doryxTAB.table1$FED[doryxTAB.table1$FED== 1] <- "Fed"
		    if (input$PI == 1) {doryxTAB.table1 <- data.frame(Status = doryxTAB.table1$FED,Median = doryxTAB.table1$Median,Variable = doryxTAB.table1$Variable)
		    }
		    if (input$PI > 1) {doryxTAB.table1 <- data.frame(Status = doryxTAB.table1$FED,Median = doryxTAB.table1$Median,CIlo = doryxTAB.table1$CIlo,CIhi = doryxTAB.table1$CIhi,Variable = doryxTAB.table1$Variable)
		    }
		  }
		  
		  if (input$DOSE1 != 1) {
		    # Summarise at t = 240 for multiple dose scenario
		    doryxTAB.data240 <- subset(doryxTAB.data1,time == 240)
		    # Summarise AUC
		    AUC.table <- ddply(doryxTAB.data240, .(FED), function(doryxTAB.data240) summary.function(doryxTAB.data240$AUC))
		    AUC.table$Variable <- "AUC(0-240 h) (microg*h/L)"
		    # Summarise Cmax (value will be found at time = 240)
		    Cmax.table <- ddply(doryxTAB.data240, .(FED), function(doryxTAB.data240) summary.function(doryxTAB.data240$Cmax))
		    Cmax.table$Variable <- "Cmax (microg/L)"
		    # Summarise Tmax (value will be found at time = 240)
		    Tmax.table <- ddply(doryxTAB.data240, .(FED), function(doryxTAB.data240) summary.function(doryxTAB.data240$Tmax))
		    Tmax.table$Variable <- "Tmax (h)"
		    # Return data frame
		    doryxTAB.table1 <- rbind(AUC.table,Cmax.table,Tmax.table)
		    #Name Fasted and Fed Values
		    doryxTAB.table1$FED[doryxTAB.table1$FED== 0] <- "Fasted"
		    doryxTAB.table1$FED[doryxTAB.table1$FED== 1] <- "Fed"
		    if (input$PI == 1) {doryxTAB.table1 <- data.frame(Status = doryxTAB.table1$FED,Median = doryxTAB.table1$Median,Variable = doryxTAB.table1$Variable)
		    } #close if
		    if (input$PI > 1) {doryxTAB.table1 <- data.frame(Status = doryxTAB.table1$FED,Median = doryxTAB.table1$Median,CIlo = doryxTAB.table1$CIlo,CIhi = doryxTAB.table1$CIhi,Variable = doryxTAB.table1$Variable)
		    } #close if
		  } #close if
		  doryxTAB.table1
		})	#Brackets closing "renderText"

	#################
	##_FORM_STATUS_##
	#################

		# Plot simulation results of Doryx MPC vs Doryx TAB for fasted 
		output$RformFasted.plot <- renderPlot({
		  # Read in the reactive data frame for summary
		  doryxMPC.summary2 <- RdoryxMPC.summary2()
		  doryxTAB.summary2 <- RdoryxTAB.summary2()
		  
		  # Plot
		  plotobj1 <- ggplot(doryxMPC.summary2)
		  # Fasted - MPC
		  plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),data = doryxMPC.summary2[doryxMPC.summary2$FED == 0,],colour = "red")
		  if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxMPC.summary2[doryxMPC.summary2$FED == 0,],fill = "red",alpha = 0.3)
		  # Fasted - TAB
		  plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),data = doryxTAB.summary2[doryxTAB.summary2$FED == 0,],colour = "blue")
		  if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxTAB.summary2[doryxTAB.summary2$FED == 0,],fill = "blue",alpha = 0.3)
		  # Plot horizontal line representing LLOQ
		  plotobj1 <- plotobj1 + geom_hline(aes(yintercept = 10),linetype = "dashed")
		  plotobj1 <- plotobj1 + scale_x_continuous("\nTime (hours)")
		  # Plot on linear or log-scale depending on input
		  if (input$LOGS == FALSE) plotobj1 <- plotobj1 + scale_y_continuous("Doxycycline Concentration (microg/L)\n")
		  if (input$LOGS == TRUE) plotobj1 <- plotobj1 + scale_y_log10("Doxycycline Concentration (microg/L)\n",breaks = c(10,100,1000),lim = c(1,NA))
		  print(plotobj1)
		})	#Brackets closing "renderPlot"
		
		# Plot simulation results of Doryx MPC vs Doryx TAB for fed 
		output$RformFed.plot <- renderPlot({
		  # Read in the reactive data frame for fed.summary
		  doryxMPC.summary2 <- RdoryxMPC.summary2()
		  doryxTAB.summary2 <- RdoryxTAB.summary2()
		  # Plot
		  plotobj2 <- ggplot(doryxTAB.summary2)
		  # Fed - MPC
		  plotobj2 <- plotobj2 + geom_line(aes(x = time,y = Median),data = doryxMPC.summary2[doryxMPC.summary2$FED == 1,],colour = "red")
		  if (input$PI > 1) plotobj2 <- plotobj2 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxMPC.summary2[doryxMPC.summary2$FED == 1,],fill = "red",alpha = 0.3)
		  # Fed
		  plotobj2 <- plotobj2 + geom_line(aes(x = time,y = Median),data = doryxTAB.summary2[doryxTAB.summary2$FED == 1,],colour = "blue")
		  if (input$PI > 1) plotobj2 <- plotobj2 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxTAB.summary2[doryxTAB.summary2$FED == 1,],fill = "blue",alpha = 0.3)
		  # Plot horizontal line representing LLOQ
		  plotobj2 <- plotobj2 + geom_hline(aes(yintercept = 10),linetype = "dashed")
		  plotobj2 <- plotobj2 + scale_x_continuous("\nTime (hours)")
		  # Plot on linear or log-scale depending on input
		  if (input$LOGS == FALSE) plotobj2 <- plotobj2 + scale_y_continuous("Doxycycline Concentration (microg/L)\n")
		  if (input$LOGS == TRUE) plotobj2 <- plotobj2 + scale_y_log10("Doxycycline Concentration (microg/L)\n",breaks = c(10,100,1000),lim = c(1,NA))
		  print(plotobj2)
		})	#Brackets closing "renderPlot"
		
		# Summary table of Doryx MPC versus Doryx TAB for fasted
		output$Rformfast.table2 <- renderTable({
		  # Read in the necessary reactive expressions
		  doryxMPC.data2 <- RdoryxMPC.data2()
		  doryxMPCfast.data <- subset(doryxMPC.data2,FED == 0)
		  doryxTAB.data2 <- RdoryxTAB.data2()
		  doryxTABfast.data <- subset(doryxTAB.data2,FED == 0)
		  fast.data <- rbind(doryxMPCfast.data,doryxTABfast.data)
		  summary.function <- Rsummary.function()
		  if (input$DOSE2 == 1) {
		    # Summarise at t = 96 hours for single dose scenarios
		    fast.data96 <- subset(fast.data,time == 96)
		    # Summarise AUC
		    AUC.table <- ddply(fast.data96, .(TRT), function(fast.data96) summary.function(fast.data96$AUC))
		    AUC.table$Variable <- "AUC(0-96 h) (microg*h/L)"
		    # Summarise Cmax (value will be found at time = 96)
		    Cmax.table <- ddply(fast.data96, .(TRT), function(fast.data96) summary.function(fast.data96$Cmax))
		    Cmax.table$Variable <- "Cmax (microg/L)"
		    # Summarise Tmax (value will be found at time = 96)
		    Tmax.table <- ddply(fast.data96, .(TRT), function(fast.data96) summary.function(fast.data96$Tmax))
		    Tmax.table$Variable <- "Tmax (h)"
		    # Return data frame
		    formfast.table2 <- rbind(AUC.table,Cmax.table,Tmax.table)
		    formfast.table2$TRT[formfast.table2$TRT== 1] <- "Doryx MPC"
		    formfast.table2$TRT[formfast.table2$TRT== 2] <- "Doryx Tablet"
		    if (input$PI == 1) {formfast.table2 <- data.frame(Formulation = formfast.table2$TRT,Median = formfast.table2$Median,Variable = formfast.table2$Variable)
		    }
		    if (input$PI > 1) {formfast.table2 <- data.frame(Formulation = formfast.table2$TRT,Median = formfast.table2$Median,CIlo = formfast.table2$CIlo,CIhi = formfast.table2$CIhi,Variable = formfast.table2$Variable)
		    }
		  }
		  
		  if (input$DOSE2 != 1) {
		    # Summarise at t = 240 for multiple dose scenario
		    fast.data240 <- subset(fast.data,time == 240)
		    # Summarise AUC
		    AUC.table <- ddply(fast.data240, .(TRT), function(fast.data240) summary.function(fast.data240$AUC))
		    AUC.table$Variable <- "AUC(0-240 h) (microg*h/L)"
		    # Summarise Cmax (value will be found at time = 240)
		    Cmax.table <- ddply(fast.data240, .(TRT), function(fast.data240) summary.function(fast.data240$Cmax))
		    Cmax.table$Variable <- "Cmax (microg/L)"
		    # Summarise Tmax (value will be found at time = 240)
		    Tmax.table <- ddply(fast.data240, .(TRT), function(fast.data240) summary.function(fast.data240$Tmax))
		    Tmax.table$Variable <- "Tmax (h)"
		    # Return data frame
		    formfast.table2 <- rbind(AUC.table,Cmax.table,Tmax.table)
		    #Name Fasted and TRT Values
		    formfast.table2$TRT[formfast.table2$TRT== 1] <- "Doryx MPC"
		    formfast.table2$TRT[formfast.table2$TRT== 2] <- "Doryx Tablet"
		    if (input$PI == 1) {formfast.table2 <- data.frame(Formulation = formfast.table2$TRT,Median = formfast.table2$Median,Variable = formfast.table2$Variable)
		    } #close if
		    if (input$PI > 1) {formfast.table2 <- data.frame(Formulation = formfast.table2$TRT,Median = formfast.table2$Median,CIlo = formfast.table2$CIlo,CIhi = formfast.table2$CIhi,Variable = formfast.table2$Variable)
		    } #close if
		  } #close if
		  formfast.table2
		})	#Brackets closing "renderText"
		
		# Summary table of Doryx MPC versus Doryx TAB for fed
		output$Rformfed.table2 <- renderTable({
		  # Read in the necessary reactive expressions
		  doryxMPC.data2 <- RdoryxMPC.data2()
		  doryxMPCfed.data <- subset(doryxMPC.data2,FED == 1)
		  doryxTAB.data2 <- RdoryxTAB.data2()
		  doryxTABfed.data <- subset(doryxTAB.data2,FED == 1)
		  fed.data <- rbind(doryxMPCfed.data,doryxTABfed.data)
		  summary.function <- Rsummary.function()
		  if (input$DOSE2 == 1) {
		    # Summarise at t = 96 hours for single dose scenarios
		    fed.data96 <- subset(fed.data,time == 96)
		    # Summarise AUC
		    AUC.table <- ddply(fed.data96, .(TRT), function(fed.data96) summary.function(fed.data96$AUC))
		    AUC.table$Variable <- "AUC(0-96 h) (microg*h/L)"
		    # Summarise Cmax (value will be found at time = 96)
		    Cmax.table <- ddply(fed.data96, .(TRT), function(fed.data96) summary.function(fed.data96$Cmax))
		    Cmax.table$Variable <- "Cmax (microg/L)"
		    # Summarise Tmax (value will be found at time = 96)
		    Tmax.table <- ddply(fed.data96, .(TRT), function(fed.data96) summary.function(fed.data96$Tmax))
		    Tmax.table$Variable <- "Tmax (h)"
		    # Return data frame
		    formfed.table2 <- rbind(AUC.table,Cmax.table,Tmax.table)
		    formfed.table2$TRT[formfed.table2$TRT== 1] <- "Doryx MPC"
		    formfed.table2$TRT[formfed.table2$TRT== 2] <- "Doryx Tablet"
		    if (input$PI == 1) {formfed.table2 <- data.frame(Formulation = formfed.table2$TRT,Median = formfed.table2$Median,Variable = formfed.table2$Variable)
		    }
		    if (input$PI > 1) {formfed.table2 <- data.frame(Formulation = formfed.table2$TRT,Median = formfed.table2$Median,CIlo = formfed.table2$CIlo,CIhi = formfed.table2$CIhi,Variable = formfed.table2$Variable)
		    }
		  }
		  
		  if (input$DOSE2 != 1) {
		    # Summarise at t = 240 for multiple dose scenario
		    fed.data240 <- subset(fed.data,time == 240)
		    # Summarise AUC
		    AUC.table <- ddply(fed.data240, .(TRT), function(fed.data240) summary.function(fed.data240$AUC))
		    AUC.table$Variable <- "AUC(0-240 h) (microg*h/L)"
		    # Summarise Cmax (value will be found at time = 240)
		    Cmax.table <- ddply(fed.data240, .(TRT), function(fed.data240) summary.function(fed.data240$Cmax))
		    Cmax.table$Variable <- "Cmax (microg/L)"
		    # Summarise Tmax (value will be found at time = 240)
		    Tmax.table <- ddply(fed.data240, .(TRT), function(fed.data240) summary.function(fed.data240$Tmax))
		    Tmax.table$Variable <- "Tmax (h)"
		    # Return data frame
		    formfed.table2 <- rbind(AUC.table,Cmax.table,Tmax.table)
		    #Name feded and TRT Values
		    formfed.table2$TRT[formfed.table2$TRT== 1] <- "Doryx MPC"
		    formfed.table2$TRT[formfed.table2$TRT== 2] <- "Doryx Tablet"
		    if (input$PI == 1) {formfed.table2 <- data.frame(Formulation = formfed.table2$TRT,Median = formfed.table2$Median,Variable = formfed.table2$Variable)
		    } #close if
		    if (input$PI > 1) {formfed.table2 <- data.frame(Formulation = formfed.table2$TRT,Median = formfed.table2$Median,CIlo = formfed.table2$CIlo,CIhi = formfed.table2$CIhi,Variable = formfed.table2$Variable)
		    } #close if
		  } #close if
		  formfed.table2
		})	#Brackets closing "renderText"

		
		################
		##_SEX_STATUS_##
		################
		# Plot simulation results of male versus female for Doryx MPC (fasted)
		output$RdoryxMPCSex.plot <- renderPlot({
		  # Read in the reactive data frame for summary
		  doryxMPC.summary3 <- RdoryxMPC.summary3()
		  
		  # Plot
		  plotobj1 <- ggplot(doryxMPC.summary3)
		  # Female
		  plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),data = doryxMPC.summary3[doryxMPC.summary3$SEX == 0,],colour = "red")
		  if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxMPC.summary3[doryxMPC.summary3$SEX == 0,],fill = "red",alpha = 0.3)
		  # Male
		  plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),data = doryxMPC.summary3[doryxMPC.summary3$SEX == 1,],colour = "blue")
		  if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxMPC.summary3[doryxMPC.summary3$SEX == 1,],fill = "blue",alpha = 0.3)
		  # Plot horizontal line representing LLOQ
		  plotobj1 <- plotobj1 + geom_hline(aes(yintercept = 10),linetype = "dashed")
		  plotobj1 <- plotobj1 + scale_x_continuous("\nTime (hours)")
		  # Plot on linear or log-scale depending on input
		  if (input$LOGS == FALSE) plotobj1 <- plotobj1 + scale_y_continuous("Doxycycline Concentration (microg/L)\n")
		  if (input$LOGS == TRUE) plotobj1 <- plotobj1 + scale_y_log10("Doxycycline Concentration (microg/L)\n",breaks = c(10,100,1000),lim = c(1,NA))
		  print(plotobj1)
		})	#Brackets closing "renderPlot"
		
		# Plot simulation results of male versus female for Doryx Tablet (fasted)
		output$RdoryxTABSex.plot <- renderPlot({
		  # Read in the reactive data frame for fed.summary
		  doryxTAB.summary3 <- RdoryxTAB.summary3()
		  
		  # Plot
		  plotobj2 <- ggplot(doryxTAB.summary3)
		  # Female
		  plotobj2 <- plotobj2 + geom_line(aes(x = time,y = Median),data = doryxTAB.summary3[doryxTAB.summary3$SEX == 0,],colour = "red")
		  if (input$PI > 1) plotobj2 <- plotobj2 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxTAB.summary3[doryxTAB.summary3$SEX == 0,],fill = "red",alpha = 0.3)
		  # Male
		  plotobj2 <- plotobj2 + geom_line(aes(x = time,y = Median),data = doryxTAB.summary3[doryxTAB.summary3$SEX == 1,],colour = "blue")
		  if (input$PI > 1) plotobj2 <- plotobj2 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxTAB.summary3[doryxTAB.summary3$SEX == 1,],fill = "blue",alpha = 0.3)
		  # Plot horizontal line representing LLOQ
		  plotobj2 <- plotobj2 + geom_hline(aes(yintercept = 10),linetype = "dashed")
		  plotobj2 <- plotobj2 + scale_x_continuous("\nTime (hours)")
		  # Plot on linear or log-scale depending on input
		  if (input$LOGS == FALSE) plotobj2 <- plotobj2 + scale_y_continuous("Doxycycline Concentration (microg/L)\n")
		  if (input$LOGS == TRUE) plotobj2 <- plotobj2 + scale_y_log10("Doxycycline Concentration (microg/L)\n",breaks = c(10,100,1000),lim = c(1,NA))
		  print(plotobj2)
		})	#Brackets closing "renderPlot"
		
		# Summary table of of male versus female for Doryx Tablet (fasted)
		output$RdoryxMPCSEX.table3 <- renderTable({
		  # Read in the necessary reactive expressions
		  doryxMPC.data3 <- RdoryxMPC.data3()
		  summary.function <- Rsummary.function()
		  if (input$DOSE3 == 1) {
		    # Summarise at t = 96 hours for single dose scenarios
		    doryxMPC.data96 <- subset(doryxMPC.data3,time == 96)
		    # Summarise AUC
		    AUC.table <- ddply(doryxMPC.data96, .(SEX), function(doryxMPC.data96) summary.function(doryxMPC.data96$AUC))
		    AUC.table$Variable <- "AUC(0-96 h) (microg*h/L)"
		    # Summarise Cmax (value will be found at time = 96)
		    Cmax.table <- ddply(doryxMPC.data96, .(SEX), function(doryxMPC.data96) summary.function(doryxMPC.data96$Cmax))
		    Cmax.table$Variable <- "Cmax (microg/L)"
		    # Summarise Tmax (value will be found at time = 96)
		    Tmax.table <- ddply(doryxMPC.data96, .(SEX), function(doryxMPC.data96) summary.function(doryxMPC.data96$Tmax))
		    Tmax.table$Variable <- "Tmax (h)"
		    # Return data frame
		    doryxMPC.table3 <- rbind(AUC.table,Cmax.table,Tmax.table)
		    doryxMPC.table3$SEX[doryxMPC.table3$SEX== 0] <- "Female"
		    doryxMPC.table3$SEX[doryxMPC.table3$SEX== 1] <- "Male"
		    if (input$PI == 1) {doryxMPC.table3 <- data.frame(Sex = doryxMPC.table3$SEX,Median = doryxMPC.table3$Median,Variable = doryxMPC.table3$Variable)
		    }
		    if (input$PI > 1) {doryxMPC.table3 <- data.frame(Sex = doryxMPC.table3$SEX,Median = doryxMPC.table3$Median,CIlo = doryxMPC.table3$CIlo,CIhi = doryxMPC.table3$CIhi,Variable = doryxMPC.table3$Variable)
		    }
		  }
		  
		  if (input$DOSE3 != 1) {
		    # Summarise at t = 240 for multiple dose scenario
		    doryxMPC.data240 <- subset(doryxMPC.data3,time == 240)
		    # Summarise AUC
		    AUC.table <- ddply(doryxMPC.data240, .(SEX), function(doryxMPC.data240) summary.function(doryxMPC.data240$AUC))
		    AUC.table$Variable <- "AUC(0-240 h) (microg*h/L)"
		    # Summarise Cmax (value will be found at time = 240)
		    Cmax.table <- ddply(doryxMPC.data240, .(SEX), function(doryxMPC.data240) summary.function(doryxMPC.data240$Cmax))
		    Cmax.table$Variable <- "Cmax (microg/L)"
		    # Summarise Tmax (value will be found at time = 240)
		    Tmax.table <- ddply(doryxMPC.data240, .(SEX), function(doryxMPC.data240) summary.function(doryxMPC.data240$Tmax))
		    Tmax.table$Variable <- "Tmax (h)"
		    # Return data frame
		    doryxMPC.table3 <- rbind(AUC.table,Cmax.table,Tmax.table)
		    #Name Fasted and Fed Values
		    doryxMPC.table3$SEX[doryxMPC.table3$SEX== 0] <- "Female"
		    doryxMPC.table3$SEX[doryxMPC.table3$SEX== 1] <- "Male"
		    if (input$PI == 1) {doryxMPC.table3 <- data.frame(Sex = doryxMPC.table3$SEX,Median = doryxMPC.table3$Median,Variable = doryxMPC.table3$Variable)
		    } #close if
		    if (input$PI > 1) {doryxMPC.table3 <- data.frame(Sex = doryxMPC.table3$SEX,Median = doryxMPC.table3$Median,CIlo = doryxMPC.table3$CIlo,CIhi = doryxMPC.table3$CIhi,Variable = doryxMPC.table3$Variable)
		    } #close if
		  } #close if
		  doryxMPC.table3
		})	#Brackets closing "renderText"
		
		# Summary table of fed versus fasted for Doryx TAB
		output$RdoryxTABSEX.table3 <- renderTable({
		  # Read in the necessary reactive expressions
		  doryxTAB.data3 <- RdoryxTAB.data3()
		  summary.function <- Rsummary.function()
		  if (input$DOSE3 == 1) {
		    # Summarise at t = 96 hours for single dose scenarios
		    doryxTAB.data96 <- subset(doryxTAB.data3,time == 96)
		    # Summarise AUC
		    AUC.table <- ddply(doryxTAB.data96, .(SEX), function(doryxTAB.data96) summary.function(doryxTAB.data96$AUC))
		    AUC.table$Variable <- "AUC(0-96 h) (microg*h/L)"
		    # Summarise Cmax (value will be found at time = 96)
		    Cmax.table <- ddply(doryxTAB.data96, .(SEX), function(doryxTAB.data96) summary.function(doryxTAB.data96$Cmax))
		    Cmax.table$Variable <- "Cmax (microg/L)"
		    # Summarise Tmax (value will be found at time = 96)
		    Tmax.table <- ddply(doryxTAB.data96, .(SEX), function(doryxTAB.data96) summary.function(doryxTAB.data96$Tmax))
		    Tmax.table$Variable <- "Tmax (h)"
		    # Return data frame
		    doryxTAB.table3 <- rbind(AUC.table,Cmax.table,Tmax.table)
		    doryxTAB.table3$SEX[doryxTAB.table3$SEX== 0] <- "Female"
		    doryxTAB.table3$SEX[doryxTAB.table3$SEX== 1] <- "Male"
		    if (input$PI == 1) {doryxTAB.table3 <- data.frame(Sex = doryxTAB.table3$SEX,Median = doryxTAB.table3$Median,Variable = doryxTAB.table3$Variable)
		    }
		    if (input$PI > 1) {doryxTAB.table3 <- data.frame(Sex = doryxTAB.table3$SEX,Median = doryxTAB.table3$Median,CIlo = doryxTAB.table3$CIlo,CIhi = doryxTAB.table3$CIhi,Variable = doryxTAB.table3$Variable)
		    }
		  }
		  
		  if (input$DOSE3 != 1) {
		    # Summarise at t = 240 for multiple dose scenario
		    doryxTAB.data240 <- subset(doryxTAB.data3,time == 240)
		    # Summarise AUC
		    AUC.table <- ddply(doryxTAB.data240, .(SEX), function(doryxTAB.data240) summary.function(doryxTAB.data240$AUC))
		    AUC.table$Variable <- "AUC(0-240 h) (microg*h/L)"
		    # Summarise Cmax (value will be found at time = 240)
		    Cmax.table <- ddply(doryxTAB.data240, .(SEX), function(doryxTAB.data240) summary.function(doryxTAB.data240$Cmax))
		    Cmax.table$Variable <- "Cmax (microg/L)"
		    # Summarise Tmax (value will be found at time = 240)
		    Tmax.table <- ddply(doryxTAB.data240, .(SEX), function(doryxTAB.data240) summary.function(doryxTAB.data240$Tmax))
		    Tmax.table$Variable <- "Tmax (h)"
		    # Return data frame
		    doryxTAB.table3 <- rbind(AUC.table,Cmax.table,Tmax.table)
		    #Name Fasted and Fed Values
		    doryxTAB.table3$SEX[doryxTAB.table3$SEX== 0] <- "Female"
		    doryxTAB.table3$SEX[doryxTAB.table3$SEX== 1] <- "Male"
		    if (input$PI == 1) {doryxTAB.table3 <- data.frame(Sex = doryxTAB.table3$SEX,Median = doryxTAB.table3$Median,Variable = doryxTAB.table3$Variable)
		    } #close if
		    if (input$PI > 1) {doryxTAB.table3 <- data.frame(Sex = doryxTAB.table3$SEX,Median = doryxTAB.table3$Median,CIlo = doryxTAB.table3$CIlo,CIhi = doryxTAB.table3$CIhi,Variable = doryxTAB.table3$Variable)
		    } #close if
		  } #close if
		  doryxTAB.table3
		})	#Brackets closing "renderText"
		


  #############
  ##_SESSION_##
  #############
  # Close the R session when Chrome closes
  session$onSessionEnded(function() {
    stopApp()
  })
})  #Brackets closing "shinyServer" function
