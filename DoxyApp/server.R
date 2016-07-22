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

	# Simulate a population of fed/fasted individuals administered Doryx MPC
		RdoryxMPC.data <- reactive({
			withProgress(
				message = "Simulating concentrations...",
				value = 0,
				{
					# Simulate concentration-time profiles for the population
					# Specify dosing input
					  if (input$DOSE_REG == 1) {	# Single dose scenario : A single 120 mg MPC dose
					    DOSE_DORYXMPC <- 120	# mg
				  		# Create input data frame for mrgsim
					  		input.doryxMPC.data <- data.frame(
					  			ID = 1:n,	# n individuals
					  			amt = DOSE_DORYXMPC*1000,	# amt in microg
					  			evid = 1,	# evid = 1; dosing event
					  			cmt = 1,	# cmt = 1; dose goes into compartment 1 = depot
					  			time = 0,	# time = 0; begin dosing at time = 0
					  			TRT = 1,	# Doryx MPC
					  			FED = rbinom(n,size = 1,prob = 0.5),	# Randomly assign fed or fasted status
					  			SEX = rbinom(n,size = 1,prob = 0.5),	# Randomly assign male or female status
					  			FFM = rlnorm(n,meanlog = log(55.49),sd = 0.09)	# Randomly generate value for fat free mass (kg)
					  		)
					  }
						if (input$DOSE_REG != 1) {	# Multiple dose scenario
							# Specify the dosing times for the clinical scenario
						  	if (input$DOSE_REG == 2) dose.times <- c(0,12,24,48,72,96,120,144) # 120 mg every 12 hours on the first day, followed by six 120 mg doses at 24 hour intervals
						  	if (input$DOSE_REG == 3) dose.times <- seq(from = 0,to = 156,by = 12) # 120 mg every 12 hours for 7 days
						  # Create input data frame for mrgsim
							  input.doryxMPC.data <- data.frame(
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
								input.doryxMPC.data <- lapply(input.doryxMPC.data,rep.int,times = length(time.multiple))
								input.doryxMPC.data <- as.data.frame(input.doryxMPC.data)	# Convert to a data frame
								input.doryxMPC.data <- input.doryxMPC.data[with(input.doryxMPC.data, order(input.doryxMPC.data$ID)),]	# Sort by ID
							# Add a time column
								input.doryxMPC.data$time <- time.multiple
							# For times that aren't dosing times, make evid = 0
								input.doryxMPC.data$evid[!c(input.doryxMPC.data$time %in% dose.times)] <- 0
							# Return the resulting data frame
								input.doryxMPC.data
						}
				  # Simulate
						doryxMPC.data <- mod %>% data_set(input.doryxMPC.data) %>% mrgsim(tgrid = TIME.tgrid)
						doryxMPC.data <- as.data.frame(doryxMPC.data)	#Convert to a data frame so that it is more useful for me!
				}	#Brackets closing expression for "withProgress"
			)	#Brackets closing "withProgress"
		})	# Brackets closing "RdoryxMPC.data"

	RdoryxMPC.summary <- reactive({
		# Read in the necessary reactive expressions
			doryxMPC.data <- RdoryxMPC.data()
			summary.function <- Rsummary.function()
		# Calculate the median and prediction intervals for calculations at each time-point
			if (input$SIM_STUDY == 1 | input$SIM_STUDY == 2) {
				doryxMPC.summary <- ddply(doryxMPC.data, .(time,FED), function(doryxMPC.data) summary.function(doryxMPC.data$IPRE))
			} else {
				doryxMPC.summary <- ddply(doryxMPC.data, .(time,SEX), function(doryxMPC.data) summary.function(doryxMPC.data$IPRE))
			}
			doryxMPC.summary
	})	# Brackets closing "RdoryxMPC.summary"

	# Simulate a population of fed/fasted individuals administered Doryx Tablet
		RdoryxTAB.data <- reactive({
			withProgress(
				message = "Simulating concentrations...",
				value = 0,
				{
			  # Simulate concentration-time profiles for the population
			  	# Specify dosing input
					  if (input$DOSE_REG == 1) {	# Single dose scenario: A single 100 mg dose of Doryx Tablet
				  		DOSE_DORYXTAB <- 100	# mg
				  		# Create input data frame for mrgsim
					  		input.doryxTAB.data <- data.frame(
					  			ID = 1:n,	# n individuals
					  			amt = DOSE_DORYXTAB*1000,	# amt in microg
					  			evid = 1,	# evid = 1; dosing event
					  			cmt = 1,	# cmt = 1; dose goes into compartment 1 = depot
					  			time = 0,	# time = 0; begin dosing at time = 0
					  			TRT = 2,	# Doryx tablet
									FED = rbinom(n,size = 1,prob = 0.5),	# Randomly assign fed or fasted status
					  			SEX = rbinom(n,size = 1,prob = 0.5),	# Randomly assign male or female status
					  			FFM = rlnorm(n,meanlog = log(55.49),sd = 0.09)	# Randomly generate value for fat free mass (kg)
					  		)
						}
						if (input$DOSE_REG != 1) {	# Multiple dose scenario
							# Specify the dosing times for the clinical scenario
						  	if (input$DOSE_REG == 2) dose.times <- c(0,12,24,48,72,96,120,144) # Doryx TAB: 100 mg every 12 hours on the first day, followed by six 100 mg doses at 24 hour intervals
						  	if (input$DOSE_REG == 3) dose.times <- seq(from = 0,to = 156,by = 12) # Doryx TAB: 100 mg every 12 hours for 7 days
						  # Create input data frame for mrgsim
							  input.doryxTAB.data <- data.frame(
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
							# Multiple input.doryxMPC.data by the length of sample times
								input.doryxTAB.data <- lapply(input.doryxTAB.data,rep.int,times = length(time.multiple))
								input.doryxTAB.data <- as.data.frame(input.doryxTAB.data)	# Convert to a data frame
								input.doryxTAB.data <- input.doryxTAB.data[with(input.doryxTAB.data, order(input.doryxTAB.data$ID)),]	# Sort by ID
							# Add a time column
								input.doryxTAB.data$time <- time.multiple
							# For times that aren't dosing times, make evid = 0
								input.doryxTAB.data$evid[!c(input.doryxTAB.data$time %in% dose.times)] <- 0
							# Return the resulting data frame
								input.doryxTAB.data
						}
					# Simulate
				  	doryxTAB.data <- mod %>% data_set(input.doryxTAB.data) %>% mrgsim(tgrid = TIME.tgrid)
				  	doryxTAB.data <- as.data.frame(doryxTAB.data)	#Convert to a data frame so that it is more useful for me!
				}	#Brackets closing expression for "withProgress"
			)	#Brackets closing "withProgress"
		})	#Brackets closing "RdoryxTAB.data"

	RdoryxTAB.summary <- reactive({
		# Read in the necessary reactive expressions
			doryxTAB.data <- RdoryxTAB.data()
			summary.function <- Rsummary.function()
		# Calculate the median and prediction intervals for calculations at each time-point
			if (input$SIM_STUDY == 1 | input$SIM_STUDY == 2) {
				doryxTAB.summary <- ddply(doryxTAB.data, .(time,FED), function(doryxTAB.data) summary.function(doryxTAB.data$IPRE))
			} else {
				doryxTAB.summary <- ddply(doryxTAB.data, .(time,SEX), function(doryxTAB.data) summary.function(doryxTAB.data$IPRE))
			}
		doryxTAB.summary
	})	#Brackets closing "RdoryxTAB.summary"

	############
	##_OUTPUT_##
	############

	# Left plot
		output$Rplot1 <- renderPlot({
			# Read in the reactive data frame for summary
				doryxMPC.summary <- RdoryxMPC.summary()
				doryxTAB.summary <- RdoryxTAB.summary()
			# Calculate maximum concentration from either dataset for plot axes
				if (input$PI == 1) max.CONC <- max(c(doryxMPC.summary$Median,doryxTAB.summary$Median))
				if (input$PI != 1) max.CONC <- max(c(doryxMPC.summary$CIhi,doryxTAB.summary$CIhi))

			# Plot
				plotobj1 <- ggplot()
			# Plot the results of fed/fasted - i.e., plot doryxMPC data
				if (input$SIM_STUDY == 1) {
					# Fasted = RED
						plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),data = doryxMPC.summary[doryxMPC.summary$FED == 0,],colour = "red")
						if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxMPC.summary[doryxMPC.summary$FED == 0,],fill = "red",alpha = 0.3)
					# Fed = BLUE
						plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),data = doryxMPC.summary[doryxMPC.summary$FED == 1,],colour = "blue")
						if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxMPC.summary[doryxMPC.summary$FED == 1,],fill = "blue",alpha = 0.3)
				}
			# Plot the results of MPC/TAB - i.e., plot fasted data from doryxMPC.data and doryxTAB.data
				if (input$SIM_STUDY == 2) {
					# DoryxMPC = RED
						plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),data = doryxMPC.summary[doryxMPC.summary$FED == 0,],colour = "red")
						if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxMPC.summary[doryxMPC.summary$FED == 0,],fill = "red",alpha = 0.3)
					# DoryxTAB = BLUE
						plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),data = doryxTAB.summary[doryxTAB.summary$FED == 0,],colour = "blue")
						if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxTAB.summary[doryxTAB.summary$FED == 0,],fill = "blue",alpha = 0.3)
				}
			# Plot the results of male/female - i.e,. plot doryxMPC.data
				if (input$SIM_STUDY == 3) {
					# Female = RED
						plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),data = doryxMPC.summary[doryxMPC.summary$SEX == 0,],colour = "red")
						if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxMPC.summary[doryxMPC.summary$SEX == 0,],fill = "red",alpha = 0.3)
					# Male = BLUE
						plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),data = doryxMPC.summary[doryxMPC.summary$SEX == 1,],colour = "blue")
						if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxMPC.summary[doryxMPC.summary$SEX == 1,],fill = "blue",alpha = 0.3)
				}
			# Plot horizontal line representing LLOQ
				plotobj1 <- plotobj1 + geom_hline(aes(yintercept = 10),linetype = "dashed")
				if (input$DOSE_REG == 1) plotobj1 <- plotobj1 + annotate("text",x = 25,y = 50,label = "Lower Limit of Quantification",colour = "black",size = 4)
				if (input$DOSE_REG != 1) plotobj1 <- plotobj1 + annotate("text",x = 60,y = 80,label = "Lower Limit of Quantification",colour = "black",size = 4)
				plotobj1 <- plotobj1 + scale_x_continuous("\nTime (hours)")
			# Plot on linear or log-scale depending on input
				plotobj1 <- plotobj1 + scale_y_continuous("Doxycycline Concentration (microg/L)\n",breaks = plot.breaks,labels = plot.breaks,lim = c(0,max.CONC))
				print(plotobj1)
		})	#Brackets closing "renderPlot"

	# Right plot
		output$Rplot2 <- renderPlot({
			# Read in the reactive data frame for summary
				doryxMPC.summary <- RdoryxMPC.summary()
				doryxTAB.summary <- RdoryxTAB.summary()
			# Calculate maximum concentration from either dataset for plot axes
				if (input$PI == 1) max.CONC <- max(c(doryxMPC.summary$Median,doryxTAB.summary$Median))
				if (input$PI != 1) max.CONC <- max(c(doryxMPC.summary$CIhi,doryxTAB.summary$CIhi))

			# Plot
				plotobj2 <- ggplot()
			# Plot the results of fed/fasted - i.e., plot doryxTAB.data
				if (input$SIM_STUDY == 1) {
					# Fasted = RED
						plotobj2 <- plotobj2 + geom_line(aes(x = time,y = Median),data = doryxTAB.summary[doryxTAB.summary$FED == 0,],colour = "red")
						if (input$PI > 1) plotobj2 <- plotobj2 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxTAB.summary[doryxTAB.summary$FED == 0,],fill = "red",alpha = 0.3)
					# Fed = BLUE
						plotobj2 <- plotobj2 + geom_line(aes(x = time,y = Median),data = doryxTAB.summary[doryxTAB.summary$FED == 1,],colour = "blue")
						if (input$PI > 1) plotobj2 <- plotobj2 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxTAB.summary[doryxTAB.summary$FED == 1,],fill = "blue",alpha = 0.3)
				}
			# Plot the results of MPC/TAB - i.e., plot fed data from doryxMPC.data and doryxTAB.data
				if (input$SIM_STUDY == 2) {
					# DoryxMPC = RED
						plotobj2 <- plotobj2 + geom_line(aes(x = time,y = Median),data = doryxMPC.summary[doryxMPC.summary$FED == 1,],colour = "red")
						if (input$PI > 1) plotobj2 <- plotobj2 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxMPC.summary[doryxMPC.summary$FED == 1,],fill = "red",alpha = 0.3)
					# DoryxTAB = BLUE
						plotobj2 <- plotobj2 + geom_line(aes(x = time,y = Median),data = doryxTAB.summary[doryxTAB.summary$FED == 1,],colour = "blue")
						if (input$PI > 1) plotobj2 <- plotobj2 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxTAB.summary[doryxTAB.summary$FED == 1,],fill = "blue",alpha = 0.3)
				}
			# Plot the results of male/female - i.e,. plot doryxTAB.data
				if (input$SIM_STUDY == 3) {
					# Female = RED
						plotobj2 <- plotobj2 + geom_line(aes(x = time,y = Median),data = doryxTAB.summary[doryxTAB.summary$SEX == 0,],colour = "red")
						if (input$PI > 1) plotobj2 <- plotobj2 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxTAB.summary[doryxTAB.summary$SEX == 0,],fill = "red",alpha = 0.3)
					# Male = BLUE
						plotobj2 <- plotobj2 + geom_line(aes(x = time,y = Median),data = doryxTAB.summary[doryxTAB.summary$SEX == 1,],colour = "blue")
						if (input$PI > 1) plotobj2 <- plotobj2 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),data = doryxTAB.summary[doryxTAB.summary$SEX == 1,],fill = "blue",alpha = 0.3)
				}
			# Plot horizontal line representing LLOQ
				plotobj2 <- plotobj2 + geom_hline(aes(yintercept = 10),linetype = "dashed")
				if (input$DOSE_REG == 1) plotobj2 <- plotobj2 + annotate("text",x = 25,y = 50,label = "Lower Limit of Quantification",colour = "black",size = 4)
				if (input$DOSE_REG != 1) plotobj2 <- plotobj2 + annotate("text",x = 60,y = 80,label = "Lower Limit of Quantification",colour = "black",size = 4)
				plotobj2 <- plotobj2 + scale_x_continuous("\nTime (hours)")
			# Plot on linear or log-scale depending on input
				plotobj2 <- plotobj2 + scale_y_continuous("Doxycycline Concentration (microg/L)\n",breaks = plot.breaks,labels = plot.breaks,lim = c(0,max.CONC))
				print(plotobj2)
		})	#Brackets closing "renderPlot"

	# Left summary table
		output$Rtable1 <- renderTable({
		  # Read in the necessary reactive expressions
			  doryxMPC.data <- RdoryxMPC.data()
				doryxTAB.data <- RdoryxTAB.data()
			  summary.function <- Rsummary.function()
				# Subset for the last time-point for each individual and combine the two formulation data frames together
					doryxMPC.data.last <- ddply(doryxMPC.data, .(ID), oneperID)
					doryxTAB.data.last <- ddply(doryxTAB.data, .(ID), oneperID)
					data.last <- rbind(doryxMPC.data.last,doryxTAB.data.last)

				# Summarise results for fed/fasted for DoryxMPC data
					if (input$SIM_STUDY == 1) {
						# Summarise AUC
					    AUC.table <- ddply(doryxMPC.data.last, .(FED), function(doryxMPC.data.last) summary.function(doryxMPC.data.last$AUC))
					    AUC.table$Variable <- "AUC (microg*h/L)"
				    # Summarise Cmax (value will be found at time = 96)
					    Cmax.table <- ddply(doryxMPC.data.last, .(FED), function(doryxMPC.data.last) summary.function(doryxMPC.data.last$Cmax))
					    Cmax.table$Variable <- "Cmax (microg/L)"
				    # Summarise Tmax (value will be found at time = 96)
					    Tmax.table <- ddply(doryxMPC.data.last, .(FED), function(doryxMPC.data.last) summary.function(doryxMPC.data.last$Tmax))
					    Tmax.table$Variable <- "Tmax (h)"
					}
				# Summarise results for MPC/TAB for fasted
					if (input$SIM_STUDY == 2) {
						data.last.fasted <- data.last[data.last$FED == 0,]
						# Summarise AUC
					    AUC.table <- ddply(data.last.fasted, .(TRT), function(data.last.fasted) summary.function(data.last.fasted$AUC))
					    AUC.table$Variable <- "AUC (microg*h/L)"
				    # Summarise Cmax (value will be found at time = 96)
					    Cmax.table <- ddply(data.last.fasted, .(TRT), function(data.last.fasted) summary.function(data.last.fasted$Cmax))
					    Cmax.table$Variable <- "Cmax (microg/L)"
				    # Summarise Tmax (value will be found at time = 96)
					    Tmax.table <- ddply(data.last.fasted, .(TRT), function(data.last.fasted) summary.function(data.last.fasted$Tmax))
					    Tmax.table$Variable <- "Tmax (h)"
					}
				# Summarise results for male/female for doryxMPC.data
					if (input$SIM_STUDY == 3) {
						# Summarise AUC
					    AUC.table <- ddply(doryxMPC.data.last, .(SEX), function(doryxMPC.data.last) summary.function(doryxMPC.data.last$AUC))
					    AUC.table$Variable <- "AUC (microg*h/L)"
				    # Summarise Cmax (value will be found at time = 96)
					    Cmax.table <- ddply(doryxMPC.data.last, .(SEX), function(doryxMPC.data.last) summary.function(doryxMPC.data.last$Cmax))
					    Cmax.table$Variable <- "Cmax (microg/L)"
				    # Summarise Tmax (value will be found at time = 96)
					    Tmax.table <- ddply(doryxMPC.data.last, .(SEX), function(doryxMPC.data.last) summary.function(doryxMPC.data.last$Tmax))
					    Tmax.table$Variable <- "Tmax (h)"
					}
				# Return data frame
					table1 <- rbind(AUC.table,Cmax.table,Tmax.table)
					if (input$SIM_STUDY == 1) {
					  table1$FED[table1$FED == 0] <- "Fasted"
					  table1$FED[table1$FED == 1] <- "Fed"
						if (input$PI == 1) {
							table1 <- data.frame(Status = table1$FED,Median = table1$Median,Variable = table1$Variable)
						}
						if (input$PI > 1) {
							table1 <- data.frame(Status = table1$FED,Median = table1$Median,CIlo = table1$CIlo,CIhi = table1$CIhi,Variable = table1$Variable)
						}
						table1
					}
					if (input$SIM_STUDY == 2){
					  table1$TRT[table1$TRT == 1] <- "Doryx MPC"
					  table1$TRT[table1$TRT == 2] <- "Doryx Tablet"
						if (input$PI == 1) {
							table1 <- data.frame(Formulation = table1$TRT,Median = table1$Median,Variable = table1$Variable)
						}
						if (input$PI > 1) {
							table1 <- data.frame(Formulation = table1$TRT,Median = table1$Median,CIlo = table1$CIlo,CIhi = table1$CIhi,Variable = table1$Variable)
						}
						table1
					}
					if (input$SIM_STUDY == 3) {
					  table1$SEX[table1$SEX == 0] <- "Female"
					  table1$SEX[table1$SEX == 1] <- "Male"
						if (input$PI == 1) {
							table1 <- data.frame(Gender = table1$SEX,Median = table1$Median,Variable = table1$Variable)
						}
						if (input$PI > 1) {
							table1 <- data.frame(Gender = table1$SEX,Median = table1$Median,CIlo = table1$CIlo,CIhi = table1$CIhi,Variable = table1$Variable)
						}
						table1
					}
			  table1
		})	#Brackets closing "renderTable"

	# Right summary table
		output$Rtable2 <- renderTable({
		  # Read in the necessary reactive expressions
			  doryxMPC.data <- RdoryxMPC.data()
				doryxTAB.data <- RdoryxTAB.data()
			  summary.function <- Rsummary.function()
				# Subset for the last time-point for each individual and combine the two formulation data frames together
					doryxMPC.data.last <- ddply(doryxMPC.data, .(ID), oneperID)
					doryxTAB.data.last <- ddply(doryxTAB.data, .(ID), oneperID)
					data.last <- rbind(doryxMPC.data.last,doryxTAB.data.last)

				# Summarise results for fed/fasted for doryxTAB data
					if (input$SIM_STUDY == 1) {
						# Summarise AUC
					    AUC.table <- ddply(doryxTAB.data.last, .(FED), function(doryxTAB.data.last) summary.function(doryxTAB.data.last$AUC))
					    AUC.table$Variable <- "AUC (microg*h/L)"
				    # Summarise Cmax (value will be found at time = 96)
					    Cmax.table <- ddply(doryxTAB.data.last, .(FED), function(doryxTAB.data.last) summary.function(doryxTAB.data.last$Cmax))
					    Cmax.table$Variable <- "Cmax (microg/L)"
				    # Summarise Tmax (value will be found at time = 96)
					    Tmax.table <- ddply(doryxTAB.data.last, .(FED), function(doryxTAB.data.last) summary.function(doryxTAB.data.last$Tmax))
					    Tmax.table$Variable <- "Tmax (h)"
					}
				# Summarise results for MPC/TAB for fed
					if (input$SIM_STUDY == 2) {
						data.last.fasted <- data.last[data.last$FED == 1,]
						# Summarise AUC
					    AUC.table <- ddply(data.last.fasted, .(TRT), function(data.last.fasted) summary.function(data.last.fasted$AUC))
					    AUC.table$Variable <- "AUC (microg*h/L)"
				    # Summarise Cmax (value will be found at time = 96)
					    Cmax.table <- ddply(data.last.fasted, .(TRT), function(data.last.fasted) summary.function(data.last.fasted$Cmax))
					    Cmax.table$Variable <- "Cmax (microg/L)"
				    # Summarise Tmax (value will be found at time = 96)
					    Tmax.table <- ddply(data.last.fasted, .(TRT), function(data.last.fasted) summary.function(data.last.fasted$Tmax))
					    Tmax.table$Variable <- "Tmax (h)"
					}
				# Summarise results for male/female for doryxTAB.data
					if (input$SIM_STUDY == 3) {
						# Summarise AUC
					    AUC.table <- ddply(doryxTAB.data.last, .(SEX), function(doryxTAB.data.last) summary.function(doryxTAB.data.last$AUC))
					    AUC.table$Variable <- "AUC (microg*h/L)"
				    # Summarise Cmax (value will be found at time = 96)
					    Cmax.table <- ddply(doryxTAB.data.last, .(SEX), function(doryxTAB.data.last) summary.function(doryxTAB.data.last$Cmax))
					    Cmax.table$Variable <- "Cmax (microg/L)"
				    # Summarise Tmax (value will be found at time = 96)
					    Tmax.table <- ddply(doryxTAB.data.last, .(SEX), function(doryxTAB.data.last) summary.function(doryxTAB.data.last$Tmax))
					    Tmax.table$Variable <- "Tmax (h)"
					}
				# Return data frame
					table2 <- rbind(AUC.table,Cmax.table,Tmax.table)
					if (input$SIM_STUDY == 1) {
					  table2$FED[table2$FED == 0] <- "Fasted"
					  table2$FED[table2$FED == 1] <- "Fed"
						if (input$PI == 1) {
							table2 <- data.frame(Status = table2$FED,Median = table2$Median,Variable = table2$Variable)
						}
						if (input$PI > 1) {
							table2 <- data.frame(Status = table2$FED,Median = table2$Median,CIlo = table2$CIlo,CIhi = table2$CIhi,Variable = table2$Variable)
						}
						table2
					}
					if (input$SIM_STUDY == 2){
					  table2$TRT[table2$TRT == 1] <- "Doryx MPC"
					  table2$TRT[table2$TRT == 2] <- "Doryx Tablet"
						if (input$PI == 1) {
							table2 <- data.frame(Formulation = table2$TRT,Median = table2$Median,Variable = table2$Variable)
						}
						if (input$PI > 1) {
							table2 <- data.frame(Formulation = table2$TRT,Median = table2$Median,CIlo = table2$CIlo,CIhi = table2$CIhi,Variable = table2$Variable)
						}
						table2
					}
					if (input$SIM_STUDY == 3) {
					  table2$SEX[table2$SEX == 0] <- "Female"
					  table2$SEX[table2$SEX == 1] <- "Male"
						if (input$PI == 1) {
							table2 <- data.frame(Gender = table2$SEX,Median = table2$Median,Variable = table2$Variable)
						}
						if (input$PI > 1) {
							table2 <- data.frame(Gender = table2$SEX,Median = table2$Median,CIlo = table2$CIlo,CIhi = table2$CIhi,Variable = table2$Variable)
						}
						table2
					}
			  table2
		})	#Brackets closing "renderTable"

  #############
  ##_SESSION_##
  #############
  # Close the R session when Chrome closes
  session$onSessionEnded(function() {
    stopApp()
  })
})  #Brackets closing "shinyServer" function
