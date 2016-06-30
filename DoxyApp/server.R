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
			input.fed.data <- expand.ev(ID = 1:n,amt = 120*1000,evid = 1,cmt = 1,time = 0)
				# n individuals
				# amt in microg
				# evid = 1; dosing event
				# cmt = 1; dose goes into compartment 1 = depot
				# time = 0; dose at time = 0
			fed.data <- mod %>% data_set(input.fed.data) %>% mrgsim(tgrid = TIME.tgrid)
			fed.data <- as.data.frame(fed.data)	#Convert to a data frame so that it is more useful for me!
	})	#Brackets closing "Rfed.data"

	# Summarise Rfed.data in median and prediction intervals
	Rfed.summary <- reactive({
		# Read in the reactive fed.data
		fed.data <- Rfed.data()
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
		# Calculate the median and prediction intervals for calculations at each time-point
		fed.summary <- ddply(fed.data, .(time), function(fed.data) summary.function(fed.data$IPRE))
	})	#Brackets closing "Rfed.summary"

	############
	##_OUTPUT_##
	############
	# Plot simulation results of fed versus fasted
	output$Rfed.plot <- renderPlot({
		# Read in the reactive data frame for fed.summary
		fed.summary <- Rfed.summary()

		# Plot
		plotobj1 <- ggplot(fed.summary)
		plotobj1 <- plotobj1 + geom_line(aes(x = time,y = Median),colour = "red")
		if (input$PI > 1) plotobj1 <- plotobj1 + geom_ribbon(aes(x = time,ymin = CIlo,ymax = CIhi),fill = "red",alpha = 0.3)
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
