# R script for simulating a population from Ash's doxycycline model
# ------------------------------------------------------------------------------
# Load package libraries
	library(ggplot2)	#Plotting
	library(grid)	#Plotting
	library(plyr)	#Split and rearrange data, ddply function
	library(dplyr)	#New plyr - required for mrgsolve
	library(mrgsolve)	#Metrum differential equation solver for pharmacometrics
# Define a custom ggplot2 theme
	theme_bw2 <- theme_set(theme_bw(base_size = 16))

# ------------------------------------------------------------------------------
# Define time sequence - using mrgsolve's tgrid function
	TIME.tgrid <- c(tgrid(0,3,0.25),tgrid(4,12,2),tgrid(16,32,8))
# Set number of individuals that make up the 95% prediction intervals
	n <- 1000
	nsim <- n+1	#Add an individual for the "PRED"
# 95% prediction interval functions - calculate the 2.5th and 97.5th percentiles
	CI95lo <- function(x) quantile(x,probs = 0.025)
	CI95hi <- function(x) quantile(x,probs = 0.975)
# Set seed for reproducible numbers
	set.seed(123456)

# ------------------------------------------------------------------------------
# Define the model parameters and equations
	# Using mrgsolve - analytical solutions
	# This compiled model is used for simulating n individuals and their concentration-time profiles
		code <- '
		$INIT			DEPOT = 0,
							TRANS1 = 0,
							TRANS2 = 0,
							CENT = 0,
							PERI = 0

		$PARAM		POPCL = 4.63,	//THETA4
							POPV = 55.2,	//THETA3
							POPCLP1 = 11.3,	//THETA6
							POPVP1 = 49.8,	//THETA5
							POPKTR = 2,	//THETA7
							POPF = 1,
							F1XC = 0.863,	//THETA8
							F1CAP = 0.978,	//THETA9
							COVFED = -0.209,	//THETA10
							COVFED2 = -0.549,	//THETA11
							ALAG1 = 0.115,	//THETA12
							FTLAG2 = 0.203,	//THETA13
							COVFEDF = 0.105,	//THETA14
							COVSEX = 0.144,	//THETA15
							COVSTDF = 1.157,	//Study averaged F for simulation
							COVSTDKTR = 0.894,	//Study averaged KTR for simulation
							COVSTDCL = 0.789,	//Study averaged CL for simulation
							COVSTDV = 0.786,	//Study averaged V for simulation
							FED = 0,
							SEX = 0,
							FFM = 70,
							TRT = 1,
							PER = 1

		$OMEGA		name = "BSV"
							block = TRUE
							labels = s(BSV_CL,BSV_KTR,BSV_VP1,BSV_V)
							0.0373
							0.0229 0.0796
							0.0106 -0.01 0.0229
							0.0522 0.0936 -0.00506 0.141

		$OMEGA		name = "BOV"
							block = FALSE
							labels = s(BOV_CL1,BOV_CL2,BOV_KTR1,BOV_KTR2)
							0.0183
							0.0183
							0.0738
							0.0738

		$SIGMA		block = FALSE
							labels = s(ERR_PRO,ERR_ADD)
							0.196
							19.8

		$MAIN			// Covariate effects
							double FEDCOV2 = 1;
							if (FED == 1) FEDCOV2 = 1+COVFEDF;
							double SEXCOV = 1;
							if (SEX == 0) SEXCOV = 1+COVSEX;
							double FEDCOV = 1;
							if (FED == 1 & TRT == 1) FEDCOV = 1+COVFED2;
							if (FED == 1 & TRT == 2) FEDCOV = 1+COVFED;
							if (FED == 1 & TRT == 3) FEDCOV = 1+COVFED;

							// Between-occassion variability
								// Clearance
									double BOV_CL = BOV_CL1;
									if (PER == 2) BOV_CL = BOV_CL2;
									double ETA_CL = BSV_CL+BOV_CL;
								// Transit
									double BOV_KTR = BOV_KTR1;
									if (PER == 2) BOV_KTR = BOV_KTR2;
									double ETA_KTR = BSV_KTR+BOV_KTR;
								// Volume - peripheral
									double ETA_VP1 = BSV_VP1;
								// Volume - central
									double ETA_V = BSV_V;

							// Population parameter values
							if (ID == 1) ETA_CL = 0;
							if (ID == 1) ETA_KTR = 0;
							if (ID == 1) ETA_V = 0;
							if (ID == 1) ETA_VP1 = 0;

							// Individual parameter values
							double CL = POPCL*pow(FFM/70,0.75)*exp(ETA_CL)*COVSTDF*COVSTDCL*FEDCOV2*SEXCOV;
							double V = POPV*(FFM/70)*exp(ETA_V)*COVSTDF*COVSTDV*FEDCOV2;
							double CLP1 = POPCLP1*pow(FFM/70,0.75)*COVSTDF*FEDCOV2;
							double VP1 = POPVP1*(FFM/70)*exp(ETA_VP1);
							double KTR = POPKTR*exp(ETA_KTR)*FEDCOV*COVSTDKTR;
							double F = POPF;
							if (TRT == 1) F = F1XC;
							if (TRT == 3) F = F1CAP;
							F_DEPOT = F;

							// Micro-rate constants
							double K12 = KTR;	//DEPOT to TRANS1
							double K23 = KTR;	//TRANS1 to TRANS2
							double K34 = KTR;	//TRANS2 to CENT
							double K45 = CLP1/V;	//CENT to PERI
							double K54 = CLP1/VP1;	//PERI to CENT
							double K46 = CL/V;	//CENT to eliminated

							// Lag time
							double TLAG = 0;
							if (TRT == 1) TLAG = ALAG1;
							if (TRT == 2) TLAG = ALAG1;
							double FTLAG = 0;
							if (FED == 1) FTLAG = FTLAG2;
							double ALAG_DEPOT = TLAG+FTLAG;

		$ODE			// Differential equations
							dxdt_DEPOT = -K12*DEPOT;
							dxdt_TRANS1 = K12*DEPOT -K23*TRANS1;
							dxdt_TRANS2 = K23*TRANS1 -K34*TRANS2;
							dxdt_CENT = K34*TRANS2 -K45*CENT +K54*PERI -K46*CENT;
							dxdt_PERI = K46*CENT -K54*PERI;

		$TABLE		table(IPRE) = CENT/V;
							table(DV) = table(IPRE)*(1+ERR_PRO)+ERR_ADD;

		$CAPTURE	CL V CLP1 VP1 KTR F ETA_CL ETA_V ETA_VP1 ETA_KTR
		'
	# Compile the model code
		mod <- mcode("popDOXY",code)
		# There is opportunity to simply update model parameters after the model code has been compiled

# ------------------------------------------------------------------------------
# Simulate concentration-time profiles for the population
	input.conc.data <- expand.ev(ID = 1:nsim,amt = 120*1000,evid = 1,cmt = 1,time = 0)
		# n individuals
		# amt in microg
		# evid = 1; dosing event
		# cmt = 1; dose goes into compartment 1 = depot
		# time = 0; dose at time = 0
	conc.data <- mod %>% data_set(input.conc.data) %>% mrgsim(tgrid = TIME.tgrid)
	# Test speed of mrgsolve
		system.time(conc.data <- mod %>% data_set(input.conc.data) %>% mrgsim(tgrid = TIME.tgrid))
	conc.data <- as.data.frame(conc.data)	#Convert to a data frame so that it is more useful for me!

# ------------------------------------------------------------------------------
# Plot results
	plotobj1 <- NULL
	plotobj1 <- ggplot()
	plotobj1 <- plotobj1 + geom_line(aes(x = time,y = IPRE),data = conc.data[conc.data$ID == 1,],colour = "red")
	plotobj1 <- plotobj1 + stat_summary(aes(x = time,y = IPRE),data = conc.data[conc.data$ID != 1,],geom = "ribbon",fun.ymin = "CI95lo",fun.ymax = "CI95hi",fill = "red",alpha = 0.3)
	plotobj1 <- plotobj1 + scale_x_continuous("\nTime (hours)")
	plotobj1 <- plotobj1 + scale_y_continuous("Doxycycline Concentration (microg/L)\n")
	print(plotobj1)
