# global.R script for DoxyApp
# Objects that are not reactive are written here
# This also a safe place for functions that are then used in server.R
# ------------------------------------------------------------------------------
# Load package libraries
  library(shiny)
  library(shinydashboard)  # Package for making cooler user-interface for Shiny applications
  library(ggplot2)  # Plotting
  library(grid)  # Plotting
  library(plyr)  # Split and rearrange data, ddply function
  library(dplyr)  # New plyr
  library(mrgsolve) # Metrum differential equation solver for pharmacometrics

# Define a custom ggplot2 theme
  theme_bw2 <- theme_set(theme_bw(base_size = 14))

# Source model code
  source("model.R")

# ------------------------------------------------------------------------------
# Define time sequence - using mrgsolve's tgrid function
  time.fine <- tgrid(0,96,0.25) # Intense sampling
  time.coarse <- tgrid(12,96,6)  # Less intense sampling later in the interval
  # Simulate concentrations for 96 hour time-period
    TIME.tgrid <- c(time.fine,time.coarse)
# Time sequences for the multiple dosing scenarios
  time.multiple <- unique(seq(from = 0,to = 240,by = 0.5))  # Intense sampling

# Set number of individuals that make up the 95% prediction intervals
	n <- 1000
# Set seed for reproducible numbers
	set.seed(123456)
# One per ID function
  oneperID <- function(x) tail(x,1)
