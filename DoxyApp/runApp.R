# Script for running the Shiny app without having to use RStudio
dir <- "/Volumes/Prosecutor/doxy-mayne/DoxyApp/"  #Application's directory
setwd(dir)  #Set the working directory to be the application's directory
library(shiny)  #Load the shiny package for the "runApp" function
runApp()
