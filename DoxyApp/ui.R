# ui.R script for DoxyApp
# The user-interface and widget input for the Shiny application is defined here
# Sends user-defined input to server.R, calls created output from server.R
# Now using shinydashboard for the user-interface
# ------------------------------------------------------------------------------
# Application's header
header <-
  dashboardHeader(
		title = "Doxycycline",
		titleWidth = 300
	)	#Brackets closing "dashboardHeader"
# Application's sidebar
sidebar <-
	dashboardSidebar(
		width = 300,	#Width of sidebar the same as width of header
		sidebarMenu(
      menuItem("Compare Fed versus Fasted",tabName = "fed-status",icon = icon("child")),
      menuItem("Compare Doryx MPC versus Doryx Tablet",tabName = "form-status",icon = icon("child")),
      menuItem("Compare Male versus Female",tabName = "gender-status",icon = icon("child")),
      hr(),
      fixedRow(
        h4(strong("Plotting Features")),  #Allow the user to control plot appearances from the sidebar
        align = "center"
      ),  #Brackets closing "fixedRow"
      checkboxInput("FACET","Stratify by secondary factor",value = FALSE),  #Allow the user to facet the plot by factors other than that stated in the "compare" tab they have chosen. i.e., in "Compare Fed versus Fasted", facet the resultant plot by formulation status
      checkboxInput("SUMSTATS","Show summary statistics",value = FALSE), #Calculate Tmax, Cmax and AUC. Show prediction intervals if a "type" of prediction intervals is previously selected (as above). Show for each facet if "FACET" is selected above.
      checkboxInput("LOGS","Plot on a log-scale",value = FALSE),  #Application initiation will plot concentrations on a linear scale
      selectInput("PI","Prediction intervals:",choices = list("No Prediction Intervals" = 1,"90% Prediction Intervals" = 2,"95% Prediction Intervals" = 3))
		)	#Brackets closing "sidebarMenu"
	) #Brackets closing "dashboardSidebar"
# Application's body
body <-
	dashboardBody(
		tabItems(
			tabItem(tabName = "fed-status",
        h4("Compare Fed versus Fasted"),
        plotOutput("Rfed.plot")
			),	#Brackets closing "tabItem" for "fed-status"
      tabItem(tabName = "form-status",
        h4("Compare Doryx MPC versus Doryx Tablet")
      ),  #Brackets closing "tabItem" for "form-status"
      tabItem(tabName = "gender-status",
        h4("Compare Male versus Female")
      ) #Brackets closing "tabItem" for "gender-status"
		)  #Brackets closing "tabItems"
	) #Brackets closing "dashboardBody"
# ------------------------------------------------------------------------------
# User-interface Object
dashboardPage(header,sidebar,body,skin = "blue")
