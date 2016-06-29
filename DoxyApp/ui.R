# ui.R script for PrototypeAPAP3
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
      menuItem("Compare Male versus Female",tabName = "gender-status",icon = icon("child"))
		)	#Brackets closing "sidebarMenu"
	) #Brackets closing "dashboardSidebar"
# Application's body
body <-
	dashboardBody(
		tabItems(
			tabItem(tabName = "fed-status",
        h4("Compare Fed versus Fasted")
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
