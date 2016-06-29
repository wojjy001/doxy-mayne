# ui.R script for PrototypeAPAP3
# The user-interface and widget input for the Shiny application is defined here
# Sends user-defined input to server.R, calls created output from server.R
# Now using shinydashboard for the user-interface
# ------------------------------------------------------------------------------
# Application's header
header <-
  dashboardHeader(
		title = "Doxycycline",
		titleWidth = 250
	)	#Brackets closing "dashboardHeader"
# Application's sidebar
sidebar <-
	dashboardSidebar(
		width = 250,	#Width of sidebar the same as width of header
		sidebarMenu(
      menuItem("Patient Information",tabName = "patient",icon = icon("child")),
      menuItem("Overdose Information",tabName = "para-info",icon = icon("medkit")),
		  menuItem("Plot and Numerical Output",tabName = "results",icon = icon("line-chart"))
		)	#Brackets closing "sidebarMenu"
	) #Brackets closing "dashboardSidebar"
# Application's body
body <-
	dashboardBody(
		tabItems(
			tabItem(tabName = "patient",
				h4("Patient Information:"),	#Heading for Patient Information section
        fixedRow(
          column(6,
    				numericInput("MRN","Medical Record Number (MRN):",value = 000000,step = 1)  #Numeric input for patient's medical record number (or unit record number)
          ) #Brackets closing "column"
        ),  #Brackets closing "fixedRow"
				fixedRow(
					column(6,
						textInput("FNAME", "First Name:","First Name")	#Text input for patient's first name
          ),  #Brackets closing "column"
          column(6,
						textInput("LNAME","Last Name:","Last Name")	#Text input for patient's last name
          ) #Brackets closing "column"
        ),  #Brackets closing "fixedRow"
        fixedRow(
          column(6,
						dateInput("BDATE","Date of Birth (DD-MM-YYYY):",value = "1980-01-01",format = "dd-mm-yyyy",startview = "year")  #Date input for patient's date of birth
          ),  #Brackets closing "column"
          column(6,
						numericInput("WT","Weight (kg):",min = 0,max = 200,value = 70)	#Numeric input for patient weight
          ) #Brackets closing "fixedRow"
        ),  #Brackets closing "fixedRow"
        fixedRow(
          column(6,
						selectInput("SEX","Gender:",choices = list("Male" = 1,"Female" = 2),selected = 1)	#Select input for patient's gender
					)	#Brackets closing "column"
				)	#Brackets closing "fixedRow"
			)	#Brackets closing "tabItem" for "patient"
		)  #Brackets closing "tabItems"
	) #Brackets closing "dashboardBody"
# ------------------------------------------------------------------------------
# User-interface Object
dashboardPage(header,sidebar,body,skin = "blue")
