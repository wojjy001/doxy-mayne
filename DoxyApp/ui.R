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
      checkboxInput("LOGS","Plot on a log-scale",value = FALSE),  #Application initiation will plot concentrations on a linear scale
      selectInput("PI","Prediction intervals:",choices = list("No Prediction Intervals" = 1,"90% Prediction Intervals" = 2,"95% Prediction Intervals" = 3))
		)	#Brackets closing "sidebarMenu"
	) #Brackets closing "dashboardSidebar"
# Application's body
body <-
	dashboardBody(
    tags$head(
			tags$link(rel = "stylesheet",type = "text/css",href = "custom.css")
		),    
		tabItems(
			tabItem(tabName = "fed-status",
        fixedRow(
          column(3,
            selectInput("DOSE1","Dose Regimen:",choices = list("120 mg Doryx MPC and 100 mg Doryx tablet" = 1,"240 mg Doryx MPC and 200 mg Doryx tablet" = 2,"Clinical Regimen Comparison"= 3),width = 500),
            checkboxInput("SUMSTATS1","Show summary statistics",value = FALSE) #Calculate Tmax, Cmax and AUC. Show prediction intervals if a "type" of prediction intervals is previously selected (as above). Show for each facet if "FACET" is selected above.
          ),  #Brackets closing "column"
          column(9,
            fixedRow(
              column(6,
                box(
                  plotOutput("RdoryxMPC.plot"),
                  conditionalPanel(condition = "input.SUMSTATS_DORYXMPC1",
                    conditionalPanel(condition = "input.NUMDOSE_DORYXMPC1 == 1",
                      tableOutput("RdoryxMPC.table")
                    ) #Brackets closing "conditionalPanel"
                  ),  #Brackets closing "condtionalPanel"
                  title = strong("Concentration-Time Profile - Doryx MPC"),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 12
                ) #Brackets closing "box"
              ),  #Brackets closing "column"
              column(6,
                box(
                  plotOutput("RdoryxTAB.plot"),
                  conditionalPanel(condition = "input.SUMSTATS_DORYXTAB1",
                    conditionalPanel(condition = "input.NUMDOSE_DORYXTAB1 == 1",
                      tableOutput("RdoryxTAB.table")
                    ) #Brackets closing "conditionalPanel"
                  ),  #Brackets closing "conditionalPanel"
                  title = strong("Concentration-Time Profile - Doryx Tablet"),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 12
                ) #Brackets closing "box"
              )  #Brackets closing "column"
            ) #Brackets closing "fixedRow"
          )  #Brackets closing "column"
        ) #Brackets closing "fixedRow"
			),	#Brackets closing "tabItem" for "fed-status"
			tabItem(tabName = "form-status",
			        fixedRow(
			          column(3,
			                 selectInput("DOSE2","Formulation Comparison:",choices = list("120 mg Doryx MPC versus 100 mg Doryx tablet" = 1,"240 mg Doryx MPC versus 200 mg Doryx tablet" = 2,"Clinical Regimen Comparison"= 3),width = 500),
			                 checkboxInput("SUMSTATS1","Show summary statistics",value = FALSE) #Calculate Tmax, Cmax and AUC. Show prediction intervals if a "type" of prediction intervals is previously selected (as above). Show for each facet if "FACET" is selected above.
			   )
			  ) 
			 ), 
			tabItem(tabName = "gender-status",
        h4("Compare Male versus Female")
      ) #Brackets closing "tabItem" for "gender-status"
		)  #Brackets closing "tabItems"
	) #Brackets closing "dashboardBody"
# ------------------------------------------------------------------------------
# User-interface Object
dashboardPage(header,sidebar,body,skin = "blue")
