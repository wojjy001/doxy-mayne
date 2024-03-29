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
            selectInput("DOSE_DORYXMPC1","Doryx MPC Dose:",choices = list("120 mg" = 1,"240 mg" = 2)),
            radioButtons("NUMDOSE_DORYXMPC1","Number of Doses:",choices = list("Single dose" = 1,"Multiple doses (once daily)" = 2)),
            conditionalPanel(condition = "input.NUMDOSE_DORYXMPC1 == 1",
              checkboxInput("SUMSTATS_DORYXMPC1","Show summary statistics",value = FALSE) #Calculate Tmax, Cmax and AUC. Show prediction intervals if a "type" of prediction intervals is previously selected (as above). Show for each facet if "FACET" is selected above.
            ),  #Brackets closing "condtionalPanel"
            hr(),
            checkboxInput("ADD_DORYXTAB1","Compare with Doryx Tablet",value = FALSE),
            conditionalPanel(condition = "input.ADD_DORYXTAB1",
              selectInput("DOSE_DORYXTAB1","Doryx Tablet Dose:",choices = list("100 mg" = 1,"200 mg" = 2)),
              radioButtons("NUMDOSE_DORYXTAB1","Number of Doses:",choices = list("Single dose" = 1,"Multiple doses (once daily)" = 2)),
              conditionalPanel(condition = "input.NUMDOSE_DORYXMPC1 == 1",
                checkboxInput("SUMSTATS_DORYXTAB1","Show summary statistics",value = FALSE) #Calculate Tmax, Cmax and AUC. Show prediction intervals if a "type" of prediction intervals is previously selected (as above). Show for each facet if "FACET" is selected above.
              )  #Brackets closing "condtionalPanel"
            )  #Brackets closing "condtionalPanel"
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
              conditionalPanel(condition = "input.ADD_DORYXTAB1",
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
              )  #Brackets closing "conditionalPanel"
            ) #Brackets closing "fixedRow"
          )  #Brackets closing "column"
        ) #Brackets closing "fixedRow"
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
