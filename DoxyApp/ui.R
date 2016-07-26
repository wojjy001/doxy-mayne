# ui.R script for DoxyApp
# The user-interface and widget input for the Shiny application is defined here
# Sends user-defined input to server.R, calls created output from server.R
# Now using shinydashboard for the user-interface
# ------------------------------------------------------------------------------
# Application's header
header <-
  dashboardHeader(
		title = "Doxycycline",
		titleWidth = 250
	)	# Brackets closing "dashboardHeader"
# Application's sidebar
sidebar <-
	dashboardSidebar(
		width = 250,	# Width of sidebar the same as width of header
		sidebarMenu(
		  menuItem("About the Application",tabName = "about",icon = icon("question-circle"),
        menuSubItem("Objective",tabName = "objective",icon = icon("dot-circle-o")),
        menuSubItem("Model (mrgsolve code)",tabName = "model",icon = icon("file-code-o"))
      ),  # Brackets closing "menuItem"
      menuItem("Simulation Studies",tabName = "sim",icon = icon("line-chart")),
      hr(),
      fixedRow(
        h4(strong("Plotting Features")),  # Allow the user to control plot appearances from the sidebar
        align = "center"
      ),  # Brackets closing "fixedRow"
      selectInput("PI","Prediction intervals:",choices = list("No Prediction Intervals" = 1,"90% Prediction Intervals" = 2,"95% Prediction Intervals" = 3)),
      checkboxInput("SUMSTATS","Show summary statistics",value = FALSE) # Calculate Tmax, Cmax and AUC. Show prediction intervals if a "type" of prediction intervals is previously selected (as above). Show for each facet if "FACET" is selected above.
		)	# Brackets closing "sidebarMenu"
	) # Brackets closing "dashboardSidebar"
# Application's body
body <-
	dashboardBody(
    tags$head(
			tags$link(rel = "stylesheet",type = "text/css",href = "custom.css")
		),
		tabItems(
      tabItem(tabName = "objective",
        includeMarkdown("objective.Rmd")
			), # Brackets closing "tabItem" for "readme"
      tabItem(tabName = "model",
        pre(includeText("model.R"))
      ),  # Brackets closing "tabItem" for "model"
      tabItem(tabName = "sim",
        box(
          fixedRow(
            column(6,
              selectInput("SIM_STUDY","Simulation Study:",choices = list("Fed versus Fasted" = 1,"Doryx MPC versus Doryx Tablet" = 2,"Male versus Female" = 3),width = 400),
              fixedRow(
                column(3,
                  p(strong("Plot Legend:"))
                ),  # Brackets closing "column"
                column(9,
                  conditionalPanel(condition = "input.SIM_STUDY == 1",
                    p(strong("Fasted"),style = "color:#B22222"),
                    p(strong("Fed"),style = "color:#3c8dbc")
                  ),  # Brackets closing "conditionalPanel"
                  conditionalPanel(condition = "input.SIM_STUDY == 2",
                    p(strong("Doryx MPC"),style = "color:#B22222"),
                    p(strong("Doryx Tablet"),style = "color:#3c8dbc")
                  ),  # Brackets closing "conditionalPanel"
                  conditionalPanel(condition = "input.SIM_STUDY == 3",
                    p(strong("Female"),style = "color:#B22222"),
                    p(strong("Male"),style = "color:#3c8dbc")
                  )  # Brackets closing "conditionalPanel"
                ) # Brackets closing "column"
              ) # Brackets closing "fixedRow"
            ),  # Brackets closing "column"
            column(6,
              selectInput("DOSE_REG","Dose Regimen:",choices = list("Single dose" = 1,"Clinical Regimen 1 (standard multiple dose regimen)" = 2,"Clinical Regimen 2 (multiple dose regimen for severe infection)"= 3),width = 450),
              conditionalPanel(condition = "input.DOSE_REG == 1",
                p("120 mg Doryx MPC over 96 hours ",em("or")),
                p("100 mg Doryx Tablet over 96 hours")
              ),  # Brackets closing "conditionalPanel"
              conditionalPanel(condition = "input.DOSE_REG == 2",
                p("120 mg Doryx MPC every 12 hours on Day 1, then 120 mg every 24 hours on Days 2 to 7 ",em("or")),
                p("100 mg Doryx Tablet every 12 hours on Day 1, then 100 mg every 24 hours on Days 2 to 7")
              ),  # Brackets closing "conditionalPanel"
              conditionalPanel(condition = "input.DOSE_REG == 3",
                p("120 mg Doryx MPC every 12 hours for 7 days ",em("or")),
                p("120 mg Doryx Tablet every 12 hours for 7 days")
              )  # Brackets closing "conditionalPanel"
            )  # Brackets closing "column"
          ),  # Brackets closing "fixedRow"
          title = strong("Simulation Options"),
          solidHeader = TRUE,
          status = "primary",
          width = 12
        ),  # Brackets closing "box"
        box(
          fixedRow(
            column(6,
              conditionalPanel(condition = "input.SIM_STUDY == 1",
                h4(strong("Doryx MPC"))
              ),  # Brackets closing "conditionalPanel"
              conditionalPanel(condition = "input.SIM_STUDY == 2",
                h4(strong("Fasted"))
              ),  # Brackets closing "conditionalPanel"
              conditionalPanel(condition = "input.SIM_STUDY == 3",
                h4(strong("Doryx MPC"))
              ),  # Brackets closing "conditionalPanel"
              plotOutput("Rplot1"),
              conditionalPanel(condition = "input.SUMSTATS",
                hr(),
                tableOutput("Rtable1")
              ) # Brackets closing "conditionalPanel"
            ),  # Brackets closing "column"
            column(6,
              conditionalPanel(condition = "input.SIM_STUDY == 1",
                h4(strong("Doryx Tablet"))
              ),  # Brackets closing "conditionalPanel"
              conditionalPanel(condition = "input.SIM_STUDY == 2",
                h4(strong("Fed"))
              ),  # Brackets closing "conditionalPanel"
              conditionalPanel(condition = "input.SIM_STUDY == 3",
                h4(strong("Doryx Tablet"))
              ),  # Brackets closing "conditionalPanel"
              plotOutput("Rplot2"),
              conditionalPanel(condition = "input.SUMSTATS",
                hr(),
                tableOutput("Rtable2")
              ) # Brackets closing "conditionalPanel"
            ),  # Brackets closing "column"
            align = "center"
          ), # Brackets closing "fixedRow"
          title = strong("Simulated Concentration-Time Profiles"),
          solidHeader = TRUE,
          status = "primary",
          width = 12
        ) # Brackets closing "box"
      )  # Brackets closing "tabItem" for "sim"
		)  # Brackets closing "tabItems"
	) # Brackets closing "dashboardBody"
# ------------------------------------------------------------------------------
# User-interface Object
  dashboardPage(header,sidebar,body,skin = "blue")
