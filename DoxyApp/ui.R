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
	)	# Brackets closing "dashboardHeader"
# Application's sidebar
sidebar <-
	dashboardSidebar(
		width = 300,	#Width of sidebar the same as width of header
		sidebarMenu(
		  menuItem("About",tabName = "about",icon = icon("child")),
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
            selectInput("DOSE1","Dose Regimen:",choices = list("120 mg Doryx MPC and 100 mg Doryx Tablet" = 1,"Clinical Regimen 1 (Standard)" = 2,"Clinical Regimen 2 (Severe Infection)"= 3),width = 500),
            conditionalPanel(condition = "input.DOSE1 == 1",
              h4("Dose Regimen:"),
              h5("Doryx MPC: A single 120 mg dose"),
              h5("Doryx Tablet: A single 100 mg dose")
            ),  # Brackets closing "conditionalPanel"
            conditionalPanel(condition = "input.DOSE1 == 2",
              h4("Dose Regimen:"),
              h5("Doryx MPC: 120 mg every 12 hours on the first day, followed by six 120 mg doses at 24 hour intervals"),
              h5("Doryx Tablet: 100 mg every 12 hours on the first day, followed by six 100 mg doses at 24 hour intervals")
            ),  # Brackets closing "conditionalPanel"
            conditionalPanel(condition = "input.DOSE1 == 3",
              h4("Dose Regimen:"),
              h5("Doryx MPC: 120 mg every 12 hours for 7 days"),
              h5("Doryx Tablet: 100 mg every 12 hours for 7 days")
            ),  # Brackets closing "conditionalPanel"
            br(),
            checkboxInput("SUMSTATS1","Show summary statistics",value = FALSE), # Calculate Tmax, Cmax and AUC. Show prediction intervals if a "type" of prediction intervals is previously selected (as above). Show for each facet if "FACET" is selected above.
            br(),
            h3("RED = Fasted Status",style = "color:red"),
            h3("BLUE = Fed Status",style = "color:blue")
          ),  # Brackets closing "column"
          column(9,
            fixedRow(
              column(6,
                box(
                  plotOutput("RdoryxMPC.plot1"),
                  conditionalPanel(condition = "input.SUMSTATS1",
                    tableOutput("RdoryxMPC.table1")
                  ), # Brackets closing "conditionalPanel"
                  title = strong("Concentration-Time Profile - Doryx MPC"),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 12
                ) # Brackets closing "box"
              ),  # Brackets closing "column"
              column(6,
                box(
                  plotOutput("RdoryxTAB.plot1"),
                  conditionalPanel(condition = "input.SUMSTATS1",
                    tableOutput("RdoryxTAB.table1")
                  ), # Brackets closing "conditionalPanel"
                  title = strong("Concentration-Time Profile - Doryx Tablet"),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 12
                ) # Brackets closing "box"
              )  # Brackets closing "column"
            ) # Brackets closing "fixedRow"
          )  # Brackets closing "column"
        ) # Brackets closing "fixedRow"
			),	# Brackets closing "tabItem" for "fed-status"
			tabItem(tabName = "form-status",
        fixedRow(
          column(3,
                 selectInput("DOSE2","Dose Regimen:",choices = list("120 mg Doryx MPC versus 100 mg Doryx Tablet" = 1,"Clinical Regimen 1 (Standard)" = 2,"Clinical Regimen 2 (Severe Infection)"= 3),width = 500),
                 conditionalPanel(condition = "input.DOSE2 == 1",
                                  h4("Dose Regimen:"),
                                  h5("Doryx MPC: A single 120 mg dose"),
                                  h5("Doryx Tablet: A single 100 mg dose")
                 ),  # Brackets closing "conditionalPanel"
                 conditionalPanel(condition = "input.DOSE2 == 2",
                                  h4("Dose Regimen:"),
                                  h5("Doryx MPC: 120 mg every 12 hours on the first day, followed by six 120 mg doses at 24 hour intervals"),
                                  h5("Doryx Tablet: 100 mg every 12 hours on the first day, followed by six 100 mg doses at 24 hour intervals")
                 ),  # Brackets closing "conditionalPanel"
                 conditionalPanel(condition = "input.DOSE2 == 3",
                                  h4("Dose Regimen:"),
                                  h5("Doryx MPC: 120 mg every 12 hours for 7 days"),
                                  h5("Doryx Tablet: 100 mg every 12 hours for 7 days")
                 ),  # Brackets closing "conditionalPanel"
                 br(),
            checkboxInput("SUMSTATS2","Show summary statistics",value = FALSE), # Calculate Tmax, Cmax and AUC. Show prediction intervals if a "type" of prediction intervals is previously selected (as above). Show for each facet if "FACET" is selected above.
            br(),
            h3("RED = Doryx MPC", style = "color:red"),
            h3("BLUE = Doryx Tablet", style = "color:blue")
			    ),  # Brackets closing "column"
			    column(9,
	          fixedRow(
	            column(6,
                box(
                  plotOutput("RformFasted.plot"),
                  conditionalPanel(condition = "input.SUMSTATS2",
                                   tableOutput("Rformfast.table2")
                  ), # Brackets closing "conditionalPanel"
                  title = strong("Concentration-Time Profile - Fasted"),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 12
                ) # Brackets closing "box"
	            ),  # Brackets closing "column"
	            column(6,
                box(
                  plotOutput("RformFed.plot"),
                  conditionalPanel(condition = "input.SUMSTATS2",
                                   tableOutput("Rformfed.table2")
                  ), # Brackets closing "conditionalPanel"
                  title = strong("Concentration-Time Profile - Fed"),
                  solidHeader = TRUE,
                  status = "primary",
                  width = 12
                ) # Brackets closing "box"
              )  # Brackets closing "column"
            ) # Brackets closing "fixedRow"
	        )  # Brackets closing "column"
	      )  # Brackets closing "fixedRow"
			),  # Brackets closing "tabItem" for "form-status"
			tabItem(tabName = "gender-status",
			fixedRow(
        column(3,
               selectInput("DOSE3","Dose Regimen:",choices = list("120 mg Doryx MPC and 100 mg Doryx Tablet" = 1,"Clinical Regimen 1 (Standard)" = 2,"Clinical Regimen 2 (Severe Infection)"= 3),width = 500),
               conditionalPanel(condition = "input.DOSE3 == 1",
                                h4("Dose Regimen:"),
                                h5("Doryx MPC: A single 120 mg dose (fasted)"),
                                h5("Doryx Tablet: A single 100 mg dose (fasted)")
               ),  # Brackets closing "conditionalPanel"
               conditionalPanel(condition = "input.DOSE3 == 2",
                                h4("Dose Regimen:"),
                                h5("Doryx MPC: 120 mg every 12 hours on the first day, followed by six 120 mg doses at 24 hour intervals (fasted)"),
                                h5("Doryx Tablet: 100 mg every 12 hours on the first day, followed by six 100 mg doses at 24 hour intervals (fasted)")
               ),  # Brackets closing "conditionalPanel"
               conditionalPanel(condition = "input.DOSE3 == 3",
                                h4("Dose Regimen:"),
                                h5("Doryx MPC: 120 mg every 12 hours for 7 days (fasted)"),
                                h5("Doryx Tablet: 100 mg every 12 hours for 7 days (fasted)")
               ),  # Brackets closing "conditionalPanel"
               br(),
               checkboxInput("SUMSTATS3","Show summary statistics",value = FALSE), # Calculate Tmax, Cmax and AUC. Show prediction intervals if a "type" of prediction intervals is previously selected (as above). Show for each facet if "FACET" is selected above.
               br(),
               h3("RED = Female", style = "color:red"),
               h3("BLUE = Male", style = "color:blue")
	        ),  # Brackets closing "column"
        column(9,
               fixedRow(
                 column(6,
                        box(
                          plotOutput("RdoryxMPCSex.plot"),
                          conditionalPanel(condition = "input.SUMSTATS3",
                                           tableOutput("RdoryxMPCSEX.table3")
                          ), # Brackets closing "conditionalPanel"
                          title = strong("Concentration-Time Profile - Doryx MPC - Fasted"),
                          solidHeader = TRUE,
                          status = "primary",
                          width = 12
                        ) # Brackets closing "box"
                 ),  # Brackets closing "column"
                 column(6,
                        box(
                          plotOutput("RdoryxTABSex.plot"),
                          conditionalPanel(condition = "input.SUMSTATS3",
                                           tableOutput("RdoryxTABSEX.table3")
                          ), # Brackets closing "conditionalPanel"
                          title = strong("Concentration-Time Profile - Doryx Tablet - Fasted"),
                          solidHeader = TRUE,
                          status = "primary",
                          width = 12
                        ) # Brackets closing "box"
                 )  # Brackets closing "column"
               ) # Brackets closing "fixedRow"
        )  # Brackets closing "column"
        )  # Brackets closing "fixedRow"
      ), # Brackets closing "tabItem" for "gender-status"
			tabItem(tabName = "about",
			        br(), 
			        p(strong("Note! This",a("Shiny",href = "http://www.rstudio.com/shiny"),"application was developed with the utilisation of the",a("mrgsolve",href = "http://mrgsolve.github.io/user_guide/"),"package.")),
			        p("This application has five tabs. Currently the 'About' tab is open which describes the application. The 'Compare Fed versus Fasted', 'Compare Doryx MPC versus Doryx Tablet' and 'Compare Male versus Female' tabs allow users to simulate and plot doxycycline concentrations following various dosage regimens. The final population pharmacokinetic model of doxycycline was a 2-compartment model with oral absorption described by 2-transit compartments",em("(See Figure 1)."),"The pharmacokinetic model was developed using",strong("single- and multi-dose"), "pharmacokinetic data of eight Phase I clinical trials comparing the Doryx tablet, Doryx capsule or Doryx MPC formulations. The 'mrgsolve Model Code' tab outlines the mrgsolve code used to describe the developed model."),

			        img(src = "FinalPKmodelDoxy.png"),
			        p(strong("Figure 1: A schematic diagram of the final population pharmacokinetic model of doxycycline")),
			        p("'Plotting Features' allow users to plot with confidence intervals or on a log-scale"),
			        p(strong("Note!"),"Multi-dose data of Doryx MPC was not used to develop the model, and therefore, these types of simulations are extrapolations."),
			        p("Simulated concentrations in this application are based on 1000 simulated subjects with the selected dosing design."),
			        p("Doryx MPC doses above 120mg and multi-dose data of Doryx MPC were not used to develop the model, and therefore, these types of simulations are extrapolations."),
			        p(strong("Important!"),"Click the", strong(em("Start Simulation")),  "button to update the plot every time you change the dosing regemin or the slider values."),
			        br(),
			        br(),
			        p(strong("Developers:")),
			        p("Ahmad Abuhelwa, University of South Australia"),
			        p("Jessica Wojciechowski, University of South Australia"),
			        p("Ashley M Hopkins, University of South Australia,", a("ashley.hopkins@mymail.unisa.edu.au", href = "mailto:ashley.hopkins@mymail.unisa.edu.au")),
			        br(),
			        p(strong("References:")),
			        p("(1)", strong("Abuhelwa AY, Foster DJ, Upton RN. 2015."), "ADVAN-style analytical solutions for common pharmacokinetic models. J Pharmacol Toxicol Methods 73:42-48.")
			) # Brackets closing "tabItem" for "about"
		)  # Brackets closing "tabItems"
	) # Brackets closing "dashboardBody"
# ------------------------------------------------------------------------------
# User-interface Object
  dashboardPage(header,sidebar,body,skin = "blue")
