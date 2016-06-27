library(shiny)

shinyUI(fluidPage(
  
  #Application Title
  fluidRow(
    column(12,
           h2("Population Pharmacokinetic Modelling of Doxycycline following Doryx tablet or Doryx MPC administration in Healthy Subjects in the Fed or Fasted State", align = "center"))
    ),   #Closing fluidrow
  
  hr(),  #Inserting line break and horizontal line
  
  #Choosing overall page layout
  sidebarLayout(
    
    #Sidebar panel with widgets that demonstrate available options
    sidebarPanel(
      #Create the action button to update plot
      actionButton("calculatePK", label = "Start Simulation"),
      p("Press this button after selecting the desired settings to update the plot"),
      p(strong("Select Fed status and dosing design:")),
      
      radioButtons("FED_STATUS", label = "Fed status:", 
                         choices = list("Fasted" = 1, "Fed" = 2),
                         selected = 1),
      
      #Creates a dropdown menu for selecting the formulation
      selectInput("SELECT", "Formulation:",
                  c("Doryx MPC"=1, "Doryx tablet"=2)
      ),   #This closes selectInput widget
      
      #Creates a dropdown menu for dosing regimen
      selectInput("FREQ", "Dosing frequency:",
                  c("Once daily"=1, "Twice daily"=2),
                  selected = 1),   #This closes selectInput widget
                
      #Create a conditional panel for each dosing scenario
      conditionalPanel(condition = "input.SELECT == 1",
                       
                       #Slider input for dose
                       sliderInput("SPO_DOSE", "Doryx MPC dose (mg):",
                                   min = 120, max = 240, value = 120, step = 120)
      ),   #Close conditional panel
      
      #Create a conditional panel for each dosing scenario
      conditionalPanel(condition = "input.SELECT == 1",
                       
                       #Slider input for dose
                       sliderInput("SPO_DAYS", "Doryx MPC treatment duration (days):",
                                   min = 1, max = 7, value = 1, step = 1)
      ),   #Close conditional panel
      
      
    conditionalPanel(condition = "input.SELECT == 2",
                       
                       #Numeric input for dose
                       sliderInput("LOZ_DOSE", "Doryx tablet dose (mg):",
                                   min = 50, max = 200, value = 100, step = 50)
                       
      ),  #Close conditional panel
      
      conditionalPanel(condition = "input.SELECT == 2",
                       
                       #Numeric input for dose
                       sliderInput("LOZ_DAYS", "Doryx tablet treatment duration (days):",
                                   min = 1, max = 7, value = 1, step = 1)
                       
      ),  #Close conditional panel
      
      
      #Create a download button to download dataset
      downloadButton("downloadsimdata",
                     "Download Simulated Data"),
      
      align = "left"),   #Closing sidebarpanel and aligning left side of page
    
    #starting the main panel
    mainPanel(    
      
      #Create panel with tabs   
      tabsetPanel(
        
        #First tab
        tabPanel("Doxycycline", 
                 br(), 
                 
                 fluidRow(
                 
                   column(5, checkboxInput("CIDOXY", "Show 90% confidence interval", 
                               value = TRUE))
                   
                 ),    #close fluidrow 
                 
                 plotOutput("plotCONC", height = 550, width = 800)
                     
                 
        ), #Closing first tab
        
        #Second tab
        tabPanel("About", 
                 br(), 
                 p(strong("Note! This",a("Shiny", 
                                         href = "http://www.rstudio.com/shiny"),
                          "application uses a hybrid R/C++ code for the ADVAN-style analytical solution of the developed pharmacokinetic model. These solutions were validated and derived using the method reported by Abuhelwa",em("et al."),"(1).")),
                 p("The final population pharmacokinetic model of doxycycline was a 2-compartment model with oral absorption described by 2-transit compartments",em("(See Figure below).")),
                 p("The pharmacokinetic model was developed using",strong("single- and multi-dose"), "pharmacokinetic data of eight Phase I clinical trials comparing the Doryx tablet, Doryx capsule or Doryx MPC formulations."),
                 p("Simulated concentrations in this application are based on 1000 simulated subjects with the selected dosing design."),
                 p("Doryx MPC doses above 120mg and multi-dose data of Doryx MPC were not used to develop the model, and therefore, these types of simulations are extrapolations."),
                 p(strong("Important!"),"Click the", strong(em("Start Simulation")),  "button to update the plot every time you change the dosing regemin or the slider values."),
                 br(),
                 img(src = "FinalPKmodelDoxy.png"),
                 p(strong("Figure 1: A schematic diagram of the final population pharmacokinetic model of doxycycline")),
                 br(),
                  p(strong("Developers:")),
                  p("Ahmad Abuhelwa, University of South Australia"),
                  p("Jessica Wojciechowski, University of South Australia"),
                  p("Ashley M Hopkins, University of South Australia,", a("ashley.hopkins@mymail.unisa.edu.au", href = "mailto:ashley.hopkins@mymail.unisa.edu.au")),
                br(),
                p(strong("References:")),
                p("(1)", strong("Abuhelwa AY, Foster DJ, Upton RN. 2015."), "ADVAN-style analytical solutions for common pharmacokinetic models. J Pharmacol Toxicol Methods 73:42-48.")
                
                        

        ) #Closing About tab
        
      ) #Closing tabsetpanel
     
    )  #Closing main panel
    
  )   #Closing sidebarlayout
  
))  #Closing fluidpage and shiny UI



