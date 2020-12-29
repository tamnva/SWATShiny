

# Load packages
library(shinydashboard)
library(shinyFiles)


# Create UI
ui <- dashboardPage(
  
  # ----------------------------------------------------------------------Header
  dashboardHeader(
    title = "SWATShiny"
    #titleWidth = 300
  ),
  
  # ---------------------------------------------------------------------Sidebar
  dashboardSidebar(
    sidebarMenu(
      menuItem("General Setting", tabName = "setting", icon = icon("fas fa-cog"))
    ),
    sidebarMenu(
      menuItem("Calibration", tabName = "cali", icon = icon("align-justify"), 
               startExpanded = TRUE, selected = TRUE,
               menuSubItem("Manual", tabName = "manual", selected = TRUE),
               menuSubItem("Automatic", tabName = "auto"))
    ),
    sidebarMenu(
      menuItem("Visualization", tabName = "visual",icon = icon("align-justify"),
               startExpanded = TRUE, selected = TRUE,
               menuSubItem("watout.dat", tabName = "watout", selected = TRUE),
               menuSubItem("output.rch", tabName = "rch", selected = TRUE),
               menuSubItem("output.hru", tabName = "hru", selected = TRUE),
               menuSubItem("output.sub", tabName = "outputsub"))
    )
  ),

  # ------------------------------------------------------------------------Body
  dashboardBody(
    
    # Tab item for General Setting
    tabItems(
      # First tab content
      tabItem(tabName = "setting",
              fluidRow(
                box(
                  textInput("Watershed", "1. Watershed directory", "./testData/Watershed"),
                  textInput("TxtInOut", "2. TxtInOut directory", "./testData/TxtInOut"),
                  textInput("WorkingDirectory", "3. Working directory", "./testData"),
                ),
                box(
                  htmlOutput("projectInfo"),
                )
              ),
              
              tags$br(),  # Line break
              fluidRow(
                box(title = "HRU map", status = "success", height = "600" ,solidHeader = T,
                    plotOutput("plotHRUmap",height = "540")),
                
                box(title = "Subbasin map", status = "success", height = "600" ,solidHeader = T,
                    plotOutput("plotSubbasin",height = "540")),
              ),
      ),
      
      #*************************************************************************
      tabItem(tabName = "hru",
              
              # Input data -----------------------------------------------------
              fluidRow(             
                column(width = 4,
                       textInput("visualWatershed", 
                                 "1. Watershed directory", 
                                 "./testData/Watershed", 
                                 width = "100%"
                                 )
                       ),              
                column(width = 4,
                       textInput("visualTxtInOut", 
                                 "2. TxtInOut directory", 
                                 "./testData/TxtInOut", 
                                 width = "100%"
                                 )
                       ),
                column(width = 4,
                       selectInput("select_col", 
                                   "3. Select data to plot", 
                                   choices = NULL, 
                                   width = "100%"
                                   )
                       )
              ),
              
              fluidRow(             
                column(width = 4,
                       dateRangeInput("date_range_hru", 
                                      "4. Select date range", 
                                      width = "100%"
                                      )
                       ),              
                column(width = 4,
                       dateInput("plot_date_month_hru", 
                                 "5. Select date/month/or year to plot", 
                                 width = "100%"
                                 )
                       ),
                column(width = 4,
                       selectInput("select_temp", 
                                   "6. Temporal aggregation", 
                                   choices = list("Daily" = 1, 
                                                  "Monthly" = 2,
                                                  "Yearly" = 3,
                                                  "Sum" = 4), 
                                   selected = 1, 
                                   width = "100%"
                                   )
                       )
              ),
            
              
              #textOutput("test"),

              # Plot -----------------------------------------------------------              
              tags$br(),  # Line break              
              fluidRow(
                box(
                  title = textOutput('titleHRUplot'), 
                  status = "success",
                  height = "600" ,
                  solidHeader = T,
                  plotOutput("plotHRUoutput",height = "540"), 
                  width = 12
                  )
                )
              ),
      
      
      #*************************************************************************
      tabItem(tabName = "watout",
              
              # Input data -----------------------------------------------------
              
              fluidRow(             
                column(width = 4,
                       fileInput("watoutFiles",
                                 "1. Choose saveconc (e.g., watout.dat) file(s)...", 
                                 multiple = TRUE
                       )
                ),
                column(width = 4,
                       selectInput("selectWatoutCol", 
                                   "2. Select data to plot", 
                                   choices = NULL, 
                                   width = "100%"
                       )
                ),              
                column(width = 4,
                       fileInput("observedSaveconcFiles",
                                 "3. Choose observed data file...", 
                                 multiple = TRUE
                       )
                )
              ),
              
              fluidRow(
                box(width = 12,
                  sliderInput("dateRange", 
                              "Select range to plot", 
                              value = c(as.Date("1990-06-01","%Y-%m-%d"), 
                                        as.Date("1993-10-01","%Y-%m-%d")), 
                              min = as.Date("1990-06-01","%Y-%m-%d"), 
                              max = as.Date("1993-10-01","%Y-%m-%d")
                              )
                  ),
              ),
              
              tags$br(),  # Line break
              
              fluidRow(
                box(title = "Simulated streamflow (m3/s)", status = "success", height = "600" ,solidHeader = T,
                    checkboxInput("display", "Display Plot"),
                    conditionalPanel(
                      condition = "input.display == true",
                      plotOutput("plotQ",height = "540"), width = 6)
                    )
              ),
      )
    )
  ) 
 #------------------------------------------------------------------------------ 
)