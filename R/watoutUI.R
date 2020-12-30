
# Module hruUI function

watoutUI <- function(id) {

  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    # Input data -----------------------------------------------------
    fluidRow(             
      column(width = 4,
             fileInput(ns("watoutFiles"),
                       "1. Choose saveconc (e.g., watout.dat) file(s)...", 
                       multiple = TRUE
             )
      ),
      column(width = 4,
             selectInput(ns("selectCol"), 
                         "2. Select data to plot", 
                         choices = NULL, 
                         width = "100%"
             )
      ),              
      column(width = 4,
             fileInput(ns("observedFile"),
                       "3. Choose observed data file...", 
                       multiple = TRUE
             )
      )
    ),
    
    fluidRow(
      box(width = 12,
          sliderInput(ns("dateRange"), 
                      "Select range to plot", 
                      value = c(as.Date("1990-06-01","%Y-%m-%d"), 
                                as.Date("1993-10-01","%Y-%m-%d")), 
                      min = as.Date("1990-06-01","%Y-%m-%d"), 
                      max = as.Date("1993-10-01","%Y-%m-%d")
          )
      ),
    ),
    
    textOutput(ns("listWatoutFiles")),
    
    fluidRow(
      box(status = NULL,
          width = 12 ,
          solidHeader = F,
          checkboxInput(ns("display"), 
                         "Display plot from saveconc file"
                        ),
          conditionalPanel(condition = "input.display  == true",
                           ns = ns,
                           plotlyOutput(ns("plotQ"))
                           )
          )
      ),
    
    #----------------
  )}

