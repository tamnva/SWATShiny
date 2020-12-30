
# Module hruUI function

hruUI <- function(id) {

  # NS(id) returns a namespace function, which was save as `ns` and will
  # invoke later.
  
  ns <- NS(id)
  
  tagList(
    
    fluidRow( 

    # Input UI            
      column(width = 4,
             textInput(ns("watershedFolder"), 
                       "1. Watershed directory",
                       "./testData/Watershed",
                       width = "100%"
                       )
             ),
      
      column(width = 4,
             textInput(ns("TxtInOutFolder"),
                       "2. TxtInOut directory",
                       "./testData/TxtInOut",
                       width = "100%"
                       )
             ),
      
      column(width = 4,
             selectInput(ns("selectCol"),
                         "3. Select data to plot",
                         choices = NULL,
                         width = "100%"
                         )
             )
      ),
    
    fluidRow(
      column(width = 4,
             dateRangeInput(ns("inputDateRange"),
                            "4. Select date range",
                            width = "100%"
                            )
             ),
      
      column(width = 4,
             dateInput(ns("plotDate"),
                       "5. Select date/month/or year to plot",
                       width = "100%"
                       )
             ),
      
      column(width = 4,
             selectInput(ns("tempAgg"),
                         "6. Temporal aggregation",
                         choices = list("Daily" = 1,
                                        "Monthly" = 2,
                                        "Yearly" = 3,
                                        "Sum" = 4
                                        ),
                         selected = 1,
                         width = "100%"
                         )
             )
      ),
    
    # Plot -----------------------------------------------------------              
    fluidRow(
      box(
        title = textOutput(ns('plotTitle')), 
        status = "success",
        height = "600" ,
        solidHeader = T,
        plotOutput(ns("plotHRU")), 
        width = 12
      )
    )
  )}

