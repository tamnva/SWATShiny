
# ------------------------------------------------------------------------------
# Creating server
server <- function(input, output,session) {
  
  # Project information
  output$projectInfo <- renderUI({ 
    str1 <- paste("Project information")
    str2 <- paste("Similation time:", "01.01.1990", "-", "31.12.1991")
    str3 <- paste("NSKIP", "2 years")
    HTML(paste(str1, str2, str3, sep = '<br/>'))
    
  })

    
  #Plot HRU map
  output$plotHRUmap <- renderPlot({
    hru_raster <-  raster(paste(input$Watershed, "/Grid/hrus1/w001001.adf", sep = ""))
    plot(hru_raster,legend.args = list(text = 'HRU number', side = 4, 
                                       font = 1, line = 2.5, cex = 0.8))
  })
  
  
  #*****************************************************************************
  #***************************************************************************** 
  # --------------------------------------------------------get watout.dat data
  getWatoutData <- reactive({
    watoutData(input$watoutFiles$datapath, 4)
  }) 
  
  
  #--------------------------------------------------------------Plot watout.dat
  output$plotQ <- renderPlot({
#    watout <- read.table("./testData/TxtInOut/watout.dat" , header = FALSE,
#                         sep = "",skip = 6)
#    start.date <- as.Date(paste(watout[1,1],"0101", sep=""), "%Y%m%d")
#    start.date <- start.date + watout[1,2] - 1
    
#    time <- seq(start.date, start.date + length(watout[,1])-1, by ="days")
#    watout.extract <- watout[,4]
    
#    s.date <- which(time == input$dateRange[1])
#    e.date <- which(time == input$dateRange[2])
    
    plot(getWatoutData()[[2]], getWatoutData()[[1]], type ="l", col = 2)
  })

    
  #*****************************************************************************
  #***************************************************************************** 
  # --------------------------------------------------------Read hru raster file
  hruRaster <- reactive({
    raster(paste(input$visualWatershed, 
                 "/Grid/hrus1/w001001.adf", 
                 sep = ""
                 )
           )
  })  

  # --------------------------------------------------------------Get HRU header
  HRUheader <- reactive({
    getHRUheader(input$visualTxtInOut)
  })
  
  # -------------------------------------------------------Get simulation period 
  sim_period <- reactive({
    getDate(input$visualTxtInOut)
  })

  #*****************************************************************************
  # Update date range hru plot    
  observe({
    mydate <- sim_period()
    updateDateRangeInput(session, "date_range_hru",
                         start = mydate[2],
                         end = mydate[3],
                         min = mydate[2],
                         max = mydate[3]
    )
  })
  # Update select column output.hru
  observe({
    updateSelectizeInput(session, "select_col",
                         choices = HRUheader()[8:length(HRUheader())],
                         selected = NULL
  )
  })
  # Update select column output.hru
  observe({
    mydate <- input$date_range_hru
    updateDateInput(session, "plot_date_month_hru",
                    min = mydate[1],
                    max = mydate[2], 
                    value = mydate[1]
    )
  })
  
  # --------------------------------------------------------Read output.hru data
  outputHRU <- reactive({
    ListOutputHRU(read.table(paste(input$TxtInOut, 
                                "./output.hru", 
                                sep = ""
    ), 
    header = FALSE,
    sep = "",
    skip = 9
    )
    )
  })

  # --------------------------------------------------------Temporal aggregation
  outputHRUCol <- reactive({
    selectedCol <- which(HRUheader() == input$select_col) - 1
    outputHRUCol <-  outputHRU()[[selectedCol]]
  })
  
  outputText <- reactive({
    if (input$select_temp == 1){
      text <- paste("You have selected to plot HRU output on  ", 
                    input$plot_date_month_hru, sep = ""
                    )
    } else if (input$select_temp == 2){
      text <- paste("You have selected to plot HRU output for month  ", 
                    strftime(input$plot_date_month_hru, "%m"), 
                    "/", strftime(input$plot_date_month_hru, "%Y"), 
                    sep = ""
                    )
    } else if (input$select_temp == 3){
      text <- paste("You have selected to plot HRU output for the year ", 
                    strftime(input$plot_date_month_hru, "%Y"), 
                    sep = ""
                    )
    } else if (input$select_temp == 4){
      text <- paste("You have selected to plot sum HRU output from ", 
                    input$date_range_hru[1], 
                    " to ", 
                    input$date_range_hru[2], sep = ""
                    )
    }
  })

  output$titleHRUplot <- outputText

  
  # ------------------------------------------Get subset/ggreagate of output.hru
  aggHRU <- reactive({
    
    # data column in HRU file
    
    subsetOutputHRU <- outputHRUCol()
    
    # get date of simulation period
    evalPeriod <- seq(sim_period()[2], 
                       sim_period()[3], 
                       by ="days"
    )
    
    # temporal aggregation daily
    plotPeriod <- seq(input$date_range_hru[1], 
                      input$date_range_hru[2], 
                      by ="days"
    )
    
    startIndex <- which(evalPeriod == input$date_range_hru[1] )
    endIndex <- which(evalPeriod == input$date_range_hru[2] )
    
    # get subset of input data
    subsetOutputHRU <- subsetOutputHRU[startIndex:endIndex,]
    subsetOutputHRU <- agg(subsetOutputHRU, plotPeriod, input$select_temp)
    
    selectTime <- input$plot_date_month_hru
    
    if (input$select_temp == 1){
      index <- which(plotPeriod == selectTime )
      subsetOutputHRU <- subsetOutputHRU[index,]
      
    } else if (input$select_temp == 2){
      
      month <- as.numeric(strftime(selectTime, "%m"))
      year <- as.numeric(strftime(selectTime, "%Y"))

      
      monthYear <- subsetOutputHRU[,1:2]
      
      for (i in 1:dim(monthYear)[1]){
        if ((monthYear[i,1] == month) & (monthYear[i,2] == year)){
          index <- i
        }
      }
      
      subsetOutputHRU <- subsetOutputHRU[index,-1:-2]
      
    } else if (input$select_temp == 3) {
      year <- as.numeric(strftime(selectTime, "%Y"))
      index <- which(subsetOutputHRU[,1] == year)
      subsetOutputHRU <- subsetOutputHRU[index,-1]
    } 
    
    hru <- hruRaster()
    val <- values(hru)
    
    for(i in 1:length(val)){
      val[i] <- subsetOutputHRU[val[i]]
    }
    
    values(hru) <- val
    hru <- hru
    
  })
  
  
  # --------------------------------------------------------Temporal aggregation 
  
  
  #Plot HRU map
  output$plotHRUoutput <- renderPlot({
    plot(aggHRU(), title("Spatial diss"))
  })  
  
  
}