
# Module server function

hruServer <- function(id) {
  
  moduleServer(id,
               ## Below is the module function
               function(input, output, session) {
                 #--------------------------------------------------------------
                 # Update date range hru plot    
                 observe({
                   
                   req(input$TxtInOutFolder)
                   
                   myDate <- simPeriod()
                   
                   updateDateRangeInput(session, "inputDateRange",
                                        start = myDate[2],
                                        end = myDate[3],
                                        min = myDate[2],
                                        max = myDate[3]
                   )
                 })
                 # Update select column output.hru
                 observe({
                   updateSelectizeInput(session, "selectCol",
                                        choices = HRUheader()[8:length(HRUheader())],
                                        selected = NULL
                   )
                 })
                 # Update select column output.hru
                 observe({
                   
                   req(input$inputDateRange)
                   
                   myDate <- input$inputDateRange
                   updateDateInput(session, "plotDate",
                                   min = myDate[1],
                                   max = myDate[2], 
                                   value = myDate[1]
                   )
                 })
                 
                 # -----------------------------------------------Get HRU header
                 HRUheader <- reactive({
                   
                   req(input$TxtInOutFolder)
                   
                   getHRUheader(input$TxtInOutFolder)
                 })
                 
                 # -----------------------------------------Read hru raster file
                 hruRaster <- reactive({
                   
                   req(input$watershedFolder)
                   
                   raster(paste(input$watershedFolder, 
                                "/Grid/hrus1/w001001.adf", 
                                sep = ""
                   )
                   )
                 }) 
                 
                 # ----------------------------------------Get simulation period 
                 simPeriod <- reactive({
                   
                   # Check if input TxtInOut folder is empty
                   req(input$TxtInOutFolder)
                   
                   getDate(input$TxtInOutFolder)
                 })
                 
                 
                 # -----------------------------------------Read output.hru data
                 outputHRU <- reactive({
                   
                   req(input$TxtInOutFolder)
                   
                   getOutputHru(read.table(paste(input$TxtInOutFolder, 
                                                  "./output.hru", 
                                                  sep = ""
                   ), 
                   header = FALSE,
                   sep = "",
                   skip = 9
                   )
                   )
                 })

                 # -----------------------------------------Temporal aggregation
                 outputHRUCol <- reactive({
                   req(input$selectCol)
                   selectColumn <- which(HRUheader() == input$selectCol) - 1
                   outputHRUCol <-  outputHRU()[[selectColumn]]
                 })

                 # -------------------------------Output text for plot box title
                 outputText <- reactive({
                   req(input$tempAgg)
                   
                   if (input$tempAgg == 1){
                     text <- paste("You have selected to plot HRU output on  ", 
                                   input$plotDate, sep = ""
                     )
                   } else if (input$tempAgg == 2){
                     text <- paste("You have selected to plot HRU output for month  ", 
                                   strftime(input$plotDate, "%m"), 
                                   "/", strftime(input$plotDate, "%Y"), 
                                   sep = ""
                     )
                   } else if (input$tempAgg == 3){
                     text <- paste("You have selected to plot HRU output for the year ", 
                                   strftime(input$plotDate, "%Y"), 
                                   sep = ""
                     )
                   } else if (input$tempAgg == 4){
                     text <- paste("You have selected to plot sum HRU output from ", 
                                   input$inputDateRange[1], 
                                   " to ", 
                                   input$inputDateRange[2], sep = ""
                     )
                   }
                 })
                 
                 output$plotTitle <- outputText
                 
                 
                 # ---------------------------Get subset/ggreagate of output.hru
                 aggHRU <- reactive({
                   
                   # data column in HRU file
                   req(input$TxtInOutFolder)
                   subsetOutputHRU <- outputHRUCol()
                   
                   # get date of simulation period
                   evalPeriod <- seq(simPeriod()[2], 
                                     simPeriod()[3], 
                                     by ="days"
                   )
                   
                   # temporal aggregation daily
                   plotPeriod <- seq(input$inputDateRange[1], 
                                     input$inputDateRange[2], 
                                     by ="days"
                   )
                   
                   startIndex <- which(evalPeriod == input$inputDateRange[1] )
                   endIndex <- which(evalPeriod == input$inputDateRange[2] )
                   
                   # get subset of input data
                   subsetOutputHRU <- subsetOutputHRU[startIndex:endIndex,]
                   subsetOutputHRU <- agg(subsetOutputHRU, plotPeriod, input$tempAgg)
                   
                   selectTime <- input$plotDate
                   
                   if (input$tempAgg == 1){
                     index <- which(plotPeriod == selectTime )
                     subsetOutputHRU <- subsetOutputHRU[index,]
                     
                   } else if (input$tempAgg == 2){
                     
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
                 output$plotHRU <- renderPlot({
                   plot(aggHRU())  
                 })
                 
                 
 
                 
                 
                 # --------------------------------------------------------Temporal aggregation 
 
                 
                 
                 #--------------------------------------------------------------  
               }
  )
  }