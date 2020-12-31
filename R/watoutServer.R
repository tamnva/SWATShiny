
# Module server function

watoutServer <- function(id) {
  
  moduleServer(id,
               ## Below is the module function
               function(input, output, session) {
                 #--------------------------------------------------------------
                 # -----------------------------------------List of watout files   
                 output$listWatoutFiles <- renderText({
                   
                   req(input$watoutFiles)
                   
                   outText <- ""
                   for (i in 1:length(input$watoutFiles$name)){
                     outText <- paste(outText, 
                                      "File ", 
                                      i, 
                                      " = ",
                                      "'", 
                                      input$watoutFiles$name[i],
                                      "'",
                                      " ", 
                                      sep = ""
                     )
                   }
                   outText
                 }) 
                   

                 # -----------------------------------Get column number for plot
                 getColWatout <- reactive({
                   selectedColumn <- 0
                   if (length(input$watoutFiles$datapath) >= 1){
                     selectedColumn <- which(getWatoutHeader(input$watoutFiles$datapath[1]) 
                                             == input$selectCol
                     )
                   }
                   selectedColumn
                 })
                 
                 # ------------------------------------------Get watout.dat data
                 getWatoutData <- reactive({
                   watoutData(input$watoutFiles$datapath, getColWatout())
                 }) 
                 
                 # --------------------------------------Update input date range                 
                 observe({
                   # Get watout Header from first watout file
                   req(input$watoutFiles)
                   simTime <- getWatoutData()[[length(getWatoutData())]]
                   updateSliderInput(session, 
                                     "dateRange", 
                                     min = simTime[1],
                                     max = simTime[length(simTime)])
                   
                   
                 })  
                 
                 # ---------------------------------------Update select variable
                 observe({
                   # Get watout Header from first watout file
                   if (length(input$watoutFiles$datapath) >= 1){
                     file <- input$watoutFiles$datapath
                     watoutHeader <- getWatoutHeader(file[1])
                     updateSelectizeInput(session, "selectCol",
                                          choices = watoutHeader[4:length(watoutHeader)],
                                          selected = NULL
                     )      
                   }
                   
                 })

                 # -------------------------------------Get observed watout data
                
                 obsWatout <- reactive({
                   getObsWatout(input$observedFile$datapath)
                 })
                 
                 #--------------------------------------------------------------Plot watout.dat
                 output$plotQ <- renderPlotly({
                   
                   # Simulated data
                   simTime <- getWatoutData()[[length(getWatoutData())]]
                   watout <- getWatoutData()[[1]]  #--------------
                   
                   sIndex <- which(simTime == input$dateRange[1])
                   eIndex <- which(simTime == input$dateRange[2])
                 
                   simTime <- simTime[sIndex:eIndex] 
                   watout <- watout[sIndex:eIndex]
                 
                 
                   # Observed data
                   #obs <- # Simulated data
                   
                   myplot <- plot_ly()
                   myplot <- add_trace(myplot, x = obsWatout()[,1], y = obsWatout()[,2], mode = "lines")
                   myplot <- add_trace(myplot, x = simTime, y = watout, mode = "lines")
                   myplot
                 
                 })  
                 
                 #--------------------------------------------------------------  
               }
  )
}