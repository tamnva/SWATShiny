
# Module server function

watoutServer <- function(id) {
  
  moduleServer(id,
               ## Below is the module function
               function(input, output, session) {
                 #--------------------------------------------------------------
                 # -----------------------------------------List of watout files   
                 output$listWatoutFiles <- renderText({
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
                 
                 
                 #--------------------------------------------------------------Plot watout.dat
                 output$plotQ <- renderPlotly({
                 
                 simTime <- getWatoutData()[[length(getWatoutData())]]
                 watout <- getWatoutData()[[1]]
                 sIndex <- which(simTime == input$dateRange[1])
                 eIndex <- which(simTime == input$dateRange[2])
                 
                 myplot <- plot_ly(x = ~simTime[sIndex:eIndex],
                                   y = ~watout[sIndex:eIndex],
                                   mode = 'lines'
                                   )
                 
                 myplot <- myplot %>% layout(xaxis = list (title = ''),
                                             yaxis = list (title = ''))
                
                 myplot
                 
                 })  
                 
                 #--------------------------------------------------------------  
               }
  )
}