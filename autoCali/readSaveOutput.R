
# Function to read and save outputs

# ------------------------------------------------------------------------------
# Read output.rch
  readOutputRch <- function(simNr, dir, workingDirectory, trimIndex, saveOutput){
    
    outputRch <- paste(dir, '/output.rch', sep ='')
    
    outputData <- read.table(outputRch, 
                             header = FALSE,
                             sep = "",
                             skip = 9)
    
    numberReach <- length(saveOutput$outrch$reachNumber)
    maxReach <- max(outputData$V2)
    numberVar <- length(saveOutput$outrch$dataColumn)
    outputData <- outputData[,c(2,saveOutput$outrch$dataColumn)]
    nrows <- nrow(outputData)/maxReach
    
    extractData <- matrix(rep(NA,numberReach*numberVar*nrows), 
                          nrow = nrows)
    counter <- 0
    
    for (i in 1:nrows){
      for (j in 1:maxReach){
        counter <- counter + 1
        for(k in 1:numberReach){
          if (outputData[counter,1] == saveOutput$outrch$reachNumber[k]) {
            istart <- (k-1)*numberVar + 1
            iend <- k*numberVar
            extractData[i,istart:iend] <- as.numeric(outputData[counter,2:(numberVar+1)])        
          }
        }        
      }
    }
   
    # Trim data
    extractData <- extractData[trimIndex[1]: trimIndex[2],]
    
    # Save to outputDir
    fileName <- strsplit(dir, 
                         split="_")[[1]]
    
    fileName <- trimws(fileName[length(fileName)])
    
    fileName <- paste(workingDirectory, 
                      '/Output/output_', 
                      fileName,
                      '.rch', 
                      sep ='')
    write.table(simNr, 
                fileName, 
                append = TRUE,
                row.names = FALSE,
                col.names = FALSE)
    
    write.table(extractData, 
                fileName, 
                append = TRUE,
                sep = '\t', 
                row.names = FALSE,
                col.names = FALSE)
  }

# ------------------------------------------------------------------------------
# Read watout.dat
  readWatoutDat <- function(simNr, dir, workingDirectory, 
                            trimIndex, saveOutput){
    
    outputRch <- paste(dir, '/watout.dat', sep ='')
    outputData <- read.table(outputRch, header = FALSE,sep = "",skip = 6)

    numberVar <- length(saveOutput$watout$dataColumn)
    outputData <- outputData[,c(saveOutput$watout$dataColumn)]
    
    
    # Save to output directory
    fileName <- strsplit(dir, split="_")[[1]]
    fileName <- trimws(fileName[length(fileName)])
    fileName <- paste(workingDirectory, '/Output/watout_', fileName,'.dat', sep ='')
    write.table(simNr, fileName, append = TRUE,row.names = FALSE,col.names = FALSE)
    write.table(outputData, fileName, append = TRUE,sep = '\t', row.names = FALSE,col.names = FALSE)
  }
  
    
# ------------------------------------------------------------------------------ 
# Get simulation period (warmup + calibration period)
  getFileCioInfo <- function(TxtInOut){
    fileCio <- readLines(paste(TxtInOut, "/file.cio", sep = ""), 60)
    startSim <- as.Date(paste(substr(fileCio[9],13,17), "0101", sep=""), "%Y%m%d")
    startSim <- startSim + as.numeric(substr(fileCio[10],13,17)) - 1
    
    endSim <- as.Date(paste(
      toString(
        as.numeric(substr(fileCio[9],13,17)) + 
          as.numeric(substr(fileCio[8],13,17)) - 1
      ), 
      "0101", 
      sep=""
    ), 
    "%Y%m%d")
    
    endSim <- endSim + as.numeric(substr(fileCio[11],13,17)) - 1
    nyearSkip  <- as.numeric(substr(fileCio[60],13,16))
    
    if(nyearSkip == 0){
      startEval <- startSim
    } else {
      startEval <- as.Date(paste(
        toString(
          as.numeric(substr(fileCio[9],13,17)) + nyearSkip
        ), 
        "0101", 
        sep=""
      ), 
      "%Y%m%d")
    }
    
    # Output TimeStep code
    
    info <- list()
    info$startSim <- startSim
    info$startEval <- startEval
    info$endSim <- endSim
    info$timeStepCode <- as.numeric(substr(fileCio[59],13,16))
    
    if (info$timeStepCode == 0){
      info$timeSeries = seq(startEval, endSim, by="months")
    } else if (info$timeStepCode == 1){
      info$timeSeries =  seq(startEval, endSim, by="days")
    } else {
      info$timeSeries = seq(startEval, endSim, by="years")
    }
    
    
    return(info)
  }

# ------------------------------------------------------------------------------ 
# get index of period
  
  getIndex <- function(fileCio, selectedPeriod){
    
    timeSeries <- fileCio$timeSeries
    timeStep <- fileCio$timeStepCode 
    startDate <- selectedPeriod$startDate
    endDate <- selectedPeriod$endDate
    
    if (timeStep == 1){
      for (i in 1:length(timeSeries)){
        if (timeSeries[i] == startDate){ index <- i}
        if (timeSeries[i] == endDate){ index <- c(index, i)}
      }
    } else if (timeStep == 0){
      
      startDate <- as.Date(paste(format(as.Date(startDate), "%Y-%m"), "-01", sep =''), "%Y-%m-%d")
      endDate <- as.Date(paste(format(as.Date(endDate), "%Y-%m"), "-01", sep =''), "%Y-%m-%d")
      
      for (i in 1:length(timeSeries)){
        if (timeSeries[i] == startDate){ index <- i}
        if (timeSeries[i] == endDate){ index <- c(index, i)}
      }      
    } else {
      
      startDate <- as.Date(paste(format(as.Date(startDate), "%Y"), "-01-01", sep =''), "%Y-%m-%d")
      endDate <- as.Date(paste(format(as.Date(endDate), "%Y"), "-01-01", sep =''), "%Y-%m-%d")
      
      for (i in 1:length(timeSeries)){
        if (timeSeries[i] == startDate){ index <- i}
        if (timeSeries[i] == endDate){ index <- c(index, i)}
      }      
    }
    return(index)
  }
  
  
  