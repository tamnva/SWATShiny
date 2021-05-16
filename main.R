#-------------------------------------------------------------------------------

# Call source file
source("C:/Users/nguyenta/Documents/para/loadPackages.R")
source("C:/Users/nguyenta/Documents/para/parallelFunction.R")
source("C:/Users/nguyenta/Documents/para/readSaveOutput.R")
source("C:/Users/nguyenta/Documents/para/parallelFunction.R")
source("C:/Users/nguyenta/Documents/para/updateTxtInOut.R")
source("C:/Users/nguyenta/Documents/para/postProcessing.R") 

#-------------------------------------------------------------------------------	
# User define input
  TxtInOut <- 'C:/Users/nguyenta/Documents/para/TxtInOut'
  workingDirectory <- 'C:/Users/nguyenta/Documents/para/workingDirectory'
  swatExe <- 'C:/SWAT/ArcSWAT/swat_64rel.exe'
  obsFiles <- c("C:/Users/nguyenta/Documents/para/workingDirectory/Observed/observed.txt")
  
  saveOutput <- list()
  saveOutput$file <- c('output.rch')
  saveOutput$reachNumber <- c(1,8)
  saveOutput$column <- c(7,8)
  saveOutput$date <- as.Date(c("1993-03-04","1996-09-12"), "%Y-%m-%d")

  nIter <- 12
  numberOfCores <- 4
  objCriteria <- c("NSE", "KGE")
  behaThreshold <- c(0.7)

#-------------------------------------------------------------------------------  
  # Parameter sampling
  parameterValue <- lhsRange(nIter, getParamRange(paste(workingDirectory, '/paramChange.txt', sep ='')))
  
  # Run SWAT parallel
  runSWATpar(workingDirectory, TxtInOut, saveOutput, numberOfCores, swatExe, parameterValue)
  
  # Global Parameter Sensitivity (using linear regression)
  outData <- getOutput(workingDirectory, numberOfCores, saveOutput, parameterValue)
  
  obsData <- readObs(obsFiles)
  
  # working on this
  objFunction <- objFuncValue(outData, obsData)
  sensitivity <- linearRegression(objFunction, parameterValue, objCriteria)
  
  # Calibration/Uncertainty Analysis
  #  uncertainty <- uncertaintyAnalysis(outData, obsData, objFunction, objCriteria, behaThreshold)
  plot(obsData$file[[1]]$variable[,1], type ="l", col = 'red')
  for (i in 1:length(outData$file[[1]]$iter)){
    lines(outData$file[[1]]$iter[[i]]$variable[[1]])
  }
