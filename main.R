
#------------------------------------------------------------------------------- 
# User define input
  RcodeDirectory <- 'C:/Users/nguyenta/Documents/GitHub/SWATShiny/Rcode'
  TxtInOut <- 'C:/Users/nguyenta/Documents/GitHub/SWATShiny/TxtInOut'
  workingDirectory <- 'C:/Users/nguyenta/Documents/GitHub/SWATShiny/workingDirectory'
  swatExe <- 'C:/SWAT/ArcSWAT/swat_64rel.exe'
  obsFiles <- c('C:/Users/nguyenta/Documents/GitHub/SWATShiny/workingDirectory/Observed/observed.txt')
  
  saveOutput <- list()
  saveOutput$file <- c('output.rch')
  
  # This is for output.rch (only necessary for output.rch, otherwise, ignore this )
  saveOutput$reachNumber <- c(8)
  saveOutput$reachColumn <- c(7)
  
  # This is for output.rch (only necessary for watout.dat, otherwise, ignore this )
  #saveOutput$watoutColumn <- c(4,5)
  
  saveOutput$date <- as.Date(c("1993-03-04","1996-09-12"), "%Y-%m-%d")

  nIter <- 1
  numberOfCores <- 1
  objCriteria <- c("NSE", "KGE")
  behaThreshold <- c(0.7)

#-------------------------------------------------------------------------------
  # Load all Rcode in
  sapply(list.files(RcodeDirectory, full.names = TRUE), source)
  
  # Parameter sampling
  parameterValue <- lhsRange(nIter, getParamRange(paste(workingDirectory, '/paramChange.txt', sep ='')))
  
  # Load all SWAT parameters
  swatPara <- loadSwatParam(paste(workingDirectory,
                                  '/swatParam.txt', 
                                  sep =''))
  
  # File name that define the parameters to be updated
  paramChangeFile <- paste(workingDirectory, '/paramChange.txt', sep ='')
  
  # Load HRU information (land use, soil, slope, subbasin)
  HRUinfo <- getHruInfo(TxtInOut)
  
  # Load all files that are going to be updated
  caliParam <- loadParamChangeFileContent(paramChangeFile, 
                                          HRUinfo, 
                                          swatPara, 
                                          TxtInOut)
  # Run SWAT parallel on numberOfCores
  copyUnchangeFiles <-  TRUE
  
  runSWATpar(workingDirectory, 
             TxtInOut, 
             saveOutput, 
             numberOfCores, 
             swatExe, 
             parameterValue,
             paramChangeFile,
             HRUinfo,
             swatPara,
             caliParam,
             copyUnchangeFiles)

  
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
