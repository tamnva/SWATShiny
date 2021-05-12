#-------------------------------------------------------------------------------	
# User define input
  TxtInOut <- 'C:/Users/nguyenta/Documents/para/TxtInOut'
  workingDirectory <- 'C:/Users/nguyenta/Documents/para/workingDirectory'
  numberOfCores <- 10
  swatExe <- 'C:/SWAT/ArcSWAT/swat_64rel.exe'
  nIter <- 40
  
  
  saveOutput <- list()
  saveOutput$outrch <- list()
  saveOutput$outrch$reachNumber <- c(3)
  saveOutput$outrch$dataColumn <- c(7)
  
  saveOutput$watout <- list()
  saveOutput$watout$dataColumn <- c(4,5)
  
  
  source("C:/Users/nguyenta/Documents/para/loadPackages.R")
  source("C:/Users/nguyenta/Documents/para/parallelFunction.R")
  source("C:/Users/nguyenta/Documents/para/readSaveOutput.R")
  source("C:/Users/nguyenta/Documents/para/parallelFunction.R")
  source("C:/Users/nguyenta/Documents/para/updateTxtInOut.R")

  selectedPeriod <- list()
  selectedPeriod$startDate <- as.Date("1995-03-04", "%Y-%m-%d")
  selectedPeriod$endDate <- as.Date("1998-09-12", "%Y-%m-%d")
    
#-------------------------------------------------------------------------------    
# Load parameter change file and file content
  swatPara <- loadSwatParam(paste(workingDirectory, 
                                  '/swatParam.txt', 
                                  sep =''))
  
  paramChangeFile <- paste(workingDirectory, 
                           '/paramChange.txt', 
                           sep ='')
  
  HRUinfo <- getHruInfo(TxtInOut)
  
  caliParam <- loadParamChangeFileContent(paramChangeFile, 
                                          HRUinfo,
                                          swatPara, 
                                          TxtInOut)	

  
# Parameter sampling

  
  paramSet <- lhsRange(nIter, caliParam$parameterRange)
  
  parameterValue <- cbind(c(1:nrow(paramSet)), paramSet)
  
# Get general info
  fileCio <- getFileCioInfo(TxtInOut)
  trimIndex <- getIndex(fileCio, selectedPeriod)

# ------------------------------------------------------------------------------
  # Create n directory in R
  createDirCopyUnchangeFile(workingDirectory, 
                            numberOfCores,
                            TxtInOut,
                            caliParam$file, 
                            swatExe)

# ------------------------------------------------------------------------------
  subParameterSet <- splitParameterValue(numberOfCores, parameterValue)
  
  cl <- parallel::makeCluster(numberOfCores)
  doParallel::registerDoParallel(cl)
  foreach(i = 1:numberOfCores, .combine = 'c') %dopar% {
    runSWAT(i, workingDirectory, swatExe, trimIndex,
            caliParam, subParameterSet[[i]], saveOutput)
  }
  
  parallel::stopCluster(cl)
  
# ------------------------------------------------------------------------------
# Post processing
# Parameter Sensitivity Analysis
# Performance criteria  
  
  
