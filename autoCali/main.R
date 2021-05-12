#-------------------------------------------------------------------------------	
# User define input
  TxtInOut <- 'C:/Users/nguyenta/Documents/para/TxtInOut'
  workingDirectory <- 'C:/Users/nguyenta/Documents/para/workingDirectory'
  numberOfCores <- 4
  swatExe <- 'C:/SWAT/ArcSWAT/swat_64rel.exe'
  saveOutput <- list()
  saveOutput$outrch <- list()
  saveOutput$outrch$reachNumber <- c(1,3)
  saveOutput$outrch$dataColumn <- c(6,7)
  saveOutput$watout <- list()
  saveOutput$watout$dataColumn <- c(4,5)

#-------------------------------------------------------------------------------    
# Load parameter change file and file content
  swatPara <- loadSwatParam(paste(workingDirectory, '/swatParam.txt', sep =''))
  paramChangeFile <- paste(workingDirectory, '/paramChange.txt', sep ='')
  HRUinfo <- getHruInfo(TxtInOut)
  caliParam <- loadParamChangeFileContent(paramChangeFile, HRUinfo,
                                          swatPara, TxtInOut)	

  
# Parameter sampling
  nIter <- 40
  paramSet <- lhsRange(nIter, caliParam$parameterRange)
  parameterValue <- cbind(c(1:nrow(paramSet)), paramSet)


# ------------------------------------------------------------------------------
  # Create n directory in R
  createDirCopyUnchangeFile(workingDirectory, numberOfCores,
                            TxtInOut,caliParam$file, swatExe)

# ------------------------------------------------------------------------------
  subParameterSet <- splitParameterValue(numberOfCores, parameterValue)
  
  cl <- parallel::makeCluster(numberOfCores)
  doParallel::registerDoParallel(cl)
  foreach(i = 1:numberOfCores, .combine = 'c') %dopar% {
    runSWAT(i, workingDirectory, swatExe, caliParam, subParameterSet[[i]], saveOutput)
  }
  parallel::stopCluster(cl)
  
#-------------------------------------------------------------------------------
  toDir <- paste(workingDirectory, '/TxtInOut_1', sep ='' )
  updateMultiFile(toDir,caliParam)



