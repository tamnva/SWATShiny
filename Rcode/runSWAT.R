
# ------------------------------------------------------------------------------
# Create n directory in R
  createDirCopyUnchangeFile <- function(workingDirectory, numberOfCores, TxtInOut, exceptFiles, swatExe){
    
    # Delete all existing TxtInOut folders in workingDirectory
    existingDir <- list.dirs(path = workingDirectory, full.names = TRUE, recursive = TRUE)
    
    for (i in 1:length(existingDir)){
      temp <- trimws(strsplit(existingDir[i], split="/")[[1]])
      temp <- temp[length(temp)]
      
      if (nchar(temp) > 8){
        if(substr(temp,1,8) == 'TxtInOut'){
          unlink(existingDir[i], recursive=TRUE,force = TRUE)
        }
      }
    }
    
    # Create new TxtInOut folders
    for (i in 1:numberOfCores){
      
      dir <- paste(workingDirectory, '/', 'TxtInOut', '_', i, sep ='')
      dir.create(dir)
      
      # Copy all unchange files
      copyAllExcept(TxtInOut, dir, exceptFiles)
      
      # Check if exe exist then delete
      temp <- strsplit(swatExe, split="/")[[1]]
      temp <- trimws(temp[length(temp)])
      if (file.exists(paste(dir, '/', temp, sep =''))) {
        file.remove(paste(dir, '/', temp, sep =''))
      }
      
      # Copy swat.exe file
      file.copy(swatExe, dir)
    }
    
    # Create output directory
    dir <- paste(workingDirectory, '/Output', sep ='')
    if(file.exists(dir)) {
      unlink(dir, recursive=TRUE,force = TRUE)
      dir.create(dir)
    } else {
      dir.create(dir)
    }
  }


# ------------------------------------------------------------------------------
# split parameter values to ncores
  splitParameterValue <- function(numberOfCores, parameterValue){
    subParameterSet <- list()
    numberParameterSet <- nrow(parameterValue)           
    numberSubset <- as.integer(numberParameterSet/numberOfCores)
    
    if (numberOfCores > 1){
      for (i in 1:(numberOfCores-1)){
        istart <- (i-1) * numberSubset + 1
        iend <- i * numberSubset
        subParameterSet[[i]] <- parameterValue[istart:iend,]
      }      
    }

    subParameterSet[[numberOfCores]] <- parameterValue[(iend + 1):
                                                         numberParameterSet,]
    
    return(subParameterSet)
  }
 
# ------------------------------------------------------------------------------
# Function to call SWAT in sequential (from parameter set 1 to n)
  runSWATSequential <- function(coreNumber, 
                                workingDirectory, 
                                swatExe, 
                                trimIndex, 
                                caliParam, 
                                subParameterSet, 
                                saveOutput){
    
    # Set working directory
    setwd(paste(workingDirectory, 
                '/', 
                'TxtInOut', 
                '_', 
                coreNumber, 
                sep = ""))
    
    # Get directory where new TxtInOut files are saved
    toDir <- getwd()
    
    # Number of parameter sets
    nParam <- ncol(subParameterSet)
    
    # Loop over number of parameter sets
    for (i in 1:nrow(subParameterSet)) {
      
      # Assign parameter values to caliParam
      caliParam$applyValue <- getParameterValue(caliParam$paramFlag, 
                                                subParameterSet[i,2:nParam]
                                                )
      
      # Update TxtInOut folder
      updateMultiFile(toDir, 
                      caliParam
                      )
      
      # Call swat.exe file
      exeFile <- strsplit(swatExe, split="/")[[1]]
      system(trimws(exeFile[length(exeFile)]))
      
      # Read and write output
      readOutputRch(subParameterSet[i,1], 
                    toDir, 
                    workingDirectory, 
                    trimIndex, 
                    saveOutput)
      
      write(paste('Current simulation number: ', 
                  i, 
                  '/', 
                  nrow(subParameterSet), 
                  ' On core: ', 
                  coreNumber,  
                  sep =''
                  ),
            file= paste(workingDirectory, 
                        '/Output/CurrentSimulationReport.log', 
                        sep =''
                        ), 
            append=TRUE
            )
    }
  }  
  
# ------------------------------------------------------------------------------   
# Run SWAT parallel on n cores with m parameter sets
  runSWATpar <- function(workingDirectory, 
                         TxtInOut, 
                         saveOutput, 
                         numberOfCores, 
                         swatExe, 
                         parameterValue,
                         paramChangeFile,
                         HRUinfo,
                         swatPara,
                         caliParam,
                         copyUnchangeFiles
                         ){
    
    
    # Get general info
    
    fileCio <- getFileCioInfo(TxtInOut)
    trimIndex <- getIndex(fileCio, saveOutput$date)
    
    # Create n directory in R
    if(copyUnchangeFiles){
      createDirCopyUnchangeFile(workingDirectory, numberOfCores, TxtInOut, caliParam$file, swatExe)
    }
    
    # --------------------------------------------------------------------------
    subParameterSet <- splitParameterValue(numberOfCores, parameterValue)
    
    cl <- parallel::makeCluster(numberOfCores)
    doParallel::registerDoParallel(cl)
    foreach(i = 1:numberOfCores, .combine = 'c', .export=c("runSWATSequential","getParameterValue", 
                                                           "updateMultiFile", "updateMultiFile", 
                                                           "updateSingleFile", "readOutputRch")) %dopar% {
      runSWATSequential(i, workingDirectory, swatExe, trimIndex, caliParam, subParameterSet[[i]], saveOutput)
    }
    parallel::stopCluster(cl)     
  }

  
  