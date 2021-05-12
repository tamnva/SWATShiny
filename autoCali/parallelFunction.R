
# ------------------------------------------------------------------------------
# Create n directory in R
  createDirCopyUnchangeFile <- function(workingDirectory, numberOfCores, 
                                        TxtInOut,exceptFiles, swatExe){
    for (i in 1:numberOfCores){
      dir <- paste(workingDirectory, '/', 'TxtInOut', '_', i, sep ='')
      if(file.exists(dir)) {
        unlink(dir, recursive=TRUE,force = TRUE)
        dir.create(dir)
      } else {
        dir.create(dir)
      }
      
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
# Function to call SWAT
  runSWAT <- function(coreNumber, workingDirectory, swatExe, 
                      caliParam, subParameterSet, saveOutput){
    
    setwd(paste(workingDirectory, '/', 'TxtInOut', '_', coreNumber, sep = ""))
    toDir <- getwd()
    nParam <- ncol(subParameterSet)
    
    # Loop over number of parameter sets
    for (i in 1:nrow(subParameterSet)) {
      
      # Assign parameter values to caliParam
      caliParam$applyValue <- getParameterValue(caliParam$paramFlag, subParameterSet[i,2:nParam])

      # Update TxtInOut folder
      updateMultiFile(toDir, caliParam)
      
      # Call swat.exe file
      exeFile <- strsplit(swatExe, split="/")[[1]]
      system(trimws(exeFile[length(exeFile)]))
      
      # Read and write output
      readOutputRch(subParameterSet[i,1], toDir, workingDirectory, saveOutput)
      
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
        istart <- (i-1) * numberSubset + i
        iend <- i * numberSubset
        subParameterSet[[i]] <- parameterValue[istart:iend,]
      }      
    }

    subParameterSet[[numberOfCores]] <- parameterValue[(iend + 1):numberParameterSet,]
    
    return(subParameterSet)
  }
  
  #----------------

 
  
  
  