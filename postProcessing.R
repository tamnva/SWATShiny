#-------------------------------------------------------------------------------
  objFunctionValue <- function(obs, sim){

    missingValue <- which(is.na(obs))
    
    if (length(missingValue) > 0){
      obs <- obs[-missingValue]
      sim <- sim[-missingValue]      
    }
    
    mObs <- mean(obs)
    mSim <- mean(sim)
    obs_mObs <- obs - mObs
    sim_mSim <- sim - mSim
    sumSim <- sum(sim)
    sumObs <- sum(obs)
    correlation <- cor(obs, sim)
    sdObs <- sd(obs)
    sdSim <- sd(sim)
    
    
    objNSE <- 1 - sum((sim - obs)**2)/sum((obs - mObs)**2)
    objR2 <- correlation ** 2
    objPBIAS <- 100 * (sumObs - sumSim)/sumObs
    obsKGE <- 1 - sqrt((correlation - 1)**2  + (sdSim/sdObs - 1)**2 + (mSim/mObs - 1)**2)

    result <- matrix(c(objNSE, obsKGE, objR2, objPBIAS), nrow = 1)
    colnames(result) <- c('NSE', 'KGE', 'R2', 'PBIAS')
    
    return(result)
  }
  
#-------------------------------------------------------------------------------
  readObs <- function(obsFiles){
    obsData <- list()
    for (i in 1:length(obsFiles)){
      obsData$file[[i]] <- list()
      temp <- read.table(obsFiles[i], header = FALSE, sep = "",skip = 1)
      obsData$file[[i]]$variable <- temp[,]
    }
    return(obsData)
  }

#-------------------------------------------------------------------------------  
  objFuncValue <- function(outData, obsData){
    
    nPerformanceCriteria <- 4
    objFunctionValue <- matrix(rep(0, nPerformanceCriteria * length(outData$file[[1]]$iter )), ncol = nPerformanceCriteria)
    
    # Loop over number of output file types
    for (i in 1:length(outData$file)){
      #Loop over number of Iterations
      for (j in 1:length(outData$file[[i]]$iter)){
        #Loop over number of variable
        temp <- rep(0, nPerformanceCriteria)
        for (k in 1:length(outData$file[[i]]$iter[[j]]$variable)){
          temp <- temp + objFunctionValue(obsData$file[[i]]$variable[,k+1], outData$file[[i]]$iter[[j]]$variable[[k]])
        }
        objFunctionValue[j,] <- objFunctionValue[j,] + temp/k
      }
    }
    colnames(objFunctionValue) <- c('NSE', 'KGE', 'R2', 'PBIAS')
    return(objFunctionValue)
  }
  
#-------------------------------------------------------------------------------
  linearRegression <- function(objFunction, parameterValue, objCriteria){
    
    parameterValue <- as.data.frame(parameterValue)
    temp <- colnames(objFunction)
    temp <- which(temp %in% objCriteria)
    
    if (length(temp) > 1) {
      parameterValue$V1 <- rowSums(objFunction[,temp])
    } else {
      parameterValue$V1 <- objFunction[,temp]
    }
    
    temp <- summary(lm(V1 ~ ., parameterValue))
    
    tValue <- as.numeric(temp$coefficients[,3])
    tValue <- tValue[2:length(tValue)]
    
    pValue <- as.numeric(temp$coefficients[,4])
    pvalue <- pValue[2:length(pValue)]
    
    result <- matrix(c(tValue, pvalue), byrow = FALSE, ncol = 2)
    colnames(result) <-  c('t-stat', 'p-value')

    rowName <- c()    
    for (i in 1:nrow(result)){
      rowName <- c(rowName, paste('Parameter_', i, sep = ''))
    }
    
    rownames(result) <- rowName
    
    return(result)
    
  }

#-------------------------------------------------------------------------------  
  uncertaintyAnalysis <- function(outData, obsData, objFunction, objCriteria, behaThreshold){
    
    result <- list()
    
    # Loop over number of output file types
    for (i in 1:length(outData$file)){
      
      #Loop over number of Iterations
      for (j in 1:length(outData$file[[i]]$iter)){
        
        #Loop over number of variable
        temp <- 0
        for (k in 1:length(outData$file[[i]]$iter[[j]]$variable)){
          
        }
        
        objFunctionValue[j,] <- objFunctionValue[j,] + temp/k
      }
    }
    
    return(result)
    
  }