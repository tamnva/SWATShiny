#-------------------------------------------------------------------------------
# Evaluate model performance (single variable)
  perCriteria <- function(obs, sim){

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
# Read observed data
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
# Calcualte objective function values for all variables, all simulations
  objFuncValue <- function(outData, obsData){
    
    nStatisticalIndex <- 4
    colName <- c('NSE', 'KGE', 'R2', 'PBIAS')
    
    
    nfiles <- length(outData$file)
    niter <- length(outData$file[[1]]$iter)

    temp <- 0
    for (i in 1:nfiles){
      temp = temp + length(outData$file[[i]]$iter[[1]]$variable) * nStatisticalIndex
    }
    
    objFunctionValue <- matrix(rep(0, niter *  temp), nrow = niter)
    counter <- 0
    istart <- 0
    iend <- 0
    
    # Loop over number of output file types
    header <- c()
    
    for (i in 1:nfiles){
      #Loop over number of Iterations
      nVariable <- length(outData$file[[i]]$iter[[1]]$variable)
      counter <- iend
      
      for (j in 1:niter){
        #Loop over number of variable
        for (k in 1:nVariable){
          
          if (j == 1){
            header <- c(header, paste(colName, "_File_", i, "_Variable_", k, sep = ""))
          }
          
          temp <- perCriteria(obsData$file[[i]]$variable[,k+1], outData$file[[i]]$iter[[j]]$variable[[k]])
          istart <- counter + 1 + (k-1) * nStatisticalIndex
          iend <- counter +  k * nStatisticalIndex
          objFunctionValue[j,istart:iend] <- temp
        }
      }
    }
    
    colnames(objFunctionValue) <- header
    
    return(objFunctionValue)
  }
  
#-------------------------------------------------------------------------------
  linearRegression <- function(objFunction, parameterValue, objCriteria){
    
    parameterValue <- as.data.frame(parameterValue)
    
    header <- strsplit(colnames(objFunction), split = "_")
    temp <- c()
    for (i in 1:length(header)){
      temp <- c(temp, header[[i]][1])
    }

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