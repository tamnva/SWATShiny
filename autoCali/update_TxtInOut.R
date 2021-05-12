
# ------------------------------------------------------------------------------
# Update a list of files
  subtofilename <- function(sub){
    if(sub < 10){
      filename <- paste("0000", sub, "0000", sep="")
    } else if (sub < 100){
      filename <- paste("000", sub, "0000", sep="")      
    } else if (sub < 1000){
      filename <- paste("00", sub, "0000", sep="")  
    } else if (sub < 10000){
      filename <- paste("0", sub, "0000", sep="")  
    } else {
      filename <- paste( sub, "0000", sep="")  
    }
    return(filename)
  }

# ------------------------------------------------------------------------------
# Function to update numbers in a file
	updateSingleFile <- function(toDir,
	                             file,
	                             fileContent,
	                             atLine,
	                             atPosition,
	                             changeMethod,
	                             applyValue,
	                             numberFormat){
	  
	  newFile <- fileContent

	  for (i in 1:length(atLine)){

	    # check if this is soil file (then change all soil layers)
	    if (substr(file, nchar(file) - 2, nchar(file)) == 'sol'){
	      if (atLine[i] >= 8){
	        
	        nchars <- atPosition[i,2] - atPosition[i,1]
	        nlayer <- length(strsplit(trimws(substr(newFile[atLine[i]],
	                                                atPosition[i,1], 
	                                                nchar(newFile[atLine[i]]))), 
	                                  split = "\\s+")[[1]])
	        
	        for (j in 1:nlayer){
	          
	          if (j > 1){
	            atPosition[i,1] <- atPosition[i,2] + 1 
	            atPosition[i,2] <- atPosition[i,1] + nchars
	          }
	          parameterValue <- as.double(substr(newFile[atLine[i]],
	                                             atPosition[i,1],
	                                             atPosition[i,2]))
	          
	          if (changeMethod[i] == "rel"){
	            newParameterValue <- (1.0 + applyValue[i]) * parameterValue
	          } else if (changeMethod[i] == "rep") {
	            newParameterValue <- applyValue[i]
	          } else {
	            newParameterValue <- applyValue[i] + parameterValue
	          }
	          
	          toText <- sprintf(numberFormat[i], newParameterValue)
	          
	          substr(newFile[atLine[i]], atPosition[i,1], atPosition[i,2]) <- toText	          
	        }
	        
	      } else {
	        parameterValue <- as.double(substr(newFile[atLine[i]],
	                                           atPosition[i,1],
	                                           atPosition[i,2]))
	        
	        if (changeMethod[i] == "rel"){
	          newParameterValue <- (1.0 + applyValue[i]) * parameterValue
	        } else if (changeMethod[i] == "rep") {
	          newParameterValue <- applyValue[i]
	        } else {
	          newParameterValue <- applyValue[i] + parameterValue
	        }
	        
	        toText <- sprintf(numberFormat[i], newParameterValue)
	        
	        substr(newFile[atLine[i]], atPosition[i,1], atPosition[i,2]) <- toText	        
	        
	      }
	      
	    } else {
	      parameterValue <- as.double(substr(newFile[atLine[i]],
	                                         atPosition[i,1],
	                                         atPosition[i,2]))
	      
	      if (changeMethod[i] == "rel"){
	        newParameterValue <- (1.0 + applyValue[i]) * parameterValue
	      } else if (changeMethod[i] == "rep") {
	        newParameterValue <- applyValue[i]
	      } else {
	        newParameterValue <- applyValue[i] + parameterValue
	      }
	      
	      toText <- sprintf(numberFormat[i], newParameterValue)
	      
	      substr(newFile[atLine[i]], atPosition[i,1], atPosition[i,2]) <- toText	      
	    }

	  }

	  writeLines(newFile,paste(toDir, '/', file, sep = ''))
	}

# ------------------------------------------------------------------------------
# Function to update many files in a folder

	updateMultiFile <-  function(toDir,caliParam){

	  # Lopp over list of files
	  for (i in 1:length(caliParam$file)){
	    
	    updateSingleFile(toDir,
	                     caliParam$file[i],
	                     caliParam$fileContent[[i]],
	                     caliParam$atLine[[i]],
	                     caliParam$atPosition[[i]],
	                     caliParam$changeMethod[[i]],
	                     caliParam$applyValue[[i]],
	                     caliParam$numberFormat[[i]])
	  }
	}

#-------------------------------------------------------------------------------
# Get HRU information from a single hru file

	getHruHeader <- function(hruHeader){

	  # Read information from header of HRU file
	  for (i in 1:nchar(hruHeader)){
	    if(substr(hruHeader,i,i+3) == "HRU:"){
	      hruID <- i
	    }
	    if(substr(hruHeader,i,i+8) == "Subbasin:"){
	      subID <- i
	    }
	    if(substr(hruHeader,i,i+4) == "Soil:"){
	      soilID <- i
	    }
	    if(substr(hruHeader,i,i+5) == "Slope:"){
	      slopeID <- i
	    }
	    if(substr(hruHeader,i,i+4) == "Luse:"){
	      luseID <- i
	    }
	  }

	  result <- list()

	  # get HRU number
	  result$hru <- as.numeric(substr(hruHeader,hruID + 4, luseID - 1))

	  # get subbasin number
	  result$sub <- as.numeric(substr(hruHeader,subID + 9, hruID - 1))

	  # get soil name
	  result$soil <- trimws(substr(hruHeader,soilID + 5,slopeID - 1))

	  # get land use name
	  result$lu <- trimws(substr(hruHeader,luseID + 5, soilID - 1))

	  # get slope class
	  result$slope <- trimws(strsplit(substr(hruHeader,
	                                         slopeID + 6,
	                                         nchar(hruHeader)), " ")[[1]][2])

	  return(result)
	}


#-------------------------------------------------------------------------------
# Get HRU information from all HRU files (in TxtInOut folder)

  getHruInfo <- function(TxtInOut){

    # list of .hru files in TxtInOut folder
    lstFiles <-  list.files(path = TxtInOut,
                            pattern = ".hru",
                            full.names = TRUE)

    # remove output.hru from list files
    file <- basename(lstFiles)
    temp <- match("output.hru", file)
    file <- file[-temp]
    lstFiles <- lstFiles[-temp]

    # Initialized vector to store HRU information from headers of HRU files
    hru <- c()
    sub <- c()
    soil <- c()
    lu <- c()
    slope <- c()

    # Loop over all HRU files
    for (i in 1:length(lstFiles)){

      temp <- getHruHeader(readLines(lstFiles[i], 1))

      # HRU number
      hru <- c(hru, temp$hru)

      # Subbasin number
      sub <- c(sub, temp$sub)

      # Soil name
      soil <- c(soil, temp$soil)

      # Land use class
      lu <- c(lu, temp$lu)

      # Slope class
      slope <- c(slope, temp$slope)
    }

    # Store HRU information in a data frame
    hruInfo <- data.frame("file" = file, "hru" = hru, "sub" = sub, 
                          "soil" = soil, "lu" = lu, "slope" = slope)

    # Return result
    return(hruInfo)
  }

#-------------------------------------------------------------------------------
# Get subset of HRU based on land use, soil type, slope, subbasin selection

  hru_subset <- function(HRUinfo, select_criteria){

    hru <- HRUinfo
    
    # Subset with land use selection
    if (is.na(which("All" %in% select_criteria$lu)[1])) {
      hru <- hru[hru$lu %in% select_criteria$lu, ]
    }

    # Subset with soil type selection
    if (is.na(which("All" %in% select_criteria$soil)[1])) {
      hru <- hru[hru$soil %in% select_criteria$soil, ]
    }

    # Subset with slope class selection
    if (is.na(which("All" %in% select_criteria$slope)[1])) {
      hru <- hru[hru$slope %in% select_criteria$slope, ]
    }

    # Subset with subbasin selection
    if (is.na(which("All" %in% select_criteria$sub)[1])) {
      hru <- hru[hru$sub %in% select_criteria$sub, ]
    }

    return(hru$file)

  }

#-------------------------------------------------------------------------------
# Read all SWAT parameters form "swat_param.txt" file

	loadSwatParam <- function(swatParamFile){

	  swat_param <- readLines(swatParamFile, warn = FALSE)
	  check <- c()

	  for (i in 1:length(swat_param)){
	    checkCommentBlankLine <- substr(trimws(swat_param[i]), 1, 1)
	    if ((checkCommentBlankLine == "!") | (checkCommentBlankLine == "")) {
	      check <- c(check, i)
	    }
	  }

	  swat_param <- swat_param[-check]

	  param <- read.table(textConnection(swat_param))
	  colnames(param) <- c("parameter", "lineNumber", "startPosition",
	                       "endPosition","precision")

	  return(param)
	}

#-------------------------------------------------------------------------------
# Load param_change.txt file
	loadParamChangeFileContent <- function(param_change_file, HRUinfo, swat_para, oriTxtInOut){

	  # Read list of parameters for change
	  user_param <- readLines(param_change_file, warn = FALSE)
	  check <- c()

	  # Remove comment/blank lines
	  for (i in 1:length(user_param)){
	    checkCommentBlankLine <- substr(trimws(user_param[i]), 1, 1)
	    if ((checkCommentBlankLine == "!") | (checkCommentBlankLine == "")) {
	      check <- c(check, i)
	    }
	  }

	 # Update parameter files (without comment/blank lines)
	  user_param <- user_param[-check]

     # List of all file types (according to the spatial resolution)
	  hruBasedFile <- c("hru", "gw", "mgt", "chm", "sdr", "sep", "soil")
	  subBasedFile <- c("sub", "rte", "swq", "pnd")
	  basinBasedFile <- c("wwq", "bsn")

	  # Convert user_param to list object
	  select_criteria <- list()
	  change <- list()
	  change$parameterRange <- list()
	  change$atLine <-list()
	  change$atPosition <- list()
	  change$numberFormat <- list()
	  change$changeMethod <- list()
	  change$applyValue <- list()

	  counter <- 0
	  
	  for (i in 1:length(user_param)){
	    temp <- strsplit(user_param[i], split = '&', fixed = TRUE)
        para <- trimws(temp[[1]][1])
        fileType <- strsplit(para, split = '.', fixed = TRUE)[[1]][2]
	    
	    temp_1 <- strsplit(temp[[1]][2], split="[|,(|,)]+")
	    changeMethod <- trimws(temp_1[[1]][1])
	    
	    if (i == 1) {
	      change$parameterRange <- matrix(as.numeric(trimws(c(temp_1[[1]][2:3]))), 
	                                      ncol = 2, byrow = TRUE)
	    } else {
	      change$parameterRange <- rbind(change$parameterRange, 
	                                     as.numeric(trimws(c(temp_1[[1]][2:3]))))
	    }


	    if(fileType %in% hruBasedFile){
	      select_criteria$sub <- trimws(strsplit(temp[[1]][3], split=",")[[1]])
	      select_criteria$lu <- trimws(strsplit(temp[[1]][4], split=",")[[1]])
	      select_criteria$soil <- trimws(strsplit(temp[[1]][5], split=",")[[1]])
	      select_criteria$slope <- trimws(strsplit(temp[[1]][6], split=",")[[1]])	
	    } else if (fileType %in% subBasedFile){
	      select_criteria$sub <- trimws(strsplit(temp[[1]][3], split=",")[[1]])
	    } else {
	    }
	    
	    
	    #Get list of files
	    if(fileType %in% hruBasedFile){
	      files <- hru_subset(HRUinfo, select_criteria)
	      files <- gsub("hru", fileType, files)
	    } else if (fileType %in% subBasedFile){
	      files <- paste(subtofilename(as.integer(select_criteria$sub[1])), 
	                     ".", fileType, sep ="")
        if (length(select_criteria$sub) > 1){
          for(j in 1:length(select_criteria$sub)){
            files <- c(files, paste(subtofilename(
              as.integer(select_criteria$sub[j])), ".", fileType, sep =""))        
          }          
        }
	    } else {
	      files <- paste("basin.", fileType, sep="")
	    }
	    
	    
	    # Find location of the parameters in the file
	    index <- which(swat_para$parameter == para)
	    atLine <- swat_para$lineNumber[[index]]
	    atPosition <- c(swat_para$startPosition[[index]], 
	                    swat_para$endPosition[[index]])
	    numberFormat <-   paste('%', atPosition[2] - atPosition[1] + 1, '.', 
	                            swat_para$precision[[index]], 'f', sep ="")

	    if (i == 1){
	      nfiles <- length(files)
	      change$file <- files
	      for (j in 1:nfiles){
	        change$atLine[[j]] <- atLine
	        change$atPosition[[j]] <- matrix(atPosition,ncol = 2, byrow = TRUE)
	        change$numberFormat[[j]] <- numberFormat
	        change$changeMethod[[j]] <- changeMethod
	        change$applyValue[[j]] <- i
	      }
	      
	    } else {
	      
	      # Find if old files need to be updated
	      intersectFiles <- intersect(change$file,files)
	      if (length(intersectFiles) >= 1){
	        idx <-  which(change$file %in% intersectFiles)	   
	        for (j in 1:length(idx)){
	          change$atLine[[j]] <- c(change$atLine[[j]], atLine)
	          change$atPosition[[j]] <- rbind(change$atPosition[[j]], c(atPosition))
	          change$numberFormat[[j]] <- c(change$numberFormat[[j]], numberFormat)
	          change$changeMethod[[j]] <- c(change$changeMethod[[j]], changeMethod)
	          change$applyValue[[j]] <- c(change$applyValue[[j]], i)
	        }	
	      }
	      
	      change$file <- unique(c(change$file, files))
	      nfiles <- length(change$file)
	      
	      for (j in counter:nfiles){
	        change$atLine[[j]] <- atLine
	        change$atPosition[[j]] <- matrix(atPosition, ncol = 2, byrow = TRUE) 
	        change$numberFormat[[j]] <- numberFormat
	        change$changeMethod[[j]] <- changeMethod
	        change$applyValue[[j]] <- i
	      }
	    }	    
	    counter <- nfiles	    
	  }
	  
	  change$fileContent <- readContentFiles(oriTxtInOut, change$file)
	  change$paramFlag <- change$applyValue
	  return(change)
	}
	
	#-------------------------------------------------------------------------------	
	# Load param_change.txt file
	getParameterValue <- function(parameter, parameterValue){
	  for (i in 1:length(parameter)){
	    for (j in 1:length(parameter[[i]])){
	      parameter[[i]][j] <- parameterValue[parameter[[i]][j]]
	    }
	  }
	  return(parameter)
	}

#-------------------------------------------------------------------------------	
# Latin Hypercube Sampling
	lhsRange <- function(nIter, paramRange){
	  
	  nParam <- nrow(paramRange)
	  paramSampling <- randomLHS(nIter, nParam)
	  
	  for (i in 1:nParam){
	    paramSampling[,i] <- paramRange[i,1] +  paramSampling[,i] * (paramRange[i,2] - paramRange[i,1])
	  }
	  return(paramSampling)
	}	

#-------------------------------------------------------------------------------	
# copy all files in a folder except files
	copyAllExcept <- function(fromDir, toDir, exceptFiles){
	  
	  listFiles <- list.files(fromDir, full.names = FALSE)
	  listFiles <- listFiles[!listFiles %in% exceptFiles]
	  listFiles <- paste(fromDir, "/", listFiles, sep = "")
	  
	  file.copy(listFiles, toDir)
	}	

#-------------------------------------------------------------------------------	
# read content of all Files
	readContentFiles <- function(fileDirectory, files){
	  
	  files <- paste(fileDirectory, "/", files, sep = "")
	  fileContent <- list()

	  for (i in 1:length(files)){
	    fileContent[[i]] <- readLines(files[i], -1)
	  }
	  
	  return(fileContent)
	}		
	