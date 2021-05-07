
# List of main functions in this file
# 1.   update_file          update multiple numbers in single file
# 2.   update_files         update multiple files  
# 3.   getHRU_info          get HRU information from the first line of .hru files
# 4.   hru_subset           get subset of hru base on selected criteria (LU, soil, slope, sub)
# 5.   load_swat_parameter  read SWAt parameter file
# 6.   param_change_file



# Get HRU information from all HRU files in the TxtInOut folder
TxtInOut <- "C:/Users/nguyenta/Documents/IWW/SWAT/Bhumika/MODIS_ETa/to_Tam/SWAT/Scenarios/Default/TxtInOut"
HRUinfo <- getHRU_info(TxtInOut)

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
	update_file <- function(fromTo,
	                      atLine,
	                      atPosition,
	                      changeMethod,
	                      applyValue,
	                      numberFormat){

	  newFile <- readLines(fromTo[1],-1)

	  for (i in 1:length(atLine)){

	    parameterValue <- as.double(substr(newFile[atLine[i]],
	                                       atPosition[i,1],
	                                       atPosition[i,2]))

	    if (changeMethod[i] == "relative"){
	      newParameterValue <- (1.0 + applyValue[i]) * parameterValue}

	    else if (changeMethod[i] == "replace") {
	      newParameterValue <- applyValue[i]}

	    else if (changeMethod[i] == "absolute") {
	      newParameterValue <- applyValue[i] + parameterValue}

	    else {
			print("Undefined changeMethod")
			print("Please select: relative, replace, or absolute")}

	    toText <- sprintf(numberFormat[i], newParameterValue)

	    substr(newFile[atLine[i]], atPosition[i,1], atPosition[i,2]) <- toText

	  }

	  writeLines(newFile,fromTo[2])
	}

	#----------------------------------------------------------------------example
	fromTo <- c("C:/Users/nguyenta/Documents/para/TxtInOut/000010002.sol",
	            "C:/Users/nguyenta/Documents/para/000010002.sol")

	atLine <- c(9,9)
	applyValue <- c(-0.2,5)
	changeMethod <- c("relative", "replace")
	numberFormat <- c("%12.2f","%12.2f")
	atPosition <- matrix(c(28,39,
	                       40,51),
	                     ncol = 2,
	                     byrow = TRUE)

	update_file(fromTo,
	            atLine,
	            atPosition,
	            changeMethod,
	            applyValue,
	            numberFormat)



# ------------------------------------------------------------------------------
# Function to update many files in a folder

	update_files <-  function(fromTo,
	                          fileList,
	                          atLine,
	                          atPosition,
	                          changeMethod,
	                          applyValue,
	                          numberFormat){

	  # Lopp over list of files
	  for (i in 1:length(fileList)){

	    # Loop over each files
	    for (j in 1:length(fileList[[i]])){

	      fromFile <- paste(fromTo[1], "/", fileList[[i]][j], sep = "")
	      toFile <- paste(fromTo[2], "/", fileList[[i]][j], sep = "")
	      fromFileToFile <- c(fromFile, toFile)

	      edit.file(fromFileToFile,
	                atLine[[i]],
	                atPosition[[i]],
	                changeMethod[[i]],
	                applyValue[[i]],
	                numberFormat[[i]])
	    }
	  }
	}

	#----------------------------------------------------------------------example
	fromTo <- c("C:/Users/nguyenta/Documents/para/TxtInOut", "D:")

	fileList <- list()
	fileList[[1]] <- c("000010001.sol", "000010002.sol", "000010003.sol")
	fileList[[2]] <- c("000010001.hru", "000010002.hru")


	atLine <- list()
	atPosition <- list()
	changeMethod <- list()
	numberFormat <- list()
	applyValue <- list()


	atLine[[1]] <- c(12,14)
	atPosition[[1]] <- matrix(c(28,39,
	                            40,51),
	                          ncol = 2,
	                          byrow = TRUE)

	changeMethod[[1]] <- c("replace", "replace")
	numberFormat[[1]] <- c("%12.2f","%12.2f")
	applyValue[[1]] <- c(99, 101)

	atLine[[2]] <- c(5)
	atPosition[[2]] <- matrix(c(1,16), ncol = 2, byrow = TRUE)
	changeMethod[[2]] <- c("replace")
	numberFormat[[2]] <- c("%16.3f")
	applyValue[[2]] <- c(50)

	update_files(fromTo,
	             fileList,
	             atLine,
	             atPosition,
	             changeMethod,
	             applyValue,
	             numberFormat)



#-------------------------------------------------------------------------------
# Get HRU information from a single hru file

	getHRU_header <- function(hruHeader){

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

	#----------------------------------------------------------------------example
	hruHeader  <- "	.hru file Watershed HRU:1 Subbasin:1 HRU:1 Luse:URML Soil: B59H5401 Slope: 0-5 1/17/2019 12:00:00 AM ArcSWAT 2012.10_2.19"
	getHRU_header(hruHeader)


#-------------------------------------------------------------------------------
# Get HRU information from all HRU files (in TxtInOut folder)

  getHRU_info <- function(TxtInOut){

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

      temp <- getHRU_header(readLines(lstFiles[i], 1))

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
    hruInfo <- data.frame("file" = file, "hru" = hru, "sub" = sub, "soil" = soil, "lu" = lu, "slope" = slope)

    # Return result
    return(hruInfo)
  }

	#----------------------------------------------------------------------example
	TxtInOut <- "C:/Users/nguyenta/Documents/para/TxtInOut"
	HRUinfo <- getHRU_info(TxtInOut)


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

	#----------------------------------------------------------------------example
	# Get list of files with a certain combination of land use + soil + slope + sub
	select_criteria <- list()
	select_criteria$lu <- c("PAST", "RNGE", "All")
	select_criteria$soil <- c("SOIL53", "SOIL46", "All")
	select_criteria$slope <- c("0-5", "All")
	select_criteria$sub <- c(1:2, "All")

	hru_subset(HRUinfo, select_criteria)

#-------------------------------------------------------------------------------
# Read all SWAT parameters form "swat_param.txt" file

	load_swat_parameter <- function(swat_param_file){

	  swat_param <- readLines(swat_param_file, warn = FALSE)
	  check <- c()

	  for (i in 1:length(swat_param)){
	    checkCommentBlankLine <- substr(trimws(swat_param[i]), 1, 1)
	    if ((checkCommentBlankLine == "!") | (checkCommentBlankLine == "")) {
	      check <- c(check, i)
	    }
	  }

	  swat_param <- swat_param[-check]

	  param <- read.table(textConnection(swat_param))
	  colnames(param) <- c("parameter",
	                       "lineNumber",
	                       "startPosition",
	                       "endPosition",
	                       "precision")

	  return(param)
	}
#----------------------------------------------------------------------example
swat_para <- load_swat_parameter("C:/Users/nguyenta/Documents/para/swat_param.txt")

#-------------------------------------------------------------------------------
	param_change_file <- "C:/Users/nguyenta/Documents/para/param_change.txt"

# Load param_change.txt file

	load_parameter_change <- function(param_change_file, HRUinfo, swat_para){

	  user_param <- readLines(param_change_file, warn = FALSE)
	  check <- c()

	  for (i in 1:length(user_param)){
	    checkCommentBlankLine <- substr(trimws(user_param[i]), 1, 1)
	    if ((checkCommentBlankLine == "!") | (checkCommentBlankLine == "")) {
	      check <- c(check, i)
	    }
	  }

	  user_param <- user_param[-check]
	  param <- list()

	  hruBasedFile <- c("hru", "gw", "mgt", "chm", "sdr", "sep", "soil")
	  subBasedFile <- c("sub", "rte", "swq", "pnd")
	  basinBasedFile <- c("wwq", "bsn")


	  namelist <- c("param",
	                "changeMethod",
	                "applyValue",
	                "subbasin",
	                "LandUse",
	                "Soil",
	                "Slope")

	  # Convert user_param to list object
	  para <- list()
	  file <- list()
	  change <- list()
	  value <- list()
	  flist <- list()
	  
	  for (i in 1:length(user_param)){
	    temp <- strsplit(user_param[i], split = '&', fixed = TRUE)

	    para[[i]] <- trimws(strsplit(temp[[1]][1], split = '.', fixed = TRUE)[[1]][1])
	    file[[i]] <- trimws(strsplit(temp[[1]][1], split = '.', fixed = TRUE)[[1]][2])
	    
	    temp_1 <- strsplit(temp[[1]][2], split="[|,(|,)]+")
	    change[[i]] <- trimws(temp_1[[1]][1])
	    value[[i]] <- as.numeric(trimws(c(temp_1[[1]][2:3])))

	    select_criteria <- list()
	    
	    if(file[[i]] %in% hruBasedFile){
	      select_criteria$sub <- trimws(strsplit(temp[[1]][3], split=",")[[1]])
	      select_criteria$lu <- trimws(strsplit(temp[[1]][4], split=",")[[1]])
	      select_criteria$soil <- trimws(strsplit(temp[[1]][5], split=",")[[1]])
	      select_criteria$slope <- trimws(strsplit(temp[[1]][6], split=",")[[1]])	
	    } else if (file[[i]] %in% subBasedFile){
	      select_criteria$sub <- trimws(strsplit(temp[[1]][3], split=",")[[1]])
	    } else {
	    }
	    
	    
	    #Get list of files
	    #Check if hru-type files
	    if(file[[i]] %in% hruBasedFile){
	      flist[[i]] <- hru_subset(HRUinfo, select_criteria)
	      flist[[i]] <- gsub("hru", file[[i]], flist[[i]])
	    } else if (file[[i]] %in% subBasedFile){
	      flist[[i]] <- paste(subtofilename(as.integer(select_criteria$sub[1])), ".", file[[i]], sep ="")
	      if (length(select_criteria$sub) > 1) {
	        for(j in 2:length(select_criteria$sub)){
	          flist[[i]] <- c(flist[[i]], paste(subtofilename(as.integer(select_criteria$sub[j])), ".", file[[i]], sep =""))        
	        }	        
	      }
	    } else {
	      flist[[i]] <- paste("basin.", file[[i]], sep="")
	    }
	  }
	  
	  
	  
	  
	  
	}

	  #uniqueFileType <- unique(fileType)

#-------------------------------------------------------------------------------
# Read parameter information

  parameters <- c("ORGP_CON", "SOLP_CON", "GW_DELAY")
	changeMethod <- c("replace", "relative", "absolute")
	applyValue <- c(1.2, -0.2, 20.3)

	para <- data.frame("Parameters" = c("ORGP_CON", "SOLP_CON", "GW_DELAY"),
	                   "ChangeMethod" = c("replace", "relative", "absolute"),
	                   "ApplyValue" = c(1.2, -0.2, 20.3),
	                   "AtLine" = c(1,5,7),
	                   "sColumn")

	atLine[[2]] <- c(5)
	atPosition[[2]] <- matrix(c(1,16), ncol = 2, byrow = TRUE)
	changeMethod[[2]] <- c("replace")
	numberFormat[[2]] <- c("%16.3f")
	applyValue[[2]] <- c(50)

