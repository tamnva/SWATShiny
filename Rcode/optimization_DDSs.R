

# Read parameters from text file
fullpara <- read.csv(file = "C:/Users/nguyenta/Documents/para_list.txt",
                 header = FALSE, sep = "")
para <- fullpara[which(fullpara$V5 == 1),]
para <- para[-c(1,5)]
names(para) <- c("min", "max", "init")
r <- 0.2
nIters <- 10

# Read obs and simulated data
obsFile <- "C:/Users/nguyenta/Documents/obs.txt"
simFile <- "C:/Users/nguyenta/Documents/output_Q_C.txt"
readObs <- read.csv(obsFile, header = FALSE, sep = "")
readSim <- read.csv(simFile, header = FALSE, skip = 1, sep = "")

# Function NSE, lnNSE, PBIAS
obj <- function(obs, sim){

  mObs <- mean(obs)
  nse <- 1 - sum((sim - obs)^2)/sum((obs - mObs)^2)
  pbias <- (sum(obs) - sum(sim))/sum(sim)

  obsSim <- data.frame(obs, sim)
  obsSim <- obsSim[which(obsSim$obs > 0 & obsSim$sim > 0),]

  obs <- log(obsSim$obs)
  sim <- log(obsSim$sim)

  mObs <- mean(obs)
  lnnse <- 1 - sum((sim - obs)^2)/sum((obs - mObs)^2)

  return(c(nse, lnnse, pbias))
}

# test
obj(readObs$V2, readSim$V2)



DDS <- function(para, nIters, r, obj, report){

  # Parameter data frame with 1st column is min, 2nd max, 3rd initial para. values
  # Number of parameters
  nParam <- length(para$min)

  # Set best parameter
  para_best = para$init

  # Evaluate objective function value f with p_best----------------------------
  obj_best <- obj(para$init)

  # Save output as data frame
  output <- list()
  output[[1]] <- para_best
  output[[2]] <- obj_best

  # Calculate sigma
  sigma <- r * (para$max - para$min)

  # DDS iteration
  for (i in 1:nIters){

    # Probabiliy for including in the perturbation list
    p <- 1 - log(i)/log(nIters)

    # Add parameter para_temp(idx) to perturb list with probability p
    idx <- which(runif(nParam) < p)

    # Select random parameter for pertubation list if it is empty
    if (length(idx) == 0) {idx <- sample.int(nParam, 1)}

    # Pertub parameters that in the pertubation list
    para_new <- para_best[idx] + sigma * rnorm(length(idx), mean = 0, sd = 1)

    # Check if min/max boundary condition is satisfied

    for (j in 1:length(idx)){

      # Check min
      if (para_new[j] < para$min[idx[j]]) {
        para_new[j] = para$min[idx[j]] + (para$min[idx[j]] - para_new[j])
        if (para_new[j] > para$max[idx[j]]) {para_new[j] = para$min[idx[j]]}
      }

      # Check max
      if (para_new[j] > para$max[idx[j]]) {
        para_new[j] = para$max[idx[j]] - (para_new[j] - para$max[idx[j]])
        if (para_new[j] < para$min[idx[j]]) { para_new[j] = para$max[idx[j]]}
      }
    }

    # Add new parameter to the full parameter list
    para_temp <- para_best
    para_temp[idx] <- para_new

    # Evaluate objective function value f with p_best----------------------------
    obj_val <- obj(para_temp)

    # Add to output list
    if (obj_val > obj_best) {
      obj_best <- obj_val
      para_best <- para_temp
    }

  }

}














