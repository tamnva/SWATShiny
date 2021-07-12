
# dependency packages
  packages <- c("foreach", "parallel", "lhs")

# Install dependency packages if necessary
  install.packages(repos = getOption("repos"),
                   setdiff(packages, rownames(installed.packages()))) 

# Load dependency packages
  library(lhs)
  library(foreach)
  library(parallel)

