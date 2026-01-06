###############################################################################
#_____________________________SLsim master script_____________________________#
###############################################################################

# packages
library(caret)
library(caretEnsemble)
library(glmnet)
library(ranger)
library(gbm)
library(tidyverse)
library(future)
library(future.apply)
library(mvtnorm)
library(truncnorm)

# package string for future_lapply
future_packages = c("Matrix","mvtnorm", "truncnorm")

# parameters
source("MLsim-main/utils/setParameters.R")

# simulation functions
source("MLsim-main/utils/sampleInteractionData.R")
source("MLsim-main/utils/sampleNonlinearData.R")
source("MLsim-main/utils/samplePiecewiseLinearData.R")
source("MLsim-main/utils/simTools.R")

# analysis functions
source("SLtools.R")
source("fitSuperLearner.R")

# generate folder for log files (if needed!)
# logFolder = "log"
# createFolder(logFolder) # we do not save data but maybe we need to extract log 
                          # files in case of errors

# set numbers of Cores to use in parallel computing
nCoresSampling <- 3 # needs to be changed when cluster is used
                    # computer used to build code only allows using 3 Cores

# generate grid with all combinations of simulation conditions
gridInter <- expand.grid(N = setParam$dgp$N,
                         reliability = setParam$dgp$reliability)

# add seeds to grid
set.seed(20240203)
seedNum <- sample(1:999999, dim(gridInter)[1], replace = FALSE) 
gridInter$sampleSeed <- seedNum[1:dim(gridInter)[1]]

# add dgp type column to grid
gridNL <- cbind(data = "inter", gridInter)  

# add other dgps and seeds
set.seed(03022024)
seedNum <- sample(1:999999, dim(gridInter)[1], replace = FALSE) 

gridFull <- rbind(gridNL, 
                  cbind(data = "pwlinear", 
                        gridInter[,!colnames(gridInter) %in% "sampleSeed"], 
                        sampleSeed = seedNum))

# add nonlinear dgp with 3 dummy variables
set.seed(02202403)
seedNum <- sample(1:999999, dim(gridInter)[1], replace = FALSE) 

gridFull <- rbind(gridFull, 
                  cbind(data = "nonlinear3", 
                        gridInter[,!colnames(gridInter) %in% "sampleSeed"], 
                        sampleSeed = seedNum)) 

# check uniqueness of set seeds
# length(unique(gridFull$sampleSeed))

# sample data im parallel
createData <- function(data, N, reliability, sampleSeed){
  
  if (data == "inter"){
    environment(sampleInteractionData) <- environment()  
  } else if (data == "nonlinear3") {
    environment(sampleNonlinearData) <- environment()  
  } else if (data == "pwlinear") {
    environment(samplePiecewiseLinearData) <- environment()  
  } else {
    stop("We can only simulate inter, nonlinear or piecewise linear data!")
  }
  
  # Initiate cluster
  plan(multisession, workers = nCoresSampling) # if not run with Rstudio but R, multicore can be used (FORKING)
  
  
  if (data == "inter"){
    sampleInteractionData() # run function to actually create data set
  } else if (data == "nonlinear3") {
    sampleNonlinearData()
  } else if (data == "pwlinear") {
    samplePiecewiseLinearData()
  }
}

# simulate and return data
pTrash <- 25
out <- do.call(mapply, c(FUN = createData, gridFull))
out_res <- lapply(out, fitSL)
# fit super learner to data and save results

View(out_res)
