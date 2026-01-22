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
library(glmnetUtils)

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

# sample data
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
  
  if (data == "inter"){
    sampleInteractionData() # run function to actually create data set
  } else if (data == "nonlinear3") {
    sampleNonlinearData()
  } else if (data == "pwlinear") {
    samplePiecewiseLinearData()
  }
}

# meta-function to combine simulation and model fit
runSLsim <- function(i, data) {
  # nSamples is number of simulation runs
  # data can be "inter", "nonlinear3" and "pwlinear" (as each Cluster is
  # supposed to run only one of these conditions)
  
  # load pre-saved test samples
  testList = get(load("testList.rda"))
  
  if (data == "inter"){
    testList <- testList[c(1:6)]
  } else if (data == "nonlinear3") {
    testList <- testList[c(7:12)]
  } else if (data == "pwlinear") {
    testList <- testList[c(13:18)]
  }
  
  # simulate data as train samples
  if (data == "inter"){
    dataList <- do.call(mapply, c(FUN = createData, gridFull[gridFull$data == "inter", ]))
  } else if (data == "nonlinear3") {
    dataList <- do.call(mapply, c(FUN = createData, gridFull[gridFull$data == "nonlinear3", ]))
  } else if (data == "pwlinear") {
    dataList <- do.call(mapply, c(FUN = createData, gridFull[gridFull$data == "pwlinear", ]))
  }
  
  # use both to train and validate super learners
  res <- mapply(
    FUN = fitSL,
    dataList = dataList,
    testList = testList,
    SIMPLIFY = FALSE
  )
  
  folder <- paste0("results/", data)
  if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
  res_name <- paste0(folder, "/res_", data, "_sample", i, ".rda")
  save(res, file = res_name)
}

# Initiate cluster
plan(multisession, workers = nCoresSampling) # if not run with Rstudio but R, multicore can be used (FORKING)

pTrash <- setParam$dgp$pTrash
nSamples <- 2
future_lapply(X = 1:nSamples, FUN = runSLsim, data = "pwlinear",
              future.packages = future_packages, 
              future.seed = TRUE)