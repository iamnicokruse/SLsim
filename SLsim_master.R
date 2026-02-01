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
set.seed(03022024)
seedNum <- sample(1:999999, dim(gridInter)[1], replace = FALSE) 
gridInter$sampleSeed <- seedNum[1:dim(gridInter)[1]]

# add dgp type column to grid
gridNL <- cbind(data = "inter", gridInter)  

# add other dgps and seeds
set.seed(20240203)
seedNum <- sample(1:999999, dim(gridInter)[1], replace = FALSE) 

gridFull <- rbind(gridNL, 
                  cbind(data = "pwlinear", 
                        gridInter[,!colnames(gridInter) %in% "sampleSeed"], 
                        sampleSeed = seedNum))

# add nonlinear dgp with 3 dummy variables
set.seed(02032024)
seedNum <- sample(1:999999, dim(gridInter)[1], replace = FALSE) 

gridFull <- rbind(gridFull, 
                  cbind(data = "nonlinear3", 
                        gridInter[,!colnames(gridInter) %in% "sampleSeed"], 
                        sampleSeed = seedNum)) 

# check uniqueness of set seeds
# length(unique(gridFull$sampleSeed))

# sample data
createData <- function(data, N, reliability, sampleSeed){
  
  set.seed(sampleSeed)
  
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
runSLsim <- function(i, data, N, reliability) {
  # nSamples is number of simulation runs
  # data can be "inter", "nonlinear3" and "pwlinear" (as each Cluster is
  # supposed to run only one of these conditions)

  row_idx <- which(gridFull$data == data & gridFull$N == N & gridFull$reliability == reliability)
  
  # extract the run seeds for that row
  run_seeds <- gridFull$run_seeds[[row_idx]]
  
  # pick the seed for this simulation run
  seed <- run_seeds[i]
  
  set.seed(seed)
  
  # load pre-saved test samples
  testList = get(load("testList.rda"))
  if(data == "inter"){
    if(reliability == "0.7"){
      testList = testList[1]
    } else if(reliability == "1"){
      testList = testList[2]
    }
  } else if(data == "pwlinear"){
    if(reliability == "0.7"){
      testList = testList[3]
    } else if(reliability == "1"){
      testList = testList[4]
    }
  } else if(data == "nonlinear3"){
    if(reliability == "0.7"){
      testList = testList[5]
    } else if(reliability == "1"){
      testList = testList[6]
    }
  }
  
  # simulate data as train samples
  dataList <- do.call(mapply, c(FUN = createData, gridFull[row_idx, !colnames(gridFull) %in% "run_seeds"]))
    
    # use both to train and validate super learners
    res <- mapply(
      FUN = fitSL,
      dataList = dataList,
      testList = testList,
      SIMPLIFY = FALSE
    )
    
    folder <- paste0("results/", data)
    if (!dir.exists(folder)) dir.create(folder, recursive = TRUE)
    res_name <- paste0(folder, "/res_", data, "_N", N, "_rel", reliability, "_sample", i, ".rda")
    save(res, file = res_name)
}

# Initiate cluster
plan(multisession, workers = nCoresSampling) # if not run with Rstudio but R, multicore can be used (FORKING)

pTrash <- setParam$dgp$pTrash
nSamples <- 2
dataType <- "inter"


# Create seed strings based on sampleSeeds for reproducibility
gridFull$run_seeds <- lapply(gridFull$sampleSeed, function(s) {
  set.seed(s, kind = "L'Ecuyer-CMRG")
  sample.int(.Machine$integer.max, nSamples)
})

run_seeds <- gridFull$run_seeds[gridFull$data == dataType]

# create subset of gridFull to run simulation with for()-Loop for only one dgp
grid_subset <- gridFull[gridFull$data == dataType, ]

# Run simulation in parallel
for (row in seq_len(nrow(grid_subset))) {
  N_val <- grid_subset$N[row]
  rel_val <- grid_subset$reliability[row]
  
  future_lapply(
    X = 1:nSamples,
    FUN = function(j) {
      runSLsim(i = j, data = dataType, N = N_val, reliability = rel_val)
    },
    future.packages = future_packages,
    future.seed = TRUE
  )
}

