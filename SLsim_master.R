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

# parameters
source("MLsim-main/utils/setParameters.R")

# simulation functions
source("MLsim-main/utils/sampleInteractionData.R")
source("MLsim-main/utils/sampleNonlinearData.R")
source("MLsim-main/utils/samplePiecewiseLinearData.R")
source("MLsim-main/utils/simTools.R")

# analysis functions
source("SLtools.R")
source("SLfunction.R")

# generate folder for log files (if needed!)
logFolder = "log"
createFolder(logFolder) # we do not save data but maybe we need to extract log 
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

# simulate and return data

# fit super learner to data and save results


