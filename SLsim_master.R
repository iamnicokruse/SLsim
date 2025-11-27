###############################################################################
#_____________________________SLsim master script_____________________________#
###############################################################################

# packages
library("caret")
library("caretEnsemble")
library("glmnet")
library("ranger") 
library("gbm")
library("tidyverse")

# parameters and specific functions
source("MLsim-main/utils/simTools.R")
source("MLsim-main/utils/setParameters.R")

source("MLsim-main/utils/sampleInteractionData.R")
source("MLsim-main/utils/sampleNonlinearData.R")
source("MLsim-main/utils/samplePiecewiseLinearData.R")

source("SLtools.R")
source("SLfunction")
source("Model_evaluation.R")


# simulation functions
# analysis functions

# generate folder for log files (if needed!)

# set numbers of Cores to use in parallel computing

# generate grid with all combinations of simulation conditions


# add seeds to grid


# add dgp type column to grid

# add other dgps and seeds

# check uniqueness of set seeds

# sample data im parallel

# simulate and return data

# fit super learner to data and save results


