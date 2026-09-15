###############################################################################
#____________________________TestList_development_____________________________#
###############################################################################
# packages
library(tidyverse)
library(mvtnorm)
library(truncnorm)


# parameters
source("MLsim-main/utils/setParameters.R")

# simulation functions
source("MLsim-main/utils/sampleInteractionData.R")
source("MLsim-main/utils/sampleNonlinearData.R")
source("MLsim-main/utils/samplePiecewiseLinearData.R")
source("MLsim-main/utils/simTools.R")

# generate grid with all combinations of simulation conditions
gridInter <- expand.grid(N = setParam$dgp$N,
                         reliability = setParam$dgp$reliability)

# add seeds to grid
set.seed(123)
seedNum <- sample(1:999999, dim(gridInter)[1], replace = FALSE) 
gridInter$sampleSeed <- seedNum[1:dim(gridInter)[1]]

# add dgp type column to grid
gridNL <- cbind(data = "inter", gridInter)  

# add other dgps and seeds
set.seed(321)
seedNum <- sample(1:999999, dim(gridInter)[1], replace = FALSE) 

gridFull <- rbind(gridNL, 
                  cbind(data = "pwlinear", 
                        gridInter[,!colnames(gridInter) %in% "sampleSeed"], 
                        sampleSeed = seedNum))

# add nonlinear dgp with 3 dummy variables
set.seed(213)
seedNum <- sample(1:999999, dim(gridInter)[1], replace = FALSE) 

gridFull <- rbind(gridFull, 
                  cbind(data = "nonlinear3", 
                        gridInter[,!colnames(gridInter) %in% "sampleSeed"], 
                        sampleSeed = seedNum)) 

# check uniqueness of set seeds
length(unique(gridFull$sampleSeed))
gridFull$testN = rep(10000, times = nrow(gridFull))

# sample data
createData <- function(data, testN, reliability){
  N = testN
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

pTrash <- 25
testList <- do.call(mapply, c(FUN = createData, gridFull[,c(1, 5, 3)])) # 1 = data,
#                                                                         5 = testN,
#                                                                         3 = reliability
# structure is dgp - rel 0.7 - N (x3 for N = 100, 1000, 3000)
#                  - rel 1.0 - N (x3 for N = 100, 1000, 3000)
#              dgp ...
save(testList, file = "testList.rda")
