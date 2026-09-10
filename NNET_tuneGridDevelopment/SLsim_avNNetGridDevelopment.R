### Developing tuneGrid for avNNet ####

# necessary packages
library(caret)
library(tidyverse)

# import files
source("MLsim-main/utils/setParameters.R")

# trial data
# get(load("testList3000.rda"))
# get(load("simulated_TrainData/data_inter_N3000_rel1_sample1.rda")) 
# get(load("simulated_TrainData/data_nonlinear3_N3000_rel1_sample1.rda")) 
# get(load("simulated_TrainData/data_pwlinear_N3000_rel1_sample1.rda"))
# get(load("simulated_TrainData/data_inter_N3000_rel0.7_sample1.rda"))    
# get(load("simulated_TrainData/data_nonlinear3_N3000_rel0.7_sample1.rda"))  
# get(load("simulated_TrainData/data_pwlinear_N3000_rel0.7_sample1.rda"))    

resList <- list()

# yVec = colnames(dataList$inter$yMat)
# yVec = colnames(dataList$nonlinear3$yMat)
yVec = colnames(dataList$pwlinear$yMat)

# for rel 0.7 dataLists (no $dgp$ sublist as data was generated manually)
# yVec = colnames(dataList$yMat)

for(y in seq_along(yVec)){

# train_data <- as.data.frame(dataList$inter$X_int[,1:29]) %>%
#   cbind("y" = dataList$inter$yMat[, y])

# for rel = 0.7, dgp = inter
# train_data <- as.data.frame(dataList$X_int[,1:29]) %>%
#   cbind("y" = dataList$yMat[, y])

# train_data <- as.data.frame(dataList$nonlinear3$X_int[,1:32]) %>%
#   cbind("y" = dataList$nonlinear3$yMat[, y])

train_data <- as.data.frame(dataList$pwlinear$X_int[,1:32]) %>%
  cbind("y" = dataList$pwlinear$yMat[, y])

# for rel = 0.7, dgp != inter
# train_data <- as.data.frame(dataList$X_int[,1:32]) %>%
#   cbind("y" = dataList$yMat[, y])

# used to try out tuneGrid for smaller sample sizes
# train_data <- train_data[1001:1100,]


# Preperation for Model fitting
trainCtrl <- trainControl(method = "cv",       # specification of tuning in inner cv for baselearner
                          number = 10,
                          savePredictions = "final", # saves predictions for optimal tuning parameters
                          allowParallel = F # must be set to FALSE, as we parallelize the outer resampling
                          )

NNETgrid <-  expand.grid(size  = c(1, 2, 3, 5, 10),
                         decay = c(0, 0.001, 0.01, 0.1, 0.3, 0.4, 0.8),
                         bag   = c(TRUE, FALSE)
                         )

# Model fitting
# res_nnet <- train(x = train_data[, 1:29],
#                   y = train_data$y,
#                   preProcess = c("scale", "center"),
#                   method = "avNNet",
#                   metric = "RMSE",
#                   trControl = trainCtrl,
#                   tuneGrid = NNETgrid,
#                   repeats = 5,
#                   linout = TRUE,
#                   allowParallel = FALSE
#                   )

res_nnet <- train(x = train_data[, 1:32],
                  y = train_data$y,
                  preProcess = c("scale", "center"),
                  method = "avNNet",
                  metric = "RMSE",
                  trControl = trainCtrl,
                  tuneGrid = NNETgrid,
                  repeats = 5,
                  linout = TRUE,
                  allowParallel = FALSE
)

# res_nnet$finalModel
# res_nnet$bestTune

resList[[y]] = res_nnet$bestTune
}

# make overview of all chosen Hyperparameters to see if Grid suffices
dgpVec = c("inter", "pwlinear", "nonlinear3")

# manual condVec for preallocations
condVec <- c("R20.2lin_inter0.5_0.5", "R20.5lin_inter0.5_0.5", "R20.8lin_inter0.5_0.5",
          "R20.2lin_inter0.0_1.0", "R20.5lin_inter0.0_1.0", "R20.8lin_inter0.0_1.0",
          "R20.2lin_inter1.0_0.0", "R20.5lin_inter1.0_0.0", "R20.8lin_inter1.0_0.0"
          )

condGrid <- expand.grid(data = dgpVec,
                        N = setParam$dgp$N,
                        reliability = setParam$dgp$reliability,
                        condition = condVec
                        )

tuneTable <- cbind(condGrid,
                   size = rep(NA_real_, nrow(condGrid)),
                   decay = rep(NA_real_, nrow(condGrid)),
                   bag = rep(NA_real_, nrow(condGrid))
                   )

for(iDGP in dgpVec) {
  relVec <- c("0.7", "1")
  
  for(iRel in relVec) {
    nVec <- c(100, 1000, 3000)
    
    for(iN in nVec) {
      filePath <- paste0("NNET_tuneGridDevelopment/")
      fileName <- paste0("Hyperparameter_", iDGP, "_rel", iRel, "_N", iN, ".rda")
      
      tmp <- (get(load(paste0(filePath, fileName))))
      
      for(iCond in condVec) {
        relIdx <- if(iRel == "1") 1.0 else 0.7
        
        rowIdentifier <- which(
          tuneTable$data == iDGP &
          tuneTable$N == iN &
          tuneTable$reliability == relIdx &
          tuneTable$condition == iCond
          )
      
       CondNum = which(condVec == iCond) 
        
      tuneTable[rowIdentifier,"size"] = tmp[[CondNum]]$size
      tuneTable[rowIdentifier,"decay"] = tmp[[CondNum]]$decay
      tuneTable[rowIdentifier,"bag"] = tmp[[CondNum]]$bag
      }
    }
  }
}

hist(tuneTable$size)
hist(tuneTable$decay)
hist(tuneTable$bag)
