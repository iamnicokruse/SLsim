### Developing tuneGrid for avNNet ####

# necessary packages
library(caret)
library(tidyverse)

# trial data
get(load("testList3000.rda"))
get(load("simulated_TrainData/data_inter_N3000_rel1_sample1.rda"))
# get(load("simulated_TrainData/data_nonlinear3_N3000_rel1_sample1.rda"))

train_data <- as.data.frame(dataList$inter$X_int[,1:29]) %>%
  cbind("y" = dataList$inter$yMat[, 9])

# train_data <- as.data.frame(dataList$nonlinear3$X_int[,1:32]) %>%
#   cbind("y" = dataList$nonlinear3$yMat[, 3])

# Only for dgp = inter
test_data <-  as.data.frame(testList3000$X_int[,1:29]) %>%
  cbind("y" = testList3000$yMat[, 9])

rm(dataList, testList3000) # keep environment clear 


# Preperation for Model fitting
trainCtrl <- trainControl(method = "cv",       # specification of tuning in inner cv for baselearner
                          number = 10,
                          savePredictions = "final", # saves predictions for optimal tuning parameters
                          allowParallel = F # must be set to FALSE, as we parallelize the outer resampling
                          )

NNETgrid <- expand.grid(size  = c(1, 2, 3, 5, 10),
                        decay = c(0, 0.001, 0.01, 0.1, 0.3, 0.4),
                        bag   = c(TRUE, FALSE)
                        )

# Model fitting
res_nnet <- train(x = train_data[, 1:29],
                  y = train_data$y,
                  preProcess = c("scale", "center"),
                  method = "avNNet",
                  metric = "RMSE",
                  trControl = trainCtrl,
                  tuneGrid = NNETgrid,
                  repeats = 5
                  )
res_nnet$results
res_nnet$bestTune

# Fit ENET to compare performance
res_enet <- train(x = train_data[, 1:29],
                  y = train_data$y,
                  method = "glmnet",
                  metric = "RMSE",
                  trControl = trainCtrl,
                  tuneLength = 25
                  )
res_enet$results
res_enet$bestTune

# Fit RF to compare performance
nPred = ncol(train_data) - 1 
ranger_grid = expand.grid(mtry = c(2, round(sqrt(nPred)), round(nPred/3), 
                               round(nPred/2), round(nPred * 0.257), round(nPred * 0.75)),
                          splitrule = c("variance"),
                          min.node.size = c(5, 10, 20, 30)
                          )


res_ranger <- train(x = train_data[, 1:29],
                  y = train_data$y,
                  method = "ranger",
                  metric = "RMSE",
                  trControl = trainCtrl,
                  tuneGrid = ranger_grid
                  )
res_ranger$results
res_ranger$bestTune


