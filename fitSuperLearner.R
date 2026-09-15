# Function to train Super Learner to data

# it needs to adapt list of sample[...]Data (dataList) to fitSL function 
# X_int can be used as predictor matrix
# each of yMat 9 columns is used once as criterion for fitSL

fitSL <- function(dataList, testList){
  Xint = as.data.frame(dataList$X_int[, !grepl(":", colnames(dataList$X_int))])
  yVec = colnames(dataList$yMat)
  testXint = as.data.frame(testList$X_int[, !grepl(":", colnames(testList$X_int))])

  resultList <- vector("list", length(yVec))
  names(resultList) <- yVec
  nPred <- ncol(Xint) # needed for mtry-tuning in ranger as dgps have unequal
                      # number of predictors
  
  for(y in seq_along(yVec)){

    trainCtrl <- trainControl(method = "cv",       # specification of tuning in inner cv for baselearner
                              number = 10,
                              savePredictions = "final", # saves predictions for optimal tuning parameters
                              allowParallel = F) # must be set to FALSE, as we parallelize the outer resampling
    
    # here the tune grids must be added 
    # start <- Sys.time()
    models <- caretList(y = dataList$yMat[, y],
                        x = Xint,  
                        trControl = trainCtrl,
                        metric = "RMSE",
                        tuneList = list(
                          ranger = caretModelSpec(method = "ranger", tuneGrid = setParam$modfit$tuneGrids$ranger_grid(nPred = nPred)),
                          gbm = caretModelSpec(method = "gbm", tuneGrid = setParam$modfit$tuneGrids$gbm_grid),
                          rpart = caretModelSpec(method = "rpart" , tuneGrid = setParam$modfit$tuneGrids$rpart_grid),
                          nnet = caretModelSpec(method = "avNNet" , tuneGrid = setParam$modfit$tuneGrids$nnet_grid, repeats = 5, 
                                                preProcess = c("center", "scale"), linout = TRUE, allowParallel = FALSE)
                          )
                        )
    
    # make predictor data frame with two way interactions)
    preds <- colnames(Xint) # predictor character string
    XallInt <- data.frame(model.matrix(as.formula(paste0("~ (", paste(preds, collapse = "+"), ")^2")), data = Xint))
    XallInt$X.Intercept. <- NULL
    
    base_glmnet <- train(y = dataList$yMat[, y],
                         x = XallInt,
                         method = "glmnet",
                         metric = "RMSE",
                         trControl = trainCtrl,
                         tuneGrid = setParam$modfit$tuneGrids$glmnet_grid
                         )
    
    models[["glmnet"]]<- base_glmnet
    
    ensemCtrl <- trainControl(method = "cv",       # specification of tuning in inner cv for metalearner
                              number = 10,
                              savePredictions = "final", # saves predictions for optimal tuning parameters
                              allowParallel = F) # must be set to FALSE, as we parallelize the outer resampling
    
    nestedList <- vector("list", length(setParam$modfit$superlearner))
    names(nestedList) <- paste0("sl_algorithm_", setParam$modfit$superlearner)
    
    for(s in seq_along(setParam$modfit$superlearner)) {
      metalearner = setParam$modfit$superlearner[s]
      
      if(metalearner == "gbm"){
      ensemble <- caretStack(models,
                             method = metalearner,
                             metric = "RMSE", 
                             trControl = ensemCtrl,
                             tuneGrid = setParam$modfit$tuneGrids$gbm_grid
      )                                                      
      } else if (metalearner == "nnls") {
      ensemble <- caretStack(models,
                             method = metalearner,           # if changed, also change weight-extraction and ensemble recoding in train_perf
                             metric = "RMSE", 
                             trControl = ensemCtrl
                             )
      } else {
      ensemble <- rowMeans(cbind(
        "glmnet" = models$glmnet$pred$pred[order(models$glmnet$pred$rowIndex)],
        "rpart" = models$rpart$pred$pred[order(models$rpart$pred$rowIndex)],
        "gbm" = models$gbm$pred$pred[order(models$gbm$pred$rowIndex)],
        "rf" = models$ranger$pred$pred[order(models$ranger$pred$rowIndex)],
        "nnet" = models$nnet$pred$pred[order(models$nnet$pred$rowIndex)]
      ))
      }
      
      # end <- Sys.time()
      # time <- difftime(end, start)
      
      # saving hyperparameters in a list
      if (metalearner == "gbm") {
      hyperparameters <- list(glmnet = models$glmnet$bestTune,
                              rpart = models$rpart$bestTune,
                              gbm = models$gbm$bestTune,
                              rf = models$ranger$bestTune,
                              nnet = models$nnet$bestTune,
                              ensemble = ensemble$ens_model$bestTune
                              )
      } else {
        hyperparameters <- list(glmnet = models$glmnet$bestTune,
                                rpart = models$rpart$bestTune,
                                gbm = models$gbm$bestTune,
                                rf = models$ranger$bestTune,
                                nnet = models$nnet$bestTune
                                )
      }
      
      # saving weights of metalearner
     weights_metamodel <- c("rpart" = NA_real_, "ranger" = NA_real_, "gbm" = NA_real_, "nnet" = NA_real_, "glmnet" = NA_real_)
        if (metalearner == "gbm") {
          varImp_tmp <- varImp(ensemble$ens_model$finalModel)
          weights_metamodel[1:5] <- varImp_tmp$Overall[1:5]
          # here we need extraction of gbm "weights"
        } else if(metalearner == "nnls") {
          weights_metamodel[1:5] <- coef(ensemble$ens_model$finalModel)
          weights_metamodel <- as.matrix(weights_metamodel)
        } else{
          weights_metamodel[1:5] <- c(0.2,0.2,0.2,0.2,0.2)
          }   
      
     
      # saving scaled weights of metalearner
     scaled_weights_metamodel <- scale_SL_weights(weights_metamodel)
     
      # save performance in training
      if (metalearner == "gbm") {
        train_perf = rbind(glmnet_train = getTrainPerf(models$glmnet),
                           rpart_train = getTrainPerf(models$rpart),
                           gbm_train = getTrainPerf(models$gbm),
                           rf_train = getTrainPerf(models$ranger),
                           nnet_train = getTrainPerf(models$nnet)) %>%
          mutate(method = recode(method, gbm = "gbm_bl")) %>%
          rbind(ensemble_train = getTrainPerf(ensemble$ens_model)) %>%    
          mutate(method = recode(method, gbm = "ensemble")) %>%
          mutate(method = recode(method, gbm_bl = "gbm")) %>%
          rename(methods = method)                          
      } else if(metalearner == "nnls"){
        train_perf = rbind(glmnet_train = getTrainPerf(models$glmnet),
                           rpart_train = getTrainPerf(models$rpart),
                           gbm_train = getTrainPerf(models$gbm),
                           rf_train = getTrainPerf(models$ranger),
                           nnet_train = getTrainPerf(models$nnet),
                           ensemble_train = getTrainPerf(ensemble$ens_model))%>%    
          mutate(method = recode(method, nnls = "ensemble")) %>%        
          rename(methods = method)                        
      } else {
        train_perf = rbind(glmnet_train = getTrainPerf(models$glmnet),
                           rpart_train = getTrainPerf(models$rpart),
                           gbm_train = getTrainPerf(models$gbm),
                           rf_train = getTrainPerf(models$ranger),
                           nnet_train = getTrainPerf(models$nnet)) %>%
          rename(methods = method)
          ens_perf <- postResample(pred = ensemble, obs = dataList$yMat[, y])
          ens_perfDF = data.frame(TrainRMSE = unname(ens_perf["RMSE"]),
                                  TrainRsquared = unname(ens_perf["Rsquared"]),
                                  TrainMAE = unname(ens_perf["MAE"]),
                                  methods = "ensemble")
          train_perf = rbind(train_perf, 
                             ensemble_train = ens_perfDF[c("TrainRMSE", "TrainRsquared", "TrainMAE", "methods")])
      } 
      
      methods = c("glmnet", "rpart", "gbm", "ranger", "avNNet", "ensemble")
      # evaluate final model using testList and comparing performances
      if (metalearner != "mean") {
        final_model <- ensemble
        
        # predictors with interactions for testing of trained glmnet-model 
        testXallInt <- data.frame(model.matrix(as.formula(paste0("testList$yMat[, y] ~ (", paste(preds, collapse = "+"), ")^2")), data = testXint))
        testXallInt$X.Intercept. <- NULL
        
        # create metalearner predictions
        test_predictions <- data.frame(
          pred = predict(final_model, testXallInt, na.action = na.pass))
        
        # adds baselearner predictions
        test_predictions$glmnet_pred <- predict(models$glmnet, testXallInt)
        test_predictions$rpart_pred  <- predict(models$rpart, testXint)
        test_predictions$gbm_pred  <- predict(models$gbm, testXint)
        test_predictions$rf_pred  <- predict(models$ranger, testXint)
        test_predictions$nnet_pred  <- predict(models$nnet, testXint)
        
        
        test_perf = data.frame(rbind(glmnet_test = postResample(pred =  test_predictions$glmnet_pred, obs = testList$yMat[, y]),
                                     rpart_test = postResample(pred = test_predictions$rpart_pred, obs = testList$yMat[, y]),
                                     gbm_test = postResample(pred = test_predictions$gbm_pred, obs = testList$yMat[, y]),
                                     rf_test = postResample(pred = test_predictions$rf_pred, obs = testList$yMat[, y]),
                                     nnet_test = postResample(pred = test_predictions$nnet_pred, obs = testList$yMat[, y]),
                                     ensemble_test = postResample(pred = test_predictions$pred, obs = testList$yMat[, y]))) %>%
          rename(TestRMSE = RMSE) %>%
          rename(TestRsquared = Rsquared) %>%
          rename(TestMAE = MAE)
        
        test_perf <- cbind(test_perf, methods)
      } else {
        # predictors with interactions for testing of trained glmnet-model 
        testXallInt <- data.frame(model.matrix(as.formula(paste0("testList$yMat[, y] ~ (", paste(preds, collapse = "+"), ")^2")), data = testXint))
        testXallInt$X.Intercept. <- NULL
        
        # create baselearner predictions
        test_predictions <- data.frame(
        glmnet_pred = predict(models$glmnet, testXallInt),
        rpart_pred  = predict(models$rpart, testXint),
        gbm_pred  = predict(models$gbm, testXint),
        rf_pred  = predict(models$ranger, testXint),
        nnet_pred = predict(models$nnet, testXint)
        )
        
        # add metalearner predictions
        test_predictions$pred <- rowMeans(test_predictions[1:5])
        
        test_perf = data.frame(rbind(glmnet_test = postResample(pred =  test_predictions$glmnet_pred, obs = testList$yMat[, y]),
                                     rpart_test = postResample(pred = test_predictions$rpart_pred, obs = testList$yMat[, y]),
                                     gbm_test = postResample(pred = test_predictions$gbm_pred, obs = testList$yMat[, y]),
                                     rf_test = postResample(pred = test_predictions$rf_pred, obs = testList$yMat[, y]),
                                     nnet_test = postResample(pred = test_predictions$nnet_pred, obs = testList$yMat[, y]),
                                     ensemble_test = postResample(pred = test_predictions$pred, obs = testList$yMat[, y]))) %>%
          rename(TestRMSE = RMSE) %>%
          rename(TestRsquared = Rsquared) %>%
          rename(TestMAE = MAE)
        
        test_perf <- cbind(test_perf, methods)
      }
      
      
      # train_perf$condition <- paste0(yVec[y], "_sl_algorithm_", metalearner)  # add sample name to train-output
      # test_perf$condition  <- paste0(yVec[y], "_sl_algorithm_", metalearner)  # add sample name to test-output
      # not necessarily needed as condition labels in list structure
      
      nestedList[[s]] <- setNames(list(train_perf, test_perf, hyperparameters, weights_metamodel, scaled_weights_metamodel, test_predictions),
                                   c("train_perf", "test_perf", "hyperparameters", "weights", "scaled_weights", "test_predictions"))  
    }
    resultList[[y]] <- nestedList 
  }
  return(resultList)
}

