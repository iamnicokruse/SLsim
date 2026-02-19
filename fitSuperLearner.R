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
  
  for(y in seq_along(yVec)){

    trainCtrl <- trainControl(method = "cv",       # specification of tuning in inner cv for baselearner
                              number = 10,
                              savePredictions = "final", # saves predictions for optimal tuning parameters
                              allowParallel = F) # must be set to FALSE, as we parallelize the outer resampling
    
    
    models <- caretList(y = dataList$yMat[, y], # same as dataList$yMat[, y]
                        x = Xint,  # same as Xint
                        trControl = trainCtrl,
                        metric = "MAE",
                        methodList = setParam$modfit$baselearner[setParam$modfit$baselearner != "glmnet"]
                        )
    
    # make predictor data frame with two way interactions)
     preds <- colnames(Xint) # predictor character string
     XallInt <- data.frame(model.matrix(as.formula(paste0("~ (", paste(preds, collapse = "+"), ")^2")), data = Xint))
     XallInt$X.Intercept. <- NULL
    
    base_glmnet <- train(y = dataList$yMat[, y],
                         x = XallInt,
                         method = "glmnet",
                         metric = "MAE",
                         trControl = trainCtrl,
                         tuneLength = 25
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
      
      if(metalearner == "ranger"){
      ensemble <- caretStack(models,
                             method = metalearner,
                             metric = "MAE", 
                             trControl = ensemCtrl,
                             tuneLength = 25,
                             importance = "impurity"         # this argument is needed for ranger as meta model
      )                                                      # Impurity: reduction of node impurity by feature (faster)
      # Permutation: permutated feature's impact on prediction
      # "How much worse" (more fair)
      } else {
      ensemble <- caretStack(models,
                             method = metalearner,           # if changed, also change weight-extraction and ensemble recoding in train_perf
                             metric = "MAE", 
                             trControl = ensemCtrl,
                             tuneLength = 25
      )}
      # saving hyperparameters in a list
      hyperparameters <- list(glmnet = models$glmnet$bestTune,
                              rpart = models$rpart$bestTune,
                              gbm = models$gbm$bestTune,
                              rf = models$ranger$bestTune,
                              ensemble = ensemble$ens_model$bestTune)
      
      # saving weights of metalearner
     weights_metamodel <- c("intercept" = NA_real_, "rpart" = NA_real_, "ranger" = NA_real_, "gbm" = NA_real_, "glmnet" = NA_real_)
        if(metalearner == "glm") {
          weights_metamodel <- as.matrix(ensemble$ens_model$finalModel$coefficients)
        } else if(metalearner == "glmnet") {
          weights_metamodel <- as.matrix(coef(ensemble$ens_model$finalModel,            
                                              s = ensemble$ens_model$bestTune$lambda))
        } else if(metalearner == "ranger") {
          weights_metamodel[2:5] <- ensemble$ens_model$finalModel$variable.importance
          weights_metamodel <- as.matrix(weights_metamodel)
        } else if(metalearner == "nnls") {
          weights_metamodel[2:5] <- coef(ensemble$ens_model$finalModel)
          weights_metamodel <- as.matrix(weights_metamodel)
        } else{paste0("No specification of weight extraction for this metalearner")}   
      
     
      # saving scaled weights of metalearner
     scaled_weights_metamodel <- scale_SL_weights(weights_metamodel)
     
      # save performance in training
      if(metalearner == "glm") {
        train_perf = rbind(glmnet_train = getTrainPerf(models$glmnet),
                           rpart_train = getTrainPerf(models$rpart),
                           gbm_train = getTrainPerf(models$gbm),
                           rf_train = getTrainPerf(models$ranger)) %>%
          mutate(method = recode(method, ranger = "rf")) %>%
          rbind(ensemble_train = getTrainPerf(ensemble$ens_model)) %>%
          mutate(method = recode(method, glm = "ensemble")) %>%  
          rename(methods = method)
      } else if(metalearner == "glmnet") {
        train_perf = rbind(glmnet_train = getTrainPerf(models$glmnet),
                           rpart_train = getTrainPerf(models$rpart),
                           gbm_train = getTrainPerf(models$gbm),
                           rf_train = getTrainPerf(models$ranger)) %>%
          mutate(method = recode(method, ranger = "rf")) %>%
          mutate(method = recode(method, glmnet = "enetglm")) %>%       # temporarily renamed to not be relabeled "ensemble"        
          rbind(ensemble_train = getTrainPerf(ensemble$ens_model)) %>%  # as metalearner = "glmnet" gets relabeled  
          mutate(method = recode(method, glmnet = "ensemble")) %>%        
          rename(methods = method) %>%                                  
          mutate(methods = recode(methods, enetglm = "glmnet"))         # baselearner glmnet gets original name back
      } else if(metalearner == "ranger") {
        train_perf = rbind(glmnet_train = getTrainPerf(models$glmnet),
                           rpart_train = getTrainPerf(models$rpart),
                           gbm_train = getTrainPerf(models$gbm),
                           rf_train = getTrainPerf(models$ranger)) %>%
          mutate(method = recode(method, ranger = "rf")) %>%
          rbind(ensemble_train = getTrainPerf(ensemble$ens_model)) %>%    
          mutate(method = recode(method, ranger = "ensemble")) %>%        
          rename(methods = method)                          
      } else if(metalearner == "nnls"){
        train_perf = rbind(glmnet_train = getTrainPerf(models$glmnet),
                           rpart_train = getTrainPerf(models$rpart),
                           gbm_train = getTrainPerf(models$gbm),
                           rf_train = getTrainPerf(models$ranger),
                           ensemble_train = getTrainPerf(ensemble$ens_model))%>%    
          mutate(method = recode(method, nnls = "ensemble")) %>%        
          rename(methods = method)                        
      }
      
      methods = c("glmnet", "rpart", "gbm", "ranger", "ensemble")
      # evaluate final model using testList and comparing performances
      final_model <- ensemble
      
      # predictors with interactions for testing of trained glmnet-model 
      testXallInt <- data.frame(model.matrix(as.formula(paste0("testList$yMat[, y] ~ (", paste(preds, collapse = "+"), ")^2")), data = testXint))
      testXallInt$X.Intercept. <- NULL
      
      # create metalearner predictions
      test_predictons <- data.frame(
                         pred = predict(final_model, testXallInt, na.action = na.pass))
      
      # adds baselearner predicitons
      test_predictons$glmnet_pred <- predict(models$glmnet, testXallInt)
      test_predictons$rpart_pred  <- predict(models$rpart, testXint)
      test_predictons$gbm_pred  <- predict(models$gbm, testXint)
      test_predictons$rf_pred  <- predict(models$ranger, testXint)
      
      
      test_perf = data.frame(rbind(glmnet_test = postResample(pred =  test_predictons$glmnet_pred, obs = testList$yMat[, y]),
                                   rpart_test = postResample(pred = test_predictons$rpart_pred, obs = testList$yMat[, y]),
                                   gbm_test = postResample(pred = test_predictons$gbm_pred, obs = testList$yMat[, y]),
                                   rf_test = postResample(pred = test_predictons$rf_pred, obs = testList$yMat[, y]),
                                   ensemble_test = postResample(pred = test_predictons$pred, obs = testList$yMat[, y]))) %>%
        rename(TestRMSE = RMSE) %>%
        rename(TestRsquared = Rsquared) %>%
        rename(TestMAE = MAE)
      
        test_perf <- cbind(test_perf, methods)
      
      # train_perf$condition <- paste0(yVec[y], "_sl_algorithm_", metalearner)  # add sample name to train-output
      # test_perf$condition  <- paste0(yVec[y], "_sl_algorithm_", metalearner)  # add sample name to test-output
      # not necessarily needed as condition labels in list structure
      
      nestedList[[s]] <- setNames(list(train_perf, test_perf, hyperparameters, weights_metamodel, scaled_weights_metamodel),
                                   c("train_perf", "test_perf", "hyperparameters", "weights", "scaled_weights"))  
    }
    resultList[[y]] <- nestedList 
  }
  return(resultList)
}

