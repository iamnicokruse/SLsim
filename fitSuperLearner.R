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
    
  # define train data
    krit <- dataList$yMat[, y]
    train_data <- data.frame(Xint, krit)
    
  # define test data
    testkrit = testList$yMat[, y]
    test_data <- data.frame(testXint,
                            "krit" = testkrit)
    
    preds <- names(train_data[!(names(train_data) %in% c("krit"))]) # predictor character string
    mod <- as.formula(paste("krit ~ ", paste(preds, collapse = "+"))) # additive predictor combination
    
    trainCtrl <- trainControl(method = "cv",       # specification of tuning in inner cv for baselearner
                              number = 10,
                              savePredictions = "final", # saves predictions for optimal tuning parameters
                              allowParallel = F) # must be set to FALSE, as we parallelize the outer resampling
    
    models <- caretList(mod,
                        data = train_data,
                        trControl = trainCtrl,
                        metric = "MAE",
                        methodList = setParam$modfit$baselearner
    )
    
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
      
      # saving weight of metalearner
      weights_metamodel <- c("intercept" = NA_real_, "glmnet" = NA_real_, rpart = NA_real_, ranger = NA_real_, gbm = NA_real_)
      if(metalearner == "glm") {
        weights_metamodel <- as.matrix(ensemble$ens_model$finalModel$coefficients)
      } else if(metalearner == "glmnet") {
        weights_metamodel <- as.matrix(coef(ensemble$ens_model$finalModel,            
                                            s = ensemble$ens_model$bestTune$lambda))
      } else if(metalearner == "ranger") {
        weights_metamodel[2:5] <- (ensemble$ens_model$finalModel$variable.importance)
        weights_metamodel <- as.matrix(weights_metamodel)
      } else{paste0("No specification of weight extraction for this metalearner")}   
      
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
          mutate(methods = recode(methods, enetglm = "glmnet"))         # baselearner glmnet gets orignal name back
      } else if(metalearner == "ranger") {
        train_perf = rbind(glmnet_train = getTrainPerf(models$glmnet),
                           rpart_train = getTrainPerf(models$rpart),
                           gbm_train = getTrainPerf(models$gbm),
                           rf_train = getTrainPerf(models$ranger)) %>%
          mutate(method = recode(method, ranger = "rf")) %>%
          rbind(ensemble_train = getTrainPerf(ensemble$ens_model)) %>%    
          mutate(method = recode(method, ranger = "ensemble")) %>%        
          rename(methods = method)                          
      }
      
      
      # evaluate final model using held out "test_data" and comparing performances
      final_model <- ensemble
      
      # next step adds predictions based on metalearner to test_data
      test_data <- cbind(test_data,
                         pred = predict(final_model, test_data, na.action = na.pass))
      
      # adds predicitons based on baselearners to test_data
      test_data$glmnet_pred <- predict(models$glmnet, test_data)
      test_data$rpart_pred  <- predict(models$rpart, test_data)
      test_data$gbm_pred  <- predict(models$gbm, test_data)
      test_data$rf_pred  <- predict(models$ranger, test_data)
      
      
      test_perf = data.frame(rbind(glmnet_test = postResample(pred =  test_data$glmnet_pred, obs = test_data$krit),
                                   rpart_test = postResample(pred = test_data$rpart_pred, obs = test_data$krit),
                                   gbm_test = postResample(pred = test_data$gbm_pred, obs = test_data$krit),
                                   rf_test = postResample(pred = test_data$rf_pred, obs = test_data$krit),
                                   ensemble_test = postResample(pred = test_data$pred, obs = test_data$krit))) %>%
        rename(TestRMSE = RMSE) %>%
        rename(TestRsquared = Rsquared) %>%
        rename(TestMAE = MAE)
      

      # train_perf$condition <- paste0(yVec[y], "_sl_algorithm_", metalearner)  # add sample name to train-output
      # test_perf$condition  <- paste0(yVec[y], "_sl_algorithm_", metalearner)  # add sample name to test-output
      # not necessarily needed as condition labels in list structure
      
      nestedList[[s]] <- setNames(list(train_perf, test_perf, hyperparameters, weights_metamodel),
                                   c("train_perf", "test_perf", "hyperparameters", "weights"))  
    }
    resultList[[y]] <- nestedList 
  }
  return(resultList)
}

