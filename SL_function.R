# SuperLearner function

runSL <- function(file, testsample, iter = 1, baselearner, metalearner) {
  
  # fileList is list()-object with files of training samples
  # testsample is one sample to be used for model validation in outer cv
  # i is number of iterations for inner cv if needed (default is 1)
  # baselearner is list()-object for caretList(..., methodList) note that to specify tuning 
  #                                                             method this must be adjusted 
  #                                                             to comply to tuneList
  # metalearner is chosen algorithm for caretStack(..., method =), needs to be in quotation marks
  
    obj_name <- load(file)
    data <- get(obj_name)
    
    yMat <- data$yMat
    Xint <- data$X_int
    data <- as.data.frame(Xint)
    data$krit <- yMat[,"R20.8lin_inter0.2_0.8"]
    data <- data[, !grepl(":", colnames(data))]
    
    nested_list <- list()
    
    for(i in 1:iter) {
      
      test_data  <- testsample # infromation taken from function
      train_data <- data       # taken from processing of current file/sample
      
      preds <- names(data[!(names(data) %in% c("krit"))]) # predictor character string
      mod <- as.formula(paste("krit ~ ", paste(preds, collapse = "+"))) # additive predictor combination
      
      trainCtrl <- trainControl(method = "cv",       # specification of tuning in inner cv for baselearner
                                number = 10,
                                savePredictions = "final", # saves predictions for optimal tuning parameters
                                allowParallel = F) # must be set to FALSE, as we parallelize the outer resampling
      
      models <- caretList(mod,
                          data = train_data,
                          trControl = trainCtrl,
                          metric = "MAE",
                          methodList = baselearner
      )
      
      ensemCtrl <- trainControl(method = "cv",       # specification of tuning in inner cv for metalearner
                                number = 10,
                                savePredictions = "final", # saves predictions for optimal tuning parameters
                                allowParallel = F) # must be set to FALSE, as we parallelize the outer resampling
      
      
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
        )                                     
      }
      
      # saving hyperparameters in a list
      hyperparameters <- list(glmnet = models$glmnet$bestTune,
                              rpart = models$rpart$bestTune,
                              gbm = models$gbm$bestTune,
                              rf = models$rf$bestTune,
                              ensemble = ensemble$ens_model$bestTune)
      
      # saving weight of metalearner
      if(metalearner == "glm") {
        weights_metamodel <- ensemble$ens_model$finalModel$coefficients
      } else if(metalearner == "glmnet") {
        weights_metamodel <- as.matrix(coef(ensemble$ens_model$finalModel,            
                                            s = ensemble$ens_model$bestTune$lambda))
      } else if(metalearner == "ranger") {
        weights_metamodel <- ensemble$ens_model$finalModel$variable.importance
      } else{paste0("No specification of weight extraction for this metalearner")}   
      
      
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
      
      train_perf = cbind(train_perf, iter = rep(i, length(baselearner)+1))
      
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
      
      models <- c("glmnet", "rpart", "gbm", "ranger", "ensemble")
      test_perf = cbind(test_perf, models, iter = rep(i, length(baselearner)+1))
      
      train_perf$dataset <- basename(file)  # add sample name to train-output
      test_perf$dataset  <- basename(file)  # add sample name to test-output
      
      weights_name <- paste0("weights_", basename(file))
      hyperparameters_name <- paste0("hyperparameters_", basename(file))
      
      iter_list <- list(train_perf = train_perf, test_perf = test_perf)
      nested_list[[i]] <- setNames(list(iter_list, hyperparameters, weights_metamodel),
                                   c("iter_list", hyperparameters_name, weights_name))
      
    }
    return(nested_list)
}
