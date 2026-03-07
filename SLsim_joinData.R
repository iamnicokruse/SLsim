###############################################################################
#____________________________join data for analysis___________________________#
###############################################################################

# Condition grid
#   3 sample size (100, 1000, 3000) - N
# x 2 reliability (0.7, 1.0)
# x 3 data generating processes (inter, pwlinear, nonlinear3) - dgp
# x 3 effect distribution (50:50, 0:100, 100:0) - linear vs. dgp
# x 3 Rsquared (0.2, 0.5, 0.8)
# x 4 meta model of super learner (nnls, glm, glmnet, ranger)

# there are performance measures (train and test), meta model weights 
# (+ standardized), tuned hyperparameters for base and meta learner for each
# of the 648 combination -> there are 100 samples for each condition

# each rda-file contains one sample for all combinations within a combination
# of sample size, reliability and dgp 

# import files
source("MLsim-main/utils/setParameters.R")

# folder to save restructured data
resFolder <- paste0("results")

# join data across iterations (1800 files into 18 files)
dgpVec <- c("inter", "pwlinear", "nonlinear3")
condGrid <- expand.grid(data = dgpVec,
                        N = setParam$dgp$N,
                        reliability = setParam$dgp$reliability
                        )  
  
for (iDGP in dgpVec) {
  # generate folder to save new rda files
  depMeasures = paste0(resFolder, "/", iDGP, "/dependentMeasures")
  if (!file.exists(depMeasures)){
    dir.create(depMeasures)
  }
  
    subGrid <- condGrid[which(condGrid$data == iDGP),]
    performPerSample <- list()
    
  for(iSim in seq_len(nrow(subGrid))) {
    sampleVec <- c(1:100)
      
    for (iFolder in 1:10){
      resFilePath <- paste0("results/", iDGP, "/", iFolder, "/")
        
      for(iSample in 1:10){
        sampleNumber <- sampleVec[10 * iFolder - 10 + iSample] # used to extract the files according to sample
        resFileName <- paste0("res_", iDGP, "_N", subGrid[iSim, "N"], "_rel", subGrid[iSim,"reliability"],
                          "_sample", sampleNumber, ".rda")
          
        tmp <- get(load(paste0(resFilePath,resFileName)))
          
        # the extracted list can now be used to save several outcomes of each list into new
        # lists/dataframes whioch can then be saved as rda files, as well
          
        if(iFolder == 1 & iSample == 1) {
          CondVec <- names(tmp[[1]])
        }
        
        for (iCond in CondVec) {
          performPerSample[[iCond]][[sampleNumber]] <- tmp[[iDGP]][[iCond]]
        }
      }
    }
    file_name <- paste0(depMeasures, "/res_", iDGP, "_N", subGrid[iSim, "N"], "_rel", 
                        subGrid[iSim,"reliability"], "_6x9_all_iter.rda")
    save(performPerSample, file = file_name)
    print("done")
    gc()
  }
}

# now there are 18 files, each containing all samples of all combinations within a
# combination of sample size, reliability and dgp.

# merge data to compare performances performances between dgp x N combinations
# (done for train and test to see potential overfit, as well)

for (jDGP in dgpVec) {
  subGrid <- condGrid[which(condGrid$data == jDGP),]
  filePath <- paste0(resFolder, "/", jDGP, "/dependentMeasures/")
  trainPerf <- list()
  testPerf <- list()
  trainPerf_SLspec <- list()
  testPerf_SLspec <- list()
  
  for (iFile in 1:6) {
    fileName <- paste0("res_", jDGP, "_N", subGrid[iFile, "N"], "_rel",
                       subGrid[iFile, "reliability"], "_6x9_all_iter.rda")
    
    tmp <- get(load(paste0(filePath, fileName)))
    CondVec <- names(tmp)
    
    for(iCond in CondVec) {
      metamodelVec <- setParam$modfit$superlearner
      
      for(iMetamodel in metamodelVec) {
        idxN <- paste0("N", subGrid[iFile, "N"])
        idxMetamodel <- paste0("sl_algorithm_", iMetamodel)
        
        if (subGrid[iFile, "reliability"] == 0.7) {
          idxRel = "1"
        } else {idxRel = "2"}
        
        for(iSample in 1:100) {
          trainPerf[[jDGP]][[idxN]] <- c(
            trainPerf[[jDGP]][[idxN]],
            tmp[[iCond]][[iSample]][[idxMetamodel]]$train_perf
          )
          testPerf[[jDGP]][[idxN]] <- c(
            testPerf[[jDGP]][[idxN]],
            tmp[[iCond]][[iSample]][[idxMetamodel]]$test_perf
          )
          
          trainPerf_SLspec[[jDGP]][[idxN]][[idxMetamodel]] <- c(
            trainPerf_SLspec[[jDGP]][[idxN]][[idxMetamodel]],
            tmp[[iCond]][[iSample]][[idxMetamodel]]$train_perf
          )
          testPerf_SLspec[[jDGP]][[idxN]][[idxMetamodel]] <- c(
            testPerf_SLspec[[jDGP]][[idxN]][[idxMetamodel]],
            tmp[[iCond]][[iSample]][[idxMetamodel]]$test_perf
          )
          
          # if needed, weights (scaled) and hyperparameters can be 
          # summarized this way, as well
          
        }
      }  
    }
  }
  depMeasures = paste0(resFolder, "/", jDGP, "/dependentMeasures")
  
  trainPerfFile <- paste0(depMeasures, "/res_train_perf_", jDGP, ".rda")
  save(trainPerf, file = trainPerfFile)
  
  testPerfFile <- paste0(depMeasures, "/res_test_perf_", jDGP, ".rda")
  save(testPerf, file = testPerfFile)
  
  trainPerfFile_SLspec <- paste0(depMeasures, "/res_train_perf_SLspec_", jDGP, ".rda")
  save(trainPerf_SLspec, file = trainPerfFile_SLspec)
  
  testPerfFile_SLspec <- paste0(depMeasures, "/res_test_perf_SLspec_", jDGP, ".rda")
  save(testPerf_SLspec, file = testPerfFile_SLspec)
  
  print("done")
  gc()
}

# Use priorly summarized lists to built data frames for each dgp and save 
# performances within these as columns (one for each performance metric x
# algorithm x N combination). These data frames can then be used to analyze 
# performances across all dgp x N combinations. Done for train and test 
# performance (table 45 x 7200 obs)

for (hDGP in dgpVec) {
  filePath <- paste0(filePath <- paste0(resFolder, "/", hDGP, "/dependentMeasures/"))
  perfVec <- c("train", "test")
  
  for (iPerf in perfVec) {
    fileName <- paste0("res_", iPerf, "_perf_", hDGP, ".rda")
    tmp <- get(load(paste0(filePath, fileName)))
    nVec <- c("N100", "N1000", "N3000")
    
    for (iN in nVec) {
     if (iN == "N100") {
       nRow <- length(tmp[[hDGP]][[iN]])/4
       PerfDF <- data.frame(matrix(nrow = nRow, ncol = 0))
     }
      metricVec <- c("RMSE", "MAE", "Rsquared")
      
      for (iMetric in metricVec) {
        algoVec <- c("glmnet", "rpart", "gbm", "ranger", "ensemble")
        
        for(iAlgo in algoVec) {
          if (iPerf == "train") {
            idxMetric <- paste0("Train", iMetric)
          } else if (iPerf == "test") {
            idxMetric = paste0("Test", iMetric)
          }
          idxValue <- which(algoVec == iAlgo)
          
          valueVec <- c()
          value <- NULL
          for (iValues in seq_len(length(tmp[[hDGP]][[iN]]))) { 
           if (names(tmp[[hDGP]][[iN]][iValues]) == idxMetric) {
              value = tmp[[hDGP]][[iN]][[iValues]][idxValue]
              valueVec <- c(valueVec, value)
           }
          }
        PerfDF[(paste0(iN, "_", iMetric, "_", iAlgo))] <- valueVec
        }
      }
    }
    depMeasures = paste0(resFolder, "/", hDGP, "/dependentMeasures")
    dfName <- paste0(depMeasures, "/res_", hDGP, "_df_",iPerf ,".rda")
    save(PerfDF, file = dfName)
  }
  print("done")
  gc()
}

# repeat above used loop to create data frames similar but with columns for each
# metalearner algorithm (table 72 x 1800 obs)

for (kDGP in dgpVec) {
  filePath <- paste0(filePath <- paste0(resFolder, "/", kDGP, "/dependentMeasures/"))
  perfVec <- c("train", "test")
  
  for (iPerf in perfVec) {
    fileName <- paste0("res_", iPerf, "_perf_SLspec_", kDGP, ".rda")
    tmp <- get(load(paste0(filePath, fileName)))
    nVec <- c("N100", "N1000", "N3000")
    
    for (iN in nVec) {
      slVec <- c("sl_algorithm_nnls", "sl_algorithm_glm", "sl_algorithm_glmnet", 
                 "sl_algorithm_ranger")
      
      for (iSL in slVec) {
        if (iN == "N100" & iSL == "sl_algorithm_nnls") {
          nRow <- length(tmp[[kDGP]][[iN]][[iSL]])/4
          PerfDF_SLspec <- data.frame(matrix(nrow = nRow, ncol = 0))
        }
        metricVec <- c("RMSE", "MAE", "Rsquared")
        
        for (iMetric in metricVec) {
          algoVec <- c("glmnet", "rpart", "gbm", "ranger", "ensemble")
          
          for(iAlgo in algoVec) {
            if (iPerf == "train") {
              idxMetric <- paste0("Train", iMetric)
            } else if (iPerf == "test") {
              idxMetric = paste0("Test", iMetric)
            }
            idxValue <- which(algoVec == iAlgo)
            
            valueVec <- c()
            value <- NULL
            for (iValues in seq_len(length(tmp[[kDGP]][[iN]][[iSL]]))) { 
              if (names(tmp[[kDGP]][[iN]][[iSL]][iValues]) == idxMetric) {
                value = tmp[[kDGP]][[iN]][[iSL]][[iValues]][idxValue]
                valueVec <- c(valueVec, value)
              }
            }
            if(iAlgo == "ensemble") {
              iAlgo <- paste0(iSL)
            }
            PerfDF_SLspec[(paste0(iN, "_", iMetric, "_", iAlgo))] <- valueVec
          }
        }
      }
    }
    depMeasures = paste0(resFolder, "/", kDGP, "/dependentMeasures")
    dfName <- paste0(depMeasures, "/res_", kDGP, "_df_SLspec_",iPerf ,".rda")
    save(PerfDF_SLspec, file = dfName)
  }
  print("done")
  gc()
}

# check if it worked (perfDf row 1:1800 should match perfDF rows as all values for sl_algorithm_nnls where extracted
# first)