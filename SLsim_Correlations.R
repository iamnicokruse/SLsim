################################################################################
#______________________Predictor/Prediction Correlations_______________________#
################################################################################

## load packages and parameters
library(nlme)
library(corrplot)
library(tidyverse)
library(purrr)


source("MLsim-main/utils/setParameters.R")

## Correlations between Predictors and dgp-relevant effects (reliability = 1, N = 1000)
testList <- get(load("testList.rda"))

# dgp = "inter"
predsInter <- as.data.frame(testList[[2]]$X_int[, c("Var1", "Var2", "Var3", "Var4", 
                                                     "Var1:Var2", "Var1:Var4", "Var2:Var3", 
                                                     "Var3:Var4")])
corInter <- cor(predsInter)
corrplot(corInter, title = "inter", method = "number")

# dgp = "pwlinear"
predsPw <- as.data.frame(testList[[4]]$X_int[, c("Var1", "Var2", "Var3", "Var4", 
                                                 "Var5", "Var6", "Var7")])
corPw <- cor(predsPw)
corrplot(corPw, title = "pw", method = "number")

# dgp = "nonlinear3"
predsNl3 <- as.data.frame(testList[[6]]$X_int[, c("Var1", "Var2", "Var3", "Var4", 
                                                  "Var5", "Var6", "Var7")])
corNl3 <- cor(predsNl3)
corrplot(corNl3, title = "nl3", method = "number")
# corMatrices for pwlinear and nonlinear3 are identical. Correlations of linear predictors
# for dgp inter are different



## Correlations between Predictions within each condition combination

# Function to calculate condition combination specific prediction correlations
getPredCorrelations <- function(dgp = c("inter", "pwlinear", "nonlinear3"), 
                                N = c(100, 1000, 3000), 
                                reliability = c(0.7, 1), 
                                R2sim = c(0.2, 0.5, 0.8), 
                                lin_effect = c(0.0, 0.5, 1.0), 
                                corrPlot = FALSE) {
  
  match.arg(dgp)
  if (!N %in% c(100, 1000, 3000)) stop("Invalid N")
  if (!reliability %in% c(0.7, 1)) stop("Invalid reliability")
  if (!R2sim %in% c(0.2, 0.5, 0.8)) stop("Invalid R2sim")
  if (!lin_effect %in% c(0.0, 0.5, 1.0)) stop("Invalid lin_effect")
  
  filePath <- paste0("results_withPreds/", dgp, "/")
  fileName <- paste0("res_", dgp, "_N", N, "_rel", reliability, "_sample1.rda")
  
  tmp <- get(load(paste0(filePath, fileName)))
  
  if (lin_effect == 0.0) {
    lin_inter = "lin_inter0.0_1.0"
  } else if (lin_effect == 0.5) {
    lin_inter = "lin_inter0.5_0.5" 
    } else if (lin_effect == 1.0) {
      lin_inter = "lin_inter1.0_0.0" 
    }
  
  cond <- paste0("R2", R2sim, lin_inter)
  tmpDF <- as.data.frame(tmp[[dgp]][[cond]]$sl_algorithm_glmnet$test_predictions) %>%
    mutate("sl_glmnet" = pred) %>%
    mutate(pred = NULL) %>%
    rename_with(~ str_replace_all(.x, "_pred", ""))
  corMatrix <- cor(tmpDF)
  
  if (corrPlot == TRUE) {
  corrPlot = corrplot(corMatrix, method = "number")
    }
  return(corMatrix)
}

# create list-Object with correlation matrices for all condition combinations
corrGrid <- expand.grid(dgp = c("inter", "pwlinear", "nonlinear3"), 
                        N = c(100, 1000, 3000), 
                        reliability = c(0.7, 1), 
                        R2sim = c(0.2, 0.5, 0.8), 
                        lin_effect = c(0.0, 0.5, 1.0),
                        stringsAsFactors = FALSE )

predCorrList <- pmap(corrGrid, getPredCorrelations)

# list number in predCorrList matches row number in corrGrid which might be useful
# for further analysis

# naming list number according to row-specific condition combination
names(predCorrList) <- apply(corrGrid, 1, function(row) {
  paste(names(row), row, sep = "=", collapse = "_")
})








