################################################################################
#______________________Predictor/Prediction Correlations#______________________#
################################################################################

## load packages
library(nlme)

## Correlations between Predictors and dgp-relevant effects
testList <- get(load("testList.rda"))

# dgp = "inter"
predsInter <- as.data.frame(testList$inter$X_int[, c("Var1", "Var2", "Var3", "Var4", 
                                                     "Var1:Var2", "Var1:Var4", "Var2:Var3", 
                                                     "Var3:Var4")])
corInter <- cor(predsInter)

# dgp = "pwlinear"
predsPw <- as.data.frame(testList$pwlinear$X_int[, c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7")])
  
# dgp = "nonlinear3"
predsNl3 <- as.data.frame(testList$nonlinear3$X_int[, c("Var1", "Var2", "Var3", "Var4", "Var5", "Var6", "Var7")])
