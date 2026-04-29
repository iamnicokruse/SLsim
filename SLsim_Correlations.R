################################################################################
#______________________Predictor/Prediction Correlations#______________________#
################################################################################

## load packages
library(nlme)
library(corrplot)

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