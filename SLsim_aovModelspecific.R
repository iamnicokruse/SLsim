################################################################################
#_______________________Model specific ANOVA calculations______________________#
################################################################################

# loading packages and data
library(afex) # for aov_ez()
library(effectsize) # for efect size calculation; generalized eta²
library(emmeans)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(purrr)
library(hardhat)

load("results/anova/aov_data.rda")
source("MLsim-main/utils/setParameters.R")

modelVec <- c(paste0(setParam$modfit$baselearner), paste0(paste0("sl_algorithm_", setParam$modfit$superlearner)))
eta2_modelSpec <- list()

for (iModel in modelVec) {
  data <- subset(aov_data, Model == iModel)
  
  aov <- aov_ez(id = "ID",
                         dv = "test_rsquared",
                         data = data,
                         between = c("dgp", "lin_inter", "N", "R2", "rel"))
  
  eta2 <- eta_squared(
    aov, # fitted model
    partial = FALSE, # not partial!
    generalized = TRUE, # generalized eta squared
    ci = 0.95,
    verbose = TRUE)
  
  eta2_ordered <- eta2[order(eta2$Eta2_generalized, decreasing = T),]
  
  eta2_modelSpec[[iModel]] <- eta2_ordered
}

save(eta2_modelSpec, file = "results/anova/Eta2_modelspecific.rda")
