###############################################################################
#___________________________________Analysis__________________________________#
###############################################################################

# load packages and parameters

library(afex) # for aov_ez()
library(effectsize) # for efect size calculation; generalized eta²
library(emmeans)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(purrr)

source("MLsim-main/utils/setParameters.R")

# load and restructure data to match expected data frame for anova 
dgpVec <- c("inter", "pwlinear", "nonlinear3")

for (iDGP in dgpVec) {
  dataVec = c("train", "test")
  for(iData in dataVec) {
    
    filePath <- paste0("results/", iDGP, "/dependentMeasures/")
    fileName <- paste0("res_", iDGP, "_df_allCond_", iData, ".rda")
    
    tmp <- get(load(paste0(filePath, fileName)))
    if(iData == "train") {
      tmp_long <- pivot_longer(tmp,
                          cols = everything(),      
                          names_to = "ID",
                          values_to = "train_Value")
      
      tmp_long <- separate(tmp_long, ID, into = c("rel", "R2_tmp", "lin_inter_tmp",
                                                  "lin_inter_tmp2", "N", "Metric", 
                                                  "Model_tmp", "ens1", "ens2"), 
                           sep = "_") %>%
        mutate(R2 = as.factor(sub("R2([0-9.]+)lin", "\\1", R2_tmp))) %>%
        mutate(Model = case_when(Model_tmp == "sl" ~ paste(Model_tmp, ens1, ens2, 
                                                           sep = "_"),
                                 Model_tmp != "sl" ~ Model_tmp)) %>%
        mutate(lin_inter = as.factor(paste("lin", lin_inter_tmp, lin_inter_tmp2, 
                                           sep = "_")))
      
      tempCols <- c("R2_tmp", "lin_inter_tmp", "lin_inter_tmp2", "Model_tmp", 
                    "ens1", "ens2")
      tmp_long <- tmp_long[, !names(tmp_long) %in% tempCols]
      tmp_long$dgp <- iDGP
      tmp_long <- tmp_long[, order(colnames(tmp_long))]
      
      rmse <- subset(tmp_long, Metric == "RMSE") %>%
        group_by(dgp, lin_inter, Model, N, R2, rel) %>%
        mutate(sample_no = seq_len(n())) %>%
        ungroup() %>%
        mutate(ID = interaction(dgp, lin_inter, Model, N, R2, rel, sample_no, drop = TRUE)) %>%
        rename(!!paste0(iData, "_rmse") := !!sym(paste0(iData, "_Value")))
      rmse$Metric <- NULL
      rmse$sample_no <- NULL
      
      mae  <- subset(tmp_long, tmp_long$Metric == "MAE") %>%
        group_by(dgp, lin_inter, Model, N, R2, rel) %>%
        mutate(sample_no = seq_len(n())) %>%
        ungroup() %>%
        mutate(ID = interaction(dgp, lin_inter, Model, N, R2, rel, sample_no, drop = TRUE))%>%
        rename(!!paste0(iData, "_mae") := !!sym(paste0(iData, "_Value")))
      mae$Metric <- NULL
      mae$sample_no <- NULL
      
      rsq  <- subset(tmp_long, tmp_long$Metric == "Rsquared") %>%
        group_by(dgp, lin_inter, Model, N, R2, rel) %>%
        mutate(sample_no = seq_len(n())) %>%
        ungroup() %>%
        mutate(ID = interaction(dgp, lin_inter, Model, N, R2, rel, sample_no, drop = TRUE)) %>%
        rename(!!paste0(iData, "_rsquared") := !!sym(paste0(iData, "_Value")))
      rsq$Metric <- NULL
      rsq$sample_no <- NULL
      
      tmp_final = left_join(rmse, mae, by = c("dgp", "lin_inter", "Model", "N", "R2", "rel", "ID"))
      tmp_final = left_join(tmp_final, rsq, by = c("dgp", "lin_inter", "Model", "N", "R2", "rel", "ID"))
      tmp_final <- tmp_final[, order(colnames(tmp_final))]
      
    } else if (iData == "test") {
      tmp_long <- pivot_longer(tmp,
                               cols = everything(),      
                               names_to = "ID",
                               values_to = "test_Value")
      
      tmp_long <- separate(tmp_long, ID, into = c("rel", "R2_tmp", "lin_inter_tmp",
                                                  "lin_inter_tmp2", "N", "Metric", 
                                                  "Model_tmp", "ens1", "ens2"), 
                           sep = "_") %>%
        mutate(R2 = as.factor(sub("R2([0-9.]+)lin", "\\1", R2_tmp))) %>%
        mutate(Model = case_when(Model_tmp == "sl" ~ paste(Model_tmp, ens1, ens2, 
                                                           sep = "_"),
                                 Model_tmp != "sl" ~ Model_tmp)) %>%
        mutate(lin_inter = as.factor(paste("lin", lin_inter_tmp, lin_inter_tmp2, 
                                           sep = "_")))
      
      tempCols <- c("R2_tmp", "lin_inter_tmp", "lin_inter_tmp2", "Model_tmp", 
                    "ens1", "ens2")
      tmp_long <- tmp_long[, !names(tmp_long) %in% tempCols]
      tmp_long$dgp <- iDGP
      tmp_long <- tmp_long[, order(colnames(tmp_long))]
      
      rmse <- subset(tmp_long, Metric == "RMSE") %>%
        group_by(dgp, lin_inter, Model, N, R2, rel) %>%
        mutate(sample_no = seq_len(n())) %>%
        ungroup() %>%
        mutate(ID = interaction(dgp, lin_inter, Model, N, R2, rel, sample_no, drop = TRUE)) %>%
        rename(!!paste0(iData, "_rmse") := !!sym(paste0(iData, "_Value")))
      rmse$Metric <- NULL
      rmse$sample_no <- NULL
      
      mae  <- subset(tmp_long, tmp_long$Metric == "MAE") %>%
        group_by(dgp, lin_inter, Model, N, R2, rel) %>%
        mutate(sample_no = seq_len(n())) %>%
        ungroup() %>%
        mutate(ID = interaction(dgp, lin_inter, Model, N, R2, rel, sample_no, drop = TRUE))%>%
        rename(!!paste0(iData, "_mae") := !!sym(paste0(iData, "_Value")))
      mae$Metric <- NULL
      mae$sample_no <- NULL
      
      rsq  <- subset(tmp_long, tmp_long$Metric == "Rsquared") %>%
        group_by(dgp, lin_inter, Model, N, R2, rel) %>%
        mutate(sample_no = seq_len(n())) %>%
        ungroup() %>%
        mutate(ID = interaction(dgp, lin_inter, Model, N, R2, rel, sample_no, drop = TRUE)) %>%
        rename(!!paste0(iData, "_rsquared") := !!sym(paste0(iData, "_Value")))
      rsq$Metric <- NULL
      rsq$sample_no <- NULL
      
      tmp_final = left_join(rmse, mae, by = c("dgp", "lin_inter", "Model", "N", "R2", "rel", "ID"))
      tmp_final = left_join(tmp_final, rsq, by = c("dgp", "lin_inter", "Model", "N", "R2", "rel", "ID"))
      tmp_final <- tmp_final[, order(colnames(tmp_final))]
    }
 
    if (iData == "train") {
    train <- tmp_final
    } else if (iData == "test") {
      test <- tmp_final
      aov_dgpSpec <- left_join(train, test,
                               by = c("dgp", "lin_inter", "Model", "N", "R2", "rel", "ID"),
                               suffix = c("", ""))
    }
  }
  
  if (iDGP == "inter") {
    aov_data <- aov_dgpSpec
  } else {
    aov_data <- rbind(aov_data, aov_dgpSpec)
  }
}








# aov_data for anova w/ 
# ... between: 3 x 2 x 3 x 3 x 3  
#   N         (3)   {100, 1000, 3000}
#   rel       (2)   {0.7, 1}
#   R2        (3)   {0.2, 0.5, 0.8}
#   lin_inter (3)   {0.0_1.0, 0.5_0.5, 1.0_0.0}
#   dgp       (3)   {inter, pwlinear, nonlinear3}
# ... within:
#   model {glmnet, rpart, gbm, ranger}




   
rmse <- subset(aov_data, aov_data$Metric == "RMSE") %>%
    group_by(N, dgp, Model) %>%
    mutate(sim = row_number()) %>%
    ungroup() %>%
    mutate(ID = interaction(N, dgp, sim, drop = TRUE)) 

mae  <- subset(aov_data, aov_data$Metric == "MAE") %>%
  group_by(N, dgp, Model) %>%
  mutate(sim = row_number()) %>%
  ungroup() %>%
  mutate(ID = interaction(N, dgp, sim, drop = TRUE))

rsq  <- subset(aov_data, aov_data$Metric == "Rsquared") %>%
  group_by(N, dgp, Model) %>%
  mutate(sim = row_number()) %>%
  ungroup() %>%
  mutate(ID = interaction(N, dgp, sim, drop = TRUE))
 
   
aov_rmse <- aov_ez(id = "ID",
                   dv = "Value",
                   data = rmse,
                   between = c("N", "dgp"),
                   within = "Model",
                   fun_aggregate = mean)

aov_mae <- aov_ez(id = "ID",
                  dv = "Value",
                  data = mae,
                  between = c("N", "dgp"),
                  within = "Model",
                  fun_aggregate = mean)

aov_rsquared <- aov_ez(id = "ID",
                       dv = "Value",
                       data = rsq,
                       between = c("N", "dgp"),
                       within = "Model",
                       fun_aggregate = mean)






 
  
  
  

  





















# anova mit sample faktoren -> delta Performanz Super learner vs. bestes Einzelmodell
# welche bedingungen sind besonders bedeutend für plots 
# generalisiertes eta quadrat
# marginale means -> post hoc tests (post hoc e means) -> HE nicht Interaktionen