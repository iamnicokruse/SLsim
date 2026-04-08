################################################################################
#_____________________________Investigating weights____________________________#
################################################################################

# load packages
library(ggplot2)
library(ggh4x)
library(tidyverse)

# get parameter values and utility functions 
source("MLsim-main/utils/setParameters.R")

# restructuring data to long format 
for (iDGP in dgpVec) {
  dataVec = c("train", "test")
  for(iData in dataVec) {
    
    filePath <- paste0("results/", iDGP, "/dependentMeasures/")
    fileName <- paste0("res_", iDGP, "_df_scaledWeightsGLMnet_", iData, ".rda")
    
    tmp <- get(load(paste0(filePath, fileName)))
    
    if(iData == "train") {
      tmp_long <- pivot_longer(tmp,
                               cols = everything(),      
                               names_to = "ID",
                               values_to = "scaled_weight_train")
      
      tmp_long <- separate(tmp_long, ID, into = c("rel", "R2_tmp", "lin_inter_tmp",
                                                  "lin_inter_tmp2", "N", "baselearner"), 
                           sep = "_") %>%
        mutate(R2 = as.factor(sub("R2([0-9.]+)lin", "\\1", R2_tmp))) %>%
        mutate(lin_inter = as.factor(paste("lin", lin_inter_tmp, lin_inter_tmp2, 
                                           sep = "_")))
      
      tempCols <- c("R2_tmp", "lin_inter_tmp", "lin_inter_tmp2")
      tmp_long <- tmp_long[, !names(tmp_long) %in% tempCols]
      tmp_long$dgp <- iDGP
      tmp_long <- tmp_long[, order(colnames(tmp_long))]
      
      baselearnerVec <- c("intercept", "rpart", "ranger", "gbm", "glmnet")
      
      for (iBase in baselearnerVec) {
        tmp_df <- subset(tmp_long, baselearner == iBase) %>%
          group_by(dgp, lin_inter, baselearner, N, R2, rel) %>%
          mutate(sample_no = seq_len(n())) %>%
          ungroup() %>%
          mutate(ID = interaction(dgp, lin_inter, N, R2, rel, sample_no, drop = TRUE)) %>%
          rename(!!paste0(iData, "_", iBase) := !!sym(paste0("scaled_weight_train")))
        tmp_df$baselearner <- NULL
        tmp_df$sample_no <- NULL
        
        tmp_df_name <- "tmp_df"
        baselearner_name <- paste0("tmp_", iBase)
        assign(baselearner_name, get(tmp_df_name))
      }
      
      tmp_final = left_join(tmp_intercept, tmp_rpart, by = c("dgp", "lin_inter", 
                                                             "N", "R2", "rel", "ID"))
      tmp_final = left_join(tmp_final, tmp_ranger, by = c("dgp", "lin_inter", 
                                                          "N", "R2", "rel", "ID"))
      tmp_final = left_join(tmp_final, tmp_gbm, by = c("dgp", "lin_inter", "N", 
                                                       "R2", "rel", "ID"))
      tmp_final = left_join(tmp_final, tmp_glmnet, by = c("dgp", "lin_inter", 
                                                          "N", "R2", "rel", "ID"))
      tmp_final <- tmp_final[, order(colnames(tmp_final))]
      
    } else if (iData == "test") {
      tmp_long <- pivot_longer(tmp,
                               cols = everything(),      
                               names_to = "ID",
                               values_to = "scaled_weight_test")
      
      tmp_long <- separate(tmp_long, ID, into = c("rel", "R2_tmp", "lin_inter_tmp",
                                                  "lin_inter_tmp2", "N", "baselearner"), 
                           sep = "_") %>%
        mutate(R2 = as.factor(sub("R2([0-9.]+)lin", "\\1", R2_tmp))) %>%
        mutate(lin_inter = as.factor(paste("lin", lin_inter_tmp, lin_inter_tmp2, 
                                           sep = "_")))
      
      tempCols <- c("R2_tmp", "lin_inter_tmp", "lin_inter_tmp2")
      tmp_long <- tmp_long[, !names(tmp_long) %in% tempCols]
      tmp_long$dgp <- iDGP
      tmp_long <- tmp_long[, order(colnames(tmp_long))]
      
      baselearnerVec <- c("intercept", "rpart", "ranger", "gbm", "glmnet")
      
      for (iBase in baselearnerVec) {
        tmp_df <- subset(tmp_long, baselearner == iBase) %>%
          group_by(dgp, lin_inter, baselearner, N, R2, rel) %>%
          mutate(sample_no = seq_len(n())) %>%
          ungroup() %>%
          mutate(ID = interaction(dgp, lin_inter, N, R2, rel, sample_no, drop = TRUE)) %>%
          rename(!!paste0(iData, "_", iBase) := !!sym(paste0("scaled_weight_test")))
        tmp_df$baselearner <- NULL
        tmp_df$sample_no <- NULL
        
        tmp_df_name <- "tmp_df"
        baselearner_name <- paste0("tmp_", iBase)
        assign(baselearner_name, get(tmp_df_name))
      }
      
      tmp_final = left_join(tmp_intercept, tmp_rpart, by = c("dgp", "lin_inter",
                                                             "N", "R2", "rel", "ID"))
      tmp_final = left_join(tmp_final, tmp_ranger, by = c("dgp", "lin_inter",
                                                          "N", "R2", "rel", "ID"))
      tmp_final = left_join(tmp_final, tmp_gbm, by = c("dgp", "lin_inter", "N",
                                                       "R2", "rel", "ID"))
      tmp_final = left_join(tmp_final, tmp_glmnet, by = c("dgp", "lin_inter", "N",
                                                          "R2", "rel", "ID"))
      tmp_final <- tmp_final[, order(colnames(tmp_final))]
    }
    if (iData == "train") {
      train <- tmp_final
    } else if (iData == "test") {
      test <- tmp_final
      scaledWeightsGLMnetDFwide_dgpSpec <- left_join(train, test,
                                                     by = c("dgp", "lin_inter",
                                                            "N", "R2", "rel", "ID"),
                                                     suffix = c("", ""))
    }
  }
  
  if (iDGP == "inter") {
    scaledWeightsGLMnetDFwide <- scaledWeightsGLMnetDFwide_dgpSpec
  } else {
    scaledWeightsGLMnetDFwide <- rbind(scaledWeightsGLMnetDFwide, 
                                       scaledWeightsGLMnetDFwide_dgpSpec)
  }
}

# focus lies on lin_inter_50:50 conditions
scaledWeights_0.5_0.5_conds <- subset(scaledWeightsGLMnetDFwide, 
                                      lin_inter == "lin_inter0.5_0.5")

# check if weights always add up to 1
# (rowSums(scaledWeights_0.5_0.5_conds[, c(12,13,15,16)], na.rm = TRUE))
# where rowSum = 0 -> weights are NaN (convergence issue with model)

scaledWeights_0.5_0.5_condsDescriptives <- scaledWeights_0.5_0.5_conds %>%
  group_by(N, R2, dgp, rel
  ) %>%
  summarise(
    times_chosen_rpart = sum(test_rpart > 0, na.rm = TRUE),
    M_rpart  = mean(test_rpart[test_rpart > 0], na.rm = TRUE),
    SD_rpart = sd(test_rpart[test_rpart > 0], na.rm = TRUE),
    SE_rpart = SD_rpart / sqrt(times_chosen_rpart),
    
    times_chosen_ranger = sum(test_ranger > 0, na.rm = TRUE),
    M_ranger  = mean(test_ranger[test_ranger > 0], na.rm = TRUE),
    SD_ranger = sd(test_ranger[test_ranger > 0], na.rm = TRUE),
    SE_ranger = SD_ranger / sqrt(times_chosen_ranger),
    
    times_chosen_gbm = sum(test_gbm > 0, na.rm = TRUE),
    M_gbm  = mean(test_gbm[test_gbm > 0], na.rm = TRUE),
    SD_gbm = sd(test_gbm[test_gbm > 0], na.rm = TRUE),
    SE_gbm = SD_gbm / sqrt(times_chosen_gbm),
    
    times_chosen_glmnet = sum(test_glmnet > 0, na.rm = TRUE),
    M_glmnet  = mean(test_glmnet[test_glmnet > 0], na.rm = TRUE),
    SD_glmnet = sd(test_glmnet[test_glmnet > 0], na.rm = TRUE),
    SE_glmnet = SD_glmnet / sqrt(times_chosen_glmnet),
    
    .groups = "drop"
  )

# as weights are expected differently for different dgps they should be investigated
# separately
weightDescriptives_inter <- subset(scaledWeights_0.5_0.5_condsDescriptives, 
                                   dgp == "inter")
weightDescriptives_pw <- subset(scaledWeights_0.5_0.5_condsDescriptives, 
                                dgp == "pwlinear")
weightDescriptives_nl3 <- subset(scaledWeights_0.5_0.5_condsDescriptives, 
                                 dgp == "nonlinear3")


## weight destribution as violin plot
# full data is needed for each dgp
rmCols <- c("train_gbm", "train_glmnet", "train_ranger", "train_rpart", 
            "train_intercept", "test_intercept")
scaledWeights_0.5_0.5_conds <- scaledWeights_0.5_0.5_conds[, !names(scaledWeights_0.5_0.5_conds)
                                                           %in% rmCols]
scaledWeights_0.5_0.5_inter = subset(scaledWeights_0.5_0.5_conds, dgp == "inter" & rel == "1")
  
vPlotData_inter <-  pivot_longer(scaledWeights_0.5_0.5_inter,
                             cols = c(test_gbm, test_glmnet, test_ranger, test_rpart),     
                             names_to = "baselearner",
                             values_to = "scaled_weight")
vPlotData_inter$baselearner <- gsub("test_", "", vPlotData_inter$baselearner)
  

violin_plot <- function(data, plotMeasure, title = "", yLabel = "") {
  ggplot(data, 
         aes(x = baselearner, y = plotMeasure, 
             group = baselearner, color = baselearner)) +
    geom_violin() +
    facet_grid2(R2 ~ N)
}

boxplotCustom <- function(data, plotMeasure, title = "", yLabel = "") {
  ggplot(data, 
         aes(x = baselearner, y = plotMeasure, 
             group = baselearner, color = baselearner)) +
  geom_boxplot(width = 0.1, outlier.shape = NA) +
  facet_grid2(R2 ~ N)
}

violin_themeFunction <- function(plotObject){
  pTmp <- plotObject + theme(
    panel.grid.major = element_line(linewidth = 0.15, linetype = 'solid', color = "lightgrey"), 
    panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid', color = "lightgrey"),
    panel.background = element_rect(color = "white", fill = "white"),
    plot.title = element_text(size = 15, face = "bold"),
    axis.text.y = element_text(size = 15),
    axis.text.x = element_text(size = 15),
    axis.title.x = element_text(size = 15),
    axis.title.y = element_text(size = 15),
    strip.text.x = element_text(size = 10),
    strip.text.y = element_text(size = 10)
  )
}

# 
vPlot_inter <- violin_plot(vPlotData_inter, plotMeasure = vPlotData_inter$scaled_weight)
(vPlot_inter <- violin_themeFunction(vPlot_inter))
bPlot_inter <- boxplotCustom(vPlotData_inter, plotMeasure = vPlotData_inter$scaled_weight)
(bPlot_inter <- violin_themeFunction(bPlot_inter))

# saving final plots
ggplot2::ggsave(filename = paste0("plots/violinPlot_inter_rel1.png"),
                plot = vPlot_inter,
                width = 18.30,
                height = 13.00,
                units = "in")

ggplot2::ggsave(filename = paste0("plots/boxPlot_inter_rel1.png"),
                plot = bPlot_inter,
                width = 18.30,
                height = 13.00,
                units = "in")

