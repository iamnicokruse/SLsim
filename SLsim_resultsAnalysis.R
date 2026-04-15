###############################################################################
#___________________________________Analysis__________________________________#
###############################################################################

## load packages and parameters

library(afex) # for aov_ez()
library(effectsize) # for efect size calculation; generalized eta²
library(emmeans)
library(ggplot2)
library(tidyverse)
library(cowplot)
library(purrr)

source("MLsim-main/utils/setParameters.R")
source("SLtools.R") # included plot functions from MLsim-Project

## load and restructure data to match expected data frame for anova 
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
        mutate(ID = interaction(dgp, lin_inter, N, R2, rel, sample_no, drop = TRUE)) %>%
        rename(!!paste0(iData, "_rmse") := !!sym(paste0(iData, "_Value")))
      rmse$Metric <- NULL
      rmse$sample_no <- NULL
      
      mae  <- subset(tmp_long, tmp_long$Metric == "MAE") %>%
        group_by(dgp, lin_inter, Model, N, R2, rel) %>%
        mutate(sample_no = seq_len(n())) %>%
        ungroup() %>%
        mutate(ID = interaction(dgp, lin_inter, N, R2, rel, sample_no, drop = TRUE))%>%
        rename(!!paste0(iData, "_mae") := !!sym(paste0(iData, "_Value")))
      mae$Metric <- NULL
      mae$sample_no <- NULL
      
      rsq  <- subset(tmp_long, tmp_long$Metric == "Rsquared") %>%
        group_by(dgp, lin_inter, Model, N, R2, rel) %>%
        mutate(sample_no = seq_len(n())) %>%
        ungroup() %>%
        mutate(ID = interaction(dgp, lin_inter, N, R2, rel, sample_no, drop = TRUE)) %>%
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
        mutate(ID = interaction(dgp, lin_inter, N, R2, rel, sample_no, drop = TRUE)) %>%
        rename(!!paste0(iData, "_rmse") := !!sym(paste0(iData, "_Value")))
      rmse$Metric <- NULL
      rmse$sample_no <- NULL
      
      mae  <- subset(tmp_long, tmp_long$Metric == "MAE") %>%
        group_by(dgp, lin_inter, Model, N, R2, rel) %>%
        mutate(sample_no = seq_len(n())) %>%
        ungroup() %>%
        mutate(ID = interaction(dgp, lin_inter, N, R2, rel, sample_no, drop = TRUE))%>%
        rename(!!paste0(iData, "_mae") := !!sym(paste0(iData, "_Value")))
      mae$Metric <- NULL
      mae$sample_no <- NULL
      
      rsq  <- subset(tmp_long, tmp_long$Metric == "Rsquared") %>%
        group_by(dgp, lin_inter, Model, N, R2, rel) %>%
        mutate(sample_no = seq_len(n())) %>%
        ungroup() %>%
        mutate(ID = interaction(dgp, lin_inter, N, R2, rel, sample_no, drop = TRUE)) %>%
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


# turn all predictors to factors
aov_data <- aov_data %>%
  mutate(
    dgp = factor(dgp),
    N = factor(N),
    rel = factor(rel),
    Model = factor(Model)
  )

# dir.create("results/anova")
# save(aov_data, file = "results/anova/aov_data.rda")

## aov_data for anova w/ 
# ... between: 3 x 2 x 3 x 3 x 3  
#   N         (3)   {100, 1000, 3000}
#   rel       (2)   {0.7, 1}
#   R2        (3)   {0.2, 0.5, 0.8}
#   lin_inter (3)   {0.0_1.0, 0.5_0.5, 1.0_0.0}
#   dgp       (3)   {inter, pwlinear, nonlinear3}
# ... within:
# base model {glmnet, rpart, gbm, ranger, ensemble}
# meta model {nnls, glm, glmnet, ranger}

   
aov_rmse <- aov_ez(id = "ID",
                   dv = "test_rmse",
                   data = aov_data,
                   between = c("dgp", "lin_inter", "N", "R2", "rel"),
                   within = "Model")

aov_mae <- aov_ez(id = "ID",
                   dv = "test_mae",
                   data = aov_data,
                   between = c("dgp", "lin_inter", "N", "R2", "rel"),
                   within = "Model")

aov_rsquared <- aov_ez(id = "ID",
                   dv = "test_rsquared",
                   data = aov_data,
                   between = c("dgp", "lin_inter", "N", "R2", "rel"),
                   within = "Model")

# saving results of anova in list
aov_results <- list() 
aov_results[["rmse"]][["aov"]] <- aov_rmse
aov_results[["mae"]][["aov"]] <- aov_mae
aov_results[["rsquared"]][["aov"]] <- aov_rsquared


## calculate effect sizes (generalized eta_squared)

eta2rmse <- eta_squared(
  aov_rmse, # fitted model
  partial = FALSE, # not partial!
  generalized = TRUE, # generalized eta squared
  ci = 0.95,
  verbose = TRUE)

eta2rmse_ordered <- eta2rmse[order(eta2rmse$Eta2_generalized, decreasing = T),]

eta2mae <- eta_squared(
  aov_mae, # fitted model
  partial = FALSE, # not partial!
  generalized = TRUE, # generalized eta squared
  ci = 0.95,
  verbose = TRUE)

eta2mae_ordered <- eta2mae[order(eta2mae$Eta2_generalized, decreasing = T),]

eta2rsquared <- eta_squared(
  aov_rsquared, # fitted model
  partial = FALSE, # not partial!
  generalized = TRUE, # generalized eta squared
  ci = 0.95,
  verbose = TRUE)

eta2rsquared_ordered <- eta2rsquared[order(eta2rsquared$Eta2_generalized, decreasing = T),]


# adding generalized eta2 results to list 
aov_results[["rmse"]][["eta2"]] <- eta2rmse
aov_results[["mae"]][["eta2"]] <- eta2mae
aov_results[["rsquared"]][["eta2"]] <- eta2rsquared

aov_results[["rmse"]][["eta2_ordered"]] <- eta2rmse_ordered
aov_results[["mae"]][["eta2_ordered"]] <- eta2mae_ordered
aov_results[["rsquared"]][["eta2_ordered"]] <- eta2rsquared_ordered

# save(aov_results, file = "results/anova/aov_results.rda")


## estimated marginal means (post hoc)
# interpretation: the higher the emmean-value the better the average performance
#                 of the according  condition of the factor of interest across 
#                 all other factors

# main effects
emmDGP <- emmeans(aov_rsquared, ~ dgp)
# dgp        emmean       SE    df lower.CL upper.CL
# inter      0.3402 0.000295 13976   0.3396   0.3408
# nonlinear3 0.3277 0.000289 13976   0.3271   0.3282
# pwlinear   0.3375 0.000285 13976   0.3369   0.3380

emmLin_inter <- emmeans(aov_rsquared, ~ lin_inter)
# lin_inter        emmean       SE    df lower.CL upper.CL
# lin_inter0.0_1.0 0.2890 0.000313 13976   0.2884   0.2897
# lin_inter0.5_0.5 0.3178 0.000286 13976   0.3172   0.3183
# lin_inter1.0_0.0 0.3985 0.000268 13976   0.3980   0.3990

emmN <- emmeans(aov_rsquared, ~ N)
# N     emmean       SE    df lower.CL upper.CL
# N100  0.2644 0.000357 13976   0.2637   0.2651
# N1000 0.3646 0.000250 13976   0.3642   0.3651
# N3000 0.3763 0.000248 13976   0.3758   0.3767

emmR2 <- emmeans(aov_rsquared, ~ R2)
# R2  emmean       SE    df lower.CL upper.CL
# 0.2 0.1075 0.000338 13976   0.1068   0.1082
# 0.5 0.3295 0.000268 13976   0.3290   0.3301
# 0.8 0.5683 0.000256 13976   0.5678   0.5688

emmRel <- emmeans(aov_rsquared, ~ rel)
# rel emmean       SE    df lower.CL upper.CL
# 0.7 0.2763 0.000246 13976   0.2758   0.2768
# 1   0.3939 0.000226 13976   0.3935   0.3943

emmModel <- emmeans(aov_rsquared, ~ Model)
# Model               emmean       SE    df lower.CL upper.CL
# gbm                 0.3429 0.000194 13976   0.3426   0.3433
# glmnet              0.3492 0.000293 13976   0.3486   0.3498
# ranger              0.3551 0.000213 13976   0.3547   0.3555
# rpart               0.1402 0.000236 13976   0.1397   0.1406
# sl_algorithm_glm    0.3789 0.000213 13976   0.3784   0.3793
# sl_algorithm_glmnet 0.3804 0.000206 13976   0.3800   0.3808
# sl_algorithm_nnls   0.3805 0.000206 13976   0.3801   0.3809
# sl_algorithm_ranger 0.3536 0.000223 13976   0.3532   0.3541


# interactions of interest
emmR2xModel <- emmeans(aov_rsquared, ~ "Model", by = "R2")
# R2 = 0.2:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.10958 0.000393 13976  0.10880  0.11035
# glmnet              0.11376 0.000592 13976  0.11260  0.11492
# ranger              0.11563 0.000430 13976  0.11479  0.11648
# rpart               0.04980 0.000478 13976  0.04887  0.05074
# sl_algorithm_glm    0.12273 0.000430 13976  0.12189  0.12357
# sl_algorithm_glmnet 0.12419 0.000416 13976  0.12338  0.12501
# sl_algorithm_nnls   0.12493 0.000416 13976  0.12411  0.12574
# sl_algorithm_ranger 0.09948 0.000452 13976  0.09859  0.10036
# 
# R2 = 0.5:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.33767 0.000312 13976  0.33706  0.33828
# glmnet              0.34158 0.000469 13976  0.34066  0.34249
# ranger              0.35165 0.000341 13976  0.35098  0.35231
# rpart               0.13999 0.000379 13976  0.13925  0.14073
# sl_algorithm_glm    0.37246 0.000341 13976  0.37179  0.37313
# sl_algorithm_glmnet 0.37476 0.000330 13976  0.37411  0.37541
# sl_algorithm_nnls   0.37443 0.000330 13976  0.37378  0.37508
# sl_algorithm_ranger 0.34385 0.000358 13976  0.34315  0.34455
# 
# R2 = 0.8:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.58157 0.000298 13976  0.58099  0.58215
# glmnet              0.59232 0.000448 13976  0.59144  0.59320
# ranger              0.59809 0.000325 13976  0.59745  0.59873
# rpart               0.23074 0.000361 13976  0.23003  0.23145
# sl_algorithm_glm    0.64138 0.000325 13976  0.64074  0.64202
# sl_algorithm_glmnet 0.64238 0.000315 13976  0.64176  0.64300
# sl_algorithm_nnls   0.64202 0.000315 13976  0.64140  0.64263
# sl_algorithm_ranger 0.61753 0.000342 13976  0.61686  0.61820
# 
# Results are averaged over the levels of: dgp, lin_inter, N, rel 
# Confidence level used: 0.95 

# emmDGPxModel <- emmeans(aov_rsquared, ~ "Model", by = "dgp")
# emmNxModelxDGP <- emmeans(aov_rsquared, ~ "Model", by = c("dgp", "N"))
# emmDGPxModelxlin_inter <- emmeans(aov_rsquared, ~ "Model", by = c("dgp", "lin_inter"))
# emmNxModelxlin_inter <- emmeans(aov_rsquared, ~ "Model", by = c("N", "lin_inter"))
# emmNxModelxR2 <- emmeans(aov_rsquared, ~ "Model", by = c("N", "R2"))
# emmNxModelxR2xlin_inter <- emmeans(aov_rsquared, ~ "Model", by = c("N", "R2", "lin_inter"))

emmR2xRel <- emmeans(aov_rsquared, ~ "rel", by = "R2")
# R2 = 0.2:
#   rel  emmean       SE    df lower.CL upper.CL
# 0.7 0.08621 0.000507 13976  0.08522   0.0872
# 1   0.12882 0.000448 13976  0.12794   0.1297
# 
# R2 = 0.5:
#   rel  emmean       SE    df lower.CL upper.CL
# 0.7 0.27234 0.000392 13976  0.27158   0.2731
# 1   0.38675 0.000366 13976  0.38603   0.3875
# 
# R2 = 0.8:
#   rel  emmean       SE    df lower.CL upper.CL
# 0.7 0.47039 0.000368 13976  0.46967   0.4711
# 1   0.66611 0.000355 13976  0.66542   0.6668
# 
# Results are averaged over the levels of: dgp, lin_inter, N, Model 
# Confidence level used: 0.95 

emmR2xlin_inter <- emmeans(aov_rsquared, ~ "lin_inter", by = "R2")
# R2 = 0.2:
#   lin_inter         emmean       SE    df lower.CL upper.CL
# lin_inter0.0_1.0 0.08743 0.000656 13976  0.08614  0.08871
# lin_inter0.5_0.5 0.09668 0.000573 13976  0.09556  0.09781
# lin_inter1.0_0.0 0.13843 0.000519 13976  0.13741  0.13945
# 
# R2 = 0.5:
#   lin_inter         emmean       SE    df lower.CL upper.CL
# lin_inter0.0_1.0 0.27773 0.000492 13976  0.27677  0.27870
# lin_inter0.5_0.5 0.31447 0.000461 13976  0.31357  0.31538
# lin_inter1.0_0.0 0.39644 0.000439 13976  0.39558  0.39730
# 
# R2 = 0.8:
#   lin_inter         emmean       SE    df lower.CL upper.CL
# lin_inter0.0_1.0 0.50197 0.000457 13976  0.50108  0.50287
# lin_inter0.5_0.5 0.54219 0.000442 13976  0.54132  0.54306
# lin_inter1.0_0.0 0.66059 0.000430 13976  0.65975  0.66144
# 
# Results are averaged over the levels of: dgp, N, rel, Model 
# Confidence level used: 0.95 

# save marginal means of interest
df_emmR2xModel <- as.data.frame(emmR2xModel)
df_emmR2xRel <- as.data.frame(emmR2xRel)
df_emmR2xlin_inter <- as.data.frame(emmR2xlin_inter)

save(df_emmR2xModel, df_emmR2xRel, df_emmR2xlin_inter,
     file = "results/anova/mixedANOVA_postHocEMMs.rda")


## plot interaction of N and model performance using R2 as dependent measure
# load("~/SLsim/results/anova/mixedANOVA_postHocEMMs.rda")
colRamp <- colorRampPalette(c("#6f9c3d", "#b8c36b", "#ffb366", "#ff8829", "#fe6b40"))(n = 200)
limit.bias <- 1

df_emmR2xModel$Model <- factor(df_emmR2xModel$Model,
                              levels = c("gbm", "ranger", "glmnet", "rpart", 
                                         "sl_algorithm_glm", "sl_algorithm_glmnet",
                                         "sl_algorithm_nnls", "sl_algorithm_ranger"), 
                              labels = c("gbm", "ranger", "glmnet", "rpart", 
                                         "sl_algorithm_glm", "sl_algorithm_glmnet", 
                                         "sl_algorithm_nnls", "sl_algorithm_ranger"))

df_emmR2xModel$R2 <- factor(df_emmR2xModel$R2, 
                              levels = c("0.2", "0.5", "0.8"), 
                              labels = c("0.2", "0.5", "0.8")) 



# plot with legend to see if colour coding is accurate
(plot4guide <- ggplot(df_emmR2xModel, 
                      aes(x = R2, y = Model, fill = emmean)) + 
    geom_tile() +
    geom_text(aes(x = R2, y = Model, label = round(emmean, 2)), 
              color="black", size=rel(5)) +
    scale_fill_gradientn("",
                         colours = colRamp[length(colRamp):1],
                         values = scales::rescale(
                           # limited do +/- limit.bias
                           x = seq(from = 0,
                                   to = limit.bias, length.out = 200),
                           from = c(0, limit.bias)),
                         limits = c(0, limit.bias))) 

(plot4guide <- themeFunction(plot4guide))

# heatmap of R2xModel interaction
(pR2xModel <- plotHeatmap(df_emmR2xModel, df_emmR2xModel$R2, df_emmR2xModel$Model,
                         xLabel = "R2", yLabel = "Model"))
(pR2xModel <- themeFunction(pR2xModel))

# heatmap of R2xRel interaction
df_emmR2xRel$R2 <- factor(df_emmR2xRel$R2, 
                          levels = c("0.2", "0.5", "0.8"), 
                          labels = c("0.2", "0.5", "0.8")) 

df_emmR2xRel$rel <- factor(df_emmR2xRel$rel, 
                            levels = c("0.7", "1"), 
                            labels = c("0.7", "1")) 

(pR2xRel <- plotHeatmap(df_emmR2xRel, df_emmR2xRel$R2, df_emmR2xRel$rel,
                          xLabel = "R2", yLabel = "Reliability"))
(pR2xRel <- themeFunction(pR2xRel))

# heatmap of R2xlin_inter interaction
df_emmR2xlin_inter$R2 <- factor(df_emmR2xlin_inter$R2, 
                          levels = c("0.2", "0.5", "0.8"), 
                          labels = c("0.2", "0.5", "0.8")) 

df_emmR2xlin_inter$lin_inter <- factor(df_emmR2xlin_inter$lin_inter, 
                                       levels = c("lin_inter0.0_1.0", "lin_inter0.5_0.5", "lin_inter1.0_0.0"),
                                       labels = c("0:100", "50:50", "100:0"))

(pR2xlin_inter <- plotHeatmap(df_emmR2xlin_inter, df_emmR2xlin_inter$R2, df_emmR2xlin_inter$lin_inter,
                        xLabel = "R2", yLabel = "Effect composition"))
(pR2xlin_inter <- themeFunction(pR2xlin_inter))

