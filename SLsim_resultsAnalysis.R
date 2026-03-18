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

# save(aov_data, file = "results/aov_data.rda")

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

# save(aov_results, file = "results/aov_results.rda")


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
emmNxModel <- emmeans(aov_rsquared, ~ "Model", by = "N")
# N = N100:
#   Model               emmean       SE    df lower.CL upper.CL
# gbm                 0.2466 0.000415 13976   0.2458   0.2474
# glmnet              0.2713 0.000625 13976   0.2701   0.2725
# ranger              0.2782 0.000454 13976   0.2773   0.2791
# rpart               0.1173 0.000504 13976   0.1163   0.1183
# sl_algorithm_glm    0.3047 0.000454 13976   0.3038   0.3056
# sl_algorithm_glmnet 0.3090 0.000440 13976   0.3082   0.3099
# sl_algorithm_nnls   0.3088 0.000439 13976   0.3080   0.3097
# sl_algorithm_ranger 0.2793 0.000477 13976   0.2783   0.2802
# 
# N = N1000:
#   Model               emmean       SE    df lower.CL upper.CL
# gbm                 0.3798 0.000291 13976   0.3792   0.3804
# glmnet              0.3849 0.000439 13976   0.3841   0.3858
# ranger              0.3839 0.000318 13976   0.3833   0.3845
# rpart               0.1491 0.000354 13976   0.1484   0.1498
# sl_algorithm_glm    0.4113 0.000319 13976   0.4107   0.4119
# sl_algorithm_glmnet 0.4116 0.000308 13976   0.4110   0.4122
# sl_algorithm_nnls   0.4118 0.000308 13976   0.4112   0.4124
# sl_algorithm_ranger 0.3847 0.000335 13976   0.3840   0.3853
# 
# N = N3000:
#   Model               emmean       SE    df lower.CL upper.CL
# gbm                 0.4024 0.000288 13976   0.4018   0.4029
# glmnet              0.3914 0.000434 13976   0.3906   0.3923
# ranger              0.4033 0.000315 13976   0.4027   0.4039
# rpart               0.1541 0.000350 13976   0.1534   0.1548
# sl_algorithm_glm    0.4206 0.000315 13976   0.4200   0.4212
# sl_algorithm_glmnet 0.4207 0.000305 13976   0.4201   0.4213
# sl_algorithm_nnls   0.4208 0.000305 13976   0.4202   0.4214
# sl_algorithm_ranger 0.3969 0.000331 13976   0.3962   0.3975

emmDGPxModel <- emmeans(aov_rsquared, ~ "Model", by = "dgp")
# dgp = inter:
#   Model               emmean       SE    df lower.CL upper.CL
# gbm                 0.3092 0.000343 13976   0.3086   0.3099
# glmnet              0.3909 0.000517 13976   0.3899   0.3919
# ranger              0.3481 0.000375 13976   0.3473   0.3488
# rpart               0.1302 0.000417 13976   0.1294   0.1310
# sl_algorithm_glm    0.3915 0.000376 13976   0.3908   0.3923
# sl_algorithm_glmnet 0.3934 0.000364 13976   0.3927   0.3941
# sl_algorithm_nnls   0.3941 0.000363 13976   0.3934   0.3948
# sl_algorithm_ranger 0.3641 0.000395 13976   0.3633   0.3649
# 
# dgp = nonlinear3:
#   Model               emmean       SE    df lower.CL upper.CL
# gbm                 0.3557 0.000336 13976   0.3550   0.3563
# glmnet              0.3225 0.000505 13976   0.3215   0.3235
# ranger              0.3522 0.000367 13976   0.3514   0.3529
# rpart               0.1462 0.000408 13976   0.1454   0.1470
# sl_algorithm_glm    0.3662 0.000367 13976   0.3655   0.3669
# sl_algorithm_glmnet 0.3678 0.000355 13976   0.3671   0.3685
# sl_algorithm_nnls   0.3684 0.000355 13976   0.3677   0.3691
# sl_algorithm_ranger 0.3423 0.000386 13976   0.3416   0.3431
# 
# dgp = pwlinear:
#   Model               emmean       SE    df lower.CL upper.CL
# gbm                 0.3639 0.000331 13976   0.3633   0.3646
# glmnet              0.3342 0.000499 13976   0.3333   0.3352
# ranger              0.3652 0.000362 13976   0.3644   0.3659
# rpart               0.1441 0.000403 13976   0.1434   0.1449
# sl_algorithm_glm    0.3788 0.000362 13976   0.3781   0.3796
# sl_algorithm_glmnet 0.3801 0.000351 13976   0.3794   0.3808
# sl_algorithm_nnls   0.3789 0.000351 13976   0.3783   0.3796
# sl_algorithm_ranger 0.3544 0.000381 13976   0.3537   0.3552

emmDGPxModelxN <- emmeans(aov_rsquared, ~ "Model", by = c("dgp", "N"))
# dgp = inter, N = N100:
#   Model               emmean       SE    df lower.CL upper.CL
# gbm                 0.1912 0.000742 13976   0.1898   0.1927
# glmnet              0.3223 0.001120 13976   0.3201   0.3245
# ranger              0.2536 0.000811 13976   0.2520   0.2552
# rpart               0.1072 0.000901 13976   0.1054   0.1089
# sl_algorithm_glm    0.3240 0.000811 13976   0.3224   0.3256
# sl_algorithm_glmnet 0.3287 0.000785 13976   0.3272   0.3303
# sl_algorithm_nnls   0.3299 0.000785 13976   0.3284   0.3315
# sl_algorithm_ranger 0.2936 0.000852 13976   0.2920   0.2953
# 
# dgp = nonlinear3, N = N100:
#   Model               emmean       SE    df lower.CL upper.CL
# gbm                 0.2775 0.000716 13976   0.2761   0.2789
# glmnet              0.2405 0.001080 13976   0.2384   0.2426
# ranger              0.2856 0.000783 13976   0.2841   0.2871
# rpart               0.1230 0.000870 13976   0.1213   0.1247
# sl_algorithm_glm    0.2913 0.000783 13976   0.2898   0.2928
# sl_algorithm_glmnet 0.2959 0.000758 13976   0.2944   0.2974
# sl_algorithm_nnls   0.2972 0.000758 13976   0.2957   0.2987
# sl_algorithm_ranger 0.2691 0.000823 13976   0.2675   0.2707
# 
# dgp = pwlinear, N = N100:
#   Model               emmean       SE    df lower.CL upper.CL
# gbm                 0.2711 0.000699 13976   0.2698   0.2725
# glmnet              0.2510 0.001050 13976   0.2490   0.2531
# ranger              0.2954 0.000764 13976   0.2939   0.2969
# rpart               0.1218 0.000849 13976   0.1202   0.1235
# sl_algorithm_glm    0.2988 0.000764 13976   0.2973   0.3003
# sl_algorithm_glmnet 0.3024 0.000740 13976   0.3010   0.3039
# sl_algorithm_nnls   0.2994 0.000740 13976   0.2979   0.3008
# sl_algorithm_ranger 0.2751 0.000803 13976   0.2735   0.2766
# 
# dgp = inter, N = N1000:
#   Model               emmean       SE    df lower.CL upper.CL
# gbm                 0.3507 0.000511 13976   0.3497   0.3517
# glmnet              0.4223 0.000768 13976   0.4208   0.4238
# ranger              0.3828 0.000558 13976   0.3817   0.3839
# rpart               0.1381 0.000620 13976   0.1369   0.1393
# sl_algorithm_glm    0.4221 0.000558 13976   0.4210   0.4232
# sl_algorithm_glmnet 0.4227 0.000541 13976   0.4216   0.4238
# sl_algorithm_nnls   0.4232 0.000540 13976   0.4222   0.4243
# sl_algorithm_ranger 0.3936 0.000587 13976   0.3925   0.3948
# 
# dgp = nonlinear3, N = N1000:
#   Model               emmean       SE    df lower.CL upper.CL
# gbm                 0.3871 0.000502 13976   0.3861   0.3881
# glmnet              0.3600 0.000756 13976   0.3585   0.3615
# ranger              0.3779 0.000549 13976   0.3769   0.3790
# rpart               0.1560 0.000610 13976   0.1548   0.1572
# sl_algorithm_glm    0.3988 0.000549 13976   0.3977   0.3998
# sl_algorithm_glmnet 0.3990 0.000532 13976   0.3980   0.4001
# sl_algorithm_nnls   0.3992 0.000531 13976   0.3982   0.4003
# sl_algorithm_ranger 0.3731 0.000577 13976   0.3719   0.3742
# 
# dgp = pwlinear, N = N1000:
#   Model               emmean       SE    df lower.CL upper.CL
# gbm                 0.4017 0.000501 13976   0.4007   0.4027
# glmnet              0.3725 0.000754 13976   0.3711   0.3740
# ranger              0.3910 0.000548 13976   0.3899   0.3920
# rpart               0.1533 0.000609 13976   0.1521   0.1545
# sl_algorithm_glm    0.4131 0.000548 13976   0.4120   0.4142
# sl_algorithm_glmnet 0.4132 0.000531 13976   0.4122   0.4142
# sl_algorithm_nnls   0.4129 0.000530 13976   0.4118   0.4139
# sl_algorithm_ranger 0.3873 0.000576 13976   0.3862   0.3885
# 
# dgp = inter, N = N3000:
#   Model               emmean       SE    df lower.CL upper.CL
# gbm                 0.3858 0.000501 13976   0.3848   0.3868
# glmnet              0.4280 0.000753 13976   0.4266   0.4295
# ranger              0.4078 0.000547 13976   0.4067   0.4088
# rpart               0.1453 0.000608 13976   0.1441   0.1465
# sl_algorithm_glm    0.4286 0.000547 13976   0.4275   0.4296
# sl_algorithm_glmnet 0.4289 0.000530 13976   0.4278   0.4299
# sl_algorithm_nnls   0.4291 0.000530 13976   0.4280   0.4301
# sl_algorithm_ranger 0.4050 0.000575 13976   0.4039   0.4061
# 
# dgp = nonlinear3, N = N3000:
#   Model               emmean       SE    df lower.CL upper.CL
# gbm                 0.4024 0.000499 13976   0.4014   0.4034
# glmnet              0.3671 0.000750 13976   0.3656   0.3685
# ranger              0.3930 0.000545 13976   0.3919   0.3940
# rpart               0.1596 0.000606 13976   0.1584   0.1608
# sl_algorithm_glm    0.4085 0.000545 13976   0.4074   0.4096
# sl_algorithm_glmnet 0.4086 0.000528 13976   0.4075   0.4096
# sl_algorithm_nnls   0.4087 0.000528 13976   0.4077   0.4097
# sl_algorithm_ranger 0.3849 0.000573 13976   0.3837   0.3860
# 
# dgp = pwlinear, N = N3000:
#   Model               emmean       SE    df lower.CL upper.CL
# gbm                 0.4189 0.000499 13976   0.4179   0.4199
# glmnet              0.3791 0.000750 13976   0.3777   0.3806
# ranger              0.4091 0.000545 13976   0.4081   0.4102
# rpart               0.1573 0.000606 13976   0.1562   0.1585
# sl_algorithm_glm    0.4247 0.000545 13976   0.4236   0.4257
# sl_algorithm_glmnet 0.4246 0.000528 13976   0.4236   0.4257
# sl_algorithm_nnls   0.4246 0.000528 13976   0.4235   0.4256
# sl_algorithm_ranger 0.4008 0.000573 13976   0.3997   0.4020

emmDGPxModelxlin_inter <- emmeans(aov_rsquared, ~ "Model", by = c("dgp", "lin_inter"))
# dgp = inter, lin_inter = lin_inter0.0_1.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.20134 0.000663 13976  0.20004  0.20264
# glmnet              0.36693 0.000998 13976  0.36497  0.36888
# ranger              0.26654 0.000724 13976  0.26512  0.26796
# rpart               0.04850 0.000805 13976  0.04692  0.05008
# sl_algorithm_glm    0.36131 0.000725 13976  0.35989  0.36273
# sl_algorithm_glmnet 0.36331 0.000702 13976  0.36193  0.36468
# sl_algorithm_nnls   0.36504 0.000701 13976  0.36367  0.36642
# sl_algorithm_ranger 0.32869 0.000761 13976  0.32719  0.33018
# 
# dgp = nonlinear3, lin_inter = lin_inter0.0_1.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.30974 0.000622 13976  0.30852  0.31096
# glmnet              0.22288 0.000937 13976  0.22104  0.22471
# ranger              0.30138 0.000680 13976  0.30005  0.30271
# rpart               0.14929 0.000756 13976  0.14781  0.15077
# sl_algorithm_glm    0.30999 0.000681 13976  0.30866  0.31133
# sl_algorithm_glmnet 0.31188 0.000659 13976  0.31059  0.31317
# sl_algorithm_nnls   0.31240 0.000659 13976  0.31111  0.31370
# sl_algorithm_ranger 0.29016 0.000715 13976  0.28876  0.29157
# 
# dgp = pwlinear, lin_inter = lin_inter0.0_1.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.33446 0.000605 13976  0.33327  0.33564
# glmnet              0.25389 0.000911 13976  0.25211  0.25568
# ranger              0.33548 0.000661 13976  0.33418  0.33678
# rpart               0.15093 0.000735 13976  0.14949  0.15237
# sl_algorithm_glm    0.34319 0.000662 13976  0.34189  0.34449
# sl_algorithm_glmnet 0.34456 0.000641 13976  0.34331  0.34582
# sl_algorithm_nnls   0.34243 0.000640 13976  0.34118  0.34369
# sl_algorithm_ranger 0.32274 0.000695 13976  0.32137  0.32410
# 
# dgp = inter, lin_inter = lin_inter0.5_0.5:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.32214 0.000576 13976  0.32101  0.32327
# glmnet              0.37998 0.000867 13976  0.37828  0.38168
# ranger              0.36078 0.000630 13976  0.35955  0.36202
# rpart               0.15359 0.000700 13976  0.15221  0.15496
# sl_algorithm_glm    0.38577 0.000630 13976  0.38453  0.38700
# sl_algorithm_glmnet 0.38771 0.000610 13976  0.38651  0.38890
# sl_algorithm_nnls   0.38722 0.000610 13976  0.38603  0.38842
# sl_algorithm_ranger 0.36244 0.000662 13976  0.36114  0.36374
# 
# dgp = nonlinear3, lin_inter = lin_inter0.5_0.5:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.34161 0.000580 13976  0.34047  0.34274
# glmnet              0.30566 0.000873 13976  0.30394  0.30737
# ranger              0.32601 0.000634 13976  0.32476  0.32725
# rpart               0.09562 0.000705 13976  0.09424  0.09700
# sl_algorithm_glm    0.34723 0.000634 13976  0.34599  0.34848
# sl_algorithm_glmnet 0.34879 0.000614 13976  0.34758  0.34999
# sl_algorithm_nnls   0.34911 0.000614 13976  0.34790  0.35031
# sl_algorithm_ranger 0.32287 0.000666 13976  0.32157  0.32418
# 
# dgp = pwlinear, lin_inter = lin_inter0.5_0.5:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.34150 0.000573 13976  0.34037  0.34262
# glmnet              0.30873 0.000862 13976  0.30704  0.31042
# ranger              0.33055 0.000626 13976  0.32932  0.33177
# rpart               0.08765 0.000696 13976  0.08629  0.08901
# sl_algorithm_glm    0.35173 0.000626 13976  0.35050  0.35296
# sl_algorithm_glmnet 0.35264 0.000606 13976  0.35145  0.35383
# sl_algorithm_nnls   0.35054 0.000606 13976  0.34935  0.35173
# sl_algorithm_ranger 0.32691 0.000658 13976  0.32562  0.32820
# 
# dgp = inter, lin_inter = lin_inter1.0_0.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.40423 0.000539 13976  0.40317  0.40528
# glmnet              0.42576 0.000811 13976  0.42417  0.42735
# ranger              0.41683 0.000589 13976  0.41568  0.41798
# rpart               0.18853 0.000655 13976  0.18724  0.18981
# sl_algorithm_glm    0.42753 0.000589 13976  0.42637  0.42868
# sl_algorithm_glmnet 0.42926 0.000570 13976  0.42815  0.43038
# sl_algorithm_nnls   0.42994 0.000570 13976  0.42883  0.43106
# sl_algorithm_ranger 0.40115 0.000619 13976  0.39993  0.40236
# 
# dgp = nonlinear3, lin_inter = lin_inter1.0_0.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.41568 0.000539 13976  0.41463  0.41674
# glmnet              0.43903 0.000811 13976  0.43744  0.44062
# ranger              0.42911 0.000589 13976  0.42796  0.43026
# rpart               0.19364 0.000654 13976  0.19236  0.19492
# sl_algorithm_glm    0.44135 0.000589 13976  0.44019  0.44250
# sl_algorithm_glmnet 0.44281 0.000570 13976  0.44169  0.44393
# sl_algorithm_nnls   0.44359 0.000570 13976  0.44248  0.44471
# sl_algorithm_ranger 0.41400 0.000619 13976  0.41279  0.41521
# 
# dgp = pwlinear, lin_inter = lin_inter1.0_0.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.41576 0.000542 13976  0.41470  0.41683
# glmnet              0.44010 0.000816 13976  0.43850  0.44170
# ranger              0.42944 0.000592 13976  0.42828  0.43060
# rpart               0.19385 0.000659 13976  0.19256  0.19514
# sl_algorithm_glm    0.44161 0.000593 13976  0.44045  0.44277
# sl_algorithm_glmnet 0.44304 0.000574 13976  0.44192  0.44417
# sl_algorithm_nnls   0.44384 0.000574 13976  0.44271  0.44496
# sl_algorithm_ranger 0.41360 0.000623 13976  0.41238  0.41482

emmNxModelxlin_inter <- emmeans(aov_rsquared, ~ "Model", by = c("N", "lin_inter"))
# N = N100, lin_inter = lin_inter0.0_1.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.16555 0.000825 13976  0.16394  0.16717
# glmnet              0.20578 0.001240 13976  0.20335  0.20822
# ranger              0.19663 0.000902 13976  0.19486  0.19840
# rpart               0.09118 0.001000 13976  0.08922  0.09314
# sl_algorithm_glm    0.25450 0.000902 13976  0.25274  0.25627
# sl_algorithm_glmnet 0.25916 0.000874 13976  0.25744  0.26087
# sl_algorithm_nnls   0.25918 0.000873 13976  0.25746  0.26089
# sl_algorithm_ranger 0.23102 0.000948 13976  0.22916  0.23288
# 
# N = N1000, lin_inter = lin_inter0.0_1.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.32388 0.000511 13976  0.32287  0.32488
# glmnet              0.31620 0.000770 13976  0.31469  0.31771
# ranger              0.33993 0.000559 13976  0.33884  0.34103
# rpart               0.12575 0.000621 13976  0.12453  0.12697
# sl_algorithm_glm    0.37443 0.000559 13976  0.37334  0.37553
# sl_algorithm_glmnet 0.37491 0.000541 13976  0.37384  0.37597
# sl_algorithm_nnls   0.37488 0.000541 13976  0.37382  0.37594
# sl_algorithm_ranger 0.34885 0.000587 13976  0.34770  0.35000
# 
# N = N3000, lin_inter = lin_inter0.0_1.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.35611 0.000501 13976  0.35513  0.35709
# glmnet              0.32171 0.000753 13976  0.32024  0.32319
# ranger              0.36684 0.000547 13976  0.36577  0.36791
# rpart               0.13179 0.000608 13976  0.13060  0.13298
# sl_algorithm_glm    0.38556 0.000547 13976  0.38449  0.38663
# sl_algorithm_glmnet 0.38569 0.000530 13976  0.38465  0.38673
# sl_algorithm_nnls   0.38582 0.000530 13976  0.38478  0.38686
# sl_algorithm_ranger 0.36172 0.000575 13976  0.36059  0.36285
# 
# N = N100, lin_inter = lin_inter0.5_0.5:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.23160 0.000703 13976  0.23022  0.23298
# glmnet              0.22765 0.001060 13976  0.22558  0.22973
# ranger              0.25691 0.000768 13976  0.25540  0.25841
# rpart               0.08954 0.000854 13976  0.08787  0.09122
# sl_algorithm_glm    0.27136 0.000769 13976  0.26986  0.27287
# sl_algorithm_glmnet 0.27539 0.000744 13976  0.27393  0.27685
# sl_algorithm_nnls   0.27349 0.000744 13976  0.27203  0.27495
# sl_algorithm_ranger 0.24710 0.000808 13976  0.24552  0.24868
# 
# N = N1000, lin_inter = lin_inter0.5_0.5:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.37596 0.000504 13976  0.37498  0.37695
# glmnet              0.37842 0.000758 13976  0.37693  0.37991
# ranger              0.36909 0.000551 13976  0.36801  0.37017
# rpart               0.12069 0.000612 13976  0.11949  0.12189
# sl_algorithm_glm    0.40076 0.000551 13976  0.39968  0.40184
# sl_algorithm_glmnet 0.40098 0.000533 13976  0.39994  0.40203
# sl_algorithm_nnls   0.40081 0.000533 13976  0.39977  0.40186
# sl_algorithm_ranger 0.37489 0.000579 13976  0.37376  0.37603
# 
# N = N3000, lin_inter = lin_inter0.5_0.5:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.39768 0.000499 13976  0.39670  0.39865
# glmnet              0.38830 0.000750 13976  0.38683  0.38977
# ranger              0.39134 0.000545 13976  0.39027  0.39240
# rpart               0.12662 0.000606 13976  0.12543  0.12780
# sl_algorithm_glm    0.41261 0.000545 13976  0.41154  0.41368
# sl_algorithm_glmnet 0.41276 0.000528 13976  0.41173  0.41379
# sl_algorithm_nnls   0.41257 0.000528 13976  0.41153  0.41360
# sl_algorithm_ranger 0.39023 0.000573 13976  0.38911  0.39135
# 
# N = N100, lin_inter = lin_inter1.0_0.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.34274 0.000614 13976  0.34154  0.34394
# glmnet              0.38044 0.000924 13976  0.37862  0.38225
# ranger              0.38107 0.000671 13976  0.37976  0.38239
# rpart               0.17121 0.000746 13976  0.16975  0.17267
# sl_algorithm_glm    0.38822 0.000671 13976  0.38690  0.38953
# sl_algorithm_glmnet 0.39249 0.000650 13976  0.39122  0.39376
# sl_algorithm_nnls   0.39378 0.000650 13976  0.39251  0.39505
# sl_algorithm_ranger 0.35970 0.000705 13976  0.35832  0.36108
# 
# N = N1000, lin_inter = lin_inter1.0_0.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.43957 0.000499 13976  0.43860  0.44055
# glmnet              0.46023 0.000751 13976  0.45876  0.46170
# ranger              0.44265 0.000545 13976  0.44158  0.44371
# rpart               0.20092 0.000606 13976  0.19973  0.20211
# sl_algorithm_glm    0.45872 0.000545 13976  0.45765  0.45979
# sl_algorithm_glmnet 0.45902 0.000528 13976  0.45798  0.46005
# sl_algorithm_nnls   0.45964 0.000528 13976  0.45861  0.46068
# sl_algorithm_ranger 0.43031 0.000573 13976  0.42919  0.43144
# 
# N = N3000, lin_inter = lin_inter1.0_0.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.45336 0.000499 13976  0.45238  0.45433
# glmnet              0.46422 0.000750 13976  0.46275  0.46569
# ranger              0.45167 0.000545 13976  0.45060  0.45273
# rpart               0.20388 0.000606 13976  0.20270  0.20507
# sl_algorithm_glm    0.46355 0.000545 13976  0.46248  0.46462
# sl_algorithm_glmnet 0.46361 0.000528 13976  0.46258  0.46464
# sl_algorithm_nnls   0.46395 0.000528 13976  0.46292  0.46499
# sl_algorithm_ranger 0.43874 0.000573 13976  0.43762  0.43986

emmNxModelxR2 <- emmeans(aov_rsquared, ~ "Model", by = c("N", "R2"))
# N = N100, R2 = 0.2:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.05112 0.000937 13976  0.04929  0.05296
# glmnet              0.04733 0.001410 13976  0.04456  0.05009
# ranger              0.06748 0.001020 13976  0.06547  0.06949
# rpart               0.03190 0.001140 13976  0.02966  0.03413
# sl_algorithm_glm    0.05699 0.001020 13976  0.05498  0.05900
# sl_algorithm_glmnet 0.06061 0.000992 13976  0.05867  0.06255
# sl_algorithm_nnls   0.06230 0.000992 13976  0.06035  0.06424
# sl_algorithm_ranger 0.04601 0.001080 13976  0.04390  0.04812
# 
# N = N1000, R2 = 0.2:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.12896 0.000513 13976  0.12795  0.12996
# glmnet              0.14198 0.000772 13976  0.14046  0.14349
# ranger              0.13170 0.000560 13976  0.13060  0.13280
# rpart               0.05615 0.000623 13976  0.05492  0.05737
# sl_algorithm_glm    0.14964 0.000561 13976  0.14854  0.15074
# sl_algorithm_glmnet 0.15021 0.000543 13976  0.14915  0.15128
# sl_algorithm_nnls   0.15042 0.000543 13976  0.14936  0.15148
# sl_algorithm_ranger 0.11947 0.000589 13976  0.11832  0.12063
# 
# N = N3000, R2 = 0.2:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.14865 0.000501 13976  0.14766  0.14963
# glmnet              0.15197 0.000753 13976  0.15049  0.15344
# ranger              0.14772 0.000547 13976  0.14665  0.14879
# rpart               0.06137 0.000608 13976  0.06018  0.06256
# sl_algorithm_glm    0.16157 0.000547 13976  0.16049  0.16264
# sl_algorithm_glmnet 0.16176 0.000530 13976  0.16072  0.16280
# sl_algorithm_nnls   0.16207 0.000530 13976  0.16103  0.16311
# sl_algorithm_ranger 0.13295 0.000575 13976  0.13182  0.13407
# 
# N = N100, R2 = 0.5:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.23091 0.000612 13976  0.22971  0.23212
# glmnet              0.24348 0.000922 13976  0.24167  0.24528
# ranger              0.26965 0.000669 13976  0.26834  0.27096
# rpart               0.11425 0.000744 13976  0.11279  0.11571
# sl_algorithm_glm    0.28352 0.000670 13976  0.28221  0.28484
# sl_algorithm_glmnet 0.29015 0.000648 13976  0.28888  0.29142
# sl_algorithm_nnls   0.28904 0.000648 13976  0.28777  0.29031
# sl_algorithm_ranger 0.25369 0.000703 13976  0.25231  0.25507
# 
# N = N1000, R2 = 0.5:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.37894 0.000502 13976  0.37795  0.37992
# glmnet              0.38742 0.000755 13976  0.38594  0.38890
# ranger              0.38274 0.000548 13976  0.38166  0.38381
# rpart               0.15087 0.000609 13976  0.14967  0.15206
# sl_algorithm_glm    0.41218 0.000549 13976  0.41111  0.41326
# sl_algorithm_glmnet 0.41242 0.000531 13976  0.41138  0.41346
# sl_algorithm_nnls   0.41254 0.000531 13976  0.41150  0.41359
# sl_algorithm_ranger 0.38234 0.000576 13976  0.38122  0.38347
# 
# N = N3000, R2 = 0.5:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.40316 0.000499 13976  0.40219  0.40414
# glmnet              0.39383 0.000750 13976  0.39236  0.39530
# ranger              0.40255 0.000545 13976  0.40148  0.40362
# rpart               0.15484 0.000606 13976  0.15366  0.15603
# sl_algorithm_glm    0.42167 0.000545 13976  0.42061  0.42274
# sl_algorithm_glmnet 0.42171 0.000528 13976  0.42068  0.42275
# sl_algorithm_nnls   0.42171 0.000528 13976  0.42068  0.42274
# sl_algorithm_ranger 0.39551 0.000573 13976  0.39439  0.39663
# 
# N = N100, R2 = 0.8:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.45786 0.000546 13976  0.45679  0.45893
# glmnet              0.52307 0.000822 13976  0.52146  0.52468
# ranger              0.49747 0.000597 13976  0.49630  0.49864
# rpart               0.20579 0.000664 13976  0.20448  0.20709
# sl_algorithm_glm    0.57357 0.000597 13976  0.57240  0.57474
# sl_algorithm_glmnet 0.57628 0.000578 13976  0.57515  0.57741
# sl_algorithm_nnls   0.57511 0.000578 13976  0.57398  0.57624
# sl_algorithm_ranger 0.53812 0.000628 13976  0.53689  0.53935
# 
# N = N1000, R2 = 0.8:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.63152 0.000500 13976  0.63054  0.63250
# glmnet              0.62545 0.000752 13976  0.62398  0.62693
# ranger              0.63723 0.000546 13976  0.63616  0.63830
# rpart               0.24035 0.000607 13976  0.23916  0.24154
# sl_algorithm_glm    0.67209 0.000546 13976  0.67102  0.67316
# sl_algorithm_glmnet 0.67227 0.000529 13976  0.67124  0.67331
# sl_algorithm_nnls   0.67238 0.000529 13976  0.67134  0.67341
# sl_algorithm_ranger 0.65224 0.000574 13976  0.65111  0.65336
# 
# N = N3000, R2 = 0.8:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.65533 0.000499 13976  0.65435  0.65631
# glmnet              0.62844 0.000750 13976  0.62697  0.62991
# ranger              0.65957 0.000545 13976  0.65850  0.66064
# rpart               0.24608 0.000606 13976  0.24489  0.24727
# sl_algorithm_glm    0.67848 0.000545 13976  0.67741  0.67955
# sl_algorithm_glmnet 0.67859 0.000528 13976  0.67755  0.67962
# sl_algorithm_nnls   0.67856 0.000528 13976  0.67753  0.67959
# sl_algorithm_ranger 0.66223 0.000573 13976  0.66111  0.66336

# welche bedingungen sind besonders bedeutend für plots 
