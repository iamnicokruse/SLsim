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

emmNxModelxDGP <- emmeans(aov_rsquared, ~ "Model", by = c("dgp", "N"))
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

emmNxModelxR2xlin_inter <- emmeans(aov_rsquared, ~ "Model", by = c("N", "R2", "lin_inter"))
# N = N100, R2 = 0.2, dgp = inter:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.04332 0.001600 13976  0.04019  0.04646
# glmnet              0.06049 0.002410 13976  0.05577  0.06520
# ranger              0.06433 0.001750 13976  0.06091  0.06776
# rpart               0.03087 0.001940 13976  0.02706  0.03467
# sl_algorithm_glm    0.06426 0.001750 13976  0.06084  0.06769
# sl_algorithm_glmnet 0.06721 0.001690 13976  0.06389  0.07053
# sl_algorithm_nnls   0.06832 0.001690 13976  0.06501  0.07164
# sl_algorithm_ranger 0.05155 0.001840 13976  0.04795  0.05515
# 
# N = N1000, R2 = 0.2, dgp = inter:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.11495 0.000906 13976  0.11317  0.11672
# glmnet              0.15848 0.001360 13976  0.15581  0.16116
# ranger              0.12824 0.000990 13976  0.12630  0.13018
# rpart               0.05382 0.001100 13976  0.05166  0.05598
# sl_algorithm_glm    0.15742 0.000991 13976  0.15548  0.15936
# sl_algorithm_glmnet 0.15823 0.000959 13976  0.15635  0.16011
# sl_algorithm_nnls   0.15908 0.000959 13976  0.15720  0.16096
# sl_algorithm_ranger 0.12549 0.001040 13976  0.12345  0.12753
# 
# N = N3000, R2 = 0.2, dgp = inter:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.13990 0.000874 13976  0.13819  0.14161
# glmnet              0.16748 0.001320 13976  0.16490  0.17006
# ranger              0.14918 0.000955 13976  0.14731  0.15105
# rpart               0.05983 0.001060 13976  0.05775  0.06191
# sl_algorithm_glm    0.16716 0.000956 13976  0.16529  0.16904
# sl_algorithm_glmnet 0.16758 0.000925 13976  0.16576  0.16939
# sl_algorithm_nnls   0.16799 0.000925 13976  0.16617  0.16980
# sl_algorithm_ranger 0.13772 0.001000 13976  0.13576  0.13969
# 
# N = N100, R2 = 0.5, dgp = inter:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.17802 0.001160 13976  0.17574  0.18030
# glmnet              0.29450 0.001750 13976  0.29107  0.29793
# ranger              0.24127 0.001270 13976  0.23878  0.24376
# rpart               0.10434 0.001410 13976  0.10157  0.10711
# sl_algorithm_glm    0.29686 0.001270 13976  0.29437  0.29936
# sl_algorithm_glmnet 0.30425 0.001230 13976  0.30184  0.30666
# sl_algorithm_nnls   0.30482 0.001230 13976  0.30241  0.30723
# sl_algorithm_ranger 0.26465 0.001340 13976  0.26203  0.26727
# 
# N = N1000, R2 = 0.5, dgp = inter:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.34202 0.000879 13976  0.34030  0.34374
# glmnet              0.41450 0.001320 13976  0.41190  0.41709
# ranger              0.37542 0.000960 13976  0.37353  0.37730
# rpart               0.13894 0.001070 13976  0.13685  0.14103
# sl_algorithm_glm    0.41429 0.000961 13976  0.41241  0.41618
# sl_algorithm_glmnet 0.41487 0.000930 13976  0.41305  0.41670
# sl_algorithm_nnls   0.41544 0.000930 13976  0.41362  0.41726
# sl_algorithm_ranger 0.38354 0.001010 13976  0.38157  0.38552
# 
# N = N3000, R2 = 0.5, dgp = inter:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.37787 0.000864 13976  0.37618  0.37956
# glmnet              0.41963 0.001300 13976  0.41708  0.42217
# ranger              0.39995 0.000944 13976  0.39810  0.40180
# rpart               0.14477 0.001050 13976  0.14271  0.14683
# sl_algorithm_glm    0.42019 0.000944 13976  0.41834  0.42204
# sl_algorithm_glmnet 0.42048 0.000914 13976  0.41869  0.42227
# sl_algorithm_nnls   0.42064 0.000914 13976  0.41885  0.42244
# sl_algorithm_ranger 0.39483 0.000992 13976  0.39288  0.39677
# 
# N = N100, R2 = 0.8, dgp = inter:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.35231 0.001020 13976  0.35031  0.35432
# glmnet              0.61190 0.001540 13976  0.60889  0.61492
# ranger              0.45530 0.001120 13976  0.45311  0.45749
# rpart               0.18625 0.001240 13976  0.18382  0.18869
# sl_algorithm_glm    0.61082 0.001120 13976  0.60863  0.61301
# sl_algorithm_glmnet 0.61471 0.001080 13976  0.61259  0.61683
# sl_algorithm_nnls   0.61661 0.001080 13976  0.61448  0.61873
# sl_algorithm_ranger 0.56475 0.001170 13976  0.56245  0.56705
# 
# N = N1000, R2 = 0.8, dgp = inter:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.59504 0.000868 13976  0.59333  0.59674
# glmnet              0.69404 0.001310 13976  0.69148  0.69660
# ranger              0.64464 0.000949 13976  0.64278  0.64650
# rpart               0.22159 0.001050 13976  0.21953  0.22366
# sl_algorithm_glm    0.69448 0.000949 13976  0.69262  0.69634
# sl_algorithm_glmnet 0.69498 0.000919 13976  0.69318  0.69678
# sl_algorithm_nnls   0.69519 0.000919 13976  0.69339  0.69699
# sl_algorithm_ranger 0.67186 0.000997 13976  0.66990  0.67381
# 
# N = N3000, R2 = 0.8, dgp = inter:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.63970 0.000864 13976  0.63800  0.64139
# glmnet              0.69699 0.001300 13976  0.69444  0.69953
# ranger              0.67414 0.000944 13976  0.67229  0.67599
# rpart               0.23141 0.001050 13976  0.22935  0.23347
# sl_algorithm_glm    0.69832 0.000944 13976  0.69647  0.70017
# sl_algorithm_glmnet 0.69852 0.000914 13976  0.69673  0.70032
# sl_algorithm_nnls   0.69853 0.000914 13976  0.69674  0.70032
# sl_algorithm_ranger 0.68244 0.000992 13976  0.68049  0.68438
# 
# N = N100, R2 = 0.2, dgp = nonlinear3:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.05721 0.001670 13976  0.05394  0.06047
# glmnet              0.04003 0.002510 13976  0.03511  0.04495
# ranger              0.06895 0.001820 13976  0.06538  0.07252
# rpart               0.03255 0.002020 13976  0.02858  0.03652
# sl_algorithm_glm    0.05334 0.001820 13976  0.04977  0.05691
# sl_algorithm_glmnet 0.05768 0.001760 13976  0.05422  0.06114
# sl_algorithm_nnls   0.05979 0.001760 13976  0.05634  0.06325
# sl_algorithm_ranger 0.04344 0.001910 13976  0.03969  0.04719
# 
# N = N1000, R2 = 0.2, dgp = nonlinear3:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.13179 0.000881 13976  0.13007  0.13352
# glmnet              0.13218 0.001330 13976  0.12958  0.13478
# ranger              0.12850 0.000963 13976  0.12662  0.13039
# rpart               0.05616 0.001070 13976  0.05406  0.05826
# sl_algorithm_glm    0.14192 0.000963 13976  0.14003  0.14381
# sl_algorithm_glmnet 0.14235 0.000932 13976  0.14053  0.14418
# sl_algorithm_nnls   0.14258 0.000932 13976  0.14075  0.14440
# sl_algorithm_ranger 0.11342 0.001010 13976  0.11144  0.11541
# 
# N = N3000, R2 = 0.2, dgp = nonlinear3:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.14820 0.000864 13976  0.14650  0.14989
# glmnet              0.14293 0.001300 13976  0.14039  0.14548
# ranger              0.14116 0.000944 13976  0.13931  0.14301
# rpart               0.06129 0.001050 13976  0.05924  0.06335
# sl_algorithm_glm    0.15427 0.000944 13976  0.15242  0.15612
# sl_algorithm_glmnet 0.15439 0.000914 13976  0.15259  0.15618
# sl_algorithm_nnls   0.15462 0.000914 13976  0.15282  0.15641
# sl_algorithm_ranger 0.12682 0.000992 13976  0.12488  0.12877
# 
# N = N100, R2 = 0.5, dgp = nonlinear3:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.26240 0.001010 13976  0.26043  0.26438
# glmnet              0.21380 0.001520 13976  0.21083  0.21678
# ranger              0.27854 0.001100 13976  0.27638  0.28069
# rpart               0.12079 0.001220 13976  0.11839  0.12319
# sl_algorithm_glm    0.27429 0.001100 13976  0.27213  0.27645
# sl_algorithm_glmnet 0.28091 0.001070 13976  0.27882  0.28300
# sl_algorithm_nnls   0.28145 0.001070 13976  0.27936  0.28354
# sl_algorithm_ranger 0.24746 0.001160 13976  0.24519  0.24973
# 
# N = N1000, R2 = 0.5, dgp = nonlinear3:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.39165 0.000864 13976  0.38996  0.39334
# glmnet              0.36774 0.001300 13976  0.36519  0.37029
# ranger              0.38167 0.000944 13976  0.37982  0.38353
# rpart               0.15948 0.001050 13976  0.15742  0.16153
# sl_algorithm_glm    0.40585 0.000945 13976  0.40400  0.40770
# sl_algorithm_glmnet 0.40601 0.000915 13976  0.40422  0.40780
# sl_algorithm_nnls   0.40620 0.000915 13976  0.40441  0.40800
# sl_algorithm_ranger 0.37652 0.000993 13976  0.37458  0.37847
# 
# N = N3000, R2 = 0.5, dgp = nonlinear3:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.40862 0.000864 13976  0.40693  0.41031
# glmnet              0.37457 0.001300 13976  0.37202  0.37712
# ranger              0.39724 0.000944 13976  0.39539  0.39909
# rpart               0.16139 0.001050 13976  0.15933  0.16344
# sl_algorithm_glm    0.41563 0.000944 13976  0.41378  0.41748
# sl_algorithm_glmnet 0.41558 0.000914 13976  0.41378  0.41737
# sl_algorithm_nnls   0.41564 0.000914 13976  0.41385  0.41743
# sl_algorithm_ranger 0.38946 0.000992 13976  0.38751  0.39140
# 
# N = N100, R2 = 0.8, dgp = nonlinear3:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.51300 0.000908 13976  0.51122  0.51478
# glmnet              0.46775 0.001370 13976  0.46508  0.47043
# ranger              0.50934 0.000992 13976  0.50739  0.51128
# rpart               0.21553 0.001100 13976  0.21337  0.21769
# sl_algorithm_glm    0.54631 0.000993 13976  0.54436  0.54825
# sl_algorithm_glmnet 0.54915 0.000961 13976  0.54726  0.55103
# sl_algorithm_nnls   0.55029 0.000961 13976  0.54841  0.55218
# sl_algorithm_ranger 0.51643 0.001040 13976  0.51439  0.51848
# 
# N = N1000, R2 = 0.8, dgp = nonlinear3:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.63780 0.000864 13976  0.63610  0.63949
# glmnet              0.58002 0.001300 13976  0.57747  0.58257
# ranger              0.62361 0.000944 13976  0.62176  0.62547
# rpart               0.25232 0.001050 13976  0.25026  0.25437
# sl_algorithm_glm    0.64851 0.000945 13976  0.64666  0.65036
# sl_algorithm_glmnet 0.64867 0.000915 13976  0.64687  0.65046
# sl_algorithm_nnls   0.64887 0.000915 13976  0.64708  0.65066
# sl_algorithm_ranger 0.62928 0.000993 13976  0.62733  0.63122
# 
# N = N3000, R2 = 0.8, dgp = nonlinear3:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.65042 0.000864 13976  0.64872  0.65211
# glmnet              0.58367 0.001300 13976  0.58112  0.58621
# ranger              0.64048 0.000944 13976  0.63863  0.64233
# rpart               0.25614 0.001050 13976  0.25408  0.25819
# sl_algorithm_glm    0.65561 0.000944 13976  0.65376  0.65746
# sl_algorithm_glmnet 0.65571 0.000914 13976  0.65392  0.65750
# sl_algorithm_nnls   0.65587 0.000914 13976  0.65408  0.65766
# sl_algorithm_ranger 0.63829 0.000992 13976  0.63634  0.64023
# 
# N = N100, R2 = 0.2, dgp = pwlinear:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.05285 0.001600 13976  0.04970  0.05599
# glmnet              0.04146 0.002410 13976  0.03673  0.04619
# ranger              0.06916 0.001750 13976  0.06573  0.07260
# rpart               0.03227 0.001950 13976  0.02845  0.03609
# sl_algorithm_glm    0.05337 0.001750 13976  0.04993  0.05681
# sl_algorithm_glmnet 0.05694 0.001700 13976  0.05361  0.06027
# sl_algorithm_nnls   0.05877 0.001700 13976  0.05545  0.06210
# sl_algorithm_ranger 0.04303 0.001840 13976  0.03942  0.04664
# 
# N = N1000, R2 = 0.2, dgp = pwlinear:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.14013 0.000877 13976  0.13841  0.14185
# glmnet              0.13527 0.001320 13976  0.13268  0.13786
# ranger              0.13835 0.000959 13976  0.13647  0.14023
# rpart               0.05845 0.001070 13976  0.05636  0.06054
# sl_algorithm_glm    0.14957 0.000959 13976  0.14769  0.15145
# sl_algorithm_glmnet 0.15005 0.000929 13976  0.14823  0.15188
# sl_algorithm_nnls   0.14960 0.000929 13976  0.14778  0.15142
# sl_algorithm_ranger 0.11951 0.001010 13976  0.11753  0.12149
# 
# N = N3000, R2 = 0.2, dgp = pwlinear:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.15784 0.000864 13976  0.15615  0.15953
# glmnet              0.14548 0.001300 13976  0.14294  0.14803
# ranger              0.15282 0.000944 13976  0.15097  0.15467
# rpart               0.06298 0.001050 13976  0.06092  0.06504
# sl_algorithm_glm    0.16327 0.000944 13976  0.16142  0.16512
# sl_algorithm_glmnet 0.16332 0.000914 13976  0.16153  0.16511
# sl_algorithm_nnls   0.16361 0.000914 13976  0.16181  0.16540
# sl_algorithm_ranger 0.13430 0.000992 13976  0.13235  0.13624
# 
# N = N100, R2 = 0.5, dgp = pwlinear:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.25232 0.001000 13976  0.25035  0.25429
# glmnet              0.22213 0.001510 13976  0.21917  0.22509
# ranger              0.28915 0.001100 13976  0.28700  0.29130
# rpart               0.11762 0.001220 13976  0.11523  0.12001
# sl_algorithm_glm    0.27942 0.001100 13976  0.27727  0.28157
# sl_algorithm_glmnet 0.28528 0.001060 13976  0.28320  0.28736
# sl_algorithm_nnls   0.28084 0.001060 13976  0.27876  0.28293
# sl_algorithm_ranger 0.24896 0.001150 13976  0.24670  0.25122
# 
# N = N1000, R2 = 0.5, dgp = pwlinear:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.40314 0.000864 13976  0.40145  0.40484
# glmnet              0.38003 0.001300 13976  0.37748  0.38257
# ranger              0.39112 0.000944 13976  0.38927  0.39297
# rpart               0.15419 0.001050 13976  0.15213  0.15624
# sl_algorithm_glm    0.41641 0.000944 13976  0.41456  0.41826
# sl_algorithm_glmnet 0.41638 0.000914 13976  0.41459  0.41818
# sl_algorithm_nnls   0.41599 0.000914 13976  0.41420  0.41778
# sl_algorithm_ranger 0.38697 0.000992 13976  0.38502  0.38891
# 
# N = N3000, R2 = 0.5, dgp = pwlinear:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.42300 0.000864 13976  0.42131  0.42469
# glmnet              0.38728 0.001300 13976  0.38473  0.38983
# ranger              0.41047 0.000944 13976  0.40862  0.41232
# rpart               0.15838 0.001050 13976  0.15632  0.16043
# sl_algorithm_glm    0.42921 0.000944 13976  0.42736  0.43106
# sl_algorithm_glmnet 0.42908 0.000914 13976  0.42729  0.43087
# sl_algorithm_nnls   0.42885 0.000914 13976  0.42706  0.43064
# sl_algorithm_ranger 0.40225 0.000992 13976  0.40030  0.40419
# 
# N = N100, R2 = 0.8, dgp = pwlinear:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.50826 0.000903 13976  0.50649  0.51003
# glmnet              0.48954 0.001360 13976  0.48688  0.49221
# ranger              0.52779 0.000987 13976  0.52585  0.52972
# rpart               0.21557 0.001100 13976  0.21342  0.21773
# sl_algorithm_glm    0.56358 0.000988 13976  0.56164  0.56552
# sl_algorithm_glmnet 0.56498 0.000956 13976  0.56311  0.56686
# sl_algorithm_nnls   0.55843 0.000956 13976  0.55656  0.56031
# sl_algorithm_ranger 0.53317 0.001040 13976  0.53114  0.53521
# 
# N = N1000, R2 = 0.8, dgp = pwlinear:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.66173 0.000864 13976  0.66004  0.66342
# glmnet              0.60230 0.001300 13976  0.59975  0.60485
# ranger              0.64345 0.000944 13976  0.64160  0.64530
# rpart               0.24713 0.001050 13976  0.24507  0.24919
# sl_algorithm_glm    0.67326 0.000944 13976  0.67141  0.67512
# sl_algorithm_glmnet 0.67318 0.000914 13976  0.67139  0.67497
# sl_algorithm_nnls   0.67308 0.000914 13976  0.67129  0.67487
# sl_algorithm_ranger 0.65557 0.000992 13976  0.65363  0.65752
# 
# N = N3000, R2 = 0.8, dgp = pwlinear:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.67588 0.000864 13976  0.67418  0.67757
# glmnet              0.60467 0.001300 13976  0.60212  0.60721
# ranger              0.66409 0.000944 13976  0.66224  0.66594
# rpart               0.25069 0.001050 13976  0.24863  0.25274
# sl_algorithm_glm    0.68150 0.000944 13976  0.67965  0.68335
# sl_algorithm_glmnet 0.68152 0.000914 13976  0.67973  0.68332
# sl_algorithm_nnls   0.68128 0.000914 13976  0.67948  0.68307
# sl_algorithm_ranger 0.66598 0.000992 13976  0.66403  0.66792
# 
# Results are averaged over the levels of: lin_inter, rel 
# Confidence level used: 0.95 
# > emmNxModelxR2xlin_inter <- emmeans(aov_rsquared, ~ "Model", by = c("N", "R2", "lin_inter"))
# > emmNxModelxR2xlin_inter
# N = N100, R2 = 0.2, lin_inter = lin_inter0.0_1.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.02503 0.001910 13976  0.02128  0.02877
# glmnet              0.02899 0.002880 13976  0.02335  0.03463
# ranger              0.03096 0.002090 13976  0.02687  0.03506
# rpart               0.01917 0.002320 13976  0.01461  0.02372
# sl_algorithm_glm    0.03255 0.002090 13976  0.02845  0.03665
# sl_algorithm_glmnet 0.03476 0.002020 13976  0.03079  0.03873
# sl_algorithm_nnls   0.03670 0.002020 13976  0.03274  0.04067
# sl_algorithm_ranger 0.02449 0.002200 13976  0.02019  0.02880
# 
# N = N1000, R2 = 0.2, lin_inter = lin_inter0.0_1.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.10237 0.000910 13976  0.10059  0.10415
# glmnet              0.11450 0.001370 13976  0.11182  0.11719
# ranger              0.10591 0.000994 13976  0.10396  0.10786
# rpart               0.04690 0.001110 13976  0.04474  0.04907
# sl_algorithm_glm    0.13300 0.000995 13976  0.13105  0.13495
# sl_algorithm_glmnet 0.13375 0.000963 13976  0.13186  0.13564
# sl_algorithm_nnls   0.13418 0.000963 13976  0.13230  0.13607
# sl_algorithm_ranger 0.10534 0.001050 13976  0.10329  0.10739
# 
# N = N3000, R2 = 0.2, lin_inter = lin_inter0.0_1.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.12686 0.000874 13976  0.12515  0.12857
# glmnet              0.12291 0.001320 13976  0.12034  0.12549
# ranger              0.12938 0.000955 13976  0.12751  0.13126
# rpart               0.05171 0.001060 13976  0.04963  0.05380
# sl_algorithm_glm    0.14635 0.000956 13976  0.14447  0.14822
# sl_algorithm_glmnet 0.14661 0.000925 13976  0.14479  0.14842
# sl_algorithm_nnls   0.14702 0.000925 13976  0.14520  0.14883
# sl_algorithm_ranger 0.11877 0.001000 13976  0.11680  0.12074
# 
# N = N100, R2 = 0.5, lin_inter = lin_inter0.0_1.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.14974 0.001190 13976  0.14740  0.15208
# glmnet              0.17456 0.001800 13976  0.17104  0.17809
# ranger              0.18088 0.001300 13976  0.17832  0.18343
# rpart               0.08506 0.001450 13976  0.08222  0.08790
# sl_algorithm_glm    0.22680 0.001310 13976  0.22424  0.22936
# sl_algorithm_glmnet 0.23399 0.001260 13976  0.23151  0.23647
# sl_algorithm_nnls   0.23418 0.001260 13976  0.23170  0.23665
# sl_algorithm_ranger 0.19975 0.001370 13976  0.19706  0.20244
# 
# N = N1000, R2 = 0.5, lin_inter = lin_inter0.0_1.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.31614 0.000879 13976  0.31442  0.31786
# glmnet              0.31095 0.001320 13976  0.30836  0.31354
# ranger              0.33305 0.000960 13976  0.33117  0.33493
# rpart               0.12712 0.001070 13976  0.12502  0.12921
# sl_algorithm_glm    0.36767 0.000961 13976  0.36579  0.36956
# sl_algorithm_glmnet 0.36816 0.000930 13976  0.36634  0.36998
# sl_algorithm_nnls   0.36789 0.000930 13976  0.36606  0.36971
# sl_algorithm_ranger 0.34065 0.001010 13976  0.33867  0.34262
# 
# N = N3000, R2 = 0.5, lin_inter = lin_inter0.0_1.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.34978 0.000864 13976  0.34808  0.35147
# glmnet              0.31620 0.001300 13976  0.31365  0.31875
# ranger              0.36011 0.000944 13976  0.35826  0.36196
# rpart               0.13196 0.001050 13976  0.12991  0.13402
# sl_algorithm_glm    0.37893 0.000944 13976  0.37708  0.38078
# sl_algorithm_glmnet 0.37903 0.000914 13976  0.37724  0.38082
# sl_algorithm_nnls   0.37906 0.000914 13976  0.37727  0.38085
# sl_algorithm_ranger 0.35395 0.000992 13976  0.35201  0.35589
# 
# N = N100, R2 = 0.8, lin_inter = lin_inter0.0_1.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.32190 0.001020 13976  0.31989  0.32390
# glmnet              0.41380 0.001540 13976  0.41078  0.41681
# ranger              0.37805 0.001120 13976  0.37586  0.38024
# rpart               0.16931 0.001240 13976  0.16688  0.17175
# sl_algorithm_glm    0.50417 0.001120 13976  0.50198  0.50636
# sl_algorithm_glmnet 0.50872 0.001080 13976  0.50660  0.51085
# sl_algorithm_nnls   0.50665 0.001080 13976  0.50453  0.50877
# sl_algorithm_ranger 0.46881 0.001170 13976  0.46651  0.47111
# 
# N = N1000, R2 = 0.8, lin_inter = lin_inter0.0_1.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.55312 0.000868 13976  0.55142  0.55482
# glmnet              0.52314 0.001310 13976  0.52058  0.52571
# ranger              0.58084 0.000949 13976  0.57898  0.58270
# rpart               0.20323 0.001050 13976  0.20116  0.20530
# sl_algorithm_glm    0.62262 0.000949 13976  0.62076  0.62448
# sl_algorithm_glmnet 0.62281 0.000919 13976  0.62101  0.62461
# sl_algorithm_nnls   0.62258 0.000919 13976  0.62078  0.62438
# sl_algorithm_ranger 0.60057 0.000997 13976  0.59862  0.60252
# 
# N = N3000, R2 = 0.8, lin_inter = lin_inter0.0_1.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.59169 0.000864 13976  0.58999  0.59338
# glmnet              0.52603 0.001300 13976  0.52348  0.52858
# ranger              0.61101 0.000944 13976  0.60916  0.61286
# rpart               0.21169 0.001050 13976  0.20963  0.21375
# sl_algorithm_glm    0.63140 0.000944 13976  0.62955  0.63325
# sl_algorithm_glmnet 0.63143 0.000914 13976  0.62964  0.63322
# sl_algorithm_nnls   0.63138 0.000914 13976  0.62959  0.63317
# sl_algorithm_ranger 0.61244 0.000992 13976  0.61049  0.61438
# 
# N = N100, R2 = 0.2, lin_inter = lin_inter0.5_0.5:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.04342 0.001570 13976  0.04034  0.04650
# glmnet              0.03075 0.002360 13976  0.02612  0.03538
# ranger              0.05953 0.001720 13976  0.05616  0.06289
# rpart               0.02382 0.001910 13976  0.02008  0.02756
# sl_algorithm_glm    0.04404 0.001720 13976  0.04068  0.04741
# sl_algorithm_glmnet 0.04713 0.001660 13976  0.04387  0.05039
# sl_algorithm_nnls   0.04875 0.001660 13976  0.04549  0.05201
# sl_algorithm_ranger 0.03628 0.001800 13976  0.03274  0.03981
# 
# N = N1000, R2 = 0.2, lin_inter = lin_inter0.5_0.5:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.12090 0.000889 13976  0.11915  0.12264
# glmnet              0.12996 0.001340 13976  0.12734  0.13259
# ranger              0.12269 0.000972 13976  0.12079  0.12460
# rpart               0.04369 0.001080 13976  0.04157  0.04580
# sl_algorithm_glm    0.13645 0.000972 13976  0.13454  0.13835
# sl_algorithm_glmnet 0.13688 0.000941 13976  0.13503  0.13872
# sl_algorithm_nnls   0.13647 0.000941 13976  0.13462  0.13831
# sl_algorithm_ranger 0.10859 0.001020 13976  0.10659  0.11059
# 
# N = N3000, R2 = 0.2, lin_inter = lin_inter0.5_0.5:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.14182 0.000864 13976  0.14013  0.14351
# glmnet              0.14462 0.001300 13976  0.14207  0.14717
# ranger              0.13760 0.000944 13976  0.13575  0.13945
# rpart               0.04887 0.001050 13976  0.04682  0.05093
# sl_algorithm_glm    0.15097 0.000944 13976  0.14912  0.15282
# sl_algorithm_glmnet 0.15124 0.000914 13976  0.14945  0.15304
# sl_algorithm_nnls   0.15130 0.000914 13976  0.14950  0.15309
# sl_algorithm_ranger 0.12463 0.000992 13976  0.12268  0.12657
# 
# N = N100, R2 = 0.5, lin_inter = lin_inter0.5_0.5:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.21421 0.001050 13976  0.21216  0.21626
# glmnet              0.19250 0.001570 13976  0.18941  0.19558
# ranger              0.25209 0.001140 13976  0.24985  0.25433
# rpart               0.08878 0.001270 13976  0.08629  0.09127
# sl_algorithm_glm    0.24771 0.001140 13976  0.24547  0.24996
# sl_algorithm_glmnet 0.25430 0.001110 13976  0.25213  0.25647
# sl_algorithm_nnls   0.25088 0.001110 13976  0.24871  0.25305
# sl_algorithm_ranger 0.21925 0.001200 13976  0.21690  0.22161
# 
# N = N1000, R2 = 0.5, lin_inter = lin_inter0.5_0.5:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.38084 0.000864 13976  0.37914  0.38253
# glmnet              0.38764 0.001300 13976  0.38509  0.39019
# ranger              0.37090 0.000944 13976  0.36905  0.37275
# rpart               0.12051 0.001050 13976  0.11845  0.12257
# sl_algorithm_glm    0.40685 0.000945 13976  0.40500  0.40870
# sl_algorithm_glmnet 0.40680 0.000915 13976  0.40500  0.40859
# sl_algorithm_nnls   0.40661 0.000915 13976  0.40481  0.40840
# sl_algorithm_ranger 0.37574 0.000993 13976  0.37379  0.37768
# 
# N = N3000, R2 = 0.5, lin_inter = lin_inter0.5_0.5:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.40391 0.000864 13976  0.40222  0.40560
# glmnet              0.39802 0.001300 13976  0.39547  0.40057
# ranger              0.39353 0.000944 13976  0.39168  0.39538
# rpart               0.12477 0.001050 13976  0.12272  0.12683
# sl_algorithm_glm    0.41955 0.000944 13976  0.41770  0.42140
# sl_algorithm_glmnet 0.41946 0.000914 13976  0.41767  0.42125
# sl_algorithm_nnls   0.41901 0.000914 13976  0.41722  0.42080
# sl_algorithm_ranger 0.39346 0.000992 13976  0.39152  0.39541
# 
# N = N100, R2 = 0.8, lin_inter = lin_inter0.5_0.5:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.43717 0.000941 13976  0.43533  0.43902
# glmnet              0.45971 0.001420 13976  0.45694  0.46249
# ranger              0.45911 0.001030 13976  0.45710  0.46113
# rpart               0.15603 0.001140 13976  0.15379  0.15827
# sl_algorithm_glm    0.52233 0.001030 13976  0.52031  0.52434
# sl_algorithm_glmnet 0.52474 0.000996 13976  0.52279  0.52669
# sl_algorithm_nnls   0.52084 0.000996 13976  0.51889  0.52279
# sl_algorithm_ranger 0.48577 0.001080 13976  0.48365  0.48789
# 
# N = N1000, R2 = 0.8, lin_inter = lin_inter0.5_0.5:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.62616 0.000864 13976  0.62447  0.62786
# glmnet              0.61765 0.001300 13976  0.61510  0.62020
# ranger              0.61367 0.000944 13976  0.61182  0.61552
# rpart               0.19788 0.001050 13976  0.19582  0.19993
# sl_algorithm_glm    0.65897 0.000945 13976  0.65712  0.66082
# sl_algorithm_glmnet 0.65928 0.000915 13976  0.65749  0.66107
# sl_algorithm_nnls   0.65937 0.000915 13976  0.65758  0.66116
# sl_algorithm_ranger 0.64035 0.000993 13976  0.63840  0.64229
# 
# N = N3000, R2 = 0.8, lin_inter = lin_inter0.5_0.5:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.64730 0.000864 13976  0.64560  0.64899
# glmnet              0.62225 0.001300 13976  0.61970  0.62479
# ranger              0.64287 0.000944 13976  0.64102  0.64472
# rpart               0.20621 0.001050 13976  0.20415  0.20826
# sl_algorithm_glm    0.66732 0.000944 13976  0.66547  0.66917
# sl_algorithm_glmnet 0.66758 0.000914 13976  0.66579  0.66937
# sl_algorithm_nnls   0.66739 0.000914 13976  0.66560  0.66918
# sl_algorithm_ranger 0.65261 0.000992 13976  0.65066  0.65455
# 
# N = N100, R2 = 0.2, lin_inter = lin_inter1.0_0.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.08493 0.001340 13976  0.08231  0.08755
# glmnet              0.08224 0.002010 13976  0.07830  0.08618
# ranger              0.11196 0.001460 13976  0.10910  0.11482
# rpart               0.05270 0.001620 13976  0.04952  0.05588
# sl_algorithm_glm    0.09438 0.001460 13976  0.09152  0.09724
# sl_algorithm_glmnet 0.09995 0.001410 13976  0.09717  0.10272
# sl_algorithm_nnls   0.10143 0.001410 13976  0.09866  0.10420
# sl_algorithm_ranger 0.07725 0.001530 13976  0.07425  0.08026
# 
# N = N1000, R2 = 0.2, lin_inter = lin_inter1.0_0.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.16361 0.000865 13976  0.16191  0.16530
# glmnet              0.18147 0.001300 13976  0.17891  0.18402
# ranger              0.16649 0.000945 13976  0.16464  0.16835
# rpart               0.07785 0.001050 13976  0.07579  0.07991
# sl_algorithm_glm    0.17946 0.000946 13976  0.17761  0.18132
# sl_algorithm_glmnet 0.18001 0.000916 13976  0.17821  0.18180
# sl_algorithm_nnls   0.18061 0.000915 13976  0.17881  0.18240
# sl_algorithm_ranger 0.14450 0.000994 13976  0.14255  0.14645
# 
# N = N3000, R2 = 0.2, lin_inter = lin_inter1.0_0.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.17725 0.000864 13976  0.17556  0.17895
# glmnet              0.18836 0.001300 13976  0.18582  0.19091
# ranger              0.17617 0.000944 13976  0.17432  0.17802
# rpart               0.08352 0.001050 13976  0.08146  0.08557
# sl_algorithm_glm    0.18739 0.000944 13976  0.18554  0.18924
# sl_algorithm_glmnet 0.18743 0.000914 13976  0.18564  0.18922
# sl_algorithm_nnls   0.18790 0.000914 13976  0.18611  0.18969
# sl_algorithm_ranger 0.15544 0.000992 13976  0.15350  0.15739
# 
# N = N100, R2 = 0.5, lin_inter = lin_inter1.0_0.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.32880 0.000924 13976  0.32698  0.33061
# glmnet              0.36338 0.001390 13976  0.36065  0.36610
# ranger              0.37599 0.001010 13976  0.37401  0.37797
# rpart               0.16891 0.001120 13976  0.16671  0.17111
# sl_algorithm_glm    0.37606 0.001010 13976  0.37408  0.37804
# sl_algorithm_glmnet 0.38215 0.000978 13976  0.38024  0.38407
# sl_algorithm_nnls   0.38206 0.000978 13976  0.38014  0.38397
# sl_algorithm_ranger 0.34207 0.001060 13976  0.33999  0.34415
# 
# N = N1000, R2 = 0.5, lin_inter = lin_inter1.0_0.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.43984 0.000864 13976  0.43814  0.44153
# glmnet              0.46367 0.001300 13976  0.46112  0.46622
# ranger              0.44426 0.000944 13976  0.44241  0.44611
# rpart               0.20498 0.001050 13976  0.20292  0.20704
# sl_algorithm_glm    0.46202 0.000944 13976  0.46017  0.46387
# sl_algorithm_glmnet 0.46231 0.000914 13976  0.46052  0.46411
# sl_algorithm_nnls   0.46314 0.000914 13976  0.46135  0.46493
# sl_algorithm_ranger 0.43065 0.000992 13976  0.42871  0.43260
# 
# N = N3000, R2 = 0.5, lin_inter = lin_inter1.0_0.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.45580 0.000864 13976  0.45411  0.45750
# glmnet              0.46726 0.001300 13976  0.46471  0.46981
# ranger              0.45401 0.000944 13976  0.45216  0.45586
# rpart               0.20779 0.001050 13976  0.20574  0.20985
# sl_algorithm_glm    0.46654 0.000944 13976  0.46469  0.46839
# sl_algorithm_glmnet 0.46665 0.000914 13976  0.46485  0.46844
# sl_algorithm_nnls   0.46706 0.000914 13976  0.46527  0.46885
# sl_algorithm_ranger 0.43912 0.000992 13976  0.43718  0.44106
# 
# N = N100, R2 = 0.8, lin_inter = lin_inter1.0_0.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.61450 0.000869 13976  0.61279  0.61620
# glmnet              0.69569 0.001310 13976  0.69313  0.69826
# ranger              0.65526 0.000950 13976  0.65340  0.65713
# rpart               0.29201 0.001060 13976  0.28994  0.29408
# sl_algorithm_glm    0.69421 0.000951 13976  0.69235  0.69607
# sl_algorithm_glmnet 0.69537 0.000920 13976  0.69357  0.69718
# sl_algorithm_nnls   0.69785 0.000920 13976  0.69604  0.69965
# sl_algorithm_ranger 0.65977 0.000999 13976  0.65782  0.66173
# 
# N = N1000, R2 = 0.8, lin_inter = lin_inter1.0_0.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.71528 0.000864 13976  0.71359  0.71698
# glmnet              0.73556 0.001300 13976  0.73301  0.73811
# ranger              0.71719 0.000944 13976  0.71534  0.71904
# rpart               0.31993 0.001050 13976  0.31788  0.32199
# sl_algorithm_glm    0.73466 0.000944 13976  0.73281  0.73652
# sl_algorithm_glmnet 0.73473 0.000914 13976  0.73294  0.73652
# sl_algorithm_nnls   0.73518 0.000914 13976  0.73339  0.73697
# sl_algorithm_ranger 0.71579 0.000992 13976  0.71384  0.71773
# 
# N = N3000, R2 = 0.8, lin_inter = lin_inter1.0_0.0:
#   Model                emmean       SE    df lower.CL upper.CL
# gbm                 0.72701 0.000864 13976  0.72532  0.72870
# glmnet              0.73705 0.001300 13976  0.73450  0.73959
# ranger              0.72482 0.000944 13976  0.72297  0.72667
# rpart               0.32034 0.001050 13976  0.31828  0.32239
# sl_algorithm_glm    0.73671 0.000944 13976  0.73486  0.73856
# sl_algorithm_glmnet 0.73676 0.000914 13976  0.73496  0.73855
# sl_algorithm_nnls   0.73691 0.000914 13976  0.73512  0.73870
# sl_algorithm_ranger 0.72166 0.000992 13976  0.71971  0.72360

# save marginal means of interest
# research question 1 and 3
df_emmNxModel <- as.data.frame(emmNxModel)

# research question 2
df_emmNxModelxR2 <- as.data.frame(emmNxModelxR2)
df_emmNxModelxDGP <- as.data.frame(emmNxModelxDGP)
df_emmNxModelxlin_inter <- as.data.frame(emmNxModelxlin_inter) 
df_emmNxModelxR2xlin_inter <- as.data.frame(emmNxModelxR2xlin_inter)
save(df_emmNxModel, df_emmNxModelxR2, df_emmNxModelxDGP, df_emmNxModelxlin_inter,
     df_emmNxModelxR2xlin_inter, file = "results/anova/mixedANOVA_postHocEMMs.rda")


## plot interaction of N and model performance using R2 as dependent measure
# load("~/SLsim/results/anova/mixedANOVA_postHocEMMs.rda")
colRamp <- colorRampPalette(c("#6f9c3d", "#b8c36b", "#ffb366", "#ff8829", "#fe6b40"))(n = 200)
limit.bias <- 1

df_emmNxModel$Model <- factor(df_emmNxModel$Model,
                              levels = c("gbm", "ranger", "glmnet", "rpart", 
                                         "sl_algorithm_glm", "sl_algorithm_glmnet",
                                         "sl_algorithm_nnls", "sl_algorithm_ranger"), 
                              labels = c("gbm", "ranger", "glmnet", "rpart", 
                                         "sl_algorithm_glm", "sl_algorithm_glmnet", 
                                         "sl_algorithm_nnls", "sl_algorithm_ranger"))

df_emmNxModel$N <- factor(df_emmNxModel$N, 
                             levels = c("N100", "N1000", "N3000"), 
                             labels = c("N100", "N1000", "N3000")) 


# plot with legend to see if colour coding is accurate
(plot4guide <- ggplot(df_emmNxModel, 
                      aes(x = N, y = Model, fill = emmean)) + 
    geom_tile() +
    geom_text(aes(x = N, y = Model, label = round(emmean, 2)), 
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

# heatmap of NxModel interaction
(pNxModel <- plotHeatmap(df_emmNxModel, df_emmNxModel$N, df_emmNxModel$Model,
                         xLabel = "N", yLabel = "Model"))
(pNxModel <- themeFunction(pNxModel))


# heatmaps of interactions of interest for research question 2

# 3-way interaction NxModelxDGP
df_emmNxModelxDGP$Model <- factor(df_emmNxModelxDGP$Model,
                              levels = c("gbm", "ranger", "glmnet", "rpart", 
                                         "sl_algorithm_glm", "sl_algorithm_glmnet",
                                         "sl_algorithm_nnls", "sl_algorithm_ranger"), 
                              labels = c("gbm", "ranger", "glmnet", "rpart", 
                                         "sl_algorithm_glm", "sl_algorithm_glmnet", 
                                         "sl_algorithm_nnls", "sl_algorithm_ranger"))

df_emmNxModelxDGP$NxDGP <- factor(df_emmNxModelxDGP$N, 
                          levels = c("N100", "N1000", "N3000"), 
                          labels = c("N100", "N1000", "N3000")) 

df_emmNxModelxDGP$NxDGP <- factor(df_emmNxModelxDGP$dgp, 
                              levels = c("inter", "nonlinear3", "pwlinear"), 
                              labels = c("inter", "nl3", "pw")) 

(pNxModelxDGP <- plotHeatmap(df_emmNxModelxDGP,
                             df_emmNxModelxDGP$dgp, 
                                   interaction(df_emmNxModelxDGP$N, df_emmNxModelxDGP$Model, sep = "_x_")))
(pNxModelxDGP <- themeFunction(pNxModelxDGP))



# 3-way interaction NxModelxlin_inter
df_emmNxModelxlin_inter$Model <- factor(df_emmNxModelxlin_inter$Model,
                              levels = c("gbm", "ranger", "glmnet", "rpart", 
                                         "sl_algorithm_glm", "sl_algorithm_glmnet",
                                         "sl_algorithm_nnls", "sl_algorithm_ranger"), 
                              labels = c("gbm", "ranger", "glmnet", "rpart", 
                                         "sl_algorithm_glm", "sl_algorithm_glmnet", 
                                         "sl_algorithm_nnls", "sl_algorithm_ranger"))

df_emmNxModelxlin_inter$N <- factor(df_emmNxModelxlin_inter$N, 
                          levels = c("N100", "N1000", "N3000"), 
                          labels = c("N100", "N1000", "N3000")) 

df_emmNxModelxlin_inter$lin_inter <- factor(df_emmNxModelxlin_inter$lin_inter,
                                            levels = c("lin_inter0.0_1.0", "lin_inter0.5_0.5", "lin_inter1.0_0.0"),
                                            labels = c("0:100", "50:50", "100:0"))

(pNxModelxlin_inter <- plotHeatmap(df_emmNxModelxlin_inter,
                             df_emmNxModelxlin_inter$lin_inter, 
                             interaction(df_emmNxModelxlin_inter$N, df_emmNxModelxlin_inter$Model, sep = "_x_")))
(pNxModelxlin_inter <- themeFunction(pNxModelxlin_inter))

# 3-way interaction NxModelxR2
df_emmNxModelxR2$Model <- factor(df_emmNxModelxR2$Model,
                              levels = c("gbm", "ranger", "glmnet", "rpart", 
                                         "sl_algorithm_glm", "sl_algorithm_glmnet",
                                         "sl_algorithm_nnls", "sl_algorithm_ranger"), 
                              labels = c("gbm", "ranger", "glmnet", "rpart", 
                                         "sl_algorithm_glm", "sl_algorithm_glmnet", 
                                         "sl_algorithm_nnls", "sl_algorithm_ranger"))

df_emmNxModelxR2$N <- factor(df_emmNxModelxR2$N, 
                          levels = c("N100", "N1000", "N3000"), 
                          labels = c("N100", "N1000", "N3000")) 

df_emmNxModelxR2$R2 <- factor(df_emmNxModelxR2$R2, 
                             levels = c("0.2", "0.5", "0.8"), 
                             labels = c("0.2", "0.5", "0.8")) 

(pNxModelxR2 <- plotHeatmap(df_emmNxModelxR2,
                                   df_emmNxModelxR2$R2, 
                                   interaction(df_emmNxModelxR2$N, df_emmNxModelxR2$Model, sep = "_x_")))
(pNxModelxR2 <- themeFunction(pNxModelxR2))


# 4-way interaction NxModelxR2xlin_inter
df_emmNxModelxR2xlin_inter$Model <- factor(df_emmNxModelxR2xlin_inter$Model,
                                 levels = c("gbm", "ranger", "glmnet", "rpart", 
                                            "sl_algorithm_glm", "sl_algorithm_glmnet",
                                            "sl_algorithm_nnls", "sl_algorithm_ranger"), 
                                 labels = c("gbm", "ranger", "glmnet", "rpart", 
                                            "sl_algorithm_glm", "sl_algorithm_glmnet", 
                                            "sl_algorithm_nnls", "sl_algorithm_ranger"))

df_emmNxModelxR2xlin_inter$N <- factor(df_emmNxModelxR2xlin_inter$N, 
                                levels = c("N100", "N1000", "N3000"), 
                                labels = c("N100", "N1000", "N3000")) 

df_emmNxModelxR2xlin_inter$R2 <- factor(df_emmNxModelxR2xlin_inter$R2, 
                                levels = c("0.2", "0.5", "0.8"), 
                                labels = c("0.2", "0.5", "0.8")) 

df_emmNxModelxR2xlin_inter$lin_inter <- factor(df_emmNxModelxR2xlin_inter$lin_inter,
                                            levels = c("lin_inter0.0_1.0", "lin_inter0.5_0.5", "lin_inter1.0_0.0"),
                                            labels = c("0:100", "50:50", "100:0"))


(pNxModelxR2xlin_inter <- plotHeatmap(df_emmNxModelxR2xlin_inter,
                                      interaction(df_emmNxModelxR2xlin_inter$R2, df_emmNxModelxR2xlin_inter$lin_inter, 
                                                  sep = "_x_"), 
                                      interaction(df_emmNxModelxR2xlin_inter$N, df_emmNxModelxR2xlin_inter$Model, 
                                                  sep = "_x_")))
(pNxModelxR2xlin_inter <- themeFunction(pNxModelxR2xlin_inter))


## closer investigating scaled weights of glmnet as meta model 

# restructuring data similar to performance measures above

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
      
      tmp_final = left_join(tmp_intercept, tmp_rpart, by = c("dgp", "lin_inter", "N", "R2", "rel", "ID"))
      tmp_final = left_join(tmp_final, tmp_ranger, by = c("dgp", "lin_inter", "N", "R2", "rel", "ID"))
      tmp_final = left_join(tmp_final, tmp_gbm, by = c("dgp", "lin_inter", "N", "R2", "rel", "ID"))
      tmp_final = left_join(tmp_final, tmp_glmnet, by = c("dgp", "lin_inter", "N", "R2", "rel", "ID"))
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
      
      tmp_final = left_join(tmp_intercept, tmp_rpart, by = c("dgp", "lin_inter", "N", "R2", "rel", "ID"))
      tmp_final = left_join(tmp_final, tmp_ranger, by = c("dgp", "lin_inter", "N", "R2", "rel", "ID"))
      tmp_final = left_join(tmp_final, tmp_gbm, by = c("dgp", "lin_inter", "N", "R2", "rel", "ID"))
      tmp_final = left_join(tmp_final, tmp_glmnet, by = c("dgp", "lin_inter", "N", "R2", "rel", "ID"))
      tmp_final <- tmp_final[, order(colnames(tmp_final))]
    }
    if (iData == "train") {
      train <- tmp_final
    } else if (iData == "test") {
      test <- tmp_final
      scaledWeightsGLMnetDFwide_dgpSpec <- left_join(train, test,
                                                     by = c("dgp", "lin_inter", "N", "R2", "rel", "ID"),
                                                     suffix = c("", ""))
    }
  }
  
  if (iDGP == "inter") {
    scaledWeightsGLMnetDFwide <- scaledWeightsGLMnetDFwide_dgpSpec
  } else {
    scaledWeightsGLMnetDFwide <- rbind(scaledWeightsGLMnetDFwide, scaledWeightsGLMnetDFwide_dgpSpec)
  }
}
