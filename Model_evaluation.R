# Plot to compare model performances 

# Plotting Mean Absolut Error as used to compare predictions ####

# save relevant values
glmnet_train_MAE   <- getTrainPerf(models_s_noInt$glmnet)$TrainMAE
glmnet_test_MAE    <- postResample(pred = test_s_noInt$glmnet_s_noInt_pred,
                                   obs  = test_s_noInt$krit)["MAE"]

rpart_train_MAE    <- getTrainPerf(models_s_noInt$rpart)$TrainMAE
rpart_test_MAE     <- postResample(pred = test_s_noInt$rpart_s_noInt_pred,
                                   obs  = test_s_noInt$krit)["MAE"]

ens_train_MAE      <- mean(ensemble_s_noInt$ens_model$resample$MAE)
ens_test_MAE       <- postResample(pred = test_s_noInt$pred,
                                   obs  = test_s_noInt$krit)["MAE"]

# make table to be used in ggplot
mae_tbl <- tibble(
  model    = rep(c("glmnet", "rpart", "Ensemble"), each = 2),  # repeats every model 2 times
  data_set = rep(c("Train",  "Test"),  times = 3),             # repeats data source 3 times to fit
  # models in each row
  MAE       = c(glmnet_train_MAE, glmnet_test_MAE,              # calculated MAE sortet accordingly
                rpart_train_MAE,  rpart_test_MAE,
                ens_train_MAE,    ens_test_MAE))

# make ggplot
ggplot(mae_tbl, aes(x = model, y = MAE,                       # defines axes
                    colour = data_set, group = data_set)) +   # used to achieve two graphs (one per data set)
  geom_line(linewidth = 1) +                                  # connects dots 
  geom_point(size = 3) +                                      # defines size of dots
  geom_text(aes(label = round(MAE, 2)),
            vjust = -0.7, size = 3) +                         # places rounded MAE above dots
  labs(title = "MAE – Train vs. Test (Small dataset)",
       y = "Mean Absolute Error", x = NULL, colour = NULL) +  # adds relevant labels
  theme_minimal(base_size = 12) +                                 # simpler layout for plot
  scale_colour_brewer(palette = "Set2")                           # define colours of graph


# Plotting Rsquared ####

# save relevant values
glmnet_train_Rsquared   <- getTrainPerf(models_s_noInt$glmnet)$TrainRsquared
glmnet_test_Rsquared    <- postResample(pred = test_s_noInt$glmnet_s_noInt_pred,
                                        obs  = test_s_noInt$krit)["Rsquared"]

rpart_train_Rsquared    <- getTrainPerf(models_s_noInt$rpart)$TrainRsquared
rpart_test_Rsquared     <- postResample(pred = test_s_noInt$rpart_s_noInt_pred,
                                        obs  = test_s_noInt$krit)["Rsquared"]

ens_train_Rsquared      <- mean(ensemble_s_noInt$ens_model$resample$Rsquared)
ens_test_Rsquared       <- postResample(pred = test_s_noInt$pred,
                                        obs  = test_s_noInt$krit)["Rsquared"]

# make table to be used in ggplot
rsquared_tbl <- tibble(
  model    = rep(c("glmnet", "rpart", "Ensemble"), each = 2),
  data_set = rep(c("Train",  "Test"),  times = 3),
  Rsquared  = c(glmnet_train_Rsquared, glmnet_test_Rsquared,
                rpart_train_Rsquared,  rpart_test_Rsquared,
                ens_train_Rsquared,    ens_test_Rsquared))

# make ggplot
ggplot(rsquared_tbl,
       aes(x = model, y = Rsquared,
           colour = data_set, group = data_set)) +
  geom_line(linewidth = 1) +          # verbindet die Punkte
  geom_point(size = 3) +              # Punkte für jeden MAE‑Wert
  geom_text(aes(label = round(Rsquared, 2)),
            vjust = -0.7, size = 3) +
  labs(title = "Rsquared – Train vs. Test (Small data set",
       y = "Rsquared", x = NULL, colour = NULL) +
  theme_minimal(base_size = 12) +
  scale_colour_brewer(palette = "Set2")


# Plotting Root Mean Square Error ####

# save relevant values
glmnet_train_RMSE   <- getTrainPerf(models_s_noInt$glmnet)$TrainRMSE
glmnet_test_RMSE    <- postResample(pred = test_s_noInt$glmnet_s_noInt_pred,
                                    obs  = test_s_noInt$krit)["RMSE"]

rpart_train_RMSE    <- getTrainPerf(models_s_noInt$rpart)$TrainRMSE
rpart_test_RMSE     <- postResample(pred = test_s_noInt$rpart_s_noInt_pred,
                                    obs  = test_s_noInt$krit)["RMSE"]

ens_train_RMSE      <- mean(ensemble_s_noInt$ens_model$resample$RMSE)
ens_test_RMSE       <- postResample(pred = test_s_noInt$pred,
                                    obs  = test_s_noInt$krit)["RMSE"]

# make table to be used in ggplot
RMSE_tbl <- tibble(
  model    = rep(c("glmnet", "rpart", "Ensemble"), each = 2),
  data_set = rep(c("Train",  "Test"),  times = 3),
  RMSE  = c(glmnet_train_RMSE, glmnet_test_RMSE,
            rpart_train_RMSE,  rpart_test_RMSE,
            ens_train_RMSE,    ens_test_RMSE))

# make ggplot
ggplot(RMSE_tbl,
       aes(x = model, y = RMSE,
           colour = data_set, group = data_set)) +
  geom_line(linewidth = 1) +          # verbindet die Punkte
  geom_point(size = 3) +              # Punkte für jeden MAE‑Wert
  geom_text(aes(label = round(RMSE, 2)),
            vjust = -0.7, size = 3) +
  labs(title = "RMSE – Train vs. Test (Small data set",
       y = "RMSE", x = NULL, colour = NULL) +
  theme_minimal(base_size = 12) +
  scale_colour_brewer(palette = "Set2")



