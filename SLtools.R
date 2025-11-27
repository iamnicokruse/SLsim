#############################################################
##____________Useful functions for superlearner____________##
#############################################################

# functions for data split ####

data.split <- function(data, ratio) {
  # Input:
  # data = dataset that is to be split 
  # ratio = split ratio between test and training data (numeric)
  data <- data
  ratio <- ratio
  trainIndex <- sample(1:nrow(data), ratio * nrow(data))
  
  # Output:
  # train_data = training data (amount of data accordinmg to ratio)
  # test_data = test data (amoúnt of data according to 1 - ratio)
  .GlobalEnv$train_data <- (data[ trainIndex, ])
  .GlobalEnv$test_data  <- (data[-trainIndex, ])
  data_list <- list(train_data, test_data)
  return(data_list)
}

local.data.split <- function(data, ratio) {
  # Input:
  # data = dataset that is to be split 
  # ratio = split ratio between test and training data (numeric)
  data <- data
  ratio <- ratio
  trainIndex <- sample(1:nrow(data), ratio * nrow(data))
  
  # Output:
  # train_data = training data (amount of data accordinmg to ratio)
  # test_data = test data (amoúnt of data according to 1 - ratio)
  train_data <- (data[ trainIndex, ])
  test_data  <- (data[-trainIndex, ])
  data_list <- list(train_data = train_data,test_data = test_data)
  return(data_list)
}

# functions for performance plots ####

# write functions to plot averaged performance (for iterations) metrics across base and meta learner

plot.performance.iter <- function(train_mean, test_mean,                    # data frames with values
                                  metric = c("RMSE", "MAE", "Rsquared")) {  # available metrics
  
  metric <- match.arg(metric)                     # limits metrics to the three above
  train_col <- paste0("Train", metric, "_mean")   # needed to adress right value below
  test_col  <- paste0("Test", metric, "_mean")            # needed to adress right value below
  # above is used to paste metric into the according name in either data frame as it is 
  # labeled different in both data sets
  
  train_df <- train_mean 
  train_df1 <-  mutate(train_mean, model = methods)                          # new model column (with methods)
  train_df2 <-  select(train_df1, model, value = !! rlang::sym(train_col))  # model names from column as symbols so
  # R searches value, not the character string
  train_df <- mutate(train_df2, set = "Train")                              # new "set" column ("Train") for group
  # distinction in plot
  
  test_df <- test_mean  
  test_df1 <- mutate(test_mean, model = methods)                             # new model column (with methods)
  test_df2 <- select(test_df1, model, value = !! rlang::sym(test_col))      # model names from column as symbols so
  # R searches value, not the character string
  test_df <- mutate(test_df2, set = "Test")                                 # new "set" column ("Test") for group
  # distinction in plot
  
  plt_tbl <- bind_rows(train_df, test_df) # data frame for plot
  
  ggplot(plt_tbl,
         aes(x = model, y = value, colour = set, group = set)) +        # defines axes and graphs
    geom_line(linewidth = 1) +                                          # connects dots 
    geom_point(size = 3) +                                              # defines size of dots
    geom_text(aes(label = round(value, 2)), vjust = -0.7, size = 3) +   # places value above dots
    labs(title = paste("Averaged", metric, "across Iterations"),        
         y = metric, x = NULL, colour = NULL) +                         # adds relevant labels
    theme_minimal(base_size = 12) +                                     # simpler layout for plots
    scale_colour_brewer(palette = "Set2")                               # define color scheme 
}


# function to plot averaged performance (for multiple data sets) metrics across base and meta learner

plot.performance.data <- function(train_mean, test_mean,                    # data frames with values
                                  metric = c("RMSE", "MAE", "Rsquared")) {  # available metrics
  
  metric <- match.arg(metric)                     # limits metrics to the three above
  train_col <- paste0("Train", metric, "_mean")   # needed to adress right value below
  test_col  <- paste0("Test", metric, "_mean")            # needed to adress right value below
  # above is used to paste metric into the according name in either data frame as it is 
  # labeled different in both data sets
  
  train_df <- train_mean 
  train_df1 <-  mutate(train_mean, model = methods)                          # new model column (with methods)
  train_df2 <-  select(train_df1, model, value = !! rlang::sym(train_col))  # model names from column as symbols so
  # R searches value, not the character string
  train_df <- mutate(train_df2, set = "Train")                              # new "set" column ("Train") for group
  # distinction in plot
  
  test_df <- test_mean  
  test_df1 <- mutate(test_mean, model = methods)                             # new model column (with methods)
  test_df2 <- select(test_df1, model, value = !! rlang::sym(test_col))      # model names from column as symbols so
  # R searches value, not the character string
  test_df <- mutate(test_df2, set = "Test")                                 # new "set" column ("Test") for group
  # distinction in plot
  
  plt_tbl <- bind_rows(train_df, test_df) # data frame for plot
  
  ggplot(plt_tbl,
         aes(x = model, y = value, colour = set, group = set)) +        # defines axes and graphs
    geom_line(linewidth = 1) +                                          # connects dots 
    geom_point(size = 3) +                                              # defines size of dots
    geom_text(aes(label = round(value, 2)), vjust = -0.7, size = 3) +   # places value above dots
    labs(title = paste("Averaged", metric, "across datasets"),        
         y = metric, x = NULL, colour = NULL) +                         # adds relevant labels
    theme_minimal(base_size = 12) +                                     # simpler layout for plots
    scale_colour_brewer(palette = "Set2")                               # define color scheme 
}


# further functions

get.rows <- function(file, file_list, methods) {
  i <- match(file, file_list)
  ((i-1) * length(methods) + 1):(i * length(methods)) # first sample rows 1:5, second sample rows 6:10, ...
}

