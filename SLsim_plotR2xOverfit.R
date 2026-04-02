###############################################################################
#____________________________Ploting R2 and Overfit___________________________#
###############################################################################

# load packages for plotting
library(ggplot2)
library(ggh4x)
library(tidyverse)

# get parameter values and utility functions 
source("MLsim-main/utils/setParameters.R")
source("MLsim-main/utils/analysisTools.R")

# create plot folder
plotFolder <- "plots"
if (!file.exists(plotFolder)){
  dir.create(plotFolder)
}

# get data
load("results/anova/aov_data.rda")
rmCols <- c("train_mae", "test_mae", "train_rmse", "test_rmse", "train_rsquared", "ID")
plot_data <- aov_data[, !names(aov_data) %in% rmCols]


performanceStats <- plot_data %>%
  group_by(lin_inter, Model, N, R2, rel, dgp) %>%
  summarise(
    M  = mean(test_rsquared, na.rm = TRUE),
    SD = sd(test_rsquared, na.rm = TRUE),
    SE = SD / sqrt(n()),
    .groups = "drop"
  )

# sl_algorithm_nnls, sl_algorithm_glm, sl_algorithm_glmnet, sl_algorithm_ranger, 
# glmnet, rpart, gbm, ranger
modelLineColors <- c("#1F77B4", "#FF7F0E", "#2CA02C" ,"#D62728" ,"#9467BD",
                     "#8C564B", "#E377C2", "#17BECF")
modelPointShapes <- c(18, 1, 3, 4, 15, 16, 17, 8)

# lin_inter_0.0_1.0, lin_inter_0.5_0.5, lin_inter_1.0_0.0
lin_interPanelColors <- c("#F44336", "#9C27B0", "#1565C0")
relColValues <- c("#999999", "#444444") # rel = 0.7, rel = 1
R2ColValues <- c("#999999", "#555555", "#222222")
linetypeVec <- c("dotted", "dashed", "solid")

# plot R2 similar to MLsim
plotR2_likeMLsim <- function(data, plotMeasure, title = "", yLabel = "",
                             yMin = NULL, yMax = NULL){
  ggplot(data,
         aes(x = N, y = plotMeasure, 
             group = interaction(R2, Model), linetype = R2, color = Model)) +
    geom_line(linewidth = 1, alpha = 0.4) +
    geom_point(aes(shape = Model), size = 3) +
    scale_linetype_manual(name = expression(R[sim]^2), values = linetypeVec,
                          guide = guide_legend(override.aes = list(size = 2, alpha = 0.8))) +
    scale_color_manual(values = modelLineColors) +
    scale_shape_manual(values = modelPointShapes) +
    scale_y_continuous(limits = c(yMin, yMax), breaks=seq(
      ifelse(yMin %% 0.2 == 0, yMin, yMin + 0.1), yMax, 0.2)) +
    geom_hline(aes(yintercept = 0)) +
    facet_grid2(rel ~ lin_inter,
                strip = strip_themed(
                  background_x = list(element_rect(fill = alpha(lin_interPanelColors[1], 0.4)),
                                      element_rect(fill = alpha(lin_interPanelColors[2], 0.4)),
                                      element_rect(fill = alpha(lin_interPanelColors[3], 0.4))),
                  background_y = list(element_rect(fill = alpha(relColValues[2], 0.4)),
                                      element_rect(fill = alpha(relColValues[1], 0.4))))) +
    geom_hline(yintercept = setParam$dgp$Rsquared[1], col = "black",
               alpha = 0.4, linetype = linetypeVec[1]) +
    geom_hline(yintercept = setParam$dgp$Rsquared[2], col = "black",
               alpha = 0.4, linetype = linetypeVec[2]) +
    geom_hline(yintercept = setParam$dgp$Rsquared[3], col = "black",
               alpha = 0.4, linetype = linetypeVec[3]) +
    ylab(yLabel) +
    xlab("Sample Size (N)") +
    ggtitle(title) +
    guides(colour = "none", shape = "none")
}

themeFunction <- function(plotObject, guides = T){
  pTmp <- plotObject + theme(
    panel.grid.major = element_line(linewidth = 0.15, linetype = 'solid', color = "lightgrey"), 
    panel.grid.minor = element_line(linewidth = 0.1, linetype = 'solid', color = "lightgrey"),
    panel.background = element_rect(color = "white", fill = "white"),
    plot.title = element_text(size = 15, face = "bold"),
    axis.text.y = element_text(size = 20),
    axis.text.x = element_text(size = 20),
    axis.title.x = element_text(size = 20),
    axis.title.y = element_text(size = 20),
    strip.text.x = element_text(size = 15),
    strip.text.y = element_text(size = 15),
    #legend.position = c(.85, .39), 
    legend.position = "bottom", 
    legend.title = element_text(size = 25),
    legend.text = element_text(size = 20),
    legend.key.width = unit(2, "cm"),
    legend.box = "horizontal")
  if (guides == F) {
    pTmp <- pTmp + guides(color = "none", shape = "none", linetype = "none")
  }
  return(pTmp)
}

# separate data sets to plot dgp specific data 
subFig_inter <- subset(performanceStats, dgp == "inter")
subFig_inter$Model <- gsub("algorithm_", "", subFig_inter$Model)
subFig_pw    <- subset(performanceStats, dgp == "pwlinear")
subFig_pw$Model <- gsub("algorithm_", "", subFig_pw$Model)
subFig_nl3   <- subset(performanceStats, dgp == "nonlinear3")
subFig_nl3$Model <- gsub("algorithm_", "", subFig_nl3$Model)



# create plots
fig_inter <- plotR2_likeMLsim(subFig_inter, plotMeasure = subFig_inter$M, title = "", yLabel = expression(R[test]^2),
                    yMin = 0, yMax = 0.9)
(fig_inter <- themeFunction(fig_inter))
(fig_inter <- fig_inter + geom_text(
  data = subFig_inter[which(subFig_inter$N == "N1000" &
                            subFig_inter$R2 == "0.8"),],
  aes(x = N, y = 0.85, label = Model, color = Model), position = position_dodge(3)))

fig_pw <- plotR2_likeMLsim(subFig_pw, plotMeasure = subFig_pw$M, title = "", yLabel = expression(R[test]^2),
                    yMin = 0, yMax = 0.9)
(fig_pw <- themeFunction(fig_pw))
(fig_pw <- fig_pw + geom_text(
  data = subFig_pw[which(subFig_pw$N == "N1000" &
                              subFig_pw$R2 == "0.8"),],
  aes(x = N, y = 0.85, label = Model, color = Model), position = position_dodge(3)))

fig_nl3 <- plotR2_likeMLsim(subFig_nl3, plotMeasure = subFig_nl3$M, title = "", yLabel = expression(R[test]^2),
                    yMin = 0, yMax = 0.9)
(fig_nl3 <- themeFunction(fig_nl3))
(fig_nl3 <- fig_nl3 + geom_text(
  data = subFig_nl3[which(subFig_nl3$N == "N1000" &
                              subFig_nl3$R2 == "0.8"),],
  aes(x = N, y = 0.85, label = Model, color = Model), position = position_dodge(3)))

# saving final plots
ggplot2::ggsave(filename = paste0(plotFolder, "/performancePlot_inter.png"),
                plot = fig_inter,
                width = 18.30,
                height = 13.00,
                units = "in")

ggplot2::ggsave(filename = paste0(plotFolder, "/performancePlot_pwlinear.png"),
                plot = fig_pw,
                width = 18.30,
                height = 13.00,
                units = "in")

ggplot2::ggsave(filename = paste0(plotFolder, "/performancePlot_nonlinear3.png"),
                plot = fig_nl3,
                width = 18.30,
                height = 13.00,
                units = "in")

# similar plots but with only meta model "sl_algorithm_glmnet" and subset of 
# data (dgp = "inter")
performanceStats_oneSL <- subset(performanceStats, Model %in% c("gbm", 
                                                                "glmnet", 
                                                                "rpart",
                                                                "ranger", 
                                                                "sl_algorithm_glmnet"
                                                                ))
# dgp = inter
subFig_inter_oneSL <- subset(performanceStats_oneSL, dgp == "inter")
subFig_inter_oneSL$Model <- gsub("algorithm_", "", subFig_inter_oneSL$Model)

fig_inter_oneSL <- plotR2_likeMLsim(subFig_inter_oneSL,
                                    plotMeasure = subFig_inter_oneSL$M, 
                                    title = "dgp = inter", yLabel = expression(R[test]^2),
                                    yMin = 0, yMax = 0.9)
(fig_inter_oneSL <- themeFunction(fig_inter_oneSL))
(fig_inter_oneSL <- fig_inter_oneSL + geom_text(
  data = subFig_inter_oneSL[which(subFig_inter_oneSL$N == "N1000" &
                              subFig_inter_oneSL$R2 == "0.8"),],
  aes(x = N, y = 0.85, label = Model, color = Model), position = position_dodge(3)))

# dgp = pwlinear
subFig_pw_oneSL <- subset(performanceStats_oneSL, dgp == "pwlinear")
subFig_pw_oneSL$Model <- gsub("algorithm_", "", subFig_pw_oneSL$Model)

fig_pw_oneSL <- plotR2_likeMLsim(subFig_pw_oneSL,
                                    plotMeasure = subFig_pw_oneSL$M, 
                                    title = "dgp = pw", yLabel = expression(R[test]^2),
                                    yMin = 0, yMax = 0.9)
(fig_pw_oneSL <- themeFunction(fig_pw_oneSL))
(fig_pw_oneSL <- fig_pw_oneSL + geom_text(
  data = subFig_pw_oneSL[which(subFig_pw_oneSL$N == "N1000" &
                                    subFig_pw_oneSL$R2 == "0.8"),],
  aes(x = N, y = 0.85, label = Model, color = Model), position = position_dodge(3)))

# dgp = nonlinear3
subFig_nl3_oneSL <- subset(performanceStats_oneSL, dgp == "nonlinear3")
subFig_nl3_oneSL$Model <- gsub("algorithm_", "", subFig_nl3_oneSL$Model)

fig_nl3_oneSL <- plotR2_likeMLsim(subFig_nl3_oneSL,
                                    plotMeasure = subFig_nl3_oneSL$M, 
                                    title = "dgp = nl3", yLabel = expression(R[test]^2),
                                    yMin = 0, yMax = 0.9)
(fig_nl3_oneSL <- themeFunction(fig_nl3_oneSL))
(fig_nl3_oneSL <- fig_nl3_oneSL + geom_text(
  data = subFig_nl3_oneSL[which(subFig_nl3_oneSL$N == "N1000" &
                                    subFig_nl3_oneSL$R2 == "0.8"),],
  aes(x = N, y = 0.85, label = Model, color = Model), position = position_dodge(3)))

ggplot2::ggsave(filename = paste0(plotFolder, "/performancePlot_inter_oneSL.png"),
                plot = fig_inter_oneSL,
                width = 18.30,
                height = 13.00,
                units = "in")

ggplot2::ggsave(filename = paste0(plotFolder, "/performancePlot_pw_oneSL.png"),
                plot = fig_pw_oneSL,
                width = 18.30,
                height = 13.00,
                units = "in")

ggplot2::ggsave(filename = paste0(plotFolder, "/performancePlot_nl3_oneSL.png"),
                plot = fig_nl3_oneSL,
                width = 18.30,
                height = 13.00,
                units = "in")


# similar plots but with only meta models and subset of 
# data (dgp = "inter")
# plotR2_likeMLsim_onlySL <- function(data, plotMeasure, title = "", yLabel = "",
#                              yMin = NULL, yMax = NULL){
#   ggplot(data,
#          aes(x = N, y = plotMeasure, 
#              group = interaction(R2, Model), linetype = R2, color = Model)) +
#     geom_line(linewidth = 1, alpha = 0.4) +
#     geom_point(aes(shape = Model), size = 3) +
#     scale_linetype_manual(name = expression(R[sim]^2), values = linetypeVec,
#                           guide = guide_legend(override.aes = list(size = 2, alpha = 0.8))) +
#     scale_color_manual(values = modelLineColors[5:8]) +
#     scale_shape_manual(values = modelPointShapes[5:8]) +
#     scale_y_continuous(limits = c(yMin, yMax), breaks=seq(
#       ifelse(yMin %% 0.2 == 0, yMin, yMin + 0.1), yMax, 0.2)) +
#     geom_hline(aes(yintercept = 0)) +
#     facet_grid2(rel ~ lin_inter,
#                 strip = strip_themed(
#                   background_x = list(element_rect(fill = alpha(lin_interPanelColors[1], 0.4)),
#                                       element_rect(fill = alpha(lin_interPanelColors[2], 0.4)),
#                                       element_rect(fill = alpha(lin_interPanelColors[3], 0.4))),
#                   background_y = list(element_rect(fill = alpha(relColValues[2], 0.4)),
#                                       element_rect(fill = alpha(relColValues[1], 0.4))))) +
#     geom_hline(yintercept = setParam$dgp$Rsquared[1], col = "black",
#                alpha = 0.4, linetype = linetypeVec[1]) +
#     geom_hline(yintercept = setParam$dgp$Rsquared[2], col = "black",
#                alpha = 0.4, linetype = linetypeVec[2]) +
#     geom_hline(yintercept = setParam$dgp$Rsquared[3], col = "black",
#                alpha = 0.4, linetype = linetypeVec[3]) +
#     ylab(yLabel) +
#     xlab("Sample Size (N)") +
#     ggtitle(title) +
#     guides(colour = "none", shape = "none")
# }
# 
# performanceStats_onlySL <- subset(performanceStats, Model %in% c("sl_algorithm_glm",
#                                                                  "sl_algorithm_nnls",
#                                                                  "sl_algorithm_ranger",
#                                                                  "sl_algorithm_glmnet"
#                                                                  ))
# 
# subFig_inter_onlySL <- subset(performanceStats_onlySL, dgp == "inter")
# subFig_inter_onlySL$Model <- gsub("algorithm_", "", subFig_inter_onlySL$Model)
# 
# fig_inter_onlySL <- plotR2_likeMLsim_onlySL(subFig_inter_onlySL,
#                                             plotMeasure = subFig_inter_onlySL$M, 
#                                             title = "dgp = inter", yLabel = expression(R[test]^2),
#                                             yMin = 0, yMax = 0.9)
# (fig_inter_onlySL <- themeFunction(fig_inter_onlySL))
# (fig_inter_onlySL <- fig_inter_onlySL + geom_text(
#   data = subFig_inter_onlySL[which(subFig_inter_onlySL$N == "N1000" &
#                                     subFig_inter_onlySL$R2 == "0.8"),],
#   aes(x = N, y = 0.85, label = Model, color = Model), position = position_dodge(3)))
# 
# ggplot2::ggsave(filename = paste0(plotFolder, "/performancePlot_inter_onlySL.png"),
#                 plot = fig_inter_onlySL,
#                 width = 18.30,
#                 height = 13.00,
#                 units = "in")

# plot R2 with R2sim Levels in rows for subset of data (rel = 1, dgp = "inter")

# new function to structure plot accordingly
# plotR2_rowR2 <- function(data, plotMeasure, title = "", yLabel = "",
#                              yMin = NULL, yMax = NULL){
#   ggplot(data,
#          aes(x = N, y = plotMeasure, 
#              group = Model, color = Model)) +
#     geom_line(linewidth = 1, alpha = 0.4) +
#     geom_point(aes(shape = Model), size = 3) +
#     scale_color_manual(values = modelLineColors) +
#     scale_shape_manual(values = modelPointShapes) +
#     scale_y_continuous(limits = c(yMin, yMax), breaks=seq(
#       ifelse(yMin %% 0.2 == 0, yMin, yMin + 0.1), yMax, 0.2)) +
#     geom_hline(aes(yintercept = 0)) +
#     facet_grid2(R2 ~ lin_inter,
#                 strip = strip_themed(
#                   background_x = list(element_rect(fill = alpha(lin_interPanelColors[1], 0.4)),
#                                       element_rect(fill = alpha(lin_interPanelColors[2], 0.4)),
#                                       element_rect(fill = alpha(lin_interPanelColors[3], 0.4))),
#                   background_y = list(element_rect(fill = alpha(R2ColValues[3], 0.4)),
#                                       element_rect(fill = alpha(R2ColValues[2], 0.4)),
#                                       element_rect(fill = alpha(R2ColValues[1], 0.4))))) +
#     geom_hline(yintercept = setParam$dgp$Rsquared[1], col = "black",
#                alpha = 0.4, linetype = linetypeVec[1]) +
#     geom_hline(yintercept = setParam$dgp$Rsquared[2], col = "black",
#                alpha = 0.4, linetype = linetypeVec[2]) +
#     geom_hline(yintercept = setParam$dgp$Rsquared[3], col = "black",
#                alpha = 0.4, linetype = linetypeVec[3]) +
#     ylab(yLabel) +
#     xlab("Sample Size (N)") +
#     ggtitle(title) +
#     guides(colour = "none", shape = "none")
# }
# 
# performanceStats_rowR2 <- subset(performanceStats, rel == "1")
# 
# subFig_inter_rowR2 <- subset(performanceStats_rowR2, dgp == "inter")
# subFig_inter_rowR2$Model <- gsub("algorithm_", "", subFig_inter_rowR2$Model)
# 
# fig_inter_rowR2 <- plotR2_rowR2(subFig_inter_rowR2,
#                                     plotMeasure = subFig_inter_rowR2$M, 
#                                     title = "dgp = inter", yLabel = expression(R[test]^2),
#                                     yMin = 0, yMax = 0.9)
# (fig_inter_rowR2 <- themeFunction(fig_inter_rowR2))
# (fig_inter_rowR2 <- fig_inter_rowR2 + geom_text(
#   data = subFig_inter_rowR2[which(subFig_inter_rowR2$N == "N1000"),],
#   aes(x = N, y = 0.85, label = Model, color = Model), position = position_dodge(4)))
# 
# ggplot2::ggsave(filename = paste0(plotFolder, "/performancePlot_inter_rowR2_rel1.png"),
#                 plot = fig_inter_rowR2,
#                 width = 18.30,
#                 height = 13.00,
#                 units = "in")
