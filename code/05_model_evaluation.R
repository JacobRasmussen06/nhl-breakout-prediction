##########################################################
# 05_model_evaluation.R
#
# Purpose: Evaluate the models using visualizations that show various features of the model and the testing dataset.
#
# This script:
#     1. Creates plots showing the evaluation metrics for the models created
#     2. Creates plots showing showing the difference between the precision and coverage models.
#     3. Creates plots showing the probability distribution of the probabilities given by the models. 
#
# Inputs:
#   xgboost_v4_precision
#   xgboost_v4_coverage
#   
#
# Outputs:
#   plot of model AUC
#   plot of model P@K for k = 10, 30, and 50
#   trade-off plot between prediction and coverage
#   plot of the important variables in the coverage model
#   plots of the density function, cdf, and ranked probabilities of the breakout probabilities
#   plots showing the relationship between actual breakout and probabilities
#
##########################################################

library(tidyverse)
library(here)


##########################################################
# Plot of Model AUC and Top K Precision
##########################################################

# Instead of coding in the AUC and Top K for each model, it was easier to just paste them into their own dataset.
model_results <- data.frame(
  model = c("GLM", "XGBoost v1", "Random Forest v1", "XGBoost v2", "Random Forest v2", "XGBoost v3", "XGBoost v4 Precision", "XGBoost v4 Coverage"),
  auc = c(0.8628, 0.8979, 0.8821, 0.8984, 0.8866, 0.8943, 0.9037, 0.9122),
  top10 = c(2,4,5,4,2,3,6,3),
  top30 = c(7,8,8,8,6,7,10,11),
  top50 = c(10,11,11,12,12,13,12,13)
)

# Create AUC Graph
auc_plot <- ggplot(model_results, aes(x = reorder(model, auc), y = auc)) + geom_col(fill = "black") + coord_flip() +
  labs(
    title = "AUC of Predictive Breakout Models",
    x = "Model",
    y = "AUC",
    caption = "Source: MoneyPuck.com. See Data Documentation Section For More Details"
  ) + theme_bw() +
  theme(
    plot.caption = element_text(hjust = 0),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  )
ggsave(
  here("figures", "aucbymodel.png"),
  auc_plot,
  width = 10,
  height = 5,
  dpi = 300
)

# P@K Graphs
precision_10 <- ggplot(model_results, aes(x = reorder(model, top10), y = top10)) + geom_col(fill = "black") +
  coord_flip() +
  labs(
    title = "Amount of Breakouts in Model's Probability Top 10",
    y = "Breakouts in Top 10",
    x = "Model",
    caption = "Source: MoneyPuck.com. See Data Documentation Section For More Details"
  ) + theme_fivethirtyeight() +
  theme(
    plot.caption = element_text(hjust = 0),
    plot.title = element_text(hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
  breaks = seq(0, max(model_results$top30), by = 2)
  )
ggsave(
  here("figures", "top10precision.png"),
  precision_10,
  width = 10,
  height = 5,
  dpi = 300
)

precision_30 <- ggplot(model_results, aes(x = reorder(model, top30), y = top30)) + geom_col(fill = "black") +
  coord_flip() +
  labs(
    title = "Amount of Breakouts in Model's Probability Top 30",
    y = "Breakouts in Top 30",
    x = "Model",
    caption = "Source: MoneyPuck.com. See Data Documentation Section For More Details"
  ) + theme_fivethirtyeight() +
  theme(
    plot.caption = element_text(hjust = 0),
    plot.title = element_text(hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
  breaks = seq(0, max(model_results$top30), by = 2)
  )
ggsave(
  here("figures", "top30precision.png"),
  precision_30,
  width = 10,
  height = 5,
  dpi = 300
)

precision_50 <- ggplot(model_results, aes(x = reorder(model, top50), y = top50)) + geom_col(fill = "black") +
  coord_flip() +
  labs(
    title = "Amount of Breakouts in Model's Probability Top 50",
    y = "Breakouts in Top 50",
    x = "Model",
    caption = "Source: MoneyPuck.com. See Data Documentation Section For More Details"
  ) + theme_fivethirtyeight() +
   theme(
    plot.caption = element_text(hjust = 0),
    plot.title = element_text(hjust = 1),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  ) +
  scale_y_continuous(
  breaks = seq(0, max(model_results$top30), by = 2)
  )
ggsave(
  here("figures", "top50precision.png"),
  precision_50,
  width = 10,
  height = 5,
  dpi = 300
)

##########################################################
# Plot of the Tradeoff Between Precision and Coverage
##########################################################

tradeoff <- data.frame(
  model = c("XGBoost Precision", "XGBoost Coverage"),
  top10 = c(6, 3),
  top50 = c(12, 13)
)

tradeoff_long <- tradeoff |>
  pivot_longer(
    cols = c(top10, top50),
    names_to = "metric",
    values_to = "correct_breakouts"
  )

tradeoff_plot <- ggplot(tradeoff_long,
       aes(x = model,
           y = correct_breakouts,
           fill = metric)) +
  geom_col(position = "dodge", color = "black") +   
  geom_text(
    aes(label = correct_breakouts),
    position = position_dodge(width = .9),
    vjust = 1.5,
    color = "black",
    fontface = "bold",
    size = 5
  ) +
  scale_fill_manual(
    values = c(
      "top10" = "#1F77B4",
"top50" = "#FF7F0E"
),
labels = c(
  "top10" = "Top 10",
  "top50" = "Top 50"
)
) +
  labs(
    title = "Precision vs Coverage Comparison",
    x = NULL,
    y = "Breakouts in Top-K",
    fill = NULL,
    caption = "Source: MoneyPuck.com. See Data Documentation Section For More Details"
  ) + theme_fivethirtyeight() +
  theme(
    plot.caption = element_text(hjust = 0),
    axis.text.x = element_text(size = 12),
    axis.text.y = element_text(size = 12),
    axis.title.y = element_text(
      size = 12,
      face = "bold"),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )
ggsave(
  here("figures", "precisionvcoverage.png"),
  tradeoff_plot,
  width = 10,
  height = 5,
  dpi = 300
)

##########################################################
# Plot of Importance
##########################################################

imp <- xgb.importance(model = model_xgb_v4_coverage)
imp_df <- head(imp, 10)
feature_map <- c(
  "I_F_xGoals" = "Expected Goals",
  "previous_best_points" = "Previous Best Points",
  "ppg" = "Points Per Game",
  "icetime" = "Ice Time",
  "onIce_corsiPercentage" = "On-Ice Corsi %",
  "I_F_highDangerxGoals" = "High-Danger xG",
  "points_2yr" = "2 Year Points Δ ",
  "change_xg" = "Δ Expected Goals",
  "offensivezoneshift_pctg" = "Offensive Zone Shift %",
  "ppg_change" = "Δ Points Per Game"
)
imp_df$Feature_clean <- feature_map[imp_df$Feature]
imp_df$Feature_clean <- ifelse(
  is.na(imp_df$Feature_clean),
  imp_df$Feature,
  imp_df$Feature_clean
)
imp_plot <- ggplot(imp_df, aes(x = reorder(Feature_clean, Gain), y = Gain)) +
  geom_col(color = "black", fill = "#FF7F0E") +
  coord_flip() +
  theme_bw() +
  labs(
    title = "Top 10 Feature Importance",
    subtitle = "XGBoost v4 Coverage Model",
    x = "Feature",
    y = "Importance (Gain)",
    caption = "Source: MoneyPuck.com. See Data Documentation Section For More Details"
  ) +
  theme(
    plot.caption = element_text(hjust = 0),
    panel.grid = element_blank(),
    plot.title = element_text(
      face = "bold"
    )
  )
ggsave(
  here("figures", "importance.png"),
  imp_plot,
  width = 10,
  height = 5,
  dpi = 300
)

##########################################################
# Plots of the Probability Distribution of the Breakout Probabilities
##########################################################

# Density plot of the breakout probabilities
density_plot <- ggplot(test, aes(x = pred_prob_xgb_v4_precision)) +
  geom_density(
    fill = "#1F77B4",
    alpha = .8,
    color = "black"
  ) + geom_rug(alpha = .3) + 
  labs(
    title = "Distribution of Predicted Breakout Probabilities",
    subtitle = "Most players are assigned a low probability of breaking out",
    x = "Predicted Probability of Breakout",
    y = "Density",
    caption = "Source: MoneyPuck.com. See Data Documentation Section For More Details"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.caption = element_text(hjust= 0),
    axis.text.y = element_blank(),
    axis.title.y = element_text(
      size = 12,
      face = "bold"),
    axis.title.x = element_text(
      size = 12,
      face = "bold"
    ),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
  )
ggsave(
  here("figures", "dist.png"),
  density_plot,
  width = 10,
  height = 5,
  dpi = 300
)

# Cumulative distribution function of breakout probs
cdf_plot <- ggplot(test, aes(x = pred_prob_xgb_v4_precision)) +
  stat_ecdf(
    linewidth = 1.2,
    color = "black"
  ) +
  theme_stata() +
  scale_y_continuous(
    labels = scales::percent
  ) +
  scale_x_continuous(
    limits = c(0, .4),
    breaks = seq(0, 1, 0.1)
  ) +
  labs(
    title = "Cumulative Distribution of Breakout Probabilities",
    subtitle = "XGBoost v4 Precision model",
    x = "Predicted Breakout Probability",
    y = "Proportion of Players",
    caption = "Source: MoneyPuck.com. See Data Documentation Section For More Details"
  ) +
  theme(
    plot.caption = element_text(hjust = 0),
    plot.title = element_text( face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  )
ggsave(
  here("figures", "cdf.png"),
  cdf_plot,
  width = 10,
  height = 5,
  dpi = 300
)

# Ranked Breakout Probabilities
plot_df <- test |>
  arrange(desc(pred_prob_xgb_v4_precision)) |>
  mutate(rank = row_number())

ranked_plot <- ggplot(plot_df, aes(rank, pred_prob_xgb_v4_precision)) +
  theme_stata() +
  geom_line(linewidth = 1.2) +
  labs(
    title = "Ranked Breakout Probabilities",
    subtitle = "Only a small number of players receive substantial breakout probabilities",
    x = "Player Rank by Predicted Breakout Probability",
    y = "Predicted Probability of Breakout",
    caption = "Source: MoneyPuck.com. See Data Documentation Section For More Details"
  ) +
  geom_vline(
    xintercept = 30, # Shows the top 30 by probability of breakout compared to the rest of the league
    linetype = "dashed",
    colour = "red"
  ) +
  annotate(
    "text",
    x = 30,
    y = 0.32,
    label = "Top 30",
    hjust = -0.1,
    size = 4
  ) + 
  theme(
    plot.caption = element_text(hjust = 0),
    plot.title = element_text( face = "bold"),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(size = 11),
    panel.grid.minor = element_blank()
  )
ggsave(
  here("figures", "ranked.png"),
  ranked_plot,
  width = 10,
  height = 5,
  dpi = 300
)

##########################################################
# Plots Showing the Relationship Between Prediction and Actual Result
##########################################################

# A plot showing the predicted probability vs. did they actually break out
jitter_plot <- ggplot(test, aes(x = pred_prob_xgb_v4_precision, y = breakout)) +
  geom_jitter(
    height = 0.03,
    width = 0.1,
    alpha = 0.20,
    size = 4,
    color = "steelblue"
  ) +
  scale_x_continuous(limits = c(0, 0.4)) +
  scale_y_continuous(breaks = c(0, 1), labels = c("No", "Yes")) +
  labs(
    title = "Predicted Breakout Probability vs Actual Outcome",
    x = "Predicted Probability",
    y = "Breakout?",
    caption = "Source: MoneyPuck.com. See Data Documentation Section For More Details",
    subtitle = "XGBoost v4 Precision Model"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),
    plot.subtitle = element_text(hjust = 0.5),
    plot.caption = element_text(hjust = 0),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.title.y = element_text(
      size = 12,
      face = "bold"),
    axis.title.x = element_text(
      size = 12,
      face = "bold"
    ),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
ggsave(
  here("figures", "predvsactual.png"),
  jitter_plot,
  width = 10,
  height = 5,
  dpi = 300
)

breakoutprob_plot <- test |>
  mutate(bin = cut(pred_prob_xgb_v4_precision,
                   breaks = seq(0, 0.4, by = 0.02))) |>
  group_by(bin) |>
  summarize(
    avg_prob = mean(pred_prob_xgb_v4_precision),
    breakout_rate = mean(breakout, na.rm = TRUE),
    n = n()
  ) |>
  ggplot(aes(x = avg_prob, y = breakout_rate)) +
  geom_line(linewidth = 1.2) +
  scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
  geom_point(size = 2) +
  labs(
    title = "Observed Breakout Rate by Predicted Probability",
    x = "Predicted Probability",
    y = "Actual Breakout Rate",
    caption = "Source: MoneyPuck.com. See Data Documentation Section For More Details"
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.caption =  element_text(hjust = 0),
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_text(
      size = 12,
      face = "bold"),
    axis.title.x = element_text(
      size = 12,
      face = "bold"
    ),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA)
  )
ggsave(
  here("figures", "breakoutbyprob.png"),
  breakoutprob_plot,
  width = 10,
  height = 5,
  dpi = 300
)

##########################################################
# End of Script
##########################################################