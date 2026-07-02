##########################################################
# 08_predictions.R
#
# Purpose: Make predictions for the 2026-27 NHL season and analyze 8 players that could break out
#
# This script:
#     1. Applies the model to the updated season, producing predictions for the upcoming season
#     2. Analyzes five high probability breakout candidates from the precision model top 20
#     3. Analyzes three "out-there" breakout candidates from the 20-50 range of the coverage model
#
# Inputs:
#   xgboost_v4_precision
#   xgboost_v4_coverage
#   model_data
#   skaters25
#
# Outputs:
#   predict_data
#   a visualization for each player selected as five high probability breakout candidates
##########################################################

library(tidyverse)
library(here)

##########################################################
# Cleaning the skaters25 Data 
##########################################################

skaters25[770, "name"] <- "Aatu Raty"
skaters25 <- skaters25 |>
  filter(situation == "all", position != "D") |> 
  mutate(
    shotpctg = I_F_goals / I_F_shotsOnGoal,
    ppg = I_F_points / games_played,
    offensivezoneshift_pctg = I_F_oZoneShiftStarts / I_F_shifts
  )
# Adding the current data to the full dataset
model_data_combined <- bind_rows( # Previous model_data_combined used a placeholder dataset that had not been cleaned.
  model_data,
  skaters25
)

# Cleaning the model_data_combined dataset
model_data_combined <- model_data_combined |>
  arrange(name, season)
model_data_combined <- model_data_combined |>
  group_by(name) |>
  mutate(
    previous_best_points =
      lag(cummax(I_F_points), default = 0),
    experience =
      row_number() - 1,
    change_points =
      I_F_points - lag(I_F_points),
    change_icetime =
      icetime - lag(icetime),
    change_xg =
      I_F_xGoals - lag(I_F_xGoals),
    change_shots =
      I_F_shotsOnGoal - lag(I_F_shotsOnGoal),
    change_corsi =
      onIce_corsiPercentage - lag(onIce_corsiPercentage),
    points_growth =
      change_points / lag(I_F_points),
    xg_growth =
      change_xg / lag(I_F_xGoals),
    icetime_growth =
      change_icetime / lag(icetime),
    points_2yr =
      I_F_points - lag(I_F_points, 2),
    xg_2yr =
      I_F_xGoals - lag(I_F_xGoals, 2),
    icetime_2yr =
      icetime - lag(icetime, 2),
    ppg_change =
      ppg - lag(ppg),
    ppg_growth =
      ppg_change / lag(ppg)
  ) |>
  ungroup()

##########################################################
# Apply predictions to 26-27 season
##########################################################

# Create the prediction dataset
predict_data <- model_data_combined |>
  filter(season == 2025) |> 
  select(
    breakout,
    name,
    season,
    team,
    position,
    ppg,
    onIce_corsiPercentage,
    I_F_goals,
    I_F_xGoals,
    I_F_highDangerxGoals,
    shotpctg,
    offensivezoneshift_pctg,
    icetime,
    previous_best_points,
    experience,
    change_points,
    change_icetime,
    change_xg,
    change_shots,
    change_corsi,
    points_growth,
    xg_growth,
    icetime_growth,
    points_2yr,
    xg_2yr,
    icetime_2yr, 
    ppg_change,
    ppg_growth
  )

predict_matrix_coverage <- as.matrix(
  predict_data[, features_v3]
)
predict_data$prob_coverage <-
  predict(
    model_xgb_v4_coverage,
    predict_matrix_coverage
  )
predict_data$prob_coverage <- predict(
  model_xgb_v4_coverage,
  predict_matrix_coverage
)

predict_matrix_precision <- as.matrix(
  predict_data[, features_v2]
)
predict_data$prob_precision <-
  predict(
    model_xgb_v4_precision,
    predict_matrix_precision
  )
predict_data$prob_precision <- predict(
  model_xgb_v4_precision,
  predict_matrix_precision
)

# Re-arrange the prediction dataset
predict_data <- predict_data |> 
  select(
    name,
    team,
    position,
    prob_precision,
    prob_coverage,
    I_F_xGoals,
    everything()
  )

##########################################################
# Player Percentiles for 26-27, Precision Model
##########################################################

# Adding percentiles to the data
model_data_combined <- model_data_combined |>
  mutate(
    xg_pctile = 100*percent_rank(I_F_xGoals),
    hdxg_pctile = 100*percent_rank(I_F_highDangerxGoals),
    corsi_pctile = 100*percent_rank(onIce_corsiPercentage),
    points_pctile = 100*percent_rank(I_F_points),
    ozone_pctile = 100*percent_rank(offensivezoneshift_pctg),
    ppg_pctile = 100*percent_rank(ppg)
  )

# Mackie Samoskevich percentiles
mackie_pct <- model_data_combined |>
  filter(name == "Mackie Samoskevich", season > 2015) |>
  select(
    season,
    xg_pctile,
    hdxg_pctile,
    corsi_pctile,
    points_pctile
  ) |>
  pivot_longer(
    -season,
    names_to = "metric",
    values_to = "percentile"
  )

# Ben Kindel percentiles
kindel_pct <- model_data_combined |>
  filter(name == "Ben Kindel", season > 2015) |>
  select(
    season,
    xg_pctile,
    hdxg_pctile,
    corsi_pctile,
    points_pctile,
    ozone_pctile,
    ppg_pctile
  ) |>
  pivot_longer(
    -season,
    names_to = "metric",
    values_to = "percentile"
  )

# Emil Heineman percentiles
heineman_pct <- model_data_combined |>
  filter(name == "Emil Heineman", season > 2015) |>
  select(
    season,
    xg_pctile,
    hdxg_pctile,
    corsi_pctile,
    points_pctile
  ) |>
  pivot_longer(
    -season,
    names_to = "metric",
    values_to = "percentile"
  )

# Logan Stankoven percentiles 
stankoven_pct <- model_data_combined |>
  filter(name == "Logan Stankoven", season > 2015) |>
  select(
    season,
    xg_pctile,
    hdxg_pctile,
    corsi_pctile,
    points_pctile
  ) |>
  pivot_longer(
    -season,
    names_to = "metric",
    values_to = "percentile"
  )

# Matthew Wood percentiles
wood_pct <- model_data_combined |>
  filter(name == "Matthew Wood", season > 2015) |>
  select(
    season,
    xg_pctile,
    hdxg_pctile,
    corsi_pctile,
    points_pctile
  ) |>
  pivot_longer(
    -season,
    names_to = "metric",
    values_to = "percentile"
  )

##########################################################
# Player Visualizations for Precision Model Picks
##########################################################

# Preparing data for Emil Heineman's comparison visualization to Kirill Marchenko
comparison <- model_data_combined |> 
  filter((name == "Kirill Marchenko" & season == 2022) | (name == "Emil Heineman" & season == 2025)) |> 
  mutate(
    goalshare_scaled = (I_F_goals / I_F_points) * 100,
    corsi_scaled = onIce_corsiPercentage * 100,
    ppg_scaled = ppg * 100,
    shotpctgscaled = shotpctg * 100
  ) |> 
  select(
    name, ppg_scaled, I_F_goals, goalshare_scaled, I_F_xGoals, corsi_scaled, I_F_highDangerxGoals, shotpctgscaled
  ) |> 
  rename(
    "PPG (Scaled)" = ppg_scaled,
    "G" = I_F_goals,
    "G/P (Scaled)" = goalshare_scaled, 
    "Corsi%" = corsi_scaled,
    "xG" = I_F_xGoals,
    "HDxG" = I_F_highDangerxGoals,
    "Shooting %" = shotpctgscaled
  )
comparison_long <- comparison |>
  pivot_longer(
    cols = -name,
    names_to = "metric",
    values_to = "value"
  )

# Emil Heineman's comparison visualization
heineman_comp <- ggplot(comparison_long,
       aes(x = metric,
           y = value,
           fill = name)) +
  geom_col(
    position = position_dodge(width = .8),
    width = .7,
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "Emil Heineman" = "#F47D30",
      "Kirill Marchenko" = "#002654"
    )
  ) +
  labs(
    title = "Similar Profiles: Kirill Marchenko and Emil Heineman",
    subtitle = "Emil Heineman's 2025 season closely resembles Marchenko's rookie year.",
    x = "",
    y = "Total",
    fill = ""
  ) +
  theme_stata() +
  theme(
    plot.title = element_text(
      hjust = .5,
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(
      hjust = .5,
      size = 10.5
    ),
    axis.text.x = element_text(
      size = 10,
      face = "bold"
    ),
    axis.title = element_text(
      face = "bold"
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
ggsave(
  here("figures", "heinemancomp.png"),
  heineman_comp,
  width = 10,
  height = 5,
  dpi = 300
)

# Cleaning data for Stankoven's percentile plot
stankoven_pct <- model_data_combined |>
  filter(name == "Logan Stankoven", season > 2015) |>
  select(
    season,
    xg_pctile,
    hdxg_pctile,
    corsi_pctile,
    points_pctile
  ) |> 
  rename(
    "xG" = xg_pctile,
    "HDxG" = hdxg_pctile,
    "Corsi%"= corsi_pctile,
    "P" = points_pctile
  ) |>
  filter(season == 2025) |> 
  pivot_longer(
    -season,
    names_to = "metric",
    values_to = "percentile"
  )
# Stankoven's percentile plot
stankoven_pct_plot <- ggplot(stankoven_pct,
       aes(x = reorder(metric, percentile),
           y = percentile,
           fill = metric)) +
  geom_col(width = .7, color = "black") +
  geom_text(
    aes(label = paste0(round(percentile), "%")),
    hjust = -0.1,
    size = 4.5
  ) +
  scale_fill_manual(
    values = c(
      "P" = "firebrick",
      "xG" = "steelblue",
      "HDxG" = "darkgreen",
      "Corsi%" = "goldenrod"
    )
  ) +
  scale_y_continuous(
    limits = c(0, 105),
    breaks = seq(0,100,20)
  ) +
  coord_flip() +
  labs(
    title = "Logan Stankoven's Points Production is Lagging Behind His Incredible Metrics",
    subtitle = "League percentile rankings entering the 2026-27 season",
    x = "",
    y = "League Percentile",
    fill = ""
  ) +
  theme_stata() +
  theme(
    legend.position = "none",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      hjust = .5,
      face = "bold",
      size = 13
    ),
    plot.subtitle = element_text(
      hjust = .5,
      size = 10.5
    )
  )
ggsave(
  here("figures", "stankovenpct.png"),
  stankoven_pct_plot,
  width = 10,
  height = 5,
  dpi = 300
)

# Samoskevich's data somehow got messed up in the previous dataset, so re-calibrating his data.
samoskevich <- model_data |>
  filter(name == "Mackie Samoskevich") |>
  left_join(
    test |>
      select(
        name,
        season,
        pred_prob_xgb_v4_precision,
        pred_prob_xgb_v4_coverage
      ),
    by = c("name", "season")
  )
samoskevich_current <- skaters25 |>
  filter(name == "Mackie Samoskevich") |> 
  filter(situation == "all")
cols_keep <- names(samoskevich)
samoskevich_current <- samoskevich_current |>
  select(any_of(cols_keep))
samoskevich <- bind_rows(
  samoskevich,
  samoskevich_current
)

# Mackie Samoskevich's career trajectory plot
samoskevich_traj <- ggplot(samoskevich, aes(x = season)) +
  geom_line(
    aes(y = I_F_points,
        color = "Points"),
    linewidth = 1.5
  ) +
  geom_point(
    aes(y = I_F_points,
        color = "Points"),
    size = 3
  ) +
  geom_line(
    aes(y = I_F_xGoals,
        color = "Expected Goals"),
    linewidth = 1.5
  ) +
  geom_point(
    aes(y = I_F_xGoals,
        color = "Expected Goals"),
    size = 3
  ) +
  annotate(
    "label",
    x = 2024.875,
    y = 8,
    label = "Will He\n Breakout?",
    fill = "white",
    color = "gray30"
  ) +
  scale_color_manual(
    values = c(
      "Points" = "firebrick",
      "Expected Goals" = "steelblue"
    )) +
  scale_x_continuous(breaks = seq(2023,2025, by=1))+
  labs(
    title = "Mackie Samoskevich Has the Makings of a Star",
    subtitle = "Samoskevich has failed to break out fully, but a high xG could indicate his future.",
    x = "",
    y = "Season Total",
    color = ""
  ) +
  geom_text(
    aes(
      y = I_F_points,
      label = I_F_points,
      color = "Points"
    ),
    nudge_y = 4,
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      y = I_F_xGoals,
      label = round(I_F_xGoals,1),
      color = "Expected Goals"
    ),
    nudge_y = -4,
    show.legend = FALSE
  )  +
  theme_stata() +
  theme(
    panel.grid.major.x = element_blank(),
    legend.position = "bottom",
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      hjust = .5,
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(
      hjust = .5,
      size = 9.5
    )
  )
ggsave(
  here("figures", "samoskevichvis.png"),
  samoskevich_traj,
  width = 10,
  height = 5,
  dpi = 300
)

# Preparing Matthew Wood's data for his visualization
woodscatter_data <- model_data_combined |>
  select(name,
         season,
         I_F_points,
         I_F_xGoals,
         I_F_highDangerxGoals,
         onIce_corsiPercentage,
         icetime) |>
  pivot_longer(
    cols = c(I_F_xGoals,
             I_F_highDangerxGoals,
             onIce_corsiPercentage,
             icetime),
    names_to = "metric",
    values_to = "value"
  ) |>
  mutate(metric = recode(metric,
                         I_F_xGoals = "Expected Goals",
                         I_F_highDangerxGoals = "High Danger xGoals",
                         onIce_corsiPercentage = "Corsi%",
                         icetime = "Ice Time"
  ))

# Matthew Wood League Comparison charts
wood_comp <- ggplot(woodscatter_data,
       aes(x = value,
           y = I_F_points)) +
  geom_point(alpha = .15,
             color = "steelblue") +
  geom_point(
    data = filter(woodscatter_data,
                  name  == "Matthew Wood",
                  season == 2025),
    color = "gold1",
    size = 4
  ) +
  geom_text(
    data = filter(woodscatter_data,
                  name  == "Matthew Wood",
                  season == 2025),
    aes(label = "Matthew Wood\n2025-26"),
    color = "black"
  ) +
  labs(
    title = "Matthew Wood Compared to the League",
    subtitle = "Wood was above average in several metrics, but lacking ice time.",
    x = "",
    y = "Total Points"
  ) +
  facet_wrap(~metric,
             scales = "free_x") +
  theme_stata() +
  theme(
    plot.title = element_text(
      hjust = .5,
      face = "bold",
      size = 16
    ),
  )
ggsave(
  here("figures", "woodvis.png"),
  wood_comp,
  width = 10,
  height = 5,
  dpi = 300
)

# Making a dataframe for Kindel's visualization using manually inputted data from the dataset
kindel_features <- tibble(
  feature = c(
    "xG",
    "HDxG",
    "PPG",
    "Corsi%",
    "O-Zone Shift%"
  ),
  importance = c(
    .15,
    .05,
    .12,
    .06,
    .03
  ),
  percentile = c(
    88,
    94,
    63,
    69,
    56
  )
)
kindel_features <- kindel_features |>
  mutate(
    importance_scaled = importance/max(importance)*100
  )

# Ben Kinde's feature percentile plot
kindel_vis <- ggplot(kindel_features,
       aes(x = reorder(feature, importance_scaled))) +
  geom_col(
    aes(y = importance_scaled),
    fill = "steelblue",
    color = "black",
    alpha = .8,
    width = .7
  ) +
  geom_point(
    aes(y = percentile),
    color = "firebrick",
    size = 4
  ) +
  geom_text(
    aes(
      y = percentile + 5,
      label = paste0(percentile,"%")
    ),
    color = "firebrick"
  ) +
  coord_flip() +
  scale_y_continuous(
    limits = c(0,105),
    name = "Relative Importance / Percentile"
  ) +
  labs(
    title = "Why Ben Kindel Is Liked by the Model",
    subtitle = "Blue bars show the model's importance to the metric. Red dots show Kindel's performance in them.",
    x = ""
  ) +
  theme_bw() +
  theme(
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(
      face="bold",
      size=16
    ),
    plot.subtitle = element_text(
      size = 10
    )
  )
ggsave(
  here("figures", "kindelvis.png"),
  kindel_vis,
  width = 10,
  height = 5,
  dpi = 300
)

##########################################################
# Player Percentiles for 26-27, Out-there picks from the Coverage Model
##########################################################

# Dalibor Dvorsky percentiles
dvorsky_pct <- model_data_combined |>
  filter(name == "Dalibor Dvorsky", season > 2015) |>
  select(
    season,
    xg_pctile,
    hdxg_pctile,
    corsi_pctile,
    points_pctile
  ) |>
  pivot_longer(
    -season,
    names_to = "metric",
    values_to = "percentile"
  )

# Drew O'Connor percentiles
oconnor_pct <- model_data_combined |>
  filter(name == "Drew O'Connor", season > 2015) |>
  select(
    season,
    xg_pctile,
    hdxg_pctile,
    corsi_pctile,
    points_pctile
  ) |>
  pivot_longer(
    -season,
    names_to = "metric",
    values_to = "percentile"
  )

# Gabe Perreault percentiles
perreault_pct <- model_data_combined |>
  filter(name == "Gabe Perreault", season > 2015) |>
  select(
    season,
    xg_pctile,
    hdxg_pctile,
    corsi_pctile,
    points_pctile
  ) |>
  pivot_longer(
    -season,
    names_to = "metric",
    values_to = "percentile"
  )

##########################################################
# End of Script
##########################################################