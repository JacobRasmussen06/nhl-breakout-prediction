##########################################################
# 07_case_studies_part_two.R
#
# Purpose: Analyze three historical case studies to show patterns that may help identify breakouts and understand how the models work. 
#
# This script:
#     1. Analyzes the case study of player Connor Zary
#     2. Analyses the case study of player Will Cuylle
#     3. Analyzes the case study of player Eetu Luostarinen
#
# Inputs:
#   xgboost_v4_precision
#   xgboost_v4_coverage
#   model_data
#   skaters25
#   test
#
# Outputs:
#   Visualizations on Connor Zary's career and why the model liked him, and why it failed
#   Visualizations on Will Cuylle's career and why the model liked him, and why he succeeded
#   Visualizations on Eetu Luostarinen's career and why the model liked him, and why his case is interesting
#
##########################################################

library(tidyverse)
library(here)


##########################################################
# Case Study Four: Connor Zary
##########################################################

# Preparing Zary's data
zary <- test |> 
  filter(name == "Connor Zary")
zary_current <- skaters25 |>
  filter(name == "Connor Zary") |> 
  filter(situation == "all")
cols_keep <- names(zary)
zary_current <- zary_current |>
  select(any_of(cols_keep))
zary <- bind_rows(
  zary,
  zary_current
)

# Connor Zary's Percentiles
model_data_combined <- model_data_combined |>
  mutate(
    xg_pctile = 100*percent_rank(I_F_xGoals),
    hdxg_pctile = 100*percent_rank(I_F_highDangerxGoals),
    corsi_pctile = 100*percent_rank(onIce_corsiPercentage),
    points_pctile = 100*percent_rank(I_F_points),
    ozone_pctile = 100*percent_rank(offensivezoneshift_pctg),
    ppg_pctile = 100*percent_rank(ppg)
  )

zary_pct <- model_data_combined |>
  filter(name == "Connor Zary", season > 2015) |>
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
zary_pct$metric <- recode(
  zary_pct$metric,
  xg_pctile = "Expected Goals",
  hdxg_pctile = "High-Danger xG",
  corsi_pctile = "Corsi %",
  points_pctile = "Points"
)

# Connor Zary percentile evolution plot
zarypct_plot <- ggplot(zary_pct,
       aes(x = season,
           y = percentile,
           color = metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(
    limits = c(0,100)
  ) +
  scale_x_continuous(breaks = seq(2023,2025, by = 1)) +
  scale_color_manual(
    values = c(
      "Points" = "firebrick",
      "Expected Goals" = "steelblue",
      "High-Danger xG" = "darkgreen",
      "Corsi %" = "goldenrod"
    )
  ) +
  labs(
    title = "Connor Zary Still Can't Find the Step Forward",
    subtitle = "Leaguewide percentile rankings show Zary's promise, despite the production lacking.",
    x = "",
    y = "Percentile",
    color = ""
  ) +
  theme_stata() +
  theme(
    plot.title = element_text(face = 'bold')
  )
ggsave(
  here("figures", "zarypct.png"),
  zarypct_plot,
  width = 10,
  height = 5,
  dpi = 300
)

# Preparing a data-frame that will help with the next visualization
highlight_players <- tibble(
  name = c(
    "Pavel Zacha",
    "Wyatt Johnston",
    "Connor Zary"
  ),
  season = c(
    2021,
    2022,
    2024
  ),
  player_type = c(
    "Pavel Zacha\n2021-22",
    "Wyatt Johnston\n2022-23",
    "Connor Zary\n2024-25"
  )
)
highlight_points <- scatter_data |>
  inner_join(
    highlight_players,
    by = c("name", "season")
  )
zary_comp_league <- ggplot(scatter_data,
       aes(x = value,
           y = I_F_points)) +
  geom_point(alpha = .15,
             color = "gray50") +
  geom_point(
    data = highlight_points,
    aes(color = player_type),
    size = 4
  ) +
  geom_smooth(
    method="lm",
    se=FALSE,
    color="black"
  ) +
  labs(
    title = "Why the Model Believed in Connor Zary",
    subtitle = "Zary had above average metrics, but couldn't find the production.",
    x = "",
    y = "Points",
    color = ""
  ) +
  scale_color_manual(
    values = c(
      "Pavel Zacha\n2021-22" = "firebrick",
      "Wyatt Johnston\n2022-23" = "forestgreen",
      "Connor Zary\n2024-25" = "royalblue"
    ) 
  ) +
  scale_y_continuous(
    breaks = seq(0,150, by = 50)
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
    plot.subtitle = element_text(
      hjust = .5,
      size = 11),
    axis.text.y = element_text(size = 6),
    legend.position = "top"
  )
ggsave(
  here("figures", "zaryvleague.png"),
  zary_comp_league,
  width = 10,
  height = 5,
  dpi = 300
)

# A series of plots showing the same thing as the previous plot, but one stat at a time. Only the HDxG was included in the report. 
zary_xg <- ggplot(test, aes(I_F_xGoals)) +
  geom_density(fill = "gray80") +
  geom_vline(
    xintercept = zary |>
      filter(season == 2024) |>
      pull(I_F_xGoals),
    color = "red",
    linewidth = 1.5
  ) +
  geom_vline(
    xintercept = johnston |>
      filter(season == 2022) |>
      pull(I_F_xGoals),
    color = "green2",
    linewidth = 1.5
  ) +
  annotate(
    "text",
    x = (johnston |>
           filter(season == 2022) |>
           pull(I_F_xGoals)) + 5.5,
    y = .03, 
    label = "Wyatt Johnston\n2022-23",
    color = "green2"
  ) +
  annotate(
    "text",
    x = (zary |>
           filter(season == 2024) |>
           pull(I_F_xGoals)) - 4.5,
    y = .01, 
    label = "Connor Zary\n2024-25",
    color = "red"
  ) +
  geom_vline(
    xintercept = zacha |>
      filter(season == 2021) |>
      pull(I_F_xGoals),
    color = "black",
    linewidth = 1.5
  ) +
  annotate(
    "text",
    x = (zacha |>
           filter(season == 2022) |>
           pull(I_F_xGoals)) - 2.7,
    y = .02, 
    label = "Pavel Zacha\n2021-22",
    color = "black"
  ) +
  labs(
    x = "Expected Goals", 
    y = "Density",
    title = "Connor Zary vs the NHL: Expected Goals",
    subtitle = ""
  ) +
  theme_fivethirtyeight() + 
  theme(
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
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )
ggsave(
  here("figures", "zaryxg.png"),
  zary_xg,
  width = 10,
  height = 5,
  dpi = 300
)

zary_corsi <- ggplot(test, aes(onIce_corsiPercentage)) +
  geom_density(fill = "gray80") +
  geom_vline(
    xintercept = zary |>
      filter(season == 2024) |>
      pull(onIce_corsiPercentage),
    color = "red",
    linewidth = 1.5
  ) +
  geom_vline(
    xintercept = johnston |>
      filter(season == 2022) |>
      pull(onIce_corsiPercentage),
    color = "green4",
    linewidth = 1.5
  ) +
  annotate(
    "text",
    x = (johnston |>
           filter(season == 2022) |>
           pull(onIce_corsiPercentage)) - .08,
    y = 1, 
    label = "Wyatt Johnston\n2022-23",
    color = "green4"
  ) +
  annotate(
    "text",
    x = (zary |>
           filter(season == 2024) |>
           pull(onIce_corsiPercentage)) + .08,
    y = 2, 
    label = "Connor Zary\n2024-25\n and Pavel Zacha\n2021-22",
    color = "red"
  ) +
  labs(
    x = "Corsi Percentage", 
    y = "Density",
    title = "Connor Zary vs the NHL: Corsi%",
    subtitle = ""
  ) +
  theme_fivethirtyeight() + 
  theme(
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
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )
ggsave(
  here("figures", "zarycorsi.png"),
  zary_corsi,
  width = 10,
  height = 5,
  dpi = 300
)

zary_hdxg <- ggplot(test, aes(I_F_highDangerxGoals)) +
  geom_density(fill = "steelblue") +
  geom_vline(
    xintercept = zary |>
      filter(season == 2024) |>
      pull(I_F_highDangerxGoals),
    color = "red",
    linewidth = 1.5
  ) +
  geom_vline(
    xintercept = johnston |>
      filter(season == 2022) |>
      pull(I_F_highDangerxGoals),
    color = "green4",
    linewidth = 1.5
  ) +
  annotate(
    "text",
    x = (johnston |>
           filter(season == 2022) |>
           pull(I_F_highDangerxGoals)) + 3,
    y = .06, 
    label = "Wyatt Johnston\n2022-23",
    color = "green4"
  ) +
  geom_vline(
    xintercept = zacha |>
      filter(season == 2021) |>
      pull(I_F_highDangerxGoals),
    color = "black",
    linewidth = 1.5
  ) +
  annotate(
    "text",
    x = (zacha |>
           filter(season == 2021) |>
           pull(I_F_highDangerxGoals)) - 2.5,
    y = .02, 
    label = "Pavel Zacha\n2021-22",
    color = "black"
  ) +
  annotate(
    "text",
    x = (zary |>
           filter(season == 2024) |>
           pull(I_F_highDangerxGoals)) - 2.5,
    y = .05, 
    label = "Connor Zary\n2024-25",
    color = "red"
  ) +
  labs(
    x = "High Danger xGoals", 
    y = "Density",
    title = "Connor Zary vs the NHL: High Danger Expected Goals",
    subtitle = ""
  ) +
  theme_fivethirtyeight() + 
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    axis.title.y = element_text(
      size = 12,
      face = "bold"),
    axis.title.x = element_text(
      size = 12,
      face = "bold"
    ),
    plot.title = element_text(
      size = 16,
      face = "bold"
    ),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )
ggsave(
  here("figures", "zaryhdxg.png"),
  zary_hdxg,
  width = 10,
  height = 5,
  dpi = 300
)

# Preparing data for the direct comparison graph between Johnston and Zary
compare_df <- model_data_combined |>
  filter(
    (name == "Wyatt Johnston" & season == 2022) |
      (name == "Connor Zary" & season == 2024)
  ) |>
  select(
    name,
    xg_pctile,
    hdxg_pctile,
    corsi_pctile
  ) |>
  pivot_longer(
    -name,
    names_to = "metric",
    values_to = "percentile"
  )
compare_df$metric <- recode(
  compare_df$metric,
  xg_pctile = "Expected Goals",
  hdxg_pctile = "High-Danger xG",
  corsi_pctile = "Corsi %"
)
# Comparison plot
zary_johnston_comp <- ggplot(compare_df,
       aes(
         x = metric,
         y = percentile,
         fill = name
       )) +
  geom_col(
    position = "dodge",
    color = "black"
  ) +
  scale_fill_manual(
    values = c(
      "Wyatt Johnston" = "forestgreen",
      "Connor Zary" = "royalblue"
    )
  ) +
  labs(
    title = "Zary and Johnston Had Similar Underlying Profiles",
    subtitle = "Great metrics doesn't guarantee production, though.",
    x = "",
    y = "League Percentile",
    fill = ""
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 16
    ),
    plot.subtitle = element_text(
      size = 11
    ),
    axis.title.y = element_text(
      size = 12,
      face = "bold"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )
ggsave(
  here("figures", "zaryjohnstoncomp.png"),
  zary_johnston_comp,
  width = 10,
  height = 5,
  dpi = 300
)

##########################################################
# Case Study Five: Will Cuylle
##########################################################

# Preparing Cuylle's data
cuylle <- test |> 
  filter(name == "Will Cuylle")
cuylle_current <- skaters25 |>
  filter(name == "Will Cuylle") |> 
  filter(situation == "all")
cols_keep <- names(cuylle)
cuylle_current <- cuylle_current |>
  select(any_of(cols_keep))
cuylle <- bind_rows(
  cuylle,
  cuylle_current
)

# Will Cuylle's career trajectory
cuylle_traj <- ggplot(cuylle, aes(x = season)) +
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
  geom_vline(
    xintercept = 2024,
    linetype = "dashed",
    color = "gray50"
  ) +
  annotate(
    "label",
    x = 2024.15,
    y = 32,
    label = "Unanticipated\nBreakout Season",
    fill = "white",
    color = "gray30"
  ) +
  scale_color_manual(
    values = c(
      "Points" = "firebrick",
      "Expected Goals" = "steelblue"
    )) +
  scale_x_continuous(breaks = seq(2015,2025, by=1))+
  labs(
    title = "Model Missed Will Cuylle's Breakout",
    subtitle = "Will Cuylle Became a Productive Player, Which the Model Couldn't Predict",
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
  here("figures", "cuylletraj.png"),
  cuylle_traj,
  width = 10,
  height = 5,
  dpi = 300
)

# Will Cuylle's percentiles
cuylle_pct <- model_data_combined |>
  filter(name == "Will Cuylle") |>
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
cuylle_pct$metric <- recode(
  cuylle_pct$metric,
  xg_pctile = "Expected Goals",
  hdxg_pctile = "High-Danger xG",
  corsi_pctile = "Corsi %",
  points_pctile = "Points"
)

# Will Cuylle's percentile evolution plot
cuylle_pct <- ggplot(cuylle_pct,
       aes(
         season,
         percentile,
         color = metric
       )) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_vline(
    xintercept = 2024,
    linetype = "dashed",
    color = "gray50"
  ) +
  scale_y_continuous(
    limits = c(0,100)
  ) +
  scale_color_manual(
    values = c(
      "Points" = "firebrick",
      "Expected Goals" = "steelblue",
      "High-Danger xG" = "darkgreen",
      "Corsi %" = "goldenrod"
    )
  )  +
  scale_x_continuous(
    breaks = seq(2022,2025,by=1)
  ) +
  labs(
    title = "Cuylle Showed Meaningful Improvement in Metrics",
    subtitle = "While his Corsi% failed to improve, his other metrics rose him to a breakout season.",
    x = "",
    y = "League Percentile",
    color = "",
    linetype = ""
  ) +
  theme_stata() +
  theme(
    plot.title = element_text(face = 'bold')
  )
ggsave(
  here("figures", "cuyllepct.png"),
  cuylle_pct,
  width = 10,
  height = 5,
  dpi = 300
)

# Fixing a datapoint that I found to be incorrect
cuylle[4,"shotpctg"] <- .127

# Will Cuylle's shot percentage to ice time plot, shows Cuylle's increase in luck and ice time, explaining his unforeseen breakout
cuylle_shot <- ggplot(cuylle, aes(x = iceTimeRank, y = shotpctg)) +
  geom_point(size = 4) +
  geom_path(
    arrow = arrow(length = unit(.15,"inches"))) +
  labs(
    title = "Increased Puck Luck and Opportunity Fueled Cuylle's Breakout",
    subtitle = "A great increase in Shooting% and Ice Time Gave Cuylle the opportunity to shine.",
    x = "Ice Time Ranking Among NHL Players",
    y = "Shooting %"
  ) +
  geom_text_repel(
    aes(label = season),
    size = 3.5,
    box.padding = 0.3,
    point.padding = 0.3,
    max.overlaps = Inf
  ) +
  theme_fivethirtyeight() +
  theme(
    plot.title = element_text(
      face = "bold",
      size = 15
    ),
    plot.subtitle = element_text(
      size = 11
    ),
    axis.title.y = element_text(
      size = 12,
      face = "bold"),
    axis.title.x = element_text(
      size = 12,
      face = "bold"),
    panel.background = element_rect(fill = "white", color = NA),
    plot.background = element_rect(fill = "white", color = NA),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
  )
ggsave(
  here("figures", "cuyllepuckluck.png"),
  cuylle_shot,
  width = 10,
  height = 5,
  dpi = 300
)

##########################################################
# Case Study Six: Eetu Luostarinen
##########################################################

# Preparing Eetu Luostarinen's data
luostarinen <- model_data |>
  filter(name == "Eetu Luostarinen") |>
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
luostarinen_current <- skaters25 |>
  filter(name == "Eetu Luostarinen") |> 
  filter(situation == "all")
cols_keep <- names(luostarinen)
luostarinen_current <- luostarinen_current |>
  select(any_of(cols_keep))
luostarinen <- bind_rows(
  luostarinen,
  luostarinen_current
)

# Luostarinen's career trajectory plot
luostarinen_traj <- ggplot(luostarinen, aes(x = season)) +
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
  geom_vline(
    xintercept = 2022,
    linetype = "dashed",
    color = "gray50"
  ) +
  annotate(
    "label",
    x = 2022.15,
    y = 25,
    label = "Model Missed\nBreakout Season",
    fill = "white",
    color = "gray30"
  ) +
  scale_color_manual(
    values = c(
      "Points" = "firebrick",
      "Expected Goals" = "steelblue"
    )) +
  scale_x_continuous(breaks = seq(2015,2025, by=1))+
  labs(
    title = "Eetu Luostarinen: One-Hit Wonder the Model Missed",
    subtitle = "Despite a high upside 2022 that the model failed to predict, Luostarinen has not repeated production",
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
  here("figures", "luotraj.png"),
  luostarinen_traj,
  width = 10,
  height = 5,
  dpi = 300
)

# Eetu Luostarinen's percentiles
luo_pct <- model_data_combined |>
  filter(name == "Eetu Luostarinen", season > 2015) |>
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
luo_pct$metric <- recode(
  luo_pct$metric,
  xg_pctile = "Expected Goals",
  hdxg_pctile = "High-Danger xG",
  corsi_pctile = "Corsi %",
  points_pctile = "Points"
)

# Luostarinen's percentile plot
luostarinen_pct_plot <- ggplot(luo_pct,
       aes(
         season,
         percentile,
         color = metric,
         linetype = metric
       )) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  geom_vline(
    xintercept = 2022,
    linetype = "dashed",
    color = "gray50"
  ) +
  scale_y_continuous(
    limits = c(0,100)
  ) +
  scale_color_manual(
    values = c(
      "Points" = "firebrick",
      "Expected Goals" = "steelblue",
      "High-Danger xG" = "darkgreen",
      "Corsi %" = "goldenrod"
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Points" = "solid",
      "Expected Goals" = "solid",
      "High-Danger xG" = "solid",
      "Corsi %" = "solid"
    )
  ) +
  scale_x_continuous(
    breaks = seq(2019,2025,by=1)
  ) +
  labs(
    title = "Luostarinen's Production Faded, But His Metrics Did Not",
    subtitle = "Luostarinen's metrics show promise, but his production has been inconsistent.",
    x = "",
    y = "League Percentile",
    color = "",
    linetype = ""
  ) +
  theme_stata()
ggsave(
  here("figures", "luopct.png"),
  luostarinen_pct_plot,
  width = 10,
  height = 5,
  dpi = 300
)

luostarinen[7,"shotpctg"] <- .103 # manually adding the missing data

# Eetu Luostarinen's shooting percentage plot - explaining his one-hit wonder status as puck luck in one season that he hasn't replicated
luostarinen_shot <- ggplot(luostarinen,
       aes(season, shotpctg)) +
  geom_line(
    linewidth = 1.5,
    color = "firebrick"
  ) +
  geom_point(
    size = 5,
    color = "firebrick"
  ) +
  geom_text(
    aes(
      label = scales::percent(shotpctg, accuracy = 0.1)
    ),
    nudge_y = .01,
    size = 3.5
  ) +
  scale_x_continuous(
    breaks = seq(2019,2025, by = 1)
  ) +
  scale_y_continuous(
    labels = scales::percent_format(accuracy = 1)
  ) +
  geom_point(
    aes(color = season == 2022),
    size = 5
  ) +
  scale_color_manual(
    values = c("FALSE" = "firebrick",
               "TRUE" = "goldenrod"),
    guide = "none"
  ) +
  labs(
    title = "Did Shooting Luck Drive Luostarinen's Breakout?",
    subtitle = "Luostarinen's shooting percentage peaked during his breakout 2022 season.",
    x = "",
    y = "Shooting Percentage"
  ) +
  theme_stata() + 
  theme(
    plot.title = element_text(
      face = 'bold'
    )
  )
ggsave(
  here("figures", "luoshot.png"),
  luostarinen_shot,
  width = 10,
  height = 5,
  dpi = 300
)

##########################################################
# End of Script
##########################################################