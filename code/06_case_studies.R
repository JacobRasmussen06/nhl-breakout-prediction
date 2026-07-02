##########################################################
# 06_case_studies_part_one.R
#
# Purpose: Analyze three historical case studies to show patterns that may help identify breakouts and understand how the models work. 
#
# This script:
#     1. Analyzes the case study of player Wyatt Johnston
#     2. Analyses the case study of player Pavel Zacha
#     3. Analyzes the case study of player Zach Benson
#
# Inputs:
#   xgboost_v4_precision
#   xgboost_v4_coverage
#   test
#
# Outputs:
#   
#
##########################################################

```{r}
# Case Study: Wyatt Johnston
johnston <- test |> 
  filter(name == "Wyatt Johnston")
johnston_current <- skaters25 |>
  filter(name == "Wyatt Johnston") |> 
  filter(situation == "all")
cols_keep <- names(johnston)
johnston_current <- johnston_current |>
  select(any_of(cols_keep))
johnston <- bind_rows(
  johnston,
  johnston_current
)
ggplot(johnston, aes(x = season)) +
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
    xintercept = 2023,
    linetype = "dashed",
    color = "gray50"
  ) +
  annotate(
    "label",
    x = 2023,
    y = 45,
    label = "Predicted Breakout\nSeason"
  ) +
  scale_color_manual(
    values = c(
      "Points" = "firebrick",
      "Expected Goals" = "steelblue"
    )) +
  scale_x_continuous(breaks = seq(2022,2025, by=1))+
  labs(
    title = "Wyatt Johnston's Rise",
    subtitle = "Both models claimed Wyatt Johnston following his rookie year was the biggest breakout candidate",
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
      size = 11
    )
  )
```

```{r}
scatter_data <- test |>
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

ggplot(scatter_data,
       aes(x = value,
           y = I_F_points)) +
  geom_point(alpha = .15,
             color = "gray50") +
  geom_point(
    data = filter(scatter_data,
                  name  == "Wyatt Johnston",
                  season == 2022),
    color = "red",
    size = 4
  ) +
  geom_text(
    data = filter(scatter_data,
                  name  == "Wyatt Johnston",
                  season == 2022),
    aes(label = "Wyatt Johnston\n2022-23"),
    color = "red"
  ) +
  labs(
    title = "Wyatt Johnston Compared to the League",
    subtitle = "As a rookie, Johnston was well above average on key metrics",
    x = "",
    y = ""
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
```

```{r}
ggplot(test, aes(I_F_xGoals)) +
  geom_density(fill = "steelblue") +
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
           pull(I_F_xGoals)) - .3,
    y = .02, 
    label = "Wyatt Johnston (2022-23)",
    color = "purple"
  ) +
  labs(
    x = "Expected Goals", 
    y = "Density",
    title = "Wyatt Johnston vs the NHL: Expected Goals",
    subtitle = "Johnston had above average xGoals in his rookie season"
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

ggplot(test, aes(onIce_corsiPercentage)) +
  geom_density(fill = "gray80") +
  geom_vline(
    xintercept = johnston |>
      filter(season == 2022) |>
      pull(onIce_corsiPercentage),
    color = "green2",
    linewidth = 1.5
  ) +
  annotate(
    "text",
    x = (johnston |>
           filter(season == 2022) |>
           pull(onIce_corsiPercentage)) - .03,
    y = 2, 
    label = "Wyatt Johnston (2022-23)",
    color = "purple"
  ) +
  labs(
    x = "Corsi Percentage", 
    y = "Density",
    title = "Wyatt Johnston vs the NHL: Corsi%",
    subtitle = "Johnston had above average Corsi% in his rookie season"
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



ggplot(test, aes(I_F_highDangerxGoals)) +
  geom_density(fill = "steelblue") +
  geom_vline(
    xintercept = johnston |>
      filter(season == 2022) |>
      pull(I_F_highDangerxGoals),
    color = "green2",
    linewidth = 1.5
  ) +
  annotate(
    "text",
    x = (johnston |>
           filter(season == 2022) |>
           pull(I_F_highDangerxGoals)) - .3,
    y = .05, 
    label = "Wyatt Johnston (2022-23)",
    color = "black"
  ) +
  labs(
    x = "High Danger xGoals", 
    y = "Density",
    title = "Wyatt Johnston vs the NHL: High Danger Expected Goals",
    subtitle = "Johnston had above average High Danger xGoals in his rookie season"
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
```

- Next player: Pavel Zacha

```{r}
# come back
zacha <- model_data |>
  filter(name == "Pavel Zacha") |>
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
zacha_current <- skaters25 |>
  filter(name == "Pavel Zacha") |> 
  filter(situation == "all")
cols_keep <- names(zacha)
zacha_current <- zacha_current |>
  select(any_of(cols_keep))
zacha <- bind_rows(
  zacha,
  zacha_current
)
```

```{r}
ggplot(zacha, aes(x = season)) +
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
  geom_point(
    data = filter(zacha,
                  season >= 2022),
    aes(y = I_F_points),
    size = 4,
    shape = 21,
    fill = "gold",
    color = "black"
  ) +
  annotate(
    "label",
    x = 2022.15,
    y = 32,
    label = "Model Predicted\nBreakout Season",
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
    title = "Pavel Zacha's Rise",
    subtitle = "Despite middling production, the model trusted Zacha's underlying metrics and correctly predicted his breakout.",
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
    nudge_y = 4.5,
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
  ) +
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
```

```{r}
ggplot(scatter_data,
       aes(x = value,
           y = I_F_points)) +
  geom_point(alpha = .15,
             color = "gray50") +
  geom_point(
    data = filter(scatter_data,
                  name  == "Pavel Zacha",
                  season == 2021),
    color = "red",
    size = 4
  ) +
  geom_text(
    data = filter(scatter_data,
                  name  == "Pavel Zacha",
                  season == 2021),
    aes(label = "Pavel Zacha\n2021-22"),
    color = "red"
  ) +
  labs(
    title = "Pavel Zacha Compared to the League",
    subtitle = "Zacha's underlying metrics showed a player with more upside than he currently was producing.",
    x = "",
    y = "Points"
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
      size = 11
    )
  )
```

```{r}
placeholder <- skaters25 |>
  filter(situation == "all")
cols_keep <- names(model_data)
placeholder <- placeholder |>
  select(any_of(cols_keep))
model_data_combined <- bind_rows(
  model_data,
  placeholder
)
model_data_combined <- model_data_combined |>
  mutate(
    xg_pctile = 100*percent_rank(I_F_xGoals),
    hdxg_pctile = 100*percent_rank(I_F_highDangerxGoals),
    corsi_pctile = 100*percent_rank(onIce_corsiPercentage),
    points_pctile = 100*percent_rank(I_F_points),
    ozone_pctile = 100*percent_rank(offensivezoneshift_pctg),
    ppg_pctile = 100*percent_rank(ppg)
  )
zacha_pct <- model_data_combined |>
  filter(name == "Pavel Zacha", season > 2015) |>
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
zacha_pct$metric <- recode(
  zacha_pct$metric,
  xg_pctile = "Expected Goals",
  hdxg_pctile = "High-Danger xG",
  corsi_pctile = "Corsi %",
  points_pctile = "Points"
)
ggplot(zacha_pct,
       aes(x = season,
           y = percentile,
           color = metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 3) +
  scale_y_continuous(
    limits = c(0,100)
  ) +
  scale_x_continuous(breaks = seq(2016,2025, by = 1)) +
  scale_color_manual(
    values = c(
      "Points" = "firebrick",
      "Expected Goals" = "steelblue",
      "High-Danger xG" = "darkgreen",
      "Corsi %" = "goldenrod"
    )
  ) +
  labs(
    title = "Pavel Zacha's Underlying Metrics Overtook his Production Before His Breakout",
    subtitle = "Leaguewide percentile rankings highlight the step Zacha took before his breakout season",
    x = "",
    y = "Percentile",
    color = ""
  ) +
  theme_stata() +
  theme(
    plot.title = element_text(face = 'bold', size = 13)
  )
```





- Next Player: Zach Benson

```{r}
benson <- test |> 
  filter(name == "Zach Benson")
benson_current <- skaters25 |>
  filter(name == "Zach Benson") |> 
  filter(situation == "all")
cols_keep <- names(benson)
benson_current <- benson_current |>
  select(any_of(cols_keep))
benson <- bind_rows(
  benson,
  benson_current
)
```

```{r}
ggplot(benson, aes(x = season)) +
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
    xintercept = 2025,
    linetype = "dashed",
    color = "gray50"
  ) +
  annotate(
    "label",
    x = 2023.08,
    y = 25,
    label = "First Model\nPrediction"
  ) +
  annotate(
    "label",
    x = 2024.08,
    y = 25,
    label = "Disappointing Year!\nModel Predicts Again"
  ) +
  annotate(
    "label",
    x = 2024.92,
    y = 25,
    label = "Benson\nBreaks Out!"
  ) +
  scale_color_manual(
    values = c(
      "Points" = "firebrick",
      "Expected Goals" = "steelblue"
    )) +
  scale_x_continuous(breaks = seq(2023,2025, by=1))+
  labs(
    title = "Zach Benson's Career Trajectory",
    subtitle = "The models stayed course after a disappointing second season, and it worked out.",
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
    nudge_y = 3,
    show.legend = FALSE
  ) +
  geom_text(
    aes(
      y = I_F_xGoals,
      label = round(I_F_xGoals,1),
      color = "Expected Goals"
    ),
    nudge_y = -3,
    show.legend = FALSE
  ) +
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
      size = 11
    )
  )
```

```{r}
model_data_combined <- model_data_combined |>
  mutate(
    xg_pctile = 100*percent_rank(I_F_xGoals),
    hdxg_pctile = 100*percent_rank(I_F_highDangerxGoals),
    corsi_pctile = 100*percent_rank(onIce_corsiPercentage),
    points_pctile = 100*percent_rank(I_F_points)
  )
benson_pct <- model_data_combined |>
  filter(name == "Zach Benson", season > 2015) |>
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
benson_pct$metric <- recode(
  benson_pct$metric,
  xg_pctile = "Expected Goals",
  hdxg_pctile = "High-Danger xG",
  corsi_pctile = "Corsi %",
  points_pctile = "Points"
)
ggplot(benson_pct,
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
    title = "Zach Benson's Production Eventually Caught up to His Metrics",
    subtitle = "Leaguewide percentile rankings show Benson's promise, before the production came.",
    x = "",
    y = "Percentile",
    color = ""
  ) +
  theme_stata() +
  theme(
    plot.title = element_text(face = 'bold')
  )
```

```{r}
benson_gap <- model_data_combined |>
  filter(name == "Zach Benson") |>
  mutate(
    gap = xg_pctile - points_pctile
  )
ggplot(benson_gap,
       aes(x = season,
           y = gap)) +
  geom_line(
    linewidth = 1.5,
    color = "steelblue"
  ) +
  geom_point(
    size = 4,
    color = "steelblue"
  ) +
  geom_text(
    aes(label = round(gap,1)),
    nudge_y = 1.5,
    size = 4
  ) +
  scale_x_continuous(
    breaks = unique(benson_gap$season)
  ) +
  labs(
    title = "Benson's Production Finally Caught Up to the Process",
    subtitle = "Difference between Expected Goals percentile and Points percentile",
    x = "",
    y = "Percentile Gap"
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
      size = 10
    ),
    panel.grid.major.x = element_blank(),
    panel.grid.minor = element_blank()
  )
```


