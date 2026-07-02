##########################################################
# 01_download_and_clean_data.R
#
# Purpose: Download, clean, and prepare the NHL skater datasets obtained from MoneyPuck.com
# that is to be used throughout the breakout prediction project.
#
# This script:
#     1. Loads historical and current skater NHL data
#     2. Filters the data to forwards only, as decided by the scope of the project
#     3. Defines breakout forwards
#     4. Creates summary datasets
#
# Outputs:
#   skaters
#   skaters25
#   breakoutskaters
#   breakouts
#   breakouts_sum
#   plot of breakouts per year
#
##########################################################

library(tidyverse)
library(here)


##########################################################
# Load data
##########################################################

# MoneyPuck skater data (2008-09 season through 24-25.)
skaters <- read_csv(here("data", "skaters_2008_to_2024.csv"))
# Filter data to just forwards in "all" situations (which restricts it to just one of each player)

skaters <- skaters |>
  filter(
    situation == "all",
    position != "D"
  ) |>
  arrange(name, season)

# Current season data
skaters25 <- read_csv(here("data", "25-26skaters.csv"))

##########################################################
# Create dataset for identifying historical breakout seasons
##########################################################

# Only players who have played at least two NHL seasons are eligible
breakoutskaters <- skaters |>
  group_by(name) |>
  filter(n() >= 2) |>
  ungroup()

##########################################################
# Engineer some relevant metrics
##########################################################

# Calculate points per game
breakoutskaters <- breakoutskaters |> 
  mutate(
    ppg = I_F_points / games_played
  )

# Calculate next season's points per game, points, and games played
breakoutskaters <- breakoutskaters |> 
  mutate(
    next_ppg = lead(ppg),
    next_points = lead(I_F_points), 
    next_gp = lead(games_played)
  )

# Calculate the change in ppg and percent change in ppg
breakoutskaters <- breakoutskaters |> 
  mutate(
    ppgchange = next_ppg - ppg,
    ppg_pct_change = ppgchange / ppg
  )

# Calculate the previous best points, otherwise known as the player's career high
breakoutskaters <- breakoutskaters |> 
  group_by(name) |>
  mutate(
    previous_best_points = cummax(lag(I_F_points, default = 0))
  )

##########################################################
# Adjust for shortened NHL seasons
##########################################################

# Three seasons in the timeframe of this project were shortened due to a lockout or COVID.
# They are assigned the correct amount of games

season_gp <- skaters |>
  group_by(season) |>
  summarize(total_games = 82) |>
  mutate(
    total_games = case_when(
      season == 2012 ~ 48, # 2012-13 lockout shortened season
      season == 2019 ~ 71,  # 2019–20 COVID season
      season == 2020 ~ 56,  # 2020–21 COVID season
      TRUE ~ total_games
    )
  )

# Calculate the Games Played Ratio
breakoutskaters <- breakoutskaters |> 
  left_join(season_gp, by = "season") |> 
  mutate(
    gpratio = games_played / total_games,
    nextgpratio = lead(gpratio)
  )

##########################################################
# Apply definition of breakout 
##########################################################

# Apply breakout definition
breakoutskaters <- breakoutskaters |> 
  mutate(
    breakout = 
      previous_best_points < 60 & # The player has never reached 60 points before
      I_F_points < 60 & # The player's current season does not hit 60 points
      ppg_pct_change >= .20 & # The percentage change of ppg raises by 20%
      ppgchange >= .15 & # The actual ppg stat raises by .15.
      next_points > previous_best_points & # The breakout season points is a career high
      next_points > I_F_points & # Improves from the previous season
      next_points >= 40 & # The breakout season points is at least 40
      gpratio >= .6 & # The player was available for at least 60% of their team's games
      nextgpratio >= .6 # For both seasons
  )

# Reorganize the dataset
breakoutskaters <- breakoutskaters |> 
  select(season, name, breakout, team, position, ppg, next_ppg, ppgchange, ppg_pct_change, I_F_points, next_points, previous_best_points, games_played, next_gp, everything())

##########################################################
# Create summary datasets
##########################################################

# Dataset of the breakouts
breakouts <- breakoutskaters |>
  filter(breakout) |>
  filter(season != 2024) |> 
  select(name, breakout, season, team, I_F_points, next_points, ppg, next_ppg, games_played, next_gp, gpratio, nextgpratio)

# Summary of the breakouts per season
breakouts_sum <- breakouts |>
  group_by(season) |>
  summarize(breakouts = sum(breakout, na.rm = TRUE))

##########################################################
# Create plot of breakouts per year
##########################################################

# Create and save plot
ggplot(breakouts_sum) + 
  geom_col(aes(x = season, y = breakouts), fill = "black") +
  scale_x_continuous(breaks = seq(2008,2023, by = 1)) +
  labs(
    title = "Amount of Breakouts By Year",
    subtitle = "There was some fluctuation in amount of breakouts per year",
    x = "Season",
    y = "Number of Breakouts",
    caption = "Source: MoneyPuck.com. See Data Documentation Section For More Details"
  ) + 
  theme_bw() +
  theme(
    plot.caption = element_text(hjust = 0),
    panel.grid = element_blank(),
    plot.title = element_text(face = "bold")
  )
ggsave(
  here("figures", "breakoutsbyyear.png"),
  breakouts_plot,
  width = 10,
  height = 5,
  dpi = 300
)

##########################################################
# End of script
##########################################################
