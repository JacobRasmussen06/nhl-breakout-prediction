##########################################################
# 02_feature_engineering.R
#
# Purpose: Create engineered features that will be used by the models and assemble the final modeling dataset
#
# This script:
#     1. Creates several engineered features such as shooting percentage that are useful in constructing the predictive model
#     2. Constructs a modeling dataset that will be used as the baseline dataset for every model
#
# Inputs:
#   breakoutskaters
#
#
# Outputs:
#   model_data
#
##########################################################

library(tidyverse)
library(here)

##########################################################
# Create efficiency metrics
##########################################################

# Create metrics that evaluate a player's effeciency and deployment, rather than their production
breakoutskaters <- breakoutskaters |>
  mutate(
    shotpctg = I_F_goals / I_F_shotsOnGoal, # Calculate shooting percentage
    offensivezoneshift_pctg = I_F_oZoneShiftStarts / I_F_shifts # Calculate offensive zone shift percentage
  )

##########################################################
# Create year-to-year change metrics
##########################################################

# Create metrics that show a player's year-to-year increase or decrease in useful statistics
breakoutskaters <- breakoutskaters |> 
  group_by(name) |> 
  mutate(
    change_points = I_F_goals - lag(I_F_goals),
    change_icetime = icetime - lag(icetime),
    change_xg = I_F_xGoals - lag(I_F_xGoals),
    change_shots = I_F_shotsOnGoal - lag(I_F_shotsOnGoal),
    change_corsi = onIce_corsiPercentage - lag(onIce_corsiPercentage)
  )

##########################################################
# Calculate a player's experience level
##########################################################

# A player's age is unavailable, so NHL experience (# of seasons played) is used as a subsitute
breakoutskaters <- breakoutskaters |> 
  group_by(name) |> 
  mutate(
    experience = row_number()
  ) |> 
  ungroup()

##########################################################
# Create growth and two-year change metrics 
##########################################################

# Calculate the metrics and add them to the dataset
breakoutskaters <- breakoutskaters |> 
  group_by(name) |> 
  mutate(
    points_growth =
      (I_F_points - lag(I_F_points)) /
      pmax(lag(I_F_points),1),
    xg_growth =
      (I_F_xGoals - lag(I_F_xGoals)) /
      pmax(lag(I_F_xGoals),1),
    icetime_growth =
      (icetime - lag(icetime)) /
      pmax(lag(icetime),1),
    points_2yr =
      I_F_points - lag(I_F_points,2),
    xg_2yr =
      I_F_xGoals - lag(I_F_xGoals,2),
    icetime_2yr =
      icetime - lag(icetime,2),
    ppg_change =
      ppg - lag(ppg),
    ppg_growth =
      (ppg - lag(ppg)) /
      pmax(lag(ppg),0.01)
  ) |> 
  ungroup()

##########################################################
# Construct the model dataset
##########################################################

# Create the dataset 
model_data <- breakoutskaters |> 
  select(
    # Retain only useful variables for the model development
    breakout, season, name, team, position, next_points, ppg, I_F_points, I_F_goals, onIce_corsiPercentage, onIce_fenwickPercentage, I_F_xGoals, I_F_xPlayContinuedInZone, I_F_flurryAdjustedxGoals, I_F_highDangerShots, I_F_highDangerGoals, I_F_highDangerxGoals, I_F_lowDangerxGoals, I_F_lowDangerShots, I_F_lowDangerGoals, I_F_mediumDangerShots, I_F_mediumDangerxGoals, I_F_mediumDangerShots, shotpctg, offensivezoneshift_pctg, OnIce_F_xGoals, icetime, iceTimeRank, previous_best_points, experience, change_points, change_icetime, change_xg, change_shots, change_corsi, points_growth, xg_growth, icetime_growth, points_2yr, xg_2yr, icetime_2yr, ppg_change, ppg_growth
  ) |> 
  filter(!is.na(breakout))

##########################################################
# End of script
##########################################################
