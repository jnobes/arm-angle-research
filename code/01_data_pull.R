# ============================================================
# 01_data_pull.R — Data Acquisition and Preparation
# Project: MLB Arm Angle Research (2024-2025)
# Author:  [Your Name]
# Updated: 2026-04-20
#
# This script pulls all raw data from three sources,
# aggregates pitch-level data to pitcher x pitch-type level,
# and builds the master analysis table.
#
# Runtime: ~5 minutes (mostly downloading parquet files)
# ============================================================

library(tidyverse)
library(arrow)

# Output directories (relative to project root)
data_dir <- "data"
out_dir  <- "outputs"


# ============================================================
# 1. PULL TJSTATS PITCH-BY-PITCH DATA
#    Source: Hugging Face (TJStatsApps/mlb_data)
#    ~140 MB per season, ~712k pitches each
# ============================================================

cat("Pulling TJStats pitch data from Hugging Face...\n")
base_url <- "https://huggingface.co/datasets/TJStatsApps/mlb_data/resolve/main/"

pitches_2024 <- read_parquet(paste0(base_url, "data/mlb_pitch_data_2024.parquet"))
cat("  2024:", nrow(pitches_2024), "pitches,", ncol(pitches_2024), "columns\n")

pitches_2025 <- read_parquet(paste0(base_url, "data/mlb_pitch_data_2025.parquet"))
cat("  2025:", nrow(pitches_2025), "pitches,", ncol(pitches_2025), "columns\n")

# Validation: expect ~700k pitches per season
stopifnot(nrow(pitches_2024) > 600000)
stopifnot(nrow(pitches_2025) > 600000)


# ============================================================
# 2. PULL TJSTUFF+ AGGREGATED DATA
#    ~600 KB per season, one row per pitcher x pitch type
# ============================================================

cat("Pulling tjStuff+ data...\n")
stuff_2024 <- read_parquet(paste0(base_url, "stuff/stuff_2024.parquet")) %>%
  mutate(season = 2024)
stuff_2025 <- read_parquet(paste0(base_url, "stuff/stuff_2025.parquet")) %>%
  mutate(season = 2025)

cat("  2024:", nrow(stuff_2024), "pitcher-pitch combos\n")
cat("  2025:", nrow(stuff_2025), "pitcher-pitch combos\n")

all_stuff <- bind_rows(stuff_2024, stuff_2025)


# ============================================================
# 3. PULL BASEBALL SAVANT ARM ANGLE LEADERBOARD
#    ~800 pitchers per season
# ============================================================

cat("Pulling Savant arm angle data...\n")
arm_2024 <- read_csv(
  "https://baseballsavant.mlb.com/leaderboard/pitcher-arm-angles?season=2024&team=&pitchHand=&min=1&sort=descending&csv=true",
  show_col_types = FALSE
) %>%
  select(pitcher_id = pitcher, arm_angle = ball_angle) %>%
  mutate(season = 2024)

arm_2025 <- read_csv(
  "https://baseballsavant.mlb.com/leaderboard/pitcher-arm-angles?season=2025&team=&pitchHand=&min=1&sort=descending&csv=true",
  show_col_types = FALSE
) %>%
  select(pitcher_id = pitcher, arm_angle = ball_angle) %>%
  mutate(season = 2025)

cat("  2024:", nrow(arm_2024), "pitchers\n")
cat("  2025:", nrow(arm_2025), "pitchers\n")

all_arm <- bind_rows(arm_2024, arm_2025)


# ============================================================
# 4. AGGREGATE PITCH DATA TO PITCHER x PITCH-TYPE x SEASON
#    From ~712k rows per season to ~4k rows per season
# ============================================================

cat("Aggregating pitch data...\n")

aggregate_pitches <- function(df, season_year) {
  df %>%
    filter(!is.na(pitch_type), pitch_type != "") %>%
    group_by(pitcher_id, pitcher_name, pitcher_hand, pitch_type) %>%
    summarise(
      n_pitches    = n(),
      avg_velo     = mean(start_speed, na.rm = TRUE),
      avg_ivb      = mean(ivb, na.rm = TRUE),
      avg_hb       = mean(hb, na.rm = TRUE),
      avg_spin     = mean(spin_rate, na.rm = TRUE),
      avg_spin_dir = mean(spin_direction, na.rm = TRUE),
      avg_release_x = mean(x0, na.rm = TRUE),
      avg_release_z = mean(z0, na.rm = TRUE),
      avg_extension = mean(extension, na.rm = TRUE),
      n_swings         = sum(is_swing, na.rm = TRUE),
      n_whiffs         = sum(is_whiff, na.rm = TRUE),
      n_in_zone        = sum(zone <= 9 & !is.na(zone), na.rm = TRUE),
      n_in_zone_swings = sum(is_swing & zone <= 9 & !is.na(zone), na.rm = TRUE),
      n_in_zone_whiffs = sum(is_whiff & zone <= 9 & !is.na(zone), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      whiff_pct         = n_whiffs / n_swings,
      in_zone_whiff_pct = n_in_zone_whiffs / n_in_zone_swings,
      season            = season_year
    )
}

pitcher_pitch_2024 <- aggregate_pitches(pitches_2024, 2024)
pitcher_pitch_2025 <- aggregate_pitches(pitches_2025, 2025)

cat("  2024:", nrow(pitcher_pitch_2024), "pitcher-pitch combos\n")
cat("  2025:", nrow(pitcher_pitch_2025), "pitcher-pitch combos\n")

all_pitches <- bind_rows(pitcher_pitch_2024, pitcher_pitch_2025)


# ============================================================
# 5. BUILD MASTER TABLE
#    Join arm angle + tjStuff+ to aggregated pitch data
# ============================================================

cat("Building master table...\n")

master <- all_pitches %>%
  left_join(all_arm, by = c("pitcher_id", "season")) %>%
  left_join(
    all_stuff %>% select(pitcher_id, pitch_type, season, tj_stuff_plus),
    by = c("pitcher_id", "pitch_type", "season")
  )

# Validation: check join rates
arm_merge_rate <- mean(!is.na(master$arm_angle))
stuff_merge_rate <- mean(!is.na(master$tj_stuff_plus))
cat("  Total rows:", nrow(master), "\n")
cat("  Arm angle merge rate:", round(arm_merge_rate, 3), "\n")
cat("  tjStuff+ merge rate:", round(stuff_merge_rate, 3), "\n")
stopifnot(arm_merge_rate > 0.90)
stopifnot(stuff_merge_rate > 0.85)


# ============================================================
# 6. BUILD TEAM + WORKLOAD REFERENCE TABLES
# ============================================================

cat("Building reference tables...\n")

# Pitcher name lookup
pitcher_names <- bind_rows(
    pitches_2024 %>% distinct(pitcher_id, pitcher_name),
    pitches_2025 %>% distinct(pitcher_id, pitcher_name)
  ) %>%
  distinct(pitcher_id, pitcher_name)

# Primary team per pitcher-season (most pitches thrown for)
pitcher_teams <- bind_rows(
    pitches_2024 %>% mutate(season = 2024),
    pitches_2025 %>% mutate(season = 2025)
  ) %>%
  count(pitcher_id, season, pitcher_team) %>%
  group_by(pitcher_id, season) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(pitcher_id, season, primary_team = pitcher_team)

# Master with team info
master_with_team <- master %>%
  left_join(pitcher_teams, by = c("pitcher_id", "season"))

# Workload and SP/RP classification
pitcher_workload <- bind_rows(
    pitches_2024 %>% mutate(season = 2024),
    pitches_2025 %>% mutate(season = 2025)
  ) %>%
  group_by(pitcher_id, season) %>%
  summarise(
    total_pitches   = n(),
    games           = n_distinct(game_id),
    pitches_per_game = total_pitches / games,
    .groups = "drop"
  ) %>%
  mutate(
    role      = if_else(pitches_per_game >= 50, "SP", "RP"),
    approx_ip = total_pitches / 15.5
  )

workload_wide <- pitcher_workload %>%
  select(pitcher_id, season, total_pitches, role, approx_ip) %>%
  pivot_wider(
    names_from = season,
    values_from = c(total_pitches, role, approx_ip),
    names_sep = "_"
  )

cat("Reference tables built.\n")


# ============================================================
# 7. SAVE PREPARED DATA
# ============================================================

cat("Saving environment snapshot...\n")
save.image(file.path(data_dir, "arm_angle_backup.RData"))
cat("Done. All data prepared.\n")
