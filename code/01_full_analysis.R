# ============================================================
# Arm Angle Research — Full Analysis
# Project: MLB pitcher arm angle changes 2024-2025
#          and effects on movement, stuff, and outcomes
# Author:  [Your Name]
# Updated: 2026-04-15
# ============================================================

library(tidyverse)
library(arrow)


# ------------------------------------------------------------
# 1. DATA PULLS
# ------------------------------------------------------------

# TJStats pitch-by-pitch data (Hugging Face)
base <- "https://huggingface.co/datasets/TJStatsApps/mlb_data/resolve/main/"
pitches_2024 <- read_parquet(paste0(base, "data/mlb_pitch_data_2024.parquet"))
pitches_2025 <- read_parquet(paste0(base, "data/mlb_pitch_data_2025.parquet"))

# tjStuff+ aggregated by pitcher × pitch type
stuff_2024 <- read_parquet(paste0(base, "stuff/stuff_2024.parquet"))
stuff_2025 <- read_parquet(paste0(base, "stuff/stuff_2025.parquet"))

# Baseball Savant arm angle leaderboard
arm_2024 <- read_csv(
  "https://baseballsavant.mlb.com/leaderboard/pitcher-arm-angles?season=2024&team=&pitchHand=&min=1&sort=descending&csv=true"
) %>%
  select(pitcher_id = pitcher, arm_angle = ball_angle) %>%
  mutate(season = 2024)

arm_2025 <- read_csv(
  "https://baseballsavant.mlb.com/leaderboard/pitcher-arm-angles?season=2025&team=&pitchHand=&min=1&sort=descending&csv=true"
) %>%
  select(pitcher_id = pitcher, arm_angle = ball_angle) %>%
  mutate(season = 2025)


# ------------------------------------------------------------
# 2. AGGREGATE TO PITCHER × PITCH-TYPE × SEASON
# ------------------------------------------------------------

aggregate_pitches <- function(df, season_year) {
  df %>%
    filter(!is.na(pitch_type), pitch_type != "") %>%
    group_by(pitcher_id, pitcher_name, pitcher_hand, pitch_type) %>%
    summarise(
      n_pitches = n(),
      avg_velo = mean(start_speed, na.rm = TRUE),
      avg_ivb = mean(ivb, na.rm = TRUE),
      avg_hb = mean(hb, na.rm = TRUE),
      avg_spin = mean(spin_rate, na.rm = TRUE),
      avg_spin_dir = mean(spin_direction, na.rm = TRUE),
      avg_release_x = mean(x0, na.rm = TRUE),
      avg_release_z = mean(z0, na.rm = TRUE),
      avg_extension = mean(extension, na.rm = TRUE),
      n_swings = sum(is_swing, na.rm = TRUE),
      n_whiffs = sum(is_whiff, na.rm = TRUE),
      n_in_zone = sum(zone <= 9 & !is.na(zone), na.rm = TRUE),
      n_in_zone_swings = sum(is_swing & zone <= 9 & !is.na(zone), na.rm = TRUE),
      n_in_zone_whiffs = sum(is_whiff & zone <= 9 & !is.na(zone), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    mutate(
      whiff_pct = n_whiffs / n_swings,
      in_zone_whiff_pct = n_in_zone_whiffs / n_in_zone_swings,
      season = season_year
    )
}

pitcher_pitch_2024 <- aggregate_pitches(pitches_2024, 2024)
pitcher_pitch_2025 <- aggregate_pitches(pitches_2025, 2025)


# ------------------------------------------------------------
# 3. JOIN ARM ANGLE + STUFF+ TO BUILD MASTER TABLE
# ------------------------------------------------------------

all_pitches <- bind_rows(pitcher_pitch_2024, pitcher_pitch_2025)
all_arm     <- bind_rows(arm_2024, arm_2025)
all_stuff   <- bind_rows(
  stuff_2024 %>% mutate(season = 2024),
  stuff_2025 %>% mutate(season = 2025)
)

master <- all_pitches %>%
  left_join(all_arm, by = c("pitcher_id", "season")) %>%
  left_join(
    all_stuff %>% select(pitcher_id, pitch_type, season, tj_stuff_plus),
    by = c("pitcher_id", "pitch_type", "season")
  )


# ------------------------------------------------------------
# 4. POST 1: LEAGUE MAP — ARM ANGLE → PITCH MOVEMENT
# ------------------------------------------------------------

analysis <- master %>%
  filter(
    n_pitches >= 50,
    !is.na(arm_angle), !is.na(avg_ivb), !is.na(avg_hb),
    pitch_type %in% c("FF", "SI", "FC", "SL", "ST", "CU", "CH")
  )

# Per-pitch-type regressions
regression_results <- analysis %>%
  group_by(season, pitch_type) %>%
  summarise(
    n = n(),
    ivb_intercept = coef(lm(avg_ivb ~ arm_angle))[1],
    ivb_slope = coef(lm(avg_ivb ~ arm_angle))[2],
    ivb_r2 = summary(lm(avg_ivb ~ arm_angle))$r.squared,
    hb_intercept = coef(lm(abs(avg_hb) ~ arm_angle))[1],
    hb_slope = coef(lm(abs(avg_hb) ~ arm_angle))[2],
    hb_r2 = summary(lm(abs(avg_hb) ~ arm_angle))$r.squared,
    .groups = "drop"
  )

# Charts
p_ivb <- analysis %>%
  ggplot(aes(x = arm_angle, y = avg_ivb)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "lm", color = "red", se = TRUE) +
  facet_wrap(~pitch_type, scales = "free_y") +
  labs(
    title = "Arm Angle vs. Induced Vertical Break by Pitch Type",
    subtitle = "2024-2025 combined. Each dot = one pitcher's season average.",
    x = "Arm Angle (degrees, 0 = sidearm, 90 = over the top)",
    y = "Induced Vertical Break (inches)"
  ) +
  theme_minimal()

ggsave("outputs/post1_arm_angle_ivb.png", p_ivb, width = 10, height = 7, dpi = 300)

p_hb <- analysis %>%
  ggplot(aes(x = arm_angle, y = abs(avg_hb))) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(method = "lm", color = "blue", se = TRUE) +
  facet_wrap(~pitch_type, scales = "free_y") +
  labs(
    title = "Arm Angle vs. Horizontal Break by Pitch Type",
    subtitle = "2024-2025 combined. Absolute HB to align L/R pitchers.",
    x = "Arm Angle (degrees)",
    y = "Horizontal Break (inches, absolute value)"
  ) +
  theme_minimal()

ggsave("outputs/post1_arm_angle_hb.png", p_hb, width = 10, height = 7, dpi = 300)


# ------------------------------------------------------------
# 5. POST 2: ARM ANGLE CHANGERS (year-over-year, same team, qualified)
# ------------------------------------------------------------

# Identify each pitcher's primary team per season
pitcher_teams <- bind_rows(
  pitches_2024 %>% mutate(season = 2024),
  pitches_2025 %>% mutate(season = 2025)
) %>%
  count(pitcher_id, season, pitcher_team) %>%
  group_by(pitcher_id, season) %>%
  slice_max(n, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(pitcher_id, season, primary_team = pitcher_team)

master_with_team <- master %>%
  left_join(pitcher_teams, by = c("pitcher_id", "season"))

# Same-team pitchers in both years
pitcher_team_season <- pitcher_teams %>%
  pivot_wider(names_from = season, values_from = primary_team, names_prefix = "team_")

same_team_pitchers <- pitcher_team_season %>%
  filter(team_2024 == team_2025)

arm_changes_same_team <- same_team_pitchers %>%
  left_join(arm_2024 %>% select(pitcher_id, angle_2024 = arm_angle), by = "pitcher_id") %>%
  left_join(arm_2025 %>% select(pitcher_id, angle_2025 = arm_angle), by = "pitcher_id") %>%
  filter(!is.na(angle_2024), !is.na(angle_2025)) %>%
  mutate(angle_change = angle_2025 - angle_2024)

# Workload and SP/RP classification
pitcher_workload <- bind_rows(
  pitches_2024 %>% mutate(season = 2024),
  pitches_2025 %>% mutate(season = 2025)
) %>%
  group_by(pitcher_id, season) %>%
  summarise(
    total_pitches = n(),
    games = n_distinct(game_id),
    pitches_per_game = total_pitches / games,
    .groups = "drop"
  ) %>%
  mutate(
    role = if_else(pitches_per_game >= 50, "SP", "RP"),
    approx_ip = total_pitches / 15.5
  )

workload_wide <- pitcher_workload %>%
  select(pitcher_id, season, total_pitches, role, approx_ip) %>%
  pivot_wider(
    names_from = season,
    values_from = c(total_pitches, role, approx_ip),
    names_sep = "_"
  )

# Pitcher names lookup
pitcher_names <- bind_rows(
  pitches_2024 %>% distinct(pitcher_id, pitcher_name),
  pitches_2025 %>% distinct(pitcher_id, pitcher_name)
) %>%
  distinct(pitcher_id, pitcher_name)

# Qualified changers: same role both years, SP ≥ 100 IP, RP ≥ 40 IP
qualified_changers <- arm_changes_same_team %>%
  left_join(pitcher_names, by = "pitcher_id") %>%
  left_join(workload_wide, by = "pitcher_id") %>%
  filter(
    role_2024 == role_2025,
    !is.na(role_2024),
    (role_2024 == "SP" & approx_ip_2024 >= 100 & approx_ip_2025 >= 100) |
      (role_2024 == "RP" & approx_ip_2024 >= 40  & approx_ip_2025 >= 40)
  )

# Team-level coaching effect
team_coaching_effect <- arm_changes_same_team %>%
  group_by(team_2024) %>%
  summarise(
    n_same_pitchers = n(),
    mean_individual_change = round(mean(angle_change), 2),
    median_individual_change = round(median(angle_change), 2),
    n_dropped_3plus = sum(angle_change <= -3),
    n_raised_3plus = sum(angle_change >= 3),
    .groups = "drop"
  ) %>%
  arrange(mean_individual_change) %>%
  rename(team = team_2024)


# ------------------------------------------------------------
# 6. SAVE ENVIRONMENT SNAPSHOT
# ------------------------------------------------------------

save.image("data/arm_angle_backup.RData")
