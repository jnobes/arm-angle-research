# ============================================================
# 02_analysis.R — Arm Angle Analysis
# Project: MLB Arm Angle Research (2024-2025)
# Author:  [Your Name]
# Updated: 2026-04-20
#
# Prerequisites: Run 01_data_pull.R first, or load the
# cached environment: load("data/arm_angle_backup.RData")
#
# This script contains four analyses:
#   A. League Map: arm angle -> pitch movement regressions
#   B. Arm Angle Changers: year-over-year individual + org changes
#   C. Movement Prediction: predicted vs actual movement deltas
#   D. Outcome Connection: did arm angle changes improve results?
# ============================================================

library(tidyverse)

# If starting fresh, load cached data
if (!exists("master_with_team")) {
  cat("Loading cached environment...\n")
  load("data/arm_angle_backup.RData")
}


# ============================================================
# A. LEAGUE MAP: ARM ANGLE -> PITCH MOVEMENT
# ============================================================

cat("=== A. LEAGUE MAP ===\n")

# Filter to 7 main pitch types, 50+ pitch threshold
analysis <- master %>%
  filter(
    n_pitches >= 50,
    !is.na(arm_angle), !is.na(avg_ivb), !is.na(avg_hb),
    pitch_type %in% c("FF", "SI", "FC", "SL", "ST", "CU", "CH")
  )

cat("Analysis dataset:", nrow(analysis), "pitcher-pitch-season combos\n")

# Pitch type labels for cleaner charts
pitch_labels <- c(
  "FF" = "Four-Seam Fastball",
  "SI" = "Sinker",
  "FC" = "Cutter",
  "SL" = "Slider",
  "ST" = "Sweeper",
  "CU" = "Curveball",
  "CH" = "Changeup"
)

# Per-pitch-type regressions
regression_results <- analysis %>%
  group_by(season, pitch_type) %>%
  summarise(
    n = n(),
    ivb_slope     = coef(lm(avg_ivb ~ arm_angle))[2],
    ivb_r2        = summary(lm(avg_ivb ~ arm_angle))$r.squared,
    hb_slope      = coef(lm(abs(avg_hb) ~ arm_angle))[2],
    hb_r2         = summary(lm(abs(avg_hb) ~ arm_angle))$r.squared,
    .groups = "drop"
  )

cat("\nRegression results:\n")
regression_results %>%
  mutate(across(where(is.numeric), ~ round(.x, 3))) %>%
  arrange(pitch_type, season) %>%
  print(n = 14)

# --- Chart A1: Arm Angle vs iVB ---
p_ivb <- analysis %>%
  mutate(pitch_label = pitch_labels[pitch_type]) %>%
  ggplot(aes(x = arm_angle, y = avg_ivb)) +
  geom_point(alpha = 0.25, size = 1.2, color = "#4a4a4a") +
  geom_smooth(method = "lm", color = "#D62728", se = TRUE,
              linewidth = 1.2, fill = "#D62728", alpha = 0.15) +
  facet_wrap(~pitch_label, scales = "free_y", ncol = 4) +
  labs(
    title = "How Arm Angle Shapes Vertical Pitch Movement",
    subtitle = "2024-2025 MLB | Each dot = one pitcher-season | 50+ pitch minimum",
    x = "Arm Angle (degrees)",
    y = "Induced Vertical Break (inches)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 10),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/post1_arm_angle_ivb.png", p_ivb,
       width = 12, height = 7, dpi = 300, bg = "white")
cat("Saved: outputs/post1_arm_angle_ivb.png\n")

# --- Chart A2: Arm Angle vs HB ---
p_hb <- analysis %>%
  mutate(pitch_label = pitch_labels[pitch_type]) %>%
  ggplot(aes(x = arm_angle, y = abs(avg_hb))) +
  geom_point(alpha = 0.25, size = 1.2, color = "#4a4a4a") +
  geom_smooth(method = "lm", color = "#1F77B4", se = TRUE,
              linewidth = 1.2, fill = "#1F77B4", alpha = 0.15) +
  facet_wrap(~pitch_label, scales = "free_y", ncol = 4) +
  labs(
    title = "How Arm Angle Shapes Horizontal Pitch Movement",
    subtitle = "2024-2025 MLB | Absolute HB to align left/right-handed pitchers",
    x = "Arm Angle (degrees)",
    y = "Horizontal Break (inches, absolute)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 10),
    strip.text = element_text(face = "bold", size = 11),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/post1_arm_angle_hb.png", p_hb,
       width = 12, height = 7, dpi = 300, bg = "white")
cat("Saved: outputs/post1_arm_angle_hb.png\n")


# ============================================================
# B. ARM ANGLE CHANGERS (2024 -> 2025)
# ============================================================

cat("\n=== B. ARM ANGLE CHANGERS ===\n")

# Same-team pitchers in both seasons
pitcher_team_season <- pitcher_teams %>%
  pivot_wider(names_from = season, values_from = primary_team, names_prefix = "team_")

same_team_pitchers <- pitcher_team_season %>%
  filter(team_2024 == team_2025)

arm_changes_same_team <- same_team_pitchers %>%
  left_join(arm_2024 %>% select(pitcher_id, angle_2024 = arm_angle),
            by = "pitcher_id") %>%
  left_join(arm_2025 %>% select(pitcher_id, angle_2025 = arm_angle),
            by = "pitcher_id") %>%
  filter(!is.na(angle_2024), !is.na(angle_2025)) %>%
  mutate(angle_change = angle_2025 - angle_2024)

cat("Same-team pitchers with both years:", nrow(arm_changes_same_team), "\n")
cat("League-wide mean change:", round(mean(arm_changes_same_team$angle_change), 2), "degrees\n")

# Team-level coaching effect
team_coaching_effect <- arm_changes_same_team %>%
  group_by(team_2024) %>%
  summarise(
    n_pitchers      = n(),
    mean_change     = round(mean(angle_change), 2),
    median_change   = round(median(angle_change), 2),
    n_dropped_3plus = sum(angle_change <= -3),
    n_raised_3plus  = sum(angle_change >= 3),
    .groups = "drop"
  ) %>%
  arrange(mean_change) %>%
  rename(team = team_2024)

cat("\nTeam coaching effects (same-pitcher changes):\n")
print(team_coaching_effect, n = 30)

# Qualified changers: SP >= 100 IP both years, RP >= 40 IP both years
qualified_changers <- arm_changes_same_team %>%
  left_join(pitcher_names, by = "pitcher_id") %>%
  left_join(workload_wide, by = "pitcher_id") %>%
  filter(
    role_2024 == role_2025,
    !is.na(role_2024),
    (role_2024 == "SP" & approx_ip_2024 >= 100 & approx_ip_2025 >= 100) |
    (role_2024 == "RP" & approx_ip_2024 >= 40  & approx_ip_2025 >= 40)
  )

cat("\nQualified changers:", nrow(qualified_changers), "\n")
cat("  Starters:", sum(qualified_changers$role_2024 == "SP"), "\n")
cat("  Relievers:", sum(qualified_changers$role_2024 == "RP"), "\n")

# Top changers
cat("\n--- Biggest arm angle DROPS (qualified) ---\n")
qualified_changers %>%
  arrange(angle_change) %>%
  head(15) %>%
  select(pitcher_name, team = team_2024, role = role_2024,
         angle_2024, angle_2025, angle_change,
         ip_2024 = approx_ip_2024, ip_2025 = approx_ip_2025) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  print()

cat("\n--- Biggest arm angle RAISES (qualified) ---\n")
qualified_changers %>%
  arrange(desc(angle_change)) %>%
  head(15) %>%
  select(pitcher_name, team = team_2024, role = role_2024,
         angle_2024, angle_2025, angle_change,
         ip_2024 = approx_ip_2024, ip_2025 = approx_ip_2025) %>%
  mutate(across(where(is.numeric), ~ round(.x, 1))) %>%
  print()

# --- Chart B1: Team coaching effects ---
p_teams <- team_coaching_effect %>%
  mutate(
    team = fct_reorder(team, mean_change),
    direction = if_else(mean_change >= 0, "Raised", "Dropped")
  ) %>%
  ggplot(aes(x = mean_change, y = team, fill = direction)) +
  geom_col(width = 0.7) +
  geom_vline(xintercept = 0, linewidth = 0.5) +
  scale_fill_manual(values = c("Dropped" = "#D62728", "Raised" = "#1F77B4"),
                    guide = "none") +
  labs(
    title = "Organizational Arm Angle Trends (2024 to 2025)",
    subtitle = "Same-pitcher changes only | Negative = org dropped arm angles",
    x = "Mean Arm Angle Change (degrees)",
    y = NULL
  ) +
  theme_minimal(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 10),
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/post2_team_coaching_effects.png", p_teams,
       width = 8, height = 8, dpi = 300, bg = "white")
cat("Saved: outputs/post2_team_coaching_effects.png\n")


# ============================================================
# C. MOVEMENT PREDICTION: PREDICTED VS ACTUAL
# ============================================================

cat("\n=== C. MOVEMENT PREDICTION ===\n")

# Build pitcher x pitch-type movement changes
pitch_level_changes <- master_with_team %>%
  filter(n_pitches >= 50, !is.na(arm_angle), !is.na(avg_ivb), !is.na(avg_hb)) %>%
  select(pitcher_id, pitcher_name, primary_team, season, pitch_type,
         arm_angle, avg_ivb, avg_hb, n_pitches, tj_stuff_plus)

movement_changes <- pitch_level_changes %>%
  select(-primary_team) %>%
  group_by(pitcher_id, pitcher_name, pitch_type, season) %>%
  summarise(
    arm_angle     = first(arm_angle),
    avg_ivb       = mean(avg_ivb, na.rm = TRUE),
    avg_hb        = mean(avg_hb, na.rm = TRUE),
    n_pitches     = sum(n_pitches),
    tj_stuff_plus = mean(tj_stuff_plus, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  pivot_wider(
    names_from = season,
    values_from = c(arm_angle, avg_ivb, avg_hb, n_pitches, tj_stuff_plus),
    names_sep = "_"
  ) %>%
  filter(
    !is.na(arm_angle_2024), !is.na(arm_angle_2025),
    !is.na(avg_ivb_2024), !is.na(avg_ivb_2025),
    n_pitches_2024 >= 50, n_pitches_2025 >= 50
  ) %>%
  mutate(
    arm_angle_change  = arm_angle_2025 - arm_angle_2024,
    actual_ivb_change = avg_ivb_2025 - avg_ivb_2024,
    actual_hb_change  = abs(avg_hb_2025) - abs(avg_hb_2024),
    stuff_change      = tj_stuff_plus_2025 - tj_stuff_plus_2024
  )

cat("Pitcher x pitch-type combos:", nrow(movement_changes), "\n")

# Apply league regression slopes
ivb_slopes <- regression_results %>%
  filter(season == 2024) %>%
  select(pitch_type, ivb_slope)

movement_eval <- movement_changes %>%
  left_join(ivb_slopes, by = "pitch_type") %>%
  mutate(
    predicted_ivb_change = arm_angle_change * ivb_slope,
    ivb_residual = actual_ivb_change - predicted_ivb_change
  ) %>%
  filter(pitch_type %in% c("FF", "SI", "FC", "SL", "ST", "CU", "CH"))

pred_actual_cor <- cor(movement_eval$predicted_ivb_change,
                       movement_eval$actual_ivb_change,
                       use = "complete.obs")
cat("Predicted vs actual iVB correlation:", round(pred_actual_cor, 3), "\n")

# --- Chart C1: Predicted vs Actual iVB change (fastballs only) ---
ff_eval <- movement_eval %>% filter(pitch_type == "FF")

# Label the biggest outliers
top_over  <- ff_eval %>% slice_max(ivb_residual, n = 5)
top_under <- ff_eval %>% slice_min(ivb_residual, n = 5)
label_set <- bind_rows(top_over, top_under)

p_pred_actual <- ff_eval %>%
  ggplot(aes(x = predicted_ivb_change, y = actual_ivb_change)) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed",
              color = "gray50", linewidth = 0.6) +
  geom_point(alpha = 0.4, size = 2, color = "#4a4a4a") +
  geom_point(data = label_set, color = "#D62728", size = 2.5) +
  ggrepel::geom_text_repel(
    data = label_set,
    aes(label = pitcher_name),
    size = 3, max.overlaps = 15, color = "#D62728",
    segment.color = "gray70", segment.size = 0.3
  ) +
  annotate("text", x = 1.2, y = -2.5, label = "Under-performers",
           color = "gray50", size = 3, fontface = "italic") +
  annotate("text", x = -1.2, y = 3.5, label = "Over-performers",
           color = "gray50", size = 3, fontface = "italic") +
  labs(
    title = "Fastball Ride Change: Predicted vs. Actual (2024 to 2025)",
    subtitle = paste0("Each dot = one pitcher | Dashed line = perfect prediction | r = ",
                      round(pred_actual_cor, 2)),
    x = "Predicted iVB Change from Arm Angle Shift (inches)",
    y = "Actual iVB Change (inches)"
  ) +
  coord_equal(xlim = c(-2, 2), ylim = c(-4, 5)) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 10),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/post2_predicted_vs_actual.png", p_pred_actual,
       width = 8, height = 8, dpi = 300, bg = "white")
cat("Saved: outputs/post2_predicted_vs_actual.png\n")


# ============================================================
# D. OUTCOME CONNECTION: DID ARM ANGLE CHANGES HELP?
# ============================================================

cat("\n=== D. OUTCOME CONNECTION ===\n")

# For fastballs: correlate arm angle change with tjStuff+ change and whiff% change
ff_outcomes <- movement_changes %>%
  filter(pitch_type == "FF", n_pitches_2024 >= 100, n_pitches_2025 >= 100) %>%
  left_join(
    all_pitches %>%
      filter(pitch_type == "FF") %>%
      select(pitcher_id, season, whiff_pct, in_zone_whiff_pct) %>%
      pivot_wider(
        names_from = season,
        values_from = c(whiff_pct, in_zone_whiff_pct),
        names_sep = "_"
      ),
    by = "pitcher_id"
  ) %>%
  mutate(
    whiff_change = whiff_pct_2025 - whiff_pct_2024,
    iz_whiff_change = in_zone_whiff_pct_2025 - in_zone_whiff_pct_2024
  ) %>%
  filter(!is.na(stuff_change), !is.na(whiff_change))

cat("Fastball outcome sample:", nrow(ff_outcomes), "pitchers\n\n")

# Correlation matrix
outcome_cors <- tibble(
  comparison = c(
    "Arm angle change vs tjStuff+ change",
    "Arm angle change vs Whiff% change",
    "Arm angle change vs IZ Whiff% change",
    "iVB change vs tjStuff+ change",
    "iVB change vs Whiff% change"
  ),
  correlation = c(
    cor(ff_outcomes$arm_angle_change, ff_outcomes$stuff_change, use = "complete.obs"),
    cor(ff_outcomes$arm_angle_change, ff_outcomes$whiff_change, use = "complete.obs"),
    cor(ff_outcomes$arm_angle_change, ff_outcomes$iz_whiff_change, use = "complete.obs"),
    cor(ff_outcomes$actual_ivb_change, ff_outcomes$stuff_change, use = "complete.obs"),
    cor(ff_outcomes$actual_ivb_change, ff_outcomes$whiff_change, use = "complete.obs")
  )
) %>%
  mutate(correlation = round(correlation, 3))

cat("Outcome correlations (fastballs, 100+ pitches both years):\n")
print(outcome_cors)

# --- Chart D1: Arm angle change vs tjStuff+ change ---
p_stuff <- ff_outcomes %>%
  ggplot(aes(x = arm_angle_change, y = stuff_change)) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_point(alpha = 0.4, size = 2, color = "#4a4a4a") +
  geom_smooth(method = "lm", color = "#D62728", se = TRUE,
              linewidth = 1, fill = "#D62728", alpha = 0.15) +
  labs(
    title = "Does Changing Arm Angle Improve Fastball Stuff?",
    subtitle = paste0(
      "2024 to 2025 | r = ",
      round(cor(ff_outcomes$arm_angle_change, ff_outcomes$stuff_change,
                use = "complete.obs"), 3)
    ),
    x = "Arm Angle Change (degrees)",
    y = "tjStuff+ Change"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 10),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/post3_stuff_vs_arm_angle.png", p_stuff,
       width = 8, height = 6, dpi = 300, bg = "white")
cat("Saved: outputs/post3_stuff_vs_arm_angle.png\n")

# --- Chart D2: iVB change vs Whiff% change ---
p_whiff <- ff_outcomes %>%
  ggplot(aes(x = actual_ivb_change, y = whiff_change)) +
  geom_hline(yintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_vline(xintercept = 0, color = "gray70", linewidth = 0.4) +
  geom_point(alpha = 0.4, size = 2, color = "#4a4a4a") +
  geom_smooth(method = "lm", color = "#1F77B4", se = TRUE,
              linewidth = 1, fill = "#1F77B4", alpha = 0.15) +
  labs(
    title = "Does Gaining Fastball Ride Improve Whiff Rate?",
    subtitle = paste0(
      "2024 to 2025 | r = ",
      round(cor(ff_outcomes$actual_ivb_change, ff_outcomes$whiff_change,
                use = "complete.obs"), 3)
    ),
    x = "Induced Vertical Break Change (inches)",
    y = "Whiff% Change"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    plot.subtitle = element_text(color = "gray40", size = 10),
    panel.grid.minor = element_blank()
  )

ggsave("outputs/post3_whiff_vs_ivb.png", p_whiff,
       width = 8, height = 6, dpi = 300, bg = "white")
cat("Saved: outputs/post3_whiff_vs_ivb.png\n")


# ============================================================
# SAVE FINAL ENVIRONMENT
# ============================================================

save.image("data/arm_angle_backup.RData")
cat("\nAll analysis complete. Charts saved to outputs/.\n")
