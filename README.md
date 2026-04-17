# Arm Angle Research

Analysis of MLB pitcher arm angle changes between 2024 and 2025, and their effects on pitch movement and outcomes.

## Research Questions

1. How does arm angle predict pitch movement across pitch types?
2. Which pitchers and organizations changed arm angles most year-over-year?
3. Did the arm angle changes produce the predicted movement changes?
4. Did the changes translate to improved outcomes (Stuff+, FIP, Whiff%)?

## Key Findings (in progress)

**Post 1: The League Map**
- Arm angle is a strong predictor of fastball, sinker, changeup, and curveball movement (R^2 = 0.40 to 0.61).
- Arm angle has essentially no relationship with cutter, slider, or sweeper movement.
- Implication: dropping arm slot will reshape your fastball, sinker, and curve, but breaking ball quality is grip-dependent.

**Post 2: The Changers**
- League-wide trend: returning pitchers dropped arm angle by an average of 0.7 degrees from 2024 to 2025.
- Astros, Royals, and Phillies led the development trend among returning arms.
- San Francisco Giants are the lone organization actively raising arm angles (+1.62 degrees on average).
- Headline individual changes: George Kirby (-7.1), Josh Hader (-5.4), Ranger Suarez (-4.9).

## Data Sources

- TJStats (Hugging Face dataset: TJStatsApps/mlb_data) — pitch-by-pitch tracking and tjStuff+ values
- Baseball Savant — pitcher arm angle leaderboards
- FanGraphs — Stuff+, Location+, FIP, SIERA (pulled via JSON API)

## Project Structure

- code/01_full_analysis.R — Pull, clean, aggregate, analyze
- data/arm_angle_backup.RData — Cached working environment
- outputs/post1_arm_angle_ivb.png — League map: arm angle vs induced vertical break
- outputs/post1_arm_angle_hb.png — League map: arm angle vs horizontal break
- README.md

## Reproducing

1. Open arm_angle_research.Rproj in RStudio
2. Run code/01_full_analysis.R from the top to reproduce all results
3. Or load the cached environment for an instant restart: load("data/arm_angle_backup.RData")

## Tools

R 4.5.3, tidyverse, arrow

## Author

[Jack Noble] — analyst.

## Status

Active. Posts 3 (org philosophy) and 4 (outcome impact) in progress.

