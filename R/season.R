# ============================================================
# Season-long functions: matchup log, records, points against,
# all-play, luck.  All take the season list from get_season_to_date().
# ============================================================

library(dplyr)
library(tidyr)

# One row per team per week: who they played, points for/against, result.
# Scores come from starter totals (weekly_team_scores), pairings from the scoreboard.
season_matchup_log <- function(all_weeks_data) {
  weekly_scores <- weekly_team_scores(all_weeks_data)

  pairings <- bind_rows(lapply(names(all_weeks_data), function(wk) {
    m <- get_matchup_results(all_weeks_data[[wk]]$scoreboard)
    if (is.null(m)) return(NULL)
    bind_rows(
      tibble(week = as.integer(wk), team = m$away_team, opponent = m$home_team),
      tibble(week = as.integer(wk), team = m$home_team, opponent = m$away_team)
    )
  }))

  pairings |>
    left_join(weekly_scores, by = c("week", "team" = "fantasy_team_name")) |>
    rename(points_for = total_points) |>
    left_join(weekly_scores, by = c("week", "opponent" = "fantasy_team_name")) |>
    rename(points_against = total_points) |>
    mutate(
      margin = points_for - points_against,
      result = case_when(
        margin > 0  ~ "W",
        margin < 0  ~ "L",
        TRUE        ~ "T"
      )
    ) |>
    arrange(week, desc(points_for))
}

# Actual record plus points for / against
season_record <- function(matchup_log) {
  matchup_log |>
    group_by(team) |>
    summarise(
      wins   = sum(result == "W"),
      losses = sum(result == "L"),
      ties   = sum(result == "T"),
      points_for     = sum(points_for, na.rm = TRUE),
      points_against = sum(points_against, na.rm = TRUE),
      avg_for        = mean(points_for, na.rm = TRUE),
      avg_against    = mean(points_against, na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(record = if_else(ties > 0,
                            sprintf("%d-%d-%d", wins, losses, ties),
                            sprintf("%d-%d", wins, losses))) |>
    arrange(desc(wins), desc(points_for))
}

# All-play: each week, a team "beats" every team it outscored.
all_play_record <- function(weekly_scores) {
  weekly_scores |>
    group_by(week) |>
    mutate(
      ap_wins   = rank(total_points, ties.method = "min") - 1,
      ap_losses = n() - rank(total_points, ties.method = "max"),
      ap_ties   = n() - 1 - ap_wins - ap_losses
    ) |>
    ungroup() |>
    group_by(team = fantasy_team_name) |>
    summarise(
      ap_wins   = sum(ap_wins),
      ap_losses = sum(ap_losses),
      ap_ties   = sum(ap_ties),
      ap_pct    = (ap_wins + 0.5 * ap_ties) / (ap_wins + ap_losses + ap_ties),
      .groups = "drop"
    ) |>
    arrange(desc(ap_pct))
}

# Luck = actual wins minus the wins you'd expect from all-play win %.
# Positive = winning more than your scoring deserves.
luck_table <- function(record, all_play) {
  record |>
    select(team, wins, losses, ties, record, points_for, points_against) |>
    left_join(all_play, by = "team") |>
    mutate(
      games         = wins + losses + ties,
      expected_wins = ap_pct * games,
      luck          = wins + 0.5 * ties - expected_wins,
      all_play      = sprintf("%d-%d", ap_wins, ap_losses)
    ) |>
    arrange(desc(luck))
}
