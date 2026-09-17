# ============================================================
# Analytics functions (operate on the objects returned by
# get_weekly_player_stats / get_week_cached)
# ============================================================

calculate_team_scores <- function(player_stats) {
  starters <- player_stats[player_stats$lineup_group == "START", ]
  team_scores <- aggregate(points ~ fantasy_team_name + fantasy_team_id,
                           data = starters, FUN = sum, na.rm = TRUE)
  names(team_scores)[3] <- "total_points"
  team_scores[order(-team_scores$total_points), ]
}

get_matchup_results <- function(scoreboard_data) {
  if (!("games" %in% names(scoreboard_data))) return(NULL)
  games <- scoreboard_data$games

  pick <- function(col) if (col %in% names(games)) games[[col]] else NA

  data.frame(
    game_id    = games$id,
    away_team  = pick("away.name"),
    away_score = pick("awayScore.score.value"),
    home_team  = pick("home.name"),
    home_score = pick("homeScore.score.value"),
    stringsAsFactors = FALSE
  )
}

analyze_position_performance <- function(player_stats) {
  starters <- player_stats[player_stats$lineup_group == "START", ]

  pos_stats <- aggregate(points ~ position, data = starters,
                         FUN = function(x) c(
                           count = length(x),
                           avg   = mean(x, na.rm = TRUE),
                           max   = max(x, na.rm = TRUE),
                           min   = min(x, na.rm = TRUE),
                           total = sum(x, na.rm = TRUE)
                         ))

  pos_df <- data.frame(
    position = pos_stats$position,
    count    = pos_stats$points[, "count"],
    avg      = pos_stats$points[, "avg"],
    max      = pos_stats$points[, "max"],
    min      = pos_stats$points[, "min"],
    total    = pos_stats$points[, "total"]
  )
  pos_df[order(-pos_df$avg), ]
}

find_best_bench_players <- function(player_stats, n = 10) {
  bench <- player_stats[player_stats$lineup_group == "BENCH", ]
  if (nrow(bench) == 0) return(NULL)
  bench <- bench[order(-bench$points), ]
  bench$points_wasted <- bench$points
  head(bench, n)
}

find_worst_starters <- function(player_stats, n = 10) {
  starters <- player_stats[player_stats$lineup_group == "START", ]
  if (nrow(starters) == 0) return(NULL)
  head(starters[order(starters$points), ], n)
}

calculate_optimal_scores <- function(team_optimal_scores) {
  if (is.null(team_optimal_scores) || nrow(team_optimal_scores) == 0) {
    warning("No team optimal scores available")
    return(NULL)
  }

  x <- team_optimal_scores
  x$points_left <- x$optimal_score - x$actual_score
  x$efficiency  <- (x$actual_score / x$optimal_score) * 100
  x$efficiency[is.nan(x$efficiency) | is.infinite(x$efficiency)] <- 0
  names(x)[names(x) == "team_name"] <- "team"

  x <- x[, c("team", "actual_score", "optimal_score", "points_left", "efficiency")]
  x[order(-x$efficiency), ]
}

# Weekly starter totals across a season list (from get_season_to_date)
weekly_team_scores <- function(all_weeks_data) {
  all_player_data <- dplyr::bind_rows(lapply(all_weeks_data, `[[`, "player_stats"))
  all_player_data |>
    dplyr::filter(lineup_group == "START") |>
    dplyr::group_by(week, fantasy_team_name) |>
    dplyr::summarise(total_points = sum(points, na.rm = TRUE), .groups = "drop")
}
