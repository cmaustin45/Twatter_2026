# ============================================================
# Fleaflicker API functions
# Source from every post: source(here::here("R/fleaflicker.R"))
# ============================================================

library(httr)
library(jsonlite)

FLEAFLICKER_BASE <- "https://www.fleaflicker.com/api"

# ---- Low-level request helper -------------------------------------------

# One place for the GET + status check + JSON parse.
# flatten = TRUE gives the scoreboard a flat data frame (away.name etc.).
# simplify = FALSE keeps box scores and standings as nested lists.
fleaflicker_get <- function(endpoint, query, flatten = FALSE, simplify = TRUE,
                            on_fail = c("stop", "warn")) {
  on_fail <- match.arg(on_fail)
  response <- GET(url = file.path(FLEAFLICKER_BASE, endpoint), query = query)

  if (status_code(response) != 200) {
    msg <- sprintf("%s request failed (HTTP %s)", endpoint, status_code(response))
    if (on_fail == "stop") stop(msg)
    warning(msg)
    return(NULL)
  }

  fromJSON(
    content(response, as = "text", encoding = "UTF-8"),
    flatten = flatten,
    simplifyDataFrame = simplify
  )
}

# ---- Endpoints ------------------------------------------------------------

get_league_scoreboard <- function(league_id, sport, season = NULL, scoring_period = NULL) {
  fleaflicker_get(
    "FetchLeagueScoreboard",
    query = list(sport = sport, league_id = league_id,
                 season = season, scoring_period = scoring_period),
    flatten = TRUE
  )
}

get_box_score <- function(league_id, game_id, sport, scoring_period) {
  fleaflicker_get(
    "FetchLeagueBoxscore",
    query = list(sport = sport, league_id = league_id,
                 fantasy_game_id = game_id, scoring_period = scoring_period),
    simplify = FALSE,
    on_fail = "warn"
  )
}

get_league_standings <- function(league_id, sport, season) {
  fleaflicker_get(
    "FetchLeagueStandings",
    query = list(sport = sport, league_id = league_id, season = season),
    simplify = FALSE,
    on_fail = "warn"
  )
}

# ---- Extractors -----------------------------------------------------------

extract_game_ids <- function(scoreboard_data) {
  if (!("games" %in% names(scoreboard_data))) return(NULL)
  as.character(scoreboard_data$games$id)
}

# Small helper: pull a nested value or return a default
`%||%` <- function(x, y) if (is.null(x)) y else x

extract_player_stats <- function(box_score_data, game_id, scoring_period) {
  if (is.null(box_score_data) || !("lineups" %in% names(box_score_data))) return(NULL)

  lineups <- box_score_data$lineups
  if (!is.list(lineups) || length(lineups) == 0) return(NULL)

  all_players <- list()

  for (lineup in lineups) {
    group_name <- lineup$group %||% "BENCH"
    if (!("slots" %in% names(lineup))) next

    for (slot in lineup$slots) {
      slot_position <- slot$position$label %||% NA

      for (side in c("away", "home")) {
        if (!(side %in% names(slot)) || is.null(slot[[side]])) next

        p <- slot[[side]]

        opponent <- NA
        if ("requestedGames" %in% names(p) && length(p$requestedGames) > 0) {
          game_data <- p$requestedGames[[1]]$game
          nfl_team  <- p$proPlayer$proTeamAbbreviation
          if (!is.null(game_data)) {
            if (!is.null(game_data$away$abbreviation) && game_data$away$abbreviation == nfl_team) {
              opponent <- game_data$home$abbreviation
            } else if (!is.null(game_data$home$abbreviation)) {
              opponent <- game_data$away$abbreviation
            }
          }
        }

        all_players[[length(all_players) + 1]] <- data.frame(
          week              = scoring_period,
          game_id           = game_id,
          fantasy_team_id   = p$owner$id %||% NA,
          fantasy_team_name = p$owner$name %||% NA,
          player_id         = p$proPlayer$id %||% NA,
          player_name       = p$proPlayer$nameFull %||% NA,
          position          = p$proPlayer$position %||% NA,
          nfl_team          = p$proPlayer$proTeamAbbreviation %||% NA,
          opponent          = opponent,
          slot              = slot_position,
          lineup_group      = group_name,
          points            = p$viewingActualPoints$value %||% 0,
          stringsAsFactors  = FALSE
        )
      }
    }
  }

  if (length(all_players) == 0) return(NULL)
  combined <- do.call(rbind, all_players)
  rownames(combined) <- NULL
  combined
}

# Away and home share the same structure; one inner helper handles both.
extract_team_scores_from_boxscore <- function(box_score_data) {
  if (is.null(box_score_data)) return(NULL)
  if (!("game" %in% names(box_score_data))) {
    warning("Unexpected box score structure")
    return(NULL)
  }

  game_data <- box_score_data$game

  one_side <- function(side, points_key) {
    if (!(side %in% names(game_data))) return(NULL)
    totals <- box_score_data[[points_key]]$total
    data.frame(
      team_name     = game_data[[side]]$name %||% NA,
      team_id       = game_data[[side]]$id %||% NA,
      actual_score  = totals$value$value %||% NA,
      optimal_score = totals$optimum$value %||% NA,
      stringsAsFactors = FALSE
    )
  }

  teams <- rbind(one_side("away", "pointsAway"), one_side("home", "pointsHome"))
  if (is.null(teams) || nrow(teams) == 0) return(NULL)
  teams
}

# ---- Weekly pull ----------------------------------------------------------

get_weekly_player_stats <- function(league_id, sport, season, scoring_period,
                                    pause = 0.5) {
  scoreboard <- get_league_scoreboard(league_id, sport, season, scoring_period)
  game_ids   <- extract_game_ids(scoreboard)
  if (is.null(game_ids) || length(game_ids) == 0) stop("No games found")

  all_player_stats <- list()
  all_team_scores  <- list()

  for (game_id in game_ids) {
    box_score <- get_box_score(league_id, game_id, sport, scoring_period)
    if (!is.null(box_score)) {
      ps <- extract_player_stats(box_score, game_id, scoring_period)
      if (!is.null(ps)) all_player_stats[[length(all_player_stats) + 1]] <- ps

      ts <- extract_team_scores_from_boxscore(box_score)
      if (!is.null(ts)) all_team_scores[[length(all_team_scores) + 1]] <- ts
    }
    Sys.sleep(pause)
  }

  if (length(all_player_stats) == 0) stop("No player stats extracted")
  combined_stats <- do.call(rbind, all_player_stats)
  rownames(combined_stats) <- NULL

  combined_team_scores <- NULL
  if (length(all_team_scores) > 0) {
    combined_team_scores <- do.call(rbind, all_team_scores)
    rownames(combined_team_scores) <- NULL
  }

  list(
    player_stats        = combined_stats,
    scoreboard          = scoreboard,
    team_optimal_scores = combined_team_scores
  )
}

# ---- Disk cache -----------------------------------------------------------

# Completed weeks never change, so keep them on disk.
# Delete data/<season>_week_<ww>.rds to force a refetch (e.g. mid-week).
get_week_cached <- function(league_id, sport, season, week,
                            dir = here::here("data"), refresh = FALSE) {
  if (!dir.exists(dir)) dir.create(dir, recursive = TRUE)
  path <- file.path(dir, sprintf("%d_week_%02d.rds", season, week))

  if (file.exists(path) && !refresh) return(readRDS(path))

  out <- get_weekly_player_stats(league_id, sport, season, week)
  saveRDS(out, path)
  out
}

# Load weeks 1:week from cache, fetching any that are missing.
get_season_to_date <- function(league_id, sport, season, week, ...) {
  weeks <- seq_len(week)
  out <- lapply(weeks, function(wk) {
    tryCatch(
      get_week_cached(league_id, sport, season, wk, ...),
      error = function(e) { warning("Could not load week ", wk, ": ", conditionMessage(e)); NULL }
    )
  })
  names(out) <- weeks
  Filter(Negate(is.null), out)
}

# Division lookup as a data frame, ready for left_join()
get_divisions <- function(standings_data) {
  if (is.null(standings_data) || !("divisions" %in% names(standings_data))) {
    return(data.frame(fantasy_team_id = integer(), fantasy_team_name = character(),
                      division = character()))
  }
  rows <- lapply(standings_data$divisions, function(div) {
    data.frame(
      fantasy_team_id   = vapply(div$teams, function(t) t$id, numeric(1)),
      fantasy_team_name = vapply(div$teams, function(t) t$name, character(1)),
      division          = div$name,
      stringsAsFactors  = FALSE
    )
  })
  do.call(rbind, rows)
}

# Replace team names in cached weeks with the current name for that team ID.
normalize_team_names <- function(all_weeks_data, divisions) {
  lookup <- setNames(divisions$fantasy_team_name, divisions$fantasy_team_id)
  lapply(all_weeks_data, function(wk) {
    ids <- as.character(wk$player_stats$fantasy_team_id)
    wk$player_stats$fantasy_team_name <- unname(lookup[ids])
    if (!is.null(wk$team_optimal_scores)) {
      tids <- as.character(wk$team_optimal_scores$team_id)
      wk$team_optimal_scores$team_name <- unname(lookup[tids])
    }
    if ("games" %in% names(wk$scoreboard)) {
      g <- wk$scoreboard$games
      g$away.name <- unname(lookup[as.character(g$away.id)])
      g$home.name <- unname(lookup[as.character(g$home.id)])
      wk$scoreboard$games <- g
    }
    wk
  })
}