# Create a new weekly post from the template.
# Usage (from the project root):  Rscript new_week.R 1
#   or in RStudio:                source("new_week.R"); new_week(1)

new_week <- function(week, season = 2026, date = Sys.Date(), overwrite = FALSE) {
  post_dir <- here::here("posts", sprintf("week-%02d", week))
  target   <- file.path(post_dir, "index.qmd")

  if (file.exists(target) && !overwrite) {
    stop(target, " already exists. Use overwrite = TRUE to replace it.")
  }
  dir.create(post_dir, showWarnings = FALSE, recursive = TRUE)

  tpl <- readLines(here::here("templates", "post-template.qmd"))
  tpl <- gsub("{{WEEK}}",   week,               tpl, fixed = TRUE)
  tpl <- gsub("{{SEASON}}", season,             tpl, fixed = TRUE)
  tpl <- gsub("{{DATE}}",   format(date, "%Y-%m-%d"), tpl, fixed = TRUE)
  writeLines(tpl, target)

  message("Created ", target)
  message("Next: quarto preview  ->  quarto render  ->  quarto publish quarto-pub")
  invisible(target)
}

# Allow: Rscript new_week.R 1
args <- commandArgs(trailingOnly = TRUE)
if (length(args) >= 1 && !interactive()) new_week(as.integer(args[1]))
