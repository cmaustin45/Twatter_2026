# ============================================================
# Display helpers: palettes and kable styling
# ============================================================

library(kableExtra)

position_colors <- c(
  "QB"   = "#FF6B6B",
  "RB"   = "#9B59B6",
  "WR"   = "#2ECC71",
  "TE"   = "#FFA07A",
  "K"    = "#95A5A6",
  "D/ST" = "#45B7D1",
  "DEF"  = "#45B7D1"
)

# 2026 team names go here. Update once after the draft.
team_colors <- c(
  "AI generated name"         = "#e74c3c",
  "One for the Thumb in 21"   = "#3498db",
  "It's Geno Time!"           = "#0E9F56",
  "Poopy Butt"                = "#8b4513",
  "Sunsets"                   = "#ff6b6b",
  "Ass Bongos"                = "#9b59b6",
  "Ginger Genius"             = "#f39c12",
  "You flew here to do this?" = "#1abc9c",
  "james = douche (2.0)"      = "#3D3D3D",
  "10mg James"                = "#ffe0c9",
  "Sleeper"                   = "#95a5a6",
  "DaBlacGodfather"           = "#0F1056"
)

# Vectorised: works directly inside mutate() without vapply
color_position <- function(position) {
  position <- as.character(position)
  fill <- unname(position_colors[position])
  fill[is.na(fill)] <- "#CCCCCC"
  cell_spec(position,
            background = fill, color = "white", bold = TRUE, align = "center",
            extra_css = "padding: 4px 8px; border-radius: 3px;")
}

# Default table look, so every post styles tables the same way
ff_kable <- function(df, ...) {
  kable(df, format = "html", escape = FALSE, digits = 2, ...) |>
    kable_styling(bootstrap_options = c("striped", "hover"), full_width = FALSE)
}
