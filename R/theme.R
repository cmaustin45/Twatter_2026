# ============================================================
# Chart theming. Source after tables.R (needs team_colors).
# ============================================================

library(ggplot2)
library(highcharter)

# Brand colors, kept in sync with _brand.yml by hand
brand <- c(
  navy = "#0F1056", orange = "#F39C12", green = "#0E9F56",
  red  = "#E74C3C", ink  = "#1F2430", paper = "#FAFAF7", slate = "#5C6370"
)

# ---- ggplot ---------------------------------------------------------------

theme_league <- function(base_size = 13) {
  theme_minimal(base_size = base_size) +
    theme(
      text             = element_text(color = brand[["ink"]]),
      plot.title       = element_text(family = "Oswald", size = base_size * 1.4,
                                      color = brand[["navy"]]),
      plot.subtitle    = element_text(color = brand[["slate"]]),
      panel.grid.minor = element_blank(),
      panel.grid.major = element_line(color = "#E6E4DE"),
      plot.background  = element_rect(fill = brand[["paper"]], color = NA),
      legend.position  = "bottom",
      legend.title     = element_blank()
    )
}

scale_fill_team  <- function(...) scale_fill_manual(values = team_colors, na.value = "grey70", ...)
scale_color_team <- function(...) scale_color_manual(values = team_colors, na.value = "grey70", ...)

# Set once in setup so every chart picks it up without repeating theme_league()
theme_set(theme_league())

# ---- highcharter ----------------------------------------------------------

hc_theme_league <- hc_theme(
  colors = unname(brand[c("navy", "orange", "green", "red", "slate")]),
  chart  = list(backgroundColor = brand[["paper"]],
                style = list(fontFamily = "Inter")),
  title  = list(style = list(fontFamily = "Oswald", color = brand[["navy"]], fontSize = "20px")),
  subtitle = list(style = list(color = brand[["slate"]])),
  xAxis  = list(gridLineWidth = 0, lineColor = "#E6E4DE"),
  yAxis  = list(gridLineColor = "#E6E4DE"),
  legend = list(itemStyle = list(fontWeight = "normal"))
)

# Team color lookup for highcharter series
team_color <- function(team) unname(ifelse(team %in% names(team_colors), team_colors[team], "#999999"))
