# install.packages(c("showtext", "geomtextpath", "ggrepel"))
library(showtext)
library(ggplot2)
library(geomtextpath)
library(ggrepel)

font_add_google("Bricolage Grotesque", "bricolage")
showtext_auto()

# 10-color palette — distinct, colour-blind-friendly, reusable for any data
cb10 <- c(
  "#1170AA", "#FC7D0B", "#57A44C", "#C85200",
  "#7B4F71", "#00928F", "#D4A017", "#5FA2CE", "#E05C5C","#A3ACB9"
)

theme_custom <- function(base_size = 12,
                         base_family = "bricolage") {
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.background    = element_rect(fill = "#F7F8FC", colour = NA),
      panel.background   = element_rect(fill = "#EEF0F8", colour = NA),
      
      panel.grid.major.y = element_line(colour = "#D4D8EB", linewidth = 0.35),
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.border       = element_blank(),
      
      axis.line.x        = element_line(colour = "#B0B5CC", linewidth = 0.4),
      axis.line.y        = element_blank(),
      
      strip.background = element_rect(fill = "#2E3250", colour = NA),
      strip.text       = element_text(
        family = base_family,
        size   = rel(0.90),
        colour = "#FFFFFF",
        face   = "plain",
        margin = margin(5, 0, 5, 0)
      ),
      
      axis.text          = element_text(size = rel(0.85), colour = "#4A4E6A"),
      axis.title         = element_blank(),
      axis.ticks.x       = element_line(colour = "#B0B5CC", linewidth = 0.3),
      axis.ticks.y       = element_blank(),
      axis.ticks.length  = unit(3, "pt"),
      
      legend.position    = "top",
      
      panel.spacing      = unit(1.2, "lines"),
      
      plot.title         = element_text(
        size   = rel(1.35),
        face   = "bold",
        colour = "#1A1E3A",
        margin = margin(b = 2)
      ),
      plot.subtitle      = element_text(
        size   = rel(0.92),
        colour = "#4A4E6A",
        margin = margin(b = 12)
      ),
      plot.caption     = element_text(
        size   = rel(0.85),
        colour = "#8A8FA8",
        hjust  = 1,
        margin = margin(t = 8)
      ),
      plot.margin        = margin(12, 14, 10, 12)
    )
}