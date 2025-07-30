# R/colors.R

# Blair color palette
wbColorList <- list(
  "BlairBlue"     = rgb(0,  65, 101, max = 255),
  "LightPurple"   = rgb(129,147,219, max = 255),
  "DarkGreen"     = rgb(0,133,102, max = 255),
  "Black"         = rgb(0,0,0, max = 255),
  "LightGrey"     = rgb(154,155,156, max = 255),
  "DarkCyan"      = rgb(0,122,201, max = 255),
  "BrightOrange"  = rgb(240,171,0, max = 255),
  "DarkGrey"      = rgb(72,72,74, max = 255),
  "BrightGreen"   = rgb(122,184,0, max = 255),
  "DarkPurple"    = rgb(87,6,140, max = 255),
  "DarkOrange"    = rgb(220,80,52, max = 255),
  "Yellow"        = rgb(243,211,17, max = 255),
  "BrightCyan"    = rgb(61,183,228, max = 255),
  "Orange"        = rgb(245,140,119, max = 255),
  "BlueGreen"     = rgb(127,198,190, max = 255),
  "Periwinkle"    = rgb(191,205,231, max = 255)
)

# Blair themes and helper functions

# A common barplot style using Blair colors
barplot_mtd_style <- function(x = factor_name, y = mtd, fill = factor_name) {
  font <- "Helvetica"
  ggplot2::theme(
    text            = element_text(family = font, size = 16),
    legend.position = "none",
    panel.background= element_rect(fill = "white"),
    axis.text.x     = element_blank(),
    axis.title.x    = element_blank(),
    axis.title.y    = element_blank(),
    strip.background= element_rect(fill = "white"),
    strip.text      = element_text(colour = wbColorList$Black),
    axis.ticks.x    = element_blank()
  )
}

# A clean black & white theme extended with Blair colors
themequantmodexp <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.x      = element_blank(),
      axis.title.y      = element_blank(),
      axis.text         = element_text(size = 10, color = wbColorList$Black),
      axis.ticks.x      = element_blank(),
      legend.position   = "bottom",
      legend.text       = element_text(size = 10),
      legend.title      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(size = 14, face = "bold", hjust = 0.5),
      plot.subtitle     = element_text(size = 10, face = "bold", hjust = 0.5),
      panel.border      = element_blank()
    )
}

# Smaller legend variant
themequantmodexp2 <- function() {
  ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title.x      = element_blank(),
      axis.title.y      = element_blank(),
      axis.text         = element_text(size = 10, color = wbColorList$Black),
      axis.text.x       = element_text(margin = margin(t = 5, b = 10)),
      axis.ticks.x      = element_blank(),
      legend.position   = "bottom",
      legend.text       = element_text(size = 10),
      legend.title      = element_blank(),
      panel.grid.major  = element_blank(),
      panel.grid.minor  = element_blank(),
      plot.title        = element_text(size = 10, face = "bold", hjust = 0.5),
      plot.subtitle     = element_text(size = 10, face = "bold", hjust = 0.5),
      panel.border      = element_blank()
    )
}

# Blair style theme
blair_style <- function() {
  font <- "Helvetica"
  ggplot2::theme(
    plot.title      = element_text(family = font, size = 16, color = wbColorList$BlairBlue),
    plot.subtitle   = element_text(family = font, size = 22, color = wbColorList$BlairBlue,
                                   margin = margin(t = 9, r = 1, b = 9, l = 1)),
    plot.caption    = element_blank(),
    legend.position = "bottom",
    legend.text.align = 0,
    legend.background = element_blank(),
    legend.title      = element_blank(),
    legend.key        = element_blank(),
    legend.text       = element_text(family = font, size = 16, color = wbColorList$Black),
    axis.title        = element_blank(),
    axis.text         = element_text(family = font, size = 12, color = wbColorList$Black),
    axis.text.x       = element_text(margin = margin(t = 5, b = 10)),
    axis.ticks        = element_blank(),
    axis.line         = element_line(),
    panel.grid.minor  = element_blank(),
    panel.grid.major.y= element_blank(),
    panel.grid.major.x= element_blank(),
    panel.background  = element_blank(),
    strip.background  = element_rect(fill = "white"),
    strip.text        = element_text(size = 22, hjust = 0)
  )
}
