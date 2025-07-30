# R/setup.R

options(repos = c(CRAN = "https://cloud.r-project.org"))

library(shiny)
library(shinydashboard)
library(tidyverse)
library(cowplot)
library(plotly)
library(gtable)
library(ggplot2)
library(glue)
library(RColorBrewer)

options(stringsAsFactors = FALSE)

# your global heatmap theme
theme_heatmap <- theme_void() +
  theme(
    plot.title  = element_text(face = "bold", hjust = 0.5, size = 10, 
                              margin = margin(b = 5)),
    plot.margin = unit(c(0, -3, -3, 0), "pt"),
    axis.ticks  = element_blank(),
    axis.line   = element_blank()
  )

# for y-labels
theme_y_strip <- theme_void() +
  theme(
    axis.text.x.top = element_blank(),
    plot.margin     = unit(c(5, 2, 5, 0), "pt"),
    axis.ticks      = element_blank(),
    axis.line       = element_blank()
  )
