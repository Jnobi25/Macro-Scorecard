# R/functions.R

library(ggplot2)

# heat data builder
get_heat_data <- function(df, sel_countries, sel_categories, view) {
  df %>%
    filter(
      Country %in% sel_countries,
      Category %in% sel_categories,
      View == view,
      Type %in% type_levels
    ) %>%
    transmute(
      Country,
      Category,
      Type,
      Score,
      RawValue = Latest
    ) %>%
    complete(
      Country = sel_countries, Category,
      Type = type_levels,
      fill = list(Score = NA, RawValue = NA)
    ) %>%
    mutate(
      Type     = factor(Type, levels = type_levels, labels = type_labels),
      Category = factor(Category, levels = sel_categories),
      Country  = factor(Country, levels = sel_countries)
    )
}

# recode IP & Autos for the general heatmaps
general_category_labels <- function(cats) {
  recode(cats,
         ip    = "IP",
         autos = "Autos",
         .default = cats
  )
}
# recode categories in financial heatmap
third_category_labels <- function(cats) {
  recode(cats, !!!third_category_map, .default = cats)
}

# Set the color scale
score_scale <- scale_fill_gradient2(
  low = "#ae123a", mid = "white", high = "#63BE7B",
  midpoint = 0, limits = c(-2, 2),
  oob = scales::squish, na.value = "grey90"
)
# color policy rate category
policy_scale_dev <- scale_fill_gradient2(
  low      = "white",
  mid      = "white",
  high     = "white",
  midpoint = 0,
  limits   = c(-2, 2),
  oob      = scales::squish,
  na.value = "white"
)
policy_scale_em <- scale_fill_gradient2(
  low      = "#f2f2f2",
  mid      = "#f2f2f2",
  high     = "#f2f2f2",
  midpoint = 0,
  limits   = c(-2, 2),
  oob      = scales::squish,
  na.value = "#f2f2f2"
)

common_layers <- function(tile_text_size) {
  list(
    geom_tile(color = NA),
    geom_text(aes(label = label), size = tile_text_size),
    guides(fill = FALSE),
    scale_x_discrete(position = "top"),
    theme_heatmap     
  )
}

build_category_plot <- function(data,             # the one‐category slice of heat_data
                                cat_disp,         # the display title for this category (wrapped if needed)
                                panel_title,      # "Developed Markets" vs "Emerging Markets"
                                is_rate,          # TRUE iff this is the "rates" category
                                panel_countries,  # the vector you passed into make_panel()
                                hide_x,
                                plot_title_size = 10,
                                cat_title_size = 10,
                                tile_text_size = 4) {
  # 1) add the formatted text label
  data <- data %>%
    mutate(label = case_when(
      is.na(RawValue)        ~ "",
      is_rate                ~ sprintf("%.2f", RawValue),
      TRUE                   ~ sprintf("%.1f", RawValue)
    ))
  
  # 2) Pick the right fill‐gradient
  fill_scale <- if (is_rate) {
    if (panel_title == "Developed Markets") policy_scale_dev else policy_scale_em
  } else {
    score_scale
  }
  
  # 2) start the ggplot
  ggplot(data, aes(x = Type, y = Country, fill = Score)) +
    common_layers(tile_text_size) +                                   # all the boilerplate
    fill_scale +                                      # red↔grey↔green or grey only
    scale_y_discrete(                                 # lock in the row order
      limits = rev(panel_countries)
      ) +
    labs(title = if (!hide_x) cat_disp else NULL) +   # only show title when hide_x=FALSE
    theme(plot.title = element_text(size = plot_title_size)) +
    theme(                                            # reapply your hide_x logic
      axis.text.x.top = if (!hide_x)
        element_text(angle = 0, hjust = 0.5, size = cat_title_size, margin = margin(b = 6))
      else
        element_blank()
    )
}

build_y_label_panel <- function(panel_countries, y_label_df, y_text_size) {
  # dummy grid to set the factor levels and row spacing
  dummy <- data.frame(
    Type    = factor("dummy", levels = type_labels),
    Country = factor(rev(panel_countries), levels = rev(panel_countries))
  )
  
  ggplot(dummy, aes(x = Type, y = Country)) +
    geom_tile(fill = "transparent", color = "transparent") +
    geom_text(
      data     = y_label_df,
      aes(label = Label),
      hjust    = 0,
      vjust    = 0.35,
      nudge_y  = y_label_df$nudge,
      size     = y_text_size,
      x        = 0.1
    ) +
    scale_y_discrete(limits = rev(panel_countries)) +
    scale_x_discrete(position = "top") +
    coord_cartesian(clip = "off") +
    theme_y_strip
}


make_panel <- function(
    df,
    panel_countries,
    panel_title,
    view           = FIXED_VIEW,
    sel_categories = categories_default,
    hide_x         = FALSE,
    shade_left     = FALSE,
    wrap_titles    = FALSE,
    page2          = FALSE,
    show_y_label   = TRUE,
    label_fn       = general_category_labels,
    y_text_size    = 3,
    drop_stance_types = FALSE,
    plot_title_size    = 10,
    cat_title_size     = 10,
    tile_text_size = 4,
    strip_mult        = 2.6,
    y_lab_width    = 0.7,
    y_label_size = 10,
    blank_mult = 1,
    is_download = FALSE
) {
  # — prepare data
  heat_data <- get_heat_data(df, panel_countries, sel_categories, view)
  if (drop_stance_types) {
    heat_data <- heat_data %>%
      filter(!(Category == "Stance" & Type %in% c("1-m∆","3-m∆")))
  }
  
  # 2) set up labels and scales
  cats <- levels(droplevels(heat_data$Category))
  # what we want to see on screen
  cats_disp <- label_fn(cats)
  # wrap anything longer than ~10 characters
  if (wrap_titles) {
    cats_disp <- str_wrap(cats_disp, width = 10)
  }
  
  # 3) build one small ggplot per category; each category tile plot
  plots <- lapply(seq_along(cats), function(i) {
      build_category_plot(
        data            = filter(heat_data, Category == cats[i]),
        cat_disp        = cats_disp[i],
        panel_title     = panel_title,
        is_rate         = (cats[i] == "rates"),
        panel_countries = panel_countries,
        hide_x          = hide_x,
        plot_title_size  = plot_title_size,
        cat_title_size = cat_title_size,
        tile_text_size  = tile_text_size
      )
    })
  
  # — if nothing to plot, bail
  if (length(plots)==0) return(NULL)
  
  # 5) build the y-axis labels and nudge logic
  
  # Shortened y-axis labels
  short_countries <- recode(
    panel_countries,
    "United States"     = "US",
    "Developed Markets" = "DMs",
    "United Kingdom"    = "UK",
    "Emerging Markets"  = "EMs",
    "Russian Federation"  = "Russia",
    "Czech Republic" =  "Czech Rep.",
    "South Africa" = "S. Africa",
    "South Korea" = "S. Korea",
    .default = panel_countries
  )
  
  # how much to shift down in emerging and developed markets
  label_nudge <- if (panel_title == "Developed Markets") {
    if (page2) {
      -0.5     # ← how much to push DOWN on page 2 only;
    } else if (wrap_titles) {
      -1.45     # original “first-page” offset
    } else {
      -0.30     # original default for all other pages
    }
  } else {
    0
  }
  
  # manual tweaks
  special_down <- c("Vietnam", "Turkey")
  special_up   <- c("Mexico")
  special_up2   <- c("Thailand")
  special <- c("EMs", "Brazil", "China")
  
  # 2-column data frame ordering the countries with the labels
  y_label_df <- data.frame(
    Country = factor(rev(panel_countries), levels = rev(panel_countries)),
    Label = rev(short_countries)
  )
  
  # --- compute DM & EM extras flags and groups ------------------------------
  # detect if Dev-Markets has extras on page2
  dm_has_extras <- page2 && panel_title == "Developed Markets" &&
    length(panel_countries) > length(page2_dev_countries)
  # page 3
  dm3_has_extras <- panel_title == "Developed Markets" &&
    length(panel_countries) > length(third_dev_countries)
  
  # detect if Emerging-Markets has extras on page2
  em_has_extras <- page2 && panel_title == "Emerging Markets" &&
    length(panel_countries) > length(page2_em_countries)
  # page 3
  em3_has_extras <- panel_title == "Emerging Markets"  &&
    length(panel_countries) > length(third_em_countries)
  
  # For DM: identify which countries are "extras" (not in the base page2_dev_countries)
  dm_extras <- if (dm_has_extras) {
    setdiff(as.character(panel_countries), page2_dev_countries)
  } else {
    character(0)
  }
  # ^ same but for page 3
  dm3_extras <- if (dm3_has_extras) {
    setdiff(as.character(panel_countries), third_dev_countries)
  } else {
    character(0)
  }
  
  label_nudge_default <- label_nudge  # existing base offset
  
  # — Bump DM labels down a bit when we're in download mode only —————————————————
  if ( panel_title == "Developed Markets" && wrap_titles && blank_mult > 1 ) {
    # tune this number until it lines up (0.2 is a good starting point)
    label_nudge_default <- label_nudge_default - 0.20
  }
  
  # 6) assemble plots + labels into one cowplot grid
  y_label_df <- y_label_df %>%
    mutate(nudge = label_nudge_default) %>%
    
    mutate(nudge = case_when(
      
      # Page-1 DM original bumps 
      wrap_titles & panel_title == "Developed Markets" & Label == "Euro Area"
      ~ label_nudge_default + 0.04,
      wrap_titles & panel_title == "Developed Markets" & Label == "Germany"
      ~ label_nudge_default + 0.3,
      wrap_titles & panel_title == "Developed Markets" & Label == "Japan"
      ~ label_nudge_default + 0.5,
      wrap_titles & panel_title == "Developed Markets" & Label == "UK"
      ~ label_nudge_default + 0.75,
      wrap_titles & panel_title == "Developed Markets" & Label == "US"
      ~ label_nudge_default + 1,   # Your custom US bump - adjust this value as needed
      
      # page 1 em original bumps
      wrap_titles & panel_title == "Emerging Markets" & Label == "EMs"        
      ~ label_nudge_default + 0.1,
      wrap_titles & panel_title == "Emerging Markets" & Label == "Brazil"        
      ~ label_nudge_default - 0.04,
      wrap_titles & panel_title == "Emerging Markets" & Label == "China"        
      ~ label_nudge_default - 0.13,
      wrap_titles & panel_title == "Emerging Markets" & Label == "India"        
      ~ label_nudge_default - 0.17,
      wrap_titles && panel_title == "Emerging Markets" & Label   == "Taiwan"         
      ~ label_nudge_default - 0.27,
      
      # — general first-page DM rule, all remaining labels
      wrap_titles & panel_title == "Developed Markets" & !Label %in% 
        c("Euro Area", "Germany", "Japan", "UK", "US")
      ~ label_nudge_default - 0.20,
      
      # Page 2 original dms
      page2 & panel_title == "Developed Markets" & !dm_has_extras & Label == "DMs"
      ~ label_nudge_default - 0.4, 
      page2 & panel_title == "Developed Markets" & !dm_has_extras & Label == "Australia"
      ~ label_nudge_default - 0.3,
      page2 & panel_title == "Developed Markets" & !dm_has_extras & Label == "Canada"
      ~ label_nudge_default - 0.25,
      page2 & panel_title == "Developed Markets" & !dm_has_extras & Label == "Euro Area"
      ~ label_nudge_default - 0.17,
      page2 & panel_title == "Developed Markets" & !dm_has_extras & Label == "Germany"
      ~ label_nudge_default - 0.1,
      page2 & panel_title == "Developed Markets" & !dm_has_extras & Label == "UK"
      ~ label_nudge_default,
      page2 & panel_title == "Developed Markets" & !dm_has_extras & Label == "US"
      ~ label_nudge_default + 0.1,
      
      #page 2 original ems
      page2 & panel_title == "Emerging Markets" & !em_has_extras & Label %in% special
      ~ label_nudge_default + 0.05,
      page2 & panel_title == "Emerging Markets" & !em_has_extras & Label %in% special_down
      ~ label_nudge_default - 0.25,
      page2 & panel_title == "Emerging Markets" & !em_has_extras & Label %in% special_up
      ~ label_nudge_default - 0.05,
      page2 & panel_title == "Emerging Markets" & !em_has_extras & Label=="S. Africa"
      ~ label_nudge_default - 0.09,
      page2 & panel_title == "Emerging Markets" & !em_has_extras & Label=="S. Korea"
      ~ label_nudge_default - 0.15,
      page2 & panel_title == "Emerging Markets" & !em_has_extras & Label=="Taiwan"
      ~ label_nudge_default - 0.2,
      page2 & panel_title == "Emerging Markets" & !em_has_extras & Label %in% special_up2
      ~ label_nudge_default - 0.2,
      
      # Page2 Adjustments after extra countries added dms
      panel_title=="Developed Markets" & page2 & dm_has_extras & Label=="Australia" 
      ~ label_nudge_default - 0.4,
      panel_title=="Developed Markets" & page2 & dm_has_extras & Country %in% 
        c("Denmark","France", "Euro Area", "Finland") ~ label_nudge_default - 0.25,
      panel_title=="Developed Markets" & page2 & dm_has_extras & Country %in% 
        c("Canada") ~ label_nudge_default - 0.3,
      panel_title=="Developed Markets" & page2 & dm_has_extras & Label=="Germany" 
      ~ label_nudge_default - 0.17,
      panel_title=="Developed Markets" & page2 & dm_has_extras & Label=="Iceland" 
      ~ label_nudge_default - 0.13,
      panel_title=="Developed Markets" & page2 & dm_has_extras & Label=="Ireland" 
      ~ label_nudge_default - 0.09,
      panel_title=="Developed Markets" & page2 & dm_has_extras & Label=="Sweden"  
      ~ label_nudge_default + 0.05,
      panel_title=="Developed Markets" & page2 & dm_has_extras & Label=="Italy"       
      ~ label_nudge_default - 0.1,
      panel_title=="Developed Markets" & page2 & dm_has_extras & Country %in% 
        c("Japan")       ~ label_nudge_default - 0.02,
      panel_title=="Developed Markets" & page2 & dm_has_extras & Country %in% 
        c("Netherlands", "Norway","Portugal","Spain","Switzerland")      
      ~ label_nudge_default,
      panel_title=="Developed Markets" & page2 & dm_has_extras & Label=="UK"          
      ~ label_nudge_default + 0.15,
      panel_title=="Developed Markets" & page2 & dm_has_extras & Label=="US"          
      ~ label_nudge_default + 0.2,
      
      # page 2 EM extras
      panel_title=="Emerging Markets" & page2 & em_has_extras &
        Country %in% c("Argentina","Brazil")  ~ label_nudge_default + 0.1,
      panel_title=="Emerging Markets" & page2 & em_has_extras & Country %in% 
        c("Indonesia","Malaysia")   ~ label_nudge_default - 0.1,
      panel_title=="Emerging Markets" & page2 & em_has_extras & Country %in% 
        c("India")   ~ label_nudge_default - 0.05,
      panel_title=="Emerging Markets" & page2 & em_has_extras & Country %in% 
        c("Mexico")   ~ label_nudge_default - 0.12,
      panel_title=="Emerging Markets" & page2 & em_has_extras & Country %in% 
        c("Philippines","Poland")          ~ label_nudge_default - 0.15,
      panel_title=="Emerging Markets" & page2 & em_has_extras & Country %in% 
        c("Peru", "Russian Federation", "South Africa") ~ label_nudge_default - 0.2,
      panel_title=="Emerging Markets" & page2 & em_has_extras & Label=="Taiwan"          
      ~ label_nudge_default - 0.21,
      panel_title=="Emerging Markets" & page2 & em_has_extras & Label=="S. Korea"          
      ~ label_nudge_default - 0.2,
      panel_title=="Emerging Markets" & page2 & em_has_extras & Country %in% 
        c("Thailand","Turkey") ~ label_nudge_default - 0.25,
      panel_title=="Emerging Markets" & page2 & em_has_extras & Country %in% 
        c("Ukraine","Vietnam")         ~ label_nudge_default - 0.3,
      
      # Page-2 DM extras - specific positioning for additional countries
      panel_title=="Developed Markets" & dm_has_extras & Country %in% dm_extras
      ~ label_nudge_default - 0.25,  # Adjust to fine-tune positioning
      
      # Page-2 DM base countries when extras exist
      panel_title=="Developed Markets" & dm_has_extras & !Country %in% dm_extras
      ~ label_nudge_default - 0.4,  # Slightly less adjustment for base countries
      
      # for all *other* Dev-Markets rows on page2, keep  baseline
      panel_title == "Developed Markets" & page2
      ~ label_nudge_default,
      
      # Third page
      # originals dm
      panel_title == "Developed Markets" & drop_stance_types & !dm3_has_extras 
      & Label == "Australia"    ~ label_nudge_default - 0.65,
      panel_title == "Developed Markets" & drop_stance_types & !dm3_has_extras 
      & Country %in% c("Canada")    ~ label_nudge_default - 0.6,    
      panel_title == "Developed Markets" & drop_stance_types & !dm3_has_extras 
      & Label == "Euro Area"    ~ label_nudge_default - 0.5,
      panel_title == "Developed Markets" & drop_stance_types & !dm3_has_extras 
      & Label == "France"    ~ label_nudge_default - 0.4,
      panel_title == "Developed Markets" & drop_stance_types & !dm3_has_extras 
      & Label == "Germany"    ~ label_nudge_default - 0.3,
      panel_title == "Developed Markets" & drop_stance_types & !dm3_has_extras 
      & Country %in% c("Japan","United Kingdom")    ~ label_nudge_default - 0.2,
      panel_title == "Developed Markets" & drop_stance_types & !dm3_has_extras 
      & Label == "US"    ~ label_nudge_default - 0.1,
      
      # originals em
      panel_title == "Emerging Markets" & drop_stance_types & !em3_has_extras 
      & Country %in% c("South Korea","Taiwan", "Thailand",
                       "Turkey", "Vietnam")    ~ label_nudge_default - 0.2,
      panel_title == "Emerging Markets" & drop_stance_types & !em3_has_extras 
      & Country %in% c("Russian Federation", "South Africa")   
      ~ label_nudge_default - 0.1,
      
      # dm with extras
      panel_title == "Developed Markets" & drop_stance_types & dm3_has_extras 
      & Label == "Australia"    ~ label_nudge_default - 0.65,
      panel_title == "Developed Markets" & drop_stance_types & dm3_has_extras 
      & Label == "Canada"    ~ label_nudge_default - 0.6,
      panel_title == "Developed Markets" & drop_stance_types & dm3_has_extras 
      & Country %in% c("Denmark","Euro Area")    
      ~ label_nudge_default - 0.5,
      panel_title == "Developed Markets" & drop_stance_types & dm3_has_extras 
      & Country %in% c("Finland","France","Germany")    
      ~ label_nudge_default - 0.45,
      panel_title == "Developed Markets" & drop_stance_types & dm3_has_extras 
      & Label == "Iceland"    ~ label_nudge_default - 0.4,
      panel_title == "Developed Markets" & drop_stance_types & dm3_has_extras 
      & Country %in% c("Ireland","Italy")    ~ label_nudge_default - 0.35,
      panel_title == "Developed Markets" & drop_stance_types & dm3_has_extras 
      & Label == "Japan"    ~ label_nudge_default - 0.3,
      panel_title == "Developed Markets" & drop_stance_types & dm3_has_extras 
      & Label == "Netherlands"    ~ label_nudge_default - 0.25,
      panel_title == "Developed Markets" & drop_stance_types & dm3_has_extras 
      & Country %in% c("Norway","Portugal")    ~ label_nudge_default - 0.2,
      panel_title == "Developed Markets" & drop_stance_types & dm3_has_extras 
      & Country %in% c("Spain","Sweden","Switzerland","United Kingdom")    
      ~ label_nudge_default - 0.1,
      panel_title == "Developed Markets" & drop_stance_types & dm3_has_extras 
      & Label == "US"    ~ label_nudge_default - 0.05,
      
      # em with extras
      panel_title == "Emerging Markets" & drop_stance_types & em3_has_extras 
      & Country %in% c("Malaysia","Mexico")    
      ~ label_nudge_default - 0.1,
      panel_title == "Emerging Markets" & drop_stance_types & em3_has_extras 
      & Country %in% c("South Africa","Russian Federation")    
      ~ label_nudge_default - 0.15,
      panel_title == "Emerging Markets" & drop_stance_types & em3_has_extras 
      & Country %in% c("Peru","Philippines","Poland")    
      ~ label_nudge_default - 0.2,
      panel_title == "Emerging Markets" & drop_stance_types & em3_has_extras 
      & Country %in% c("South Korea","Taiwan",
                       "Thailand","Turkey","Ukraine","Vietnam")    
      ~ label_nudge_default - 0.25,
      
      # default: all others stay at base offset
      TRUE ~ nudge
    ))
  
  if ( panel_title=="Emerging Markets" && is_download ) {
    y_label_df <- y_label_df %>%
      mutate(nudge = case_when(
        Label == "India"  ~ label_nudge_default - 0.25,  # tune to taste
        Label == "Taiwan" ~ label_nudge_default - 0.35,  # tune to taste
        TRUE               ~ nudge
      ))
  }
  
  # 6) assemble plots + labels into one cowplot grid
  y_labels <- build_y_label_panel(
    panel_countries = panel_countries,
    y_label_df      = y_label_df,
    y_text_size     = y_text_size
  )
  
  cat_row <- plot_grid(
    plotlist   = plots,
    nrow       = 1,
    rel_widths = sapply(cats, function(ct) length(unique(heat_data$Type[heat_data$Category==ct]))),
    align      = "hv",
    axis       = "tb"
  )
  
  # 2) then stick  labels on the left
  combined <- plot_grid(
    y_labels,
    cat_row,
    nrow       = 1,
    rel_widths = c(y_lab_width, 12),  # ← now adjustable
    align      = "v",
    axis       = "lr"
  )
  
  # Adds empty spacer on far left 
  padded <- plot_grid(
    NULL,
    combined,
    nrow = 1,
    rel_widths = c(blank_mult, 15)  # tight margin
  )
  
  # compute the fraction of width taken by the left strip
  left_frac <- 1 / (1 + 15)
  
  # ---------------Draw all onto one canvas ----------
  
  # Rectangle backdrop behind Emerging Markets
  bg_strip <- NULL
  if (shade_left) {
    bg_strip <- grid::rectGrob(
      x      = left_frac/2,
      y      = unit(0.5, "npc") - unit(1.5, "pt"),  
      width  = left_frac*strip_mult,
      height = 0.99,
      gp     = grid::gpar(fill = "#f2f2f2", col = NA)
    )
  }
  
  # compose the final ggdraw()
  p <- cowplot::ggdraw() 
  if (!is.null(bg_strip)) {
    p <- p + cowplot::draw_grob(bg_strip)
  }
  p <- p + cowplot::draw_plot(padded)
  
  if (show_y_label) {
    p <- p +
      cowplot::draw_label(
        panel_title,
        x       = left_frac/2,
        y       = 0.5,
        angle   = 90,
        fontface= "bold",
        size    = y_label_size,
        hjust   = 0.5,
        vjust   = 0.5
      )
  }
  
  p
}

# select input functions
update_extra_picker <- function(id, base_vec, extra_vec, session) {
  choices <- sort(setdiff(unique(base_vec), extra_vec))
  updateSelectizeInput(session, id, choices = choices, server = TRUE)
}