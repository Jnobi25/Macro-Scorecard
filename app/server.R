# app/server.R

source("R/setup.R")
source("R/variables.R")
source("R/data_load.R")
source("R/data_validation.R")
source("R/functions.R")
source("R/dataprep.R")
source("R/colors.R")

server <- function(input, output, session) {
  mb_data <- prepare_data()
  
  # 2) now populate those selectizeInputs that depended on mb_data
  update_extra_picker("second_extra_dev",
      mb_data$Country[mb_data$Market=="Developed Markets"],
       page2_dev_countries, session)
    update_extra_picker("second_extra_em",
      mb_data$Country[mb_data$Market=="Emerging Markets"],
      page2_em_countries, session)
    update_extra_picker("third_extra_dev",
      mb_data$Country[mb_data$Market=="Developed Markets"],
      c(third_dev_countries, "Developed Markets"), session)
    update_extra_picker("third_extra_em",
      mb_data$Country[mb_data$Market=="Emerging Markets"],
      c(third_em_countries, "Emerging Markets"), session)
  
    observe({
      updateTabItems(session, "tabs", selected = "home")
    })
    
    observe({
      hash <- session$clientData$url_hash  # e.g. "#second"
      if (!is.null(hash) && nchar(hash) > 1) {
        tab <- substring(hash, 2)  # remove "#"
        updateTabItems(session, "tabs", selected = tab)
      }
    })
    
    observeEvent(input$tabs, {
      updateQueryString(paste0("#", input$tabs), mode = "replace", session = session)
    })
# ——— Page 1: small panels —————————————————————————————
for (i in seq_along(views)) {
  local({
    idx <- i
    vv  <- views[idx]
    
    # Developed Markets
    output[[paste0("view", idx, "_dev")]] <- renderPlot({
      make_panel(
        mb_data,
        panel_countries = dev_first_countries,
        panel_title     = "Developed Markets",
        view            = vv,
        sel_categories  = categories_first,
        hide_x          = FALSE,
        shade_left      = FALSE,
        wrap_titles     = TRUE,
        show_y_label    = FALSE
      )
    })
    
    # Emerging Markets
    output[[paste0("view", idx, "_em")]] <- renderPlot({
      make_panel(
        mb_data,
        panel_countries = em_first_countries,
        panel_title     = "Emerging Markets",
        view            = vv,
        sel_categories  = categories_first,
        hide_x          = TRUE,
        shade_left      = TRUE,
        wrap_titles     = TRUE,
        show_y_label    = FALSE
      )
    })
  })
}
  build_view_panel <- function(vv) {
    # 1) DM only (no stripe)
    p_dm <- make_panel(
      mb_data,
      panel_countries = dev_first_countries,
      panel_title     = "Developed Markets",
      view            = vv,
      sel_categories  = categories_first,
      hide_x          = FALSE,
      shade_left      = FALSE,
      wrap_titles     = TRUE,
      show_y_label    = FALSE,
      plot_title_size   = 6,
      cat_title_size    = 6,
      tile_text_size = 2.6,
      y_lab_width     = 1.4,
      y_text_size    = 2.5,
      blank_mult        = 1.3,
      is_download = TRUE
    )
    # 2) EM ─ turn off shade_left, draw our own stripe below
    p_em_base <- make_panel(
      mb_data,
      panel_countries = em_first_countries,
      panel_title     = "Emerging Markets",
      view            = vv,
      sel_categories  = categories_first,
      hide_x          = TRUE,
      shade_left      = FALSE,   # disable built‑in bar
      wrap_titles     = TRUE,
      show_y_label    = FALSE,
      plot_title_size   = 6,
      cat_title_size    = 6,
      tile_text_size = 2.6,
      y_lab_width     = 1.4,
      y_text_size    = 2.5,
      blank_mult        = 1.3,
      is_download = TRUE
    )
    # overlay a grey bar behind EM panel
    p_em <- ggdraw() +
      # draw a light‐grey rectanglel
      draw_grob(
        grid::rectGrob(
          gp = grid::gpar(fill = "#f2f2f2", col = NA)
        ),
        x      = 0.005,   # 10% from left of panel
        y      = -0.01,    # vertically centered
        width  = 0.17,   # 15% of panel width
        height = 0.98,       # full panel height
        vjust  = 0        # 0=bottom, 1=top, 0.5=center
      ) +
      # lactual EM panel on top
      draw_plot(p_em_base)
    # 3) stack DM above EM, with relative heights by # of rows
    n_dm <- length(dev_first_countries)
    n_em <- length(em_first_countries)
    cowplot::plot_grid(
      p_dm, p_em,
      ncol        = 1,
      align       = "v",
      axis        = "lr",
      rel_heights = c(n_dm, n_em)
    )
  }
  
  panels_reactive <- reactive({
    lapply(views, function(vv) {
      build_view_panel(vv)    # helper that returns DM/EM stack
    })
  })
  
  full_page_reactive <- reactive({
    # grab the four DM/EM stacks
    panels <- panels_reactive()
    spacer <- ggplot() + theme_void()
    
    # 1) make little title grobs for each view
    title_grobs <- lapply(views, function(txt){
      cowplot::ggdraw() +
        cowplot::draw_label(txt,
                            fontface = "plain",
                            size     = 12,
                            x        = 0.5, hjust = 0.5)
    })
    
    # 2) row‑1 titles (Latest / 1 Month Ago)
    title_top <- cowplot::plot_grid(
      title_grobs[[1]], spacer, title_grobs[[2]],
      ncol       = 3,
      rel_widths = c(1, 0.02, 1)
    )
    # 3) row‑2 titles (3 Months Ago / 1 Year Ago)
    title_bot <- cowplot::plot_grid(
      title_grobs[[3]], spacer, title_grobs[[4]],
      ncol       = 3,
      rel_widths = c(1, 0.02, 1)
    )
    
    # 4) your existing panel rows
    top_panels <- cowplot::plot_grid(
      panels[[1]], spacer, panels[[2]],
      ncol       = 3,
      rel_widths = c(1, 0.02, 1),
      align      = "hv", axis = "tblr"
    )
    bot_panels <- cowplot::plot_grid(
      panels[[3]], spacer, panels[[4]],
      ncol       = 3,
      rel_widths = c(1, 0.02, 1),
      align      = "hv", axis = "tblr"
    )
    gap <- ggplot() + theme_void()
    
    # 5) stitch it all together: titles + panels
    cowplot::plot_grid(
      title_top,    # view 1 & 2 labels
      top_panels,   # row of panels 1 & 2
      title_bot,    # view 3 & 4 labels
      bot_panels,   # row of panels 3 & 4
      ncol        = 1,
      rel_heights = c(0.1, 1, 0.1, 1)
    )
  })
  
  output$download_page1 <- downloadHandler(
    filename = function() paste0("page1_panels_", Sys.Date(), ".png"),
    content = function(file) {
      full_page <- full_page_reactive()
      
      
      png(file, width=900, height=580, units="px", res=96)
      print(full_page)
      dev.off()
    }
  )
  
  observeEvent(input$goto_second, {
    updateTabItems(session, "tabs", selected = "second")
  })
  observeEvent(input$goto_third, {
    updateTabItems(session, "tabs", selected = "third")
  })
  observeEvent(input$goto_fourth, {
    updateTabItems(session, "tabs", selected = "fourth")
  })
  

# ——— Page 2: dynamic heatmaps + download ———————————————————
output$heatmap_dev <- renderPlot({
  # merge base + extras, then sort
  all_dev <- unique(c(page2_dev_countries, input$second_extra_dev))
  sel_dev <- c("Developed Markets", sort(setdiff(all_dev, "Developed Markets")))
  
  make_panel(
    mb_data,
    panel_countries = sel_dev,
    panel_title     = "Developed Markets",
    hide_x          = FALSE,
    shade_left      = FALSE,
    page2           = TRUE
  )
}, height = function() {
  30 * length(unique(c(page2_dev_countries, input$second_extra_dev)))
})

output$heatmap_em <- renderPlot({
  all_em <- unique(c(page2_em_countries, input$second_extra_em))
  sel_em <- c("Emerging Markets", sort(setdiff(all_em, "Emerging Markets")))
  
  make_panel(
    mb_data,
    panel_countries = sel_em,
    panel_title     = "Emerging Markets",
    hide_x          = TRUE,
    shade_left      = TRUE,
    page2           = TRUE
  )
}, height = function() {
  30 * length(unique(c(page2_em_countries, input$second_extra_em)))
})

output$download_both <- downloadHandler(
  filename = function() paste0("page2_heatmaps_", Sys.Date(), ".png"),
  content = function(file) {
    # 1) how many rows in each panel?
    n_dev <- length(unique(c(page2_dev_countries, input$second_extra_dev)))
    n_em  <- length(unique(c(page2_em_countries,  input$second_extra_em)))
    total_h <- 30 * (n_dev + n_em)    # total pixel height
    
    # 2) build the two ggplot objects just as before
    p_dev <- make_panel(
      mb_data,
      panel_countries   = c("Developed Markets", sort(setdiff(unique(c(page2_dev_countries, input$second_extra_dev)), "Developed Markets"))),
      panel_title       = "Developed Markets",
      hide_x            = FALSE,
      shade_left        = FALSE,
      page2             = TRUE,
      plot_title_size   = 5.5,
      cat_title_size    = 7,
      tile_text_size = 2.7,
      y_lab_width     = 1.2,
      y_label_size    = 8,
      blank_mult        = 1.3
    ) + theme(plot.margin = margin(t = 5, r = 5, b = 0, l = 0, unit = "pt"))
    
    p_em <- make_panel(
      mb_data,
      panel_countries   = c("Emerging Markets", sort(setdiff(unique(c(page2_em_countries, input$second_extra_em)), "Emerging Markets"))),
      panel_title       = "Emerging Markets",
      hide_x            = TRUE,
      shade_left        = TRUE,
      page2             = TRUE,
      tile_text_size = 2.7,
      strip_mult        = 4.2,
      y_lab_width     = 1.2,
      y_label_size    = 8,
      blank_mult        = 1.3
    ) + theme(plot.margin = margin(t = 0, r = 5, b = 5, l = 0, unit = "pt"))
    
    # 3) stack them *by row‐count*, not equally
    combined <- cowplot::plot_grid(
      p_dev, p_em,
      nrow       = 2,
      align      = "v",
      axis       = "lr",
      rel_heights = c(n_dev, n_em)
    )
    
    # 4) write out a PNG whose height is exactly 30px per row
    png(file,
        width  = 900,
        height = total_h,
        units  = "px",
        res    = 96
    )
    print(combined)
    dev.off()
  }
)

observeEvent(input$clear_extra_dev, {
  updateSelectizeInput(session, "second_extra_dev", selected = character(0))
})
observeEvent(input$clear_extra_em, {
  updateSelectizeInput(session, "second_extra_em", selected = character(0))
})

# ——— Page 3: financial heatmaps + download ——————————————————
output$fin_heatmap_dev <- renderPlot({
  all_dev3 <- unique(c(third_dev_countries, input$third_extra_dev))
  sel_dev3 <- sort(all_dev3)
  
  make_panel(
    mb_data,
    panel_countries   = sel_dev3,
    panel_title       = "Developed Markets",
    sel_categories    = financial_categories,
    hide_x            = FALSE,
    shade_left        = FALSE,
    wrap_titles       = FALSE,
    show_y_label      = TRUE,
    label_fn          = third_category_labels,
    drop_stance_types = TRUE
  )
}, height = function() 30 * length(unique(c(third_dev_countries, input$third_extra_dev))))

output$fin_heatmap_em <- renderPlot({
  all_em3 <- unique(c(third_em_countries, input$third_extra_em))
  sel_em3 <- sort(all_em3)
  
  make_panel(
    mb_data,
    panel_countries   = sel_em3,
    panel_title       = "Emerging Markets",
    sel_categories    = financial_categories,
    hide_x            = TRUE,
    shade_left        = TRUE,
    wrap_titles       = FALSE,
    show_y_label      = TRUE,
    label_fn          = third_category_labels,
    drop_stance_types = TRUE
  )
}, height = function() 30 * length(unique(c(third_em_countries, input$third_extra_em))))

observeEvent(input$clear_extra3_dev, {
  updateSelectizeInput(session, "third_extra_dev", selected = character(0))
})
observeEvent(input$clear_extra3_em, {
  updateSelectizeInput(session, "third_extra_em", selected = character(0))
})

output$download_third <- downloadHandler(
  filename = function() paste0("page3_heatmaps_", Sys.Date(), ".png"),
  content = function(file) {
    
    all_dev3 <- unique(c(third_dev_countries, input$third_extra_dev))
        sel_dev3 <- sort(all_dev3)
        all_em3  <- unique(c(third_em_countries, input$third_extra_em))
        sel_em3  <- sort(all_em3)
    n_dev3 <- length(unique(c(third_dev_countries, input$third_extra_dev)))
    n_em3  <- length(unique(c(third_em_countries,  input$third_extra_em)))
    total_h3 <- 30 * (n_dev3 + n_em3)    # total pixel height
    
    p_dev3 <- make_panel(
      mb_data,
      panel_countries   = sel_dev3,
      panel_title       = "Developed Markets",
      sel_categories    = financial_categories,
      hide_x            = FALSE,
      shade_left        = FALSE,
      show_y_label      = TRUE,
      label_fn          = third_category_labels,
      drop_stance_types = TRUE,
      page2             = FALSE,
      plot_title_size   = 5.5,
      cat_title_size    = 7,
      tile_text_size    = 2.7,
      y_lab_width       = 1.2,
      y_label_size      = 8,
      blank_mult        = 1.3
      ) +
      theme(plot.margin = margin(t = 10, r = 0, b = 0, l = 0, unit = "pt"))
    
    
    p_em3 <- make_panel(
      mb_data,
      panel_countries   = sel_em3,
      panel_title       = "Emerging Markets",
      sel_categories    = financial_categories,
      hide_x            = TRUE,
      shade_left        = TRUE,
      show_y_label      = TRUE,
      label_fn          = third_category_labels,
      drop_stance_types = TRUE,
      page2             = FALSE,
      plot_title_size   = 5.5,
      cat_title_size    = 7,
      tile_text_size    = 2.7,
      y_lab_width       = 1.2,
      y_label_size      = 8,
      blank_mult        = 1.3,
      strip_mult        = 4
    ) +
      theme(plot.margin = margin(t = 0, r = 5, b = 5, l = 0, unit = "pt"))

combined3 <- cowplot::plot_grid(
  p_dev3, p_em3,
  nrow        = 2,
  align       = "v",
  axis        = "lr",
  rel_heights = c(n_dev3, n_em3)
)

png(file,
    width  = 900,
    height = total_h3,
    units  = "px",
    res    = 96
)
print(combined3)
dev.off()
  }
)

# Jump from Latest Heatmap (page2) to the other three tabs
observeEvent(input$goto_first, {
  updateTabItems(session, "tabs", selected = "first")
})
observeEvent(input$goto_third, {
  updateTabItems(session, "tabs", selected = "third")
})
observeEvent(input$goto_fourth, {
  updateTabItems(session, "tabs", selected = "fourth")
})

# from page 3 → Page 1
observeEvent(input$goto_first_from3, {
  updateTabItems(session, "tabs", selected = "first")
})
observeEvent(input$goto_second_from3, {
  updateTabItems(session, "tabs", selected = "second")
})
observeEvent(input$goto_fourth_from3, {
  updateTabItems(session, "tabs", selected = "fourth")
})

  
# ——— Page 4: comet plot ——————————————————————————————
# 1) Dynamically render the “Countries” picker based on Market
output$country_picker <- renderUI({
  req(input$market)
  mname   <- if (input$market=="dev") "Developed Markets" else "Emerging Markets"
  choices <- sort(setdiff(unique(mb_data$Country[mb_data$Market==mname]),
                          c("Developed Markets","Emerging Markets","Euro Area")))
  selectizeInput("countries","Countries", choices=choices, selected=choices, 
                 multiple=TRUE, options=list(plugins="remove_button"))
})

  xcat_sel <- reactive({
    comparison_map[[ input$comparison ]][1]
  })
  ycat_sel <- reactive({
    comparison_map[[ input$comparison ]][2]
  })
  
  # ——— 1. Build & complete the wide comet‐data frame —————————————————
  # CAPTURE ZOOM / PAN EVENTS
  zoom_state <- reactiveValues(
    x_range = NULL,
    y_range = NULL
  )
  
  # reset zoom_state when the selected categories change
  observeEvent(
    list(xcat_sel(), ycat_sel(), input$period),
    {
      zoom_state$x_range <- NULL
      zoom_state$y_range <- NULL
    }
  )
  
  cometData <- reactive({
    req(xcat_sel(), ycat_sel(), input$period, input$countries)
    prev_view <- view_map[[input$period]]
    
    mb_data %>%
      filter(
        Country  %in% input$countries,
        Category %in% c(xcat_sel(), ycat_sel()),
        View     %in% c(FIXED_VIEW, prev_view),
        Type     == "Value"
      ) %>%
      distinct() %>%
      transmute(
        Country,
        Category,
        View,
        Value = Latest
      ) %>%
      complete(
        Country  = input$countries,
        Category = c(xcat_sel(), ycat_sel()),
        View     = c(FIXED_VIEW, prev_view),
        fill     = list(Value = NA)
      ) %>%
      pivot_wider(
        names_from  = View,
        values_from = Value,
        values_fn = list(Value = first)
      ) %>%
      rename(
        start = !!prev_view,
        end   = Latest
      ) %>%
      pivot_wider(
        id_cols     = Country,
        names_from  = Category,
        values_from = c(start, end),
        names_sep   = "_",
        values_fn   = list(start = first, end = first)
        
      )
  })
  
  toggled <- reactiveVal(character())
  
  # single click handler
  observeEvent(input$legend_clicked, {
    clicked_country <- input$legend_clicked
    if (is.null(clicked_country) || clicked_country == "") return()
    
    # Toggle the country in the toggled list
    current_toggled <- toggled()
    if (clicked_country %in% current_toggled) {
      toggled(setdiff(current_toggled, clicked_country))
    } else {
      toggled(c(current_toggled, clicked_country))
    }
  })
  
  # double click handler
  observeEvent(input$legend_double_clicked, {
    clicked_country <- input$legend_double_clicked
    if (is.null(clicked_country) || clicked_country == "") return()
    
    # Update the countries selectize input to show only the double-clicked country
    updateSelectizeInput(session, "countries", selected = clicked_country)
    
    # Clear any toggled annotations since we're changing the country selection
    toggled(character())
  })
  
  # ——— Render the comet‐plot itself ————————————————————————
  output$cometPlot <- renderPlotly({
    raw_dat <- req(cometData())
    # filter out rows missing any start or end value
    dat <- raw_dat %>% filter(
      !is.na(.data[[paste0("start_", xcat_sel())]]) &
        !is.na(.data[[paste0("start_", ycat_sel())]]) &
        !is.na(.data[[paste0("end_",   xcat_sel())]]) &
        !is.na(.data[[paste0("end_",   ycat_sel())]])
    )
    
    # if no data remain, show empty placeholder
    if (nrow(dat) == 0) {
      return(
        plotly_empty(type = "scatter", mode = "none", source = "cometPlot") %>%
          event_register("plotly_relayout") %>%
          layout(title = "No data available for selected countries and categories")
      )
    }
    
    p <- plot_ly(source = "cometPlot") %>%
      event_register("plotly_legendclick") %>%
      event_register("plotly_relayout")
    
    # call your reactives once:
    xcat <- xcat_sel()   # e.g. "chg3M"
    ycat <- ycat_sel()
    
    # create hover‐text columns up front
    dat <- dat %>%
      mutate(
        hover_text_start = paste0(
          "Country: ", Country,
          "<br>", xcat, " (start): ", ifelse(is.na(.data[[paste0("start_",xcat)]]), "N/A",
                                             round(.data[[paste0("start_",xcat)]], 1)),
          "<br>", ycat, " (start): ", ifelse(is.na(.data[[paste0("start_",ycat)]]), "N/A",
                                             round(.data[[paste0("start_",ycat)]], 1))
        ),
        hover_text_end = paste0(
          "Country: ", Country,
          "<br>", xcat, " (end): ",   ifelse(is.na(.data[[paste0("end_",xcat)]]), "N/A",
                                             round(.data[[paste0("end_",xcat)]], 1)),
          "<br>", ycat, " (end): ",   ifelse(is.na(.data[[paste0("end_",ycat)]]), "N/A",
                                             round(.data[[paste0("end_",ycat)]], 1))
        )
      )
    
    # Filter out rows with missing data
    dat <- dat %>%
      filter(
        !is.na(.data[[paste0("start_",xcat)]]) &
          !is.na(.data[[paste0("start_",ycat)]]) &
          !is.na(.data[[paste0("end_",xcat)]]) &
          !is.na(.data[[paste0("end_",ycat)]])
      )
    
    # Calculate data ranges
    sx <- c(dat[[paste0("start_",xcat)]], dat[[paste0("end_",xcat)]])
    sy <- c(dat[[paste0("start_",ycat)]], dat[[paste0("end_",ycat)]])
    base_x <- range(sx, na.rm = TRUE)
    base_y <- range(sy, na.rm = TRUE)
    
    # Ranges for trapezoid geometry vs. axes
    geom_x <- if (!is.null(zoom_state$x_range)) zoom_state$x_range else base_x
    geom_y <- if (!is.null(zoom_state$y_range)) zoom_state$y_range else base_y
    axis_x <- base_x; axis_y <- base_y
    
    # Padding for axis display
    pad_x <- diff(axis_x) * 0.05
    pad_y <- diff(axis_y) * 0.05
    
    # Get container pixels with fallback
    pw <- session$clientData$output_cometPlot_width
    ph <- session$clientData$output_cometPlot_height
    if (is.null(pw) || pw == 0) pw <- 800
    if (is.null(ph) || ph == 0) ph <- 600
    
    # Conversion functions: data <-> pixels
    x2px <- function(x) if (diff(geom_x) == 0) pw/2 else (x - geom_x[1]) / diff(geom_x) * pw
    y2px <- function(y) if (diff(geom_y) == 0) ph/2 else (y - geom_y[1]) / diff(geom_y) * ph
    px2x <- function(xp) if (diff(geom_x) == 0) geom_x[1] else xp / pw * diff(geom_x) + geom_x[1]
    px2y <- function(yp) if (diff(geom_y) == 0) geom_y[1] else yp / ph * diff(geom_y) + geom_y[1]
    
    # Fixed pixel widths for trapezoids
    w0_px <- 8   # start width
    w1_px <- 16  # end width
    
    # Start Plotly and create color palette
    n_countries <- nrow(dat)
    base_cols   <- brewer.pal(min(n_countries, 8), "Set2")   # up to 8 shades
    if (n_countries > 8) {
      # interpolate if you have more than 8
      cols <- colorRampPalette(base_cols)(n_countries)
    } else {
      cols <- base_cols
    }
    names(cols) <- dat$Country
    cols <- scales::alpha(cols, 1)
    
    # bring ISOCode in
    iso_map <- mb_data %>% distinct(Country, ISOCode)
    dat     <- dat %>% left_join(iso_map, by="Country")
    
    # Draw trapezoids and markers for each country
    for (country in dat$Country) {
      row <- dat[dat$Country == country, ]
      x0 <- row[[paste0("start_",xcat)]]
      y0 <- row[[paste0("start_",ycat)]]
      x1 <- row[[paste0("end_",  xcat)]]
      y1 <- row[[paste0("end_",  ycat)]]
      
      # Skip if start and end are the same point
      if (x0 == x1 && y0 == y1) next
      
      # Convert to pixels
      xp0 <- x2px(x0); yp0 <- y2px(y0)
      xp1 <- x2px(x1); yp1 <- y2px(y1)
      
      # Calculate perpendicular unit vector in pixel space
      L_px <- sqrt((xp1-xp0)^2 + (yp1-yp0)^2)
      
      # Skip if length is too small
      if (L_px < 1e-6) next
      
      nx_px <- -(yp1 - yp0) / L_px
      ny_px <-  (xp1 - xp0) / L_px
      
      # Calculate pixel offsets
      dxp0 <- nx_px * w0_px/2; dyp0 <- ny_px * w0_px/2
      dxp1 <- nx_px * w1_px/2; dyp1 <- ny_px * w1_px/2
      
      # Convert back to data coordinates
      x0a <- px2x(xp0 + dxp0);  y0a <- px2y(yp0 + dyp0)
      x0b <- px2x(xp0 - dxp0);  y0b <- px2y(yp0 - dyp0)
      x1a <- px2x(xp1 + dxp1);  y1a <- px2y(yp1 + dyp1)
      x1b <- px2x(xp1 - dxp1);  y1b <- px2y(yp1 - dyp1)
      
      
      # Add trapezoid trace
      p <- p %>% add_trace(
        x = c(x0a, x0b, x1b, x1a, x0a),  # Close the polygon
        y = c(y0a, y0b, y1b, y1a, y0a),  # Close the polygon
        type = "scatter",
        mode = "lines",
        fill = "toself",
        fillcolor = cols[country],
        line = list(color = cols[country], width = 1),
        opacity = 0.6,
        showlegend = FALSE,
        legendgroup = country,
        hoverinfo = "skip"
      )
      
      # Add start marker
      p <- p %>% add_markers(
        x = x0, y = y0,
        marker = list(
          symbol = "circle",
          color = "white",
          line = list(color = cols[country], width = 2),
          size = 4
        ),
        hovertemplate = paste0(row$hover_text_start, "<extra></extra>"),
        showlegend = FALSE,
        legendgroup = country
      )
      
      # Add end marker
      p <- p %>% add_markers(
        x           = row[[paste0("end_", xcat)]],
        y           = row[[paste0("end_", ycat)]],
        marker = list(color = cols[country], size = 12),
        name = country,
        legendgroup = country,
        hovertemplate = paste0(row$hover_text_end, "<extra></extra>"),
        showlegend = TRUE,
        legendgroup = country
      )
    }
    
    # now add ISO labels at end points
    ex_vals <- dat[[ paste0("end_", xcat) ]]
    ey_vals <- dat[[ paste0("end_", ycat) ]]
    ctys    <- dat$ISOCode
    
    p <- p %>% add_text(
      x            = ex_vals,
      y            = ey_vals,
      text         = paste0("<b>", ctys, "</b>"),
      textposition = "top right",
      textfont     = list(color = "black", size = 12),
      showlegend   = FALSE,
      hoverinfo    = "none"
    )
    
    
    # Now append annotations for every “clicked” country:
    valid_countries <- intersect(toggled(), dat$Country)
    
    for(country in valid_countries) {
      row <- dat[dat$Country == country, ]
      # skip if we didn’t find exactly one matching row
      if (nrow(row) != 1) next
      
      # start‑point annotation
      p <- p %>% add_annotations(
        x           = row[[paste0("start_", xcat)]],
        y           = row[[paste0("start_", ycat)]],
        text        = row$hover_text_start,
        bgcolor     = "lightyellow",
        bordercolor = "gray50",
        showarrow   = TRUE,
        arrowcolor  = cols[country],
        font        = list(size = 12, color = "navy")
      )
      
      # end‑point annotation
      p <- p %>% add_annotations(
        x           = row[[paste0("end_",   xcat)]],
        y           = row[[paste0("end_",   ycat)]],
        text        = row$hover_text_end,
        bgcolor     = "lightyellow",
        bordercolor = "gray50",
        showarrow   = TRUE,
        arrowcolor  = cols[country],
        font        = list(size = 12, color = "navy")
      )
    }
    
    # Update zoom state when user zooms/pans
    observeEvent(
      event_data("plotly_relayout", source="cometPlot"),
      {
        zd <- event_data("plotly_relayout", source="cometPlot")
        # if user did a manual zoom/pan:
        if (!is.null(zd[["xaxis.range[0]"]])) {
          zoom_state$x_range <- c(zd[["xaxis.range[0]"]], zd[["xaxis.range[1]"]])
          zoom_state$y_range <- c(zd[["yaxis.range[0]"]], zd[["yaxis.range[1]"]])
        }
        # if user clicked “Auto Scale”:
        if (!is.null(zd[["xaxis.autorange"]]) && zd[["xaxis.autorange"]]) {
          zoom_state$x_range <- NULL
          zoom_state$y_range <- NULL
        }
      },
      ignoreNULL = TRUE   # don’t fire on the initial NULL
    )
    
    # Determine the plot ranges respecting user zoom
    plot_range_x <- if (!is.null(zoom_state$x_range)) zoom_state$x_range else c(axis_x[1] - pad_x, axis_x[2] + pad_x)
    plot_range_y <- if (!is.null(zoom_state$y_range)) zoom_state$y_range else c(axis_y[1] - pad_y, axis_y[2] + pad_y)
    
    # Invisible legend entries for missing countries
    miss <- setdiff(input$countries, dat$Country)
    mid_x <- mean(plot_range_x); mid_y <- mean(plot_range_y)
    for(country in miss) {
      p <- p %>% add_markers(
        x = mid_x, y = mid_y,
        marker = list(color = cols[country], opacity = 0, size = 1),
        name = paste0(country, " (no data)"),
        legendgroup = country,
        hoverinfo = "none"
      )
    }
    
    # pull the full “A vs B” label from your comparison dropdown
    main_title <- comparison_labels[[ input$comparison ]]
    # split it into the two parts around “ vs ”
    comps     <- strsplit(main_title, " vs ")[[1]]
    x_label   <- comps[1]
    y_label   <- comps[2]
    
    # build title string
    sub_title  <- paste0("Change vs. ", view_map[[input$period]])
    
    # Layout
    p %>% layout(
      margin     = list(t = 80, b = 40, l = 60, r = 40),
      uirevision = "comet",
      dragmode = "pan",
      xaxis = list(
        title = x_label,
        range    = plot_range_x,
        zeroline = TRUE,
        zerolinecolor = "gray",
        zerolinewidth = 1,
        showgrid = TRUE,
        gridcolor = "lightgray"
      ),
      yaxis = list(
        title = y_label,
        range    = plot_range_y,
        zeroline = TRUE,
        zerolinecolor = "gray",
        zerolinewidth = 1,
        showgrid = TRUE,
        gridcolor = "lightgray"
      ),
      hoverlabel = list(
        bgcolor     = "lightyellow",
        bordercolor = "gray50",
        font        = list(color = "navy", size = 12)
      ),
      legend = list(title = list(text = "Country"),
                    itemclick = FALSE
      )
    ) %>%
      # main header annotation (bold, larger)
      add_annotations(
        x         = 0,      xref    = "paper", xanchor = "left",
        y         = 1.08,   yref    = "paper", yanchor = "bottom",
        text      = main_title,
        showarrow = FALSE,
        font      = list(size = 16, color = "black")
      ) %>%
      # subheader annotation (smaller, gray)
      add_annotations(
        x         = 0,      xref    = "paper", xanchor = "left",
        y         = 1.02,   yref    = "paper", yanchor = "bottom",
        text      = sub_title,
        showarrow = FALSE,
        font      = list(size = 12, color = "gray50")
      ) %>%
      # html for clicking on legend countries
      htmlwidgets::onRender("
    function(el, x) {
      var clickTimeout;
      var clickCount = 0;
      
      el.on('plotly_legendclick', function(d) {
        var country = d.data[d.curveNumber].name;
        // Remove ' (no data)' suffix if present
        country = country.replace(/ \\(no data\\)$/, '');
        
        clickCount++;
        
        if (clickCount === 1) {
          // Single click - wait to see if there's a double click
          clickTimeout = setTimeout(function() {
            // This is a single click
            Shiny.setInputValue('legend_clicked', country, {priority: 'event'});
            clickCount = 0;
          }, 300); // 300ms delay to detect double click
        } else if (clickCount === 2) {
          // Double click
          clearTimeout(clickTimeout);
          Shiny.setInputValue('legend_double_clicked', country, {priority: 'event'});
          clickCount = 0;
        }
        
        return false;
      });
    }
  ") %>%
      config(
        displaylogo = FALSE,
        modeBarButtons = list(
          list(
            "zoomIn2d",
            "zoomOut2d",
            "pan2d",
            "autoScale2d",
            "toImage",
            "toggleSpikelines",          # spike-line toggle
            "hoverClosestCartesian",     # show closest on hover
            "hoverCompareCartesian"      # compare on hover
          )
        ),
        responsive = TRUE 
      ) %>%
      event_register("plotly_relayout")
  })
  
  
  # ——— Select All / Clear All buttons ————————————————————————
  observeEvent(input$select_all, {
    updateSelectizeInput(session, "countries",
                         selected = sort(unique(mb_data$Country))
    )
  })
  observeEvent(input$select_none, {
    updateSelectizeInput(session, "countries",
                         selected = character(0))
  })
  
  # page 4 → page 1
  observeEvent(input$goto_first_from4, {
    updateTabItems(session, "tabs", selected = "first")
  })
  observeEvent(input$goto_second_from4, {
    updateTabItems(session, "tabs", selected = "second")
  })
  observeEvent(input$goto_third_from4, {
    updateTabItems(session, "tabs", selected = "third")
  })
}