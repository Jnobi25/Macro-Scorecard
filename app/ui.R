# app/ui.R

source("R/setup.R")
source("R/variables.R")

# ─── UI definition: tabs for pages 1–4 ───────────────────────────────────────
ui <- dashboardPage(
  dashboardHeader(
    titleWidth = "260px",
    title = tagList(
      # 1) the logo
      tags$a(
        href = "https://active.williamblair.com/about-us/",   # wherever you want it to point
        target = "_blank",
        tags$img(src = "wb-logo-blue-transparent.png", height = "50px", style = "margin-right:5px;")
      ),
      # 2) the text
      span("Macro Scorecard",
           style = "vertical-align: middle; white-space: nowrap;")
    )
  ),
  
  dashboardSidebar(
    collapsed = TRUE,
    sidebarMenu(
      id = "tabs",
      menuItem("Home",           tabName = "home",    icon = icon("home")),
      menuItem("Views Heatmap",  tabName = "first",  icon = icon("th")),   # empty
      menuItem("Latest Heatmap", tabName = "second", icon = icon("th-large")),
      menuItem("Financials Heatmap",  tabName = "third",  icon = icon("chart-bar")),
      menuItem("Cometplot",  tabName = "fourth",  icon = icon("dot-circle"))
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet",
                href = "https://fonts.googleapis.com/css2?family=Montserrat&display=swap"),
      # 2) Global font + your existing navbar/sidebar styles
      tags$style(HTML(sprintf("
      /* ── GLOBAL FONT ─────────────────────────────────────────── */
      body,
      .main-header .logo,
      .main-header .navbar,
      .main-sidebar,
      .sidebar-menu li a,
      .content-wrapper,
      .right-side,
      .btn,
      .form-control,
      .selectize-input {
        font-family: 'Cambria', sans-serif !important;
      }

      /* ── NAVBAR & LOGO ───────────────────────────────────────── */
      .main-header .navbar,
      .main-header .logo {
        background-color: %s !important;  /* navy blue */
        overflow: visible !important;      /* don't clip long text */
      }
       /* ── Ensure the span never wraps or ellipsizes ──── */
      .main-header .logo span {
        white-space: nowrap !important;
      }
      /* ── SIDEBAR BACKGROUND ─────────────────────────────────── */
      .main-sidebar {
        background-color: %s !important; 
      }
      /* ── ACTIVE & HOVERED SIDEBAR ITEMS ─────────────────────── */
      .main-sidebar .sidebar-menu .active > a,
      .main-sidebar .sidebar-menu a:hover, {
        background-color: #ffffff !important;  /* white on selection/hover */
        color: %s !important;             /* navy text */
      }
      /* ── CONTENT BACKGROUND ─────────────────────────────────── */
      .content-wrapper,
      .right-side {
        background-color: #ffffff !important;
      }
      #home-container {
      background-color: %s;
      }
    ",
     wbColorList$BlairBlue,   # for navbar & logo
     wbColorList$BlairBlue,   # for sidebar
     wbColorList$BlairBlue,    # for hovered item text
     wbColorList$BlairBlue # for home screen
     ))),
     tags$style(HTML("
      @keyframes spinY {
        from { transform: rotateY(0deg); }
        to { transform: rotateY(360deg); }
      }

      .coin-spin {
        animation: spinY 2.5s linear infinite;
        transform-style: preserve-3d;
        backface-visibility: hidden;
      }
    "))
    ),
    tabItems(
      # ---- Home Page -----
      tabItem(tabName = "home",
              div(id = "home-container",
                  style = "height: 100vh; display: flex; flex-direction: column; justify-content: center; align-items: center;",
                  div(
                    style = "text-align: center; padding-top: 50px;",
                    tags$img(
                      src = "wb-logo-white-transparent.png",
                      height = "400px",
                      class = "coin-spin"
                    ),
                    h1("Macro Scorecard",
                       style = "font-family: Cambria; color: white; margin-top: 0px;")
                  )
              )
      ),
      # ——— First Page —————————————————————————————
      tabItem(tabName = "first",
              fluidPage(
                fluidRow(
                  column(6,
                         h3(views[1], align = "center",  style = "font-size: 1.6em;"),
                         plotOutput("view1_dev",
                                    height = paste0(30 * length(dev_first_countries), "px")
                         ),
                         plotOutput("view1_em",
                                    height = paste0(30 * length(em_first_countries), "px")
                         )
                  ),
                  column(6,
                         h3(views[2], align = "center",  style = "font-size: 1.6em;"),
                         plotOutput("view2_dev",
                                    height = paste0(30 * length(dev_first_countries), "px")
                         ),
                         plotOutput("view2_em",
                                    height = paste0(30 * length(em_first_countries), "px")
                         )
                  )
                ),
                
                # — bottom row: views 3 & 4 —
                fluidRow(
                  column(6,
                         h3(views[3], align = "center",  style = "font-size: 1.6em;"),
                         plotOutput("view3_dev",
                                    height = paste0(30 * length(dev_first_countries), "px")
                         ),
                         plotOutput("view3_em",
                                    height = paste0(30 * length(em_first_countries), "px")
                         )
                  ),
                  column(6,
                         h3(views[4], align = "center",  style = "font-size: 1.6em;"),
                         plotOutput("view4_dev",
                                    height = paste0(30 * length(dev_first_countries), "px")
                         ),
                         plotOutput("view4_em",
                                    height = paste0(30 * length(em_first_countries), "px")
                         )
                  )
                ),
                fluidRow(
                  style = "margin-top: 40px;",
                  column(12, align="center",
                         downloadButton("download_page1", "Download")
                  )
                
              ),
              fluidRow(
                column(12, align = "center",
                       actionLink("goto_second",
                                  "For the most recent and further information, go to the Latest Heatmap."
                       ), br(), br(),
                       actionLink("goto_third",
                                  "For a financial view, go to the Financials Heatmap."
                       ), br(), br(),
                       actionLink("goto_fourth",
                                  "For a comparison of different factors, go to the Cometplot."
                       )
                )
              )
              )
      ),
      
      # ——— Second Page (dynamic heatmaps + download) —————————————————————————
      tabItem(
        tabName = "second",
        fluidPage(
          fluidRow(
            ## ◀── Extras sidebar ──▶
            column(width = 2,
                   h4("Add/Remove countries"),
                   selectizeInput(
                     "second_extra_dev",
                     "Additional Developed Markets:",
                     choices = NULL,
                     selected = NULL,
                     multiple = TRUE,
                     options  = list(
                       placeholder = 'None',
                       plugins     = list('remove_button') 
                     )
                   ),
                   # one‐click clear button
                   actionButton("clear_extra_dev", "Clear DEV", class = "btn-xs"),
                   
                   br(), # little spacing
                   
                   selectizeInput(
                     "second_extra_em",
                     "Additional Emerging Markets:",
                     choices = NULL,
                     selected = NULL,
                     multiple = TRUE,
                     options  = list(
                       placeholder = 'None',
                       plugins     = list('remove_button')
                     )
                   ),
                   actionButton("clear_extra_em", "Clear EM", class = "btn-xs"),
                   
                   div(style = "margin-top: 20px;",
                       downloadButton("download_both", "Download", class = "btn-sm")
                   )
            ),
            
            ## ◀── Main plots ──▶
            column(width = 10,
                   shinydashboard::box(
                     width = NULL,
                     plotOutput("heatmap_dev", height = "auto"),
                     plotOutput("heatmap_em", height = "auto")
                   )
            )
          ),
          fluidRow(
            column(12, align = "center",
                   br(),
                   actionLink("goto_first",  "For a broader view, go to the View Heatmap."),
                   br(), br(),
                   actionLink("goto_third",  "For a financial view, go to the Financials Heatmap."),
                   br(), br(),
                   actionLink("goto_fourth", "For a comparison of different factors, go to the Cometplot.")
            )
          )
        )
      ),
      # —-------------------- third, financial heatmaps + download —----------
      tabItem(
        tabName = "third",
        fluidPage(
          fluidRow(
            column(width = 2,
                   h4("Add/Remove countries"),
                   selectizeInput(
                     "third_extra_dev",
                     "Additional Developed Markets:",
                     choices = NULL,
                     selected = NULL,
                     multiple = TRUE,
                     options  = list(
                       placeholder = 'None',
                       plugins     = list('remove_button')
                     )
                   ),
                   actionButton("clear_extra3_dev", "Clear DEV", class = "btn-xs"),
                   br(),
                   selectizeInput(
                     "third_extra_em",
                     "Additional Emerging Markets:",
                     choices = NULL,
                     selected = NULL,
                     multiple = TRUE,
                     options  = list(
                       placeholder = 'None',
                       plugins     = list('remove_button')
                     )
                   ),
                   actionButton("clear_extra3_em", "Clear EM", class = "btn-xs"),
                   div(style = "margin-top: 20px;",
                       downloadButton("download_third", "Download", class = "btn-sm")
                   )
            ),
            column(width = 10,
                   shinydashboard::box(
                     width = NULL,
                     plotOutput("fin_heatmap_dev", height = "auto"),
                     plotOutput("fin_heatmap_em", height = "auto")
                   )
            )
          ),
          fluidRow(
            column(12, align = "center",
                   br(),
                   actionLink("goto_first_from3",
                              "For a broader view, go to the View Heatmap."
                   ), br(), br(),
                   actionLink("goto_second_from3",
                              "For the most recent and further information, go to the Latest Heatmap."
                   ), br(), br(),
                   actionLink("goto_fourth_from3",
                              "For a comparison of different factors, go to the Cometplot."
                   )
            )
          )
        )
      ),
      # — fourth, interactive comet plot —------------------------------
      tabItem(
        tabName = "fourth",
        fluidPage(
          tags$div(
            style = "
        display: flex;
        flex-wrap: wrap;
        align-items: center;
        gap: 12px;
        margin-bottom: 16px;
      ",
            
            actionButton("select_all",  "Select all"),
            actionButton("select_none", "Clear all"),
            
            selectInput(
              "market", "Market:",
              choices  = c("Developed Markets" = "dev",
                           "Emerging Markets" = "em"),
              selected = "dev",
              width    = "180px"
            ),
            
            uiOutput("country_picker"),  # will render a selectizeInput
            
            selectInput(
              "comparison", "Category comparison:",
              choices = list(
                "Trade Balance vs Current Account"  = "fgdp_cagdp",
                "IP vs Real Retail Sales"           = "ip_rrs",
                "Private Credit vs Loan‑to‑Deposit" = "pc_lad",
                "Imports vs Trade Balance"         = "im_fgdp",
                "Policy Rates vs Inflation"         = "rt_cpi",
                "Inflation vs PPI"                  = "cpi_ppi"
              ),
              selected = "cpi_ppi",
              width    = "240px"
            ),
            
            selectInput(
              "period", "Period:",
              choices  = c("1‑Month" = "chg1M",
                           "3‑Month" = "chg3M",
                           "1‑Year"  = "chg12M"),
              selected = "chg3M",
              width    = "140px"
            )
          ),
          fluidRow(
            column(
              width = 12,
              plotlyOutput("cometPlot", width = "100%", height = "670px"),
              tags$p(
                "Single click on a country on the right to see the hover box.",
                style = "margin-top: 8px; font-size: 1em; color: #555;"
              )
          )
        ),
        fluidRow(
          column(12, align = "center",
                 br(),
                 actionLink("goto_first_from4",
                            "For a broader view, go to the View Heatmap."
                 ), br(), br(),
                 actionLink("goto_second_from4",
                            "For the most recent and further information, go to the Latest Heatmap."
                 ), br(), br(),
                 actionLink("goto_third_from4",
                            "For a financial view, go to the Financials Heatmap."
                 )
          )
        )
      )
      )
    )
    )
)