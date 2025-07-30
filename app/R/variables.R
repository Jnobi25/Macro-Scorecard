# R/variables.R


# ─── Country group definitions ────────────────────────────────────────────────
developed_markets <- sort(c(
  "Australia", "Canada", "Developed Markets", "Euro Area",
  "Germany", "Japan", "United Kingdom", "United States"
))
emerging_markets <- sort(c(
  "Brazil", "China", "Emerging Markets", "India", "Indonesia",
  "Mexico", "South Africa", "South Korea", "Taiwan", "Thailand", "Turkey", "Vietnam"
))

# ─── Category and view settings ───────────────────────────────────────────────
categories_default <- c(
  "Manufacturing PMIs", "Orders-Inventories PMIs", "ip",
  "Real Retail Sales", "autos", "Services PMIs"
)
all_plot_categories <- c(
  "Broad Money", "CA.GDP", "Domestic Claims", "Exports", "Foreign.Trade.GDP",
  "Imports", "Inventories PMIs", "Loan.Deposit", "Manufacturing PMIs", "Orders PMIs", 
  "Orders-Inventories PMIs", "PPI", "Private Credit", "Private.Debt.GDP", "Real Retail Sales", "Rev.Imp",
  "Services PMIs", "Stance", "Trade.Bal.Ex.Fuel.GDP", "Trade.Bal.Fuel.GDP", "autos", "cpi", "ip", "rates"
)
categories_first <- categories_default[1:5] # for page 1
views <- c("Latest", "1 Month Ago", "3 Months Ago", "1 Year Ago")
FIXED_VIEW <- "Latest"
type_levels <- c("Value", "chg1M", "chg3M")
type_labels <- c("Latest", "1-m∆", "3-m∆")

# ─── Page-specific country lists ──────────────────────────────────────────────
dev_first_countries <- c(
  "Developed Markets", "Euro Area", "Germany",
  "Japan", "United Kingdom", "United States"
)
em_first_countries  <- c(
  "Emerging Markets", "Brazil", "China", "India", "Taiwan"
)
page2_dev_countries <- c(
  "Developed Markets",
  sort(setdiff(developed_markets, "Developed Markets"))
)
page2_em_countries  <- c(
  "Emerging Markets",
  sort(setdiff(emerging_markets,  "Emerging Markets"))
)
third_dev_countries <- c(
  "Australia", "Canada", "Euro Area", "France",
  "Germany", "Japan", "United Kingdom", "United States"
)
third_em_countries <- c(
  "Brazil", "China", "India", "Indonesia", "Mexico", 
  "Russian Federation", "South Africa", "South Korea", "Taiwan", 
  "Thailand", "Turkey", "Vietnam"
)

# ─── Financial categories for page 3 ────────────────────────────────────────
financial_categories <- c(
  "cpi", "rates", "Stance",
  "Private Credit", "Domestic Claims", "PPI"
)

third_category_map <- c(
  cpi              = "Inflation",
  rates            = "Policy Rates",
  `Private Credit` = "Private Credit Growth (YoY%)"
)

# ─── All-countries helper vectors ─────────────────────────────────────────────
all_page1_countries <- c(dev_first_countries, em_first_countries)
all_page2_countries <- c(developed_markets, emerging_markets)
all_page3_countries <- c(third_dev_countries, third_em_countries)
all_countries <- sort(
  setdiff(
    unique(
      c(all_page1_countries,
        all_page2_countries,
        all_page3_countries)),
    c("Developed Markets", "Emerging Markets", "Euro Area")
  )
)

# ─── Comparison display labels ─────────────────────────────────────────────
comparison_labels <- c(
  cpi_ppi    = "CPI vs PPI",
  ip_rrs     = "IP vs Real Retail Sales",
  fgdp_cagdp = "Trade Balance vs Current Account",
  pc_lad     = "Private Credit vs Loan to Deposit",
  im_fgdp    = "Imports vs Trade Balance",
  rt_cpi     = "Policy Rates vs Inflation"
)

category_display <- c(
  cpi                 = "CPI",
  "CA.GDP"            = "CA GDP",
  ip                  = "IP",
  "Loan.Deposit"      = "Loan Deposit",
  rates               = "Policy Rates",
  "Foreign.Trade.GDP" = "Foreign Trade GDP",
  "Private Credit"    = "Private Credit",
  Imports             = "Imports",
  PPI                 = "PPI",
  "Real Retail Sales" = "Real Retail Sales"
)

comparison_map <- list(
  cpi_ppi    = c("cpi",               "PPI"),
  ip_rrs     = c("ip",                "Real Retail Sales"),
  fgdp_cagdp = c("Foreign.Trade.GDP", "CA.GDP"),
  pc_lad     = c("Private Credit",    "Loan.Deposit"),
  im_fgdp    = c("Imports", "Foreign.Trade.GDP"),
  rt_cpi     = c("rates", "cpi")
)

# ─── Lookup maps ─────────────────────────────────────────────
view_map <- c(
  chg1M  = "1 Month Ago",
  chg3M  = "3 Months Ago",
  chg12M = "1 Year Ago"
)