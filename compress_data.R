# (a) Read the original CSV
dat <- read.csv("MB Testing Data June 2025.csv", stringsAsFactors = FALSE)

# (b) Write it out as a compressed RDS
#     “xz” gives very high compression; you can also use "gzip" or "bzip2"
saveRDS(dat, file = "data.rds", compress = "xz")

# (c) (Optional) check how big it is now:
file.info("data.rds")$size
