# Script Info -------------------------------------------------------------
# wellDepth_v1.R (Well Depth Analysis)
# Version 1
# 06/16/14
# Jon Wilkey


# Version History ---------------------------------------------------------
# v1 -Loads *.rda files, generates probability density to match measured well
#     depth for oil and gas wells, plots output, saves result to *.rda file.


# Options -----------------------------------------------------------------
# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)


# Paths -------------------------------------------------------------------
# Windows
# Prepared data directory
data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"
# Plot directory
plot_root <- "D:/Dropbox/CLEAR/DOGM Data/Plots"
# Functions directory
fin <- "D:/Dropbox/CLEAR/DOGM Data/Functions"
# Working directory
work_root <- "D:/Dropbox/CLEAR/DOGM Data"

# # Mac
# # Prepared data directory
# data_root <- "/Users/john/Dropbox/CLEAR/DOGM Data/Prepared Data"
# # Plot directory
# plot_root <- "/Users/john/Dropbox/CLEAR/DOGM Data/Plots"
# # Functions directory
# fin <- "/Users/john/Dropbox/CLEAR/DOGM Data/Functions"
# # Working directory
# work_root <- "/Users/john//Dropbox/CLEAR/DOGM Data"

setwd(work_root)


# Functions ---------------------------------------------------------------
# List of functions used in this script to be loaded here


# Libraries ---------------------------------------------------------------
library(sqldf)


# Load required data files ------------------------------------------------

# DOGM data
load(file.path(data_root, "histdata.rda"))
load(file.path(data_root, "production.rda"))

# Rename for brevity
p <- production
h <- histdata
remove(production, histdata)

# Merge datasets
m <- merge(x = p, y = h, by.x = "p_api", by.y = "h_api")

# Subset m to be just wells located in Uintah or Duchesne counties, keep only
# measured depth, well type, and unique well API
m <- sqldf("select distinct(p_api), w_well_type, h_td_md, w_county
           from m
           where w_county = 'UINTAH' or w_county = 'DUCHESNE'
           group by p_api")

# Omit outlier rows (wells with measured depth = 0 ft and depth = 64,590 ft)
m <- subset(m, subset = (m$h_td_md > 0 & m$h_td_md < 64590))

# Further subset oil wells and gas wells
m.ow <- subset(m, subset = (w_well_type == "OW"))
m.gw <- subset(m, subset = (w_well_type == "GW"))


# Well Depth Analysis -----------------------------------------------------

# PDF fit
# Density criteria
# Number of points
n <- 10^3
# Lower limit
f <- 1000

pdf.ow <- density(m.ow$h_td_md, from = f, n = n)
pdf.gw <- density(m.gw$h_td_md, from = f, n = n)

# CDF
cdf.ow <- cumsum(pdf.ow$y*diff(pdf.ow$x[1:2]))
cdf.gw <- cumsum(pdf.gw$y*diff(pdf.gw$x[1:2]))

# Normalize
cdf.ow <- cdf.ow/max(cdf.ow)
cdf.gw <- cdf.gw/max(cdf.gw)


# Plot Output -------------------------------------------------------------

# Save plots to PDF file
pdf(file.path(plot_root, "wellDepth_v1 Results.pdf"))

# Density Plots
plot(pdf.ow,
     type = "l",
     xlab = "Well Depth (ft)",
     ylab = "Probability Density",
     main = "PDF for Depth of Oil Wells")

plot(pdf.gw,
     type = "l",
     xlab = "Well Depth (ft)",
     ylab = "Probability Density",
     main = "PDF for Depth of Gas Wells")

# CDF Plots
plot(pdf.gw$x, cdf.ow,
     type = "l",
     xlab = "Well Depth (ft)",
     ylab = "Cumulative Probability Density",
     main = "CDF for Depth of Oil Wells")

plot(pdf.gw$x, cdf.gw,
     type = "l",
     xlab = "Well Depth (ft)",
     ylab = "Cumulative Probability Density",
     main = "CDF for Depth of Gas Wells")

# Close PDF file
dev.off()


# Save CDFs to *.rda file -------------------------------------------------

# Rename for export
cdf.depth.ow <- data.frame(pdf.ow$x,cdf.ow)
cdf.depth.gw <- data.frame(pdf.gw$x,cdf.gw)
names(cdf.depth.ow) <- c("x", "y")
names(cdf.depth.gw) <- c("x", "y")

save(file=file.path(data_root, "cdf_wellDepth_v1.rda"),
     list=c("cdf.depth.ow",
            "cdf.depth.gw"))