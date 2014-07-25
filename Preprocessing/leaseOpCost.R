# Script Info -------------------------------------------------------------
# leaseOpCost_v1.R (Least squares analysis of EIA Lease Operating Cost Data)
# Version 1
# 06/08/14
# Jon Wilkey


# Version History ---------------------------------------------------------
# v1 -Loads *.csv files with EIA lease cost data, prepares data for analysis
#     with lm(), runs lm(), plots output, and saves lm fit objects.


# Options -----------------------------------------------------------------
# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)


# Paths -------------------------------------------------------------------
# Windows
# Raw data directory
raw_root <- "D:/Dropbox/CLEAR/DOGM Data/Raw Data"
# Prepared data directory
data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"
# Plot directory
plot_root <- "D:/Dropbox/CLEAR/DOGM Data/Plots"
# Working directory
work_root <- "D:/Dropbox/CLEAR/DOGM Data"

# # Mac
# # Raw data directory
# raw_root <- "/Users/john/Dropbox/CLEAR/DOGM Data/Raw Data"
# # Prepared data directory
# data_root <- "/Users/john/Dropbox/CLEAR/DOGM Data/Prepared Data"
# # Plot directory
# plot_root <- "/Users/john/Dropbox/CLEAR/DOGM Data/Plots"
# # Working directory
# work_root <- "/Users/john//Dropbox/CLEAR/DOGM Data"

# Set working directory
setwd(work_root)


# Functions ---------------------------------------------------------------


# Libraries ---------------------------------------------------------------
library(scatterplot3d)

# Load required data files ------------------------------------------------

# Load EIA *.csv files
LOC.oil <- read.csv(file.path(raw_root, "LOC oil.csv"))
LOC.gas <- read.csv(file.path(raw_root, "LOC gas.csv"))


# Process raw data --------------------------------------------------------

# All that we need to do is rename columns
names(LOC.oil) <- c("year", "CPI", "nominal.price", "real.price", "depth", "cost")
names(LOC.gas) <- c("year", "CPI", "nominal.price", "real.price", "prodrate", "depth", "cost")

# Run lm() ----------------------------------------------------------------

fit.LOC.oil <- lm(formula = cost ~ real.price + depth,
              data = LOC.oil)
fit.LOC.gas <- lm(formula = cost ~ real.price + depth + prodrate,
              data = LOC.gas)

# Plot results ------------------------------------------------------------
# Save plots to PDF file
pdf(file.path(plot_root, "leaseOpCost_v1 Results.pdf"))

oil <-scatterplot3d(x = LOC.oil$real.price,
                    y = LOC.oil$depth,
                    z = LOC.oil$cost,
                    pch=16,
                    highlight.3d=TRUE,
                    type="h",
                    xlab = "Oil Price (2009 $/bbl)",
                    ylab = "Depth (ft)",
                    zlab = "Operating Cost (2009 $/yr)",
                    main="Annual Lease Operating Costs for Oil Wells")
oil$plane3d(fit.LOC.oil)

pairs(~cost + prodrate + depth + real.price,
      data = LOC.gas, 
      main = "Gas Lease Operating Cost Scatterplot Matrix")

dev.off()


# Save results ------------------------------------------------------------

save(file=file.path(data_root, "leaseOpCost_v1.rda"),
     list=c("fit.LOC.oil",
            "fit.LOC.gas"))