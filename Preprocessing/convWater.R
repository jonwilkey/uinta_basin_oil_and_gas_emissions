# Script Info -------------------------------------------------------------
# convWater.R (Conventional Oil and Gas Water Model)
# Jon Wilkey


# Inputs ------------------------------------------------------------------

# 


# Outputs -----------------------------------------------------------------

# 


# Description -------------------------------------------------------------

# blah


# Options -----------------------------------------------------------------

# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)


# Paths -------------------------------------------------------------------

# Windows
raw_root  <- "D:/Dropbox/CLEAR/DOGM Data/Raw Data"             # Raw data
data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"        # Prepared data
plot_root <- "D:/Dropbox/CLEAR/DOGM Data/Plots"                # Plots
fin <-       "C:/Users/Jon/Documents/R/ub_oilandgas/Functions" # Functions
work_root <- "C:/Users/Jon/Documents/R/ub_oilandgas/"          # Working dir.

# # Mac
# raw_root  <- "/Users/john/Dropbox/CLEAR/DOGM Data/Raw Data"
# data_root <- "/Users/john/Dropbox/CLEAR/DOGM Data/Prepared Data"
# plot_root <- "/Users/john/Dropbox/CLEAR/DOGM Data/Plots"
# fin <-       "/Users/john/Documents/ub_oilandgas/ub_oilandgas/Functions"
# work_root <- "/Users/john/Documents/ub_oilandgas/ub_oilandgas"

# Set working directory
setwd(work_root)


# Functions ---------------------------------------------------------------

# List of functions used in this script to be loaded here
flst <- file.path(fin, c("write_excel.R",
                         "diffMonPOSIX.R"))

# Load each function in list
for (f in flst) source(f)


# Libraries ---------------------------------------------------------------

library(sqldf)
library(zoo)
library(foreign)
library(data.table)


# Load required data files ------------------------------------------------

# Load *.rda files
load(file.path(raw_root, "uic_projects.rda"))
load(file.path(raw_root, "uic_wells.rda"))
load(file.path(raw_root, "project_volumes.rda"))
load(file.path(raw_root, "disposal.rda"))
load(file.path(data_root, "fieldata.rda"))
load(file.path(data_root, "production.rda"))
load(file.path(data_root, "histdata.rda"))
load(file.path(data_root, "frackWater.rda"))
load(file.path(data_root, "qtr_evap.rda"))

# Rename some dataframes for brevity
d <- disposal
v <- project_volumes
p <- production
h <- histdata
remove(disposal, project_volumes, production, histdata)


# Process data - Water Injected as Time Series ----------------------------

# Select distinct project numbers located in Uintah and Duchesne counties
project.num <- na.omit(sqldf("select distinct(project_unit_number)
                             from uic_wells
                             where county = 'UINTAH' or county = 'DUCHESNE'"))

# Create a sequence of dates from start of modeling period to final date in
# dataframe v (2013-11-01)
date <- seq(from = as.Date("1993-05-01"), to = as.Date("2013-11-01"),
            by = "months")

# Create matrix of water injections with col = project numbers and rows = dates,
# then sum across rows to get time series of water injected in Basin
inj <- matrix(0, ncol = length(project.num[,1]), nrow = length(date))
for (i in 1:length(date)) {
  temp <- subset(v, report_date == date[i])
  for (j in 1:length(project.num[,1])) {
    temp1 <- subset(temp, project_number == project.num[j,1])
    if (length(temp1$volume_liquid) == 1) {
      if (is.na(temp1$volume_liquid) ==  FALSE) {
        inj[i,j] <- temp1$volume_liquid
      }
    }
  }
}
inj <- rowSums(inj)


# Process data - Water Disposal as Time Series ----------------------------

# Disposal data is listed by API #, so to select all data located in Uintah and 
# Duchesne counties start by defining list of API #s located in each county (013
# - Duchesne, 047 - Uintah)

d.api <- c("43-047-15592", "43-047-15594", "43-047-15681", "43-047-15695",
           "43-047-15880", "43-047-20060", "43-047-20193", "43-047-30058",
           "43-047-30359", "43-047-30384", "43-047-30419", "43-047-30534",
           "43-047-30566", "43-047-30973", "43-047-31615", "43-047-31822",
           "43-047-31915", "43-047-31945", "43-047-31996", "43-047-32070",
           "43-047-32351", "43-047-32747", "43-047-33026", "43-047-33116",
           "43-047-33406", "43-047-33449", "43-047-33671", "43-047-33709",
           "43-047-35362", "43-047-36389", "43-047-36390", "43-047-37836",
           "43-047-38434", "43-047-38875", "43-047-40017", "43-047-40253",
           "43-047-40255", "43-047-50290", "43-047-50300", "43-047-50301",
           "43-047-50302", "43-047-50303", "43-047-50304", "43-047-50806",
           "43-047-51396", "43-047-53004", "43-013-15122", "43-013-15321",
           "43-013-20255", "43-013-30009", "43-013-30021", "43-013-30031",
           "43-013-30038", "43-013-30056", "43-013-30060", "43-013-30097",
           "43-013-30121", "43-013-30146", "43-013-30224", "43-013-30266",
           "43-013-30272", "43-013-30289", "43-013-30311", "43-013-30337",
           "43-013-30340", "43-013-30346", "43-013-30361", "43-013-30362",
           "43-013-30367", "43-013-30368", "43-013-30371", "43-013-30372",
           "43-013-30388", "43-013-30389", "43-013-30391", "43-013-30478",
           "43-013-30738", "43-013-30879", "43-013-30920", "43-013-30971",
           "43-013-31217", "43-013-32308", "43-013-32747", "43-013-32748",
           "43-013-32893", "43-013-32975", "43-013-33388", "43-013-50646",
           "43-013-50753", "43-013-50781", "43-013-50921")

# Put reporting dates in dataframe d into YYYY-MM-DD format
d$Report.Date <- as.Date(d$Report.Date, "%m/%d/%Y")

# Create matrix of water disposal with col = api numbers and rows = dates,
# then sum across rows to get time series of water disposal in Basin
disp <- matrix(0, ncol = length(d.api), nrow = length(date))
for (i in 1:length(date)) {
  temp <- subset(d, Report.Date == date[i])
  for (j in 1:length(d.api)) {
    temp1 <- subset(temp, API.Well.Number == d.api[j])
    if (length(temp1$Volume.Liquid) == 1) {
      if (is.na(temp1$Volume.Liquid) ==  FALSE) {
        disp[i,j] <- temp1$Volume.Liquid
      }
    }
  }
}
disp <- rowSums(disp)


# Process data - Water Produced as Time Series ----------------------------

# Create list of wells that have dates of first production
h.first <- na.omit(sqldf("select distinct(h_api), h_first_prod from h"))

# Merge p and h so that all wells have completion dates (for use in average well
# age calculation in next step)
m <- merge(p, h.first, by.x = "p_api", by.y = "h_api")

# Subset m to just wells located in Uintah and Duchesne
m <- subset(m, w_county == "UINTAH" | w_county == "DUCHESNE")

# Convert to data.table, set table keys, and use Michael's function to determine
# number of months since well first produced (i.e. well age), define as "time"
m <- data.table(m)
setkey(m, p_api, p_rpt_period, h_first_prod)
m[, time := 1 + diffMonPOSIX(first = min(h_first_prod), last = p_rpt_period),
  by = "p_api"]
# Convert back to dataframe
m <- as.data.frame(m)

# Determine time series for water production from m
prod.ow <- sqldf("select p_rpt_period, sum(p_water_prod)
                 from m
                 where w_well_type = 'OW'
                 group by p_rpt_period")
prod.gw <- sqldf("select p_rpt_period, sum(p_water_prod)
                 from m
                 where w_well_type = 'GW'
                 group by p_rpt_period")
# Drop NA values and last month (p has data through 2013-12-01), drop dates
prod.ow <- prod.ow[2:248,2]
prod.gw <- prod.gw[2:248,2]
# Convert to numeric variable type
prod.ow <- as.numeric(prod.ow)
prod.gw <- as.numeric(prod.gw)


# Process data - Average Well Age as Time Series --------------------------

# Use SQL statement to determine average well age
age <- sqldf("select p_rpt_period, avg(time)
             from m
             group by p_rpt_period")
# Drop NA values and last month, drop dates
age <- age[2:248,2]
# Convert to numeric variable type
age <- as.numeric(age)


# Process data - Oil production as Time Series ----------------------------

# Use SQL statement to determine total oil production as series of time
oil <- sqldf("select p_rpt_period, sum(p_oil_prod)
             from m
             group by p_rpt_period")
# Drop NA values and last month, drop dates
oil <- oil[2:248,2]
# Convert to numeric variable type
oil <- as.numeric(oil)



# Process data - Gas production as Time Series ----------------------------

# Use SQL statement to determine total oil production as series of time
gas <- sqldf("select p_rpt_period, sum(p_gas_prod)
             from m
             group by p_rpt_period")
# Drop NA values and last month, drop dates
gas <- gas[2:248,2]
# Convert to numeric variable type
gas <- as.numeric(gas)

# Fit Fracking Water Usage Model ------------------------------------------

# Produced water - linear regression
prod.all <- prod.ow+prod.gw
prod.water.lm <- lm(prod.all~oil)


# Disposal well injection water - linear model
disp.water.lm <- lm(disp~prod.all)


# Evaporation pond water - CDF

# Get PDF
evap.pdf <- density(qtr.evap$evap/qtr.evap$prod)

# Calculate cumulative sum
evap.cdf <- cumsum(evap.pdf$y*diff(evap.pdf$x[1:2]))

# Normalize
evap.cdf <- evap.cdf/max(evap.cdf)

# Create data.frame for export
evap.cdf <- data.frame(evap.pdf$x, evap.cdf)
names(evap.cdf) <- c("ratio","CDF")


# Fracking water - CDF

# Select only entries from fracfocus.org data.frame which are not NA
fw <- frackWater[which(!is.na(frackWater$water)),]

# Drop the single row which has 1 gal of frac water usage
fw <- fw[-which(fw$water == 1),]

# Get subsets for oil wells and gas wells
fw.ow <- fw[which(fw$wellType == "OW"),]
fw.gw <- fw[which(fw$wellType == "GW"),]

# Convert water units from gal to bbl
fw.ow$water <- fw.ow$water/42
fw.gw$water <- fw.gw$water/42

# Get PDFs
fw.ow.pdf <- with(fw.ow, density(water, from = min(water), to = max(water)))
fw.gw.pdf <- with(fw.gw, density(water, from = min(water), to = max(water)))

# Calculate cumulative sum
fw.ow.cdf <- cumsum(fw.ow.pdf$y*diff(fw.ow.pdf$x[1:2]))
fw.gw.cdf <- cumsum(fw.gw.pdf$y*diff(fw.gw.pdf$x[1:2]))

# Normalize
fw.ow.cdf <- fw.ow.cdf/max(fw.ow.cdf)
fw.gw.cdf <- fw.gw.cdf/max(fw.gw.cdf)

# Create data.frame for export
fw.ow.cdf <- data.frame(fw.ow.pdf$x, fw.ow.cdf)
fw.gw.cdf <- data.frame(fw.gw.pdf$x, fw.gw.cdf)
names(fw.ow.cdf) <- c("frackwater", "CDF")
names(fw.gw.cdf) <- c("frackwater", "CDF")


# Flooding water - CDF

# Get PDF for ratio (flooding water):(oil production)
flood.pdf <- density(inj/oil, from  = min(inj/oil), to = max(inj/oil))

# Calculate CDF
flood.cdf <- cumsum(flood.pdf$y*diff(flood.pdf$x[1:2]))

# Normalize
flood.cdf <- flood.cdf/max(flood.cdf)

# Create data.frame for export
flood.cdf <- data.frame(flood.pdf$x, flood.cdf)
names(flood.cdf) <- c("ratio", "CDF")


# Drilling water - CDF

# Need to do some data gathering from DOGM for this. There is too much 
# variability in reports on water usage for drilling in the literature to be 
# confident that there is any "national" average water usage for drilling. In
# the meantime, assume that 10% of all water used for to drill and fracture a
# well is used in drilling.


# Data export -------------------------------------------------------------

# Save to file
save(file=file.path(data_root, "water_models.rda"),
     list=c("prod.water.lm",
            "disp.water.lm",
            "evap.cdf",
            "fw.ow.cdf",
            "fw.gw.cdf",
            "flood.cdf"))

# Plot Output -------------------------------------------------------------

# Save to pdf
pdf("convWater_v3 Results.pdf")

# Water balance terms
plot(date, prod,
     type = "l",
     lty = 1,
     col = "black",
     xlab = "Date",
     ylim = c(0,10e6),
     ylab = "Water Production (bbl per month)",
     main = "Water Balance Terms as f(time)")
lines(date, inj, col = "red")
lines(date, disp, col = "blue")
legend("topleft",
       c("Produced", "Injected", "Disposed"),
       lty = c(1,1,1),
       col = c("black", "red", "blue"))

# Water Balance Ratio
bal <- (prod-inj-disp)/oil
plot(date, bal,
     type = "l",
     lty = 1,
     col = "black",
     xlab = "Date",
     ylab = "Water:Oil Ratio (volume basis)",
     main = "Water Balance Ratio as f(time)")

# Water Balance Ratio as f(well age)
plot(age, bal,
     type = "p",
     pch = 1,
     col = "black",
     xlab = "Average Well Age (months)",
     ylab = "Water:Oil Ratio (volume basis)",
     main = "Water Balance Ratio vs. Average Well Age")

dev.off()

pdf(file.path(plot_root, "Flood water vs oil.pdf"))
hexbinplot(inj ~ oil,
           #data = fw.ow,
           aspect = 1,
           xbins = 30,
           type = "r",
           lwd = 2,
           xlab = "Oil Production (bbl)",
           ylab = "Water Flood (bbl)",
           main = "Water Flooding vs Oil Production")
dev.off()