#-------------------------------------------------------------------------------
# Script Info
#-------------------------------------------------------------------------------
# convWater_v1.R (Conventional Oil and Gas Water Model)
# Version 2
# 04/08/14
# Jon Wilkey
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Version History
#-------------------------------------------------------------------------------
# --- Version 1 ---
# 1. Loads *.rda files, compared production dataframes field numbers to
#    project_volumes project numbers, determined they didn't match up
# 2. Created time series of water injection, disposal, and production for Basin.

#-------------------------------------------------------------------------------
# Options 
#-------------------------------------------------------------------------------
# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

#-------------------------------------------------------------------------------
# Paths
#-------------------------------------------------------------------------------
# Raw data directory
raw_root <- "D:/Dropbox/CLEAR/DOGM Data/Raw Data"
# Prepared data directory
data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"
# Working directory
work_root <- "D:/Dropbox/CLEAR/DOGM Data"
setwd(work_root)
# Functions 
fin <- "D:/Dropbox/CLEAR/DOGM Data/Functions"

#-------------------------------------------------------------------------------
# Functions
#-------------------------------------------------------------------------------
# Copy/Paste function
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

# Diff month function
flst <- file.path(fin,c("diffMonPOSIX.R"))
for (f in flst) source(f)

#-------------------------------------------------------------------------------
# Libraries
#-------------------------------------------------------------------------------
library(sqldf)
library(zoo)
library(foreign)
library(data.table)

#-------------------------------------------------------------------------------
# Load required data files
#-------------------------------------------------------------------------------
# Load *.rda files
load(file.path(raw_root, "uic_projects.rda"))
load(file.path(raw_root, "uic_wells.rda"))
load(file.path(raw_root, "project_volumes.rda"))
load(file.path(raw_root, "disposal.rda"))
load(file.path(data_root, "fieldata.rda"))
load(file.path(data_root, "production.rda"))
load(file.path(data_root, "histdata.rda"))

# Rename some dataframes for brevity
d <- disposal
v <- project_volumes
p <- production
h <- histdata
remove(disposal, project_volumes, production, histdata)

#-------------------------------------------------------------------------------
# Process data - Water Injected as Time Series
#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Process data - Water Disposal as Time Series 
#-------------------------------------------------------------------------------
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

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Process data - Water Produced as Time Series 
#-------------------------------------------------------------------------------
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
prod <- sqldf("select p_rpt_period, sum(p_water_prod)
              from m
              group by p_rpt_period")
# Drop NA values and last month (p has data through 2013-12-01), drop dates
prod <- prod[2:248,2]
# Convert to numeric variable type
prod <- as.numeric(prod)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Process data - Average Well Age as Time Series
#-------------------------------------------------------------------------------
# Use SQL statement to determine average well age
age <- sqldf("select p_rpt_period, avg(time)
             from m
             group by p_rpt_period")
# Drop NA values and last month, drop dates
age <- age[2:248,2]
# Convert to numeric variable type
age <- as.numeric(age)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Process data - Oil production as Time Series
#-------------------------------------------------------------------------------
# Use SQL statement to determine total oil production as series of time
oil <- sqldf("select p_rpt_period, sum(p_oil_prod)
             from m
             group by p_rpt_period")
# Drop NA values and last month, drop dates
oil <- oil[2:248,2]
# Convert to numeric variable type
oil <- as.numeric(oil)

#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Plot Output
#-------------------------------------------------------------------------------
# Save to pdf
pdf("convWater_v2 Results.pdf")

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

#-------------------------------------------------------------------------------