#-------------------------------------------------------------------------------
# emissionFactorData_v1.R (Emission Factor Scaling Data)
# Version 1
# 01/30/14
# Jon Wilkey
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Version History
#-------------------------------------------------------------------------------
# --- Version 1 ---
# = Loads production.rda, extracts requested data on monthly basis, truncates
#   that data to a per year basis, and then exports to Excel.

#-------------------------------------------------------------------------------
# Options 
#-------------------------------------------------------------------------------
# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

#-------------------------------------------------------------------------------
# Paths
#-------------------------------------------------------------------------------
# Change this to the folder where you keep your *.rda files
data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"
setwd(data_root)

#-------------------------------------------------------------------------------
# Functions
#-------------------------------------------------------------------------------
# Copy/Paste function
write.excel <- function(x,row.names=FALSE,col.names=TRUE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

#-------------------------------------------------------------------------------
# Libraries
#-------------------------------------------------------------------------------
# need 'zoo' for 'as.yearmon' - see below - install.packages("zoo") if not have
library(zoo)
# need 'sqldf' for SQL queries- install.packages("sqldf") if not have
library(sqldf)

#-------------------------------------------------------------------------------
# Load required data files
#-------------------------------------------------------------------------------
load(file.path(data_root, "histdata.rda"))
load(file.path(data_root, "proddata.rda"))
load(file.path(data_root, "welldata.rda"))
# Rename dataframes for brevity
h <- histdata; remove(histdata)
p <- proddata; remove(proddata)
w <- welldata; remove(welldata)

#-------------------------------------------------------------------------------
# Data preparation (merge data, subset to selected data, truncate dates)
#-------------------------------------------------------------------------------
# Inner join p and h on p_api and h_api.
m <- merge(p, h, by.x = "p_api", by.y = "h_api")

# Inner join m and w on p_api and w_api.
m <- merge(m, w, by.x = "p_api", by.y = "w_api")

# Drop records prior to 1999-01-01
m <- subset(m, subset = (h_apd_aprovd >= as.Date("1999-01-01") &
                         p_rpt_period >= as.Date("1999-01-01")))

# Only keep records of wells located in Uintah or Duchesne county
m <- subset(m, subset = (w_county == "UINTAH" | w_county == "DUCHESNE"))

# Add column 'spud_date' as truncated-to-month version of h_spud_dry.
m$spud_date <- as.Date(as.yearmon(m[,"h_spud_dry"]))

# Add column 'compl_date' as truncated-to-month version of h_compl_date.
m$compl_date <- as.Date(as.yearmon(m[,"h_compl_date"]))

#-------------------------------------------------------------------------------
# SQL Queries
#-------------------------------------------------------------------------------
# Total well / spud well Counts
spud.wells <- sqldf("SELECT spud_date, COUNT(DISTINCT p_api) FROM m GROUP BY spud_date")
# Gas well condensate (oil from gas wells)
ogw <- sqldf("SELECT p_rpt_period, SUM(p_oil_prod) FROM m WHERE h_well_type = 'GW' GROUP BY p_rpt_period")
# Oil from oil wells
oow <- sqldf("SELECT p_rpt_period, SUM(p_gas_prod) FROM m WHERE h_well_type = 'OW' GROUP BY p_rpt_period")
# Total gas production
gas <- sqldf("SELECT p_rpt_period, SUM(p_gas_prod) FROM m GROUP BY p_rpt_period")

#-------------------------------------------------------------------------------
# Excel export
#-------------------------------------------------------------------------------
# Uncomment and run one line at a time to copy/paste into Excel
# write.excel(spud.wells, col.names = FALSE)
# write.excel(ogw, col.names = FALSE)
# write.excel(oow, col.names = FALSE)
# write.excel(gas, col.names = FALSE)