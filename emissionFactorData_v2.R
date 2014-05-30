#-------------------------------------------------------------------------------
# emissionFactorData_v1.R (Emission Factor Scaling Data)
# Version 2
# 02/05/14
# Jon Wilkey
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Version History
#-------------------------------------------------------------------------------
# --- Version 1 ---
# = Loads production.rda, extracts requested data on monthly basis, truncates
#   that data to a per year basis, and then exports to Excel.
# --- Version 2 ---
# = Separates data based on county

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
h <- histdata
p <- proddata
w <- welldata
remove(histdata, proddata, welldata)

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

# Add column 'spud_date' as truncated-to-month version of h_spud_dry.
m$spud_date <- as.Date(as.yearmon(m[,"h_spud_dry"]))

# Add column 'compl_date' as truncated-to-month version of h_compl_date.
m$compl_date <- as.Date(as.yearmon(m[,"h_compl_date"]))

# Only keep records in specified county
# For wells located in Uintah county
u <- subset(m, subset = (w_county == "UINTAH"))

# For wells located in Duchesne county
d <- subset(m, subset = (w_county == "DUCHESNE"))

#-------------------------------------------------------------------------------
# SQL Queries
#-------------------------------------------------------------------------------

# Uintah County
# Total well / spud well Counts
u.spud.wells <- sqldf("SELECT spud_date, COUNT(DISTINCT p_api) FROM u GROUP BY spud_date")
# Gas well condensate (oil from gas wells)
u.ogw <- sqldf("SELECT p_rpt_period, SUM(p_oil_prod) FROM u WHERE h_well_type = 'GW' GROUP BY p_rpt_period")
# Oil from oil wells
u.oow <- sqldf("SELECT p_rpt_period, SUM(p_gas_prod) FROM u WHERE h_well_type = 'OW' GROUP BY p_rpt_period")
# Total gas production
u.gas <- sqldf("SELECT p_rpt_period, SUM(p_gas_prod) FROM u GROUP BY p_rpt_period")

# Duchesne County
# Total well / spud well Counts
d.spud.wells <- sqldf("SELECT spud_date, COUNT(DISTINCT p_api) FROM d GROUP BY spud_date")
# Gas well condensate (oil from gas wells)
d.ogw <- sqldf("SELECT p_rpt_period, SUM(p_oil_prod) FROM d WHERE h_well_type = 'GW' GROUP BY p_rpt_period")
# Oil from oil wells
d.oow <- sqldf("SELECT p_rpt_period, SUM(p_gas_prod) FROM d WHERE h_well_type = 'OW' GROUP BY p_rpt_period")
# Total gas production
d.gas <- sqldf("SELECT p_rpt_period, SUM(p_gas_prod) FROM d GROUP BY p_rpt_period")

#-------------------------------------------------------------------------------
# Excel export
#-------------------------------------------------------------------------------
# # Uncomment and run one line at a time to copy/paste into Excel
# # Uintah County
# write.excel(u.spud.wells[,2], col.names = FALSE)
# write.excel(u.ogw[,2], col.names = FALSE)
# write.excel(u.oow[,2], col.names = FALSE)
# write.excel(u.gas[,2], col.names = FALSE)
# # Duchesne County
# write.excel(d.spud.wells[,2], col.names = FALSE)
# write.excel(d.ogw[,2], col.names = FALSE)
# write.excel(d.oow[,2], col.names = FALSE)
# write.excel(d.gas[,2], col.names = FALSE)