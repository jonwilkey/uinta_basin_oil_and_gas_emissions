#-------------------------------------------------------------------------------
# Script Info
#-------------------------------------------------------------------------------
# convWater_v1.R (Conventional Oil and Gas Water Model)
# Version 1
# 04/01/14
# Jon Wilkey
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Version History
#-------------------------------------------------------------------------------
# --- Version 1 ---
# 1. Loads *.rda files, BLAH

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
library(sqldf)

#-------------------------------------------------------------------------------
# Load required data files
#-------------------------------------------------------------------------------
# Load *.rda files
load(file.path(raw_root, "uic_projects.rda"))
load(file.path(raw_root, "uic_wells.rda"))
load(file.path(raw_root, "project_volumes.rda"))
load(file.path(raw_root, "disposal.rda"))
load(file.path(data_root, "fieldata.rda"))

# Rename some dataframes for brevity
d <- disposal
v <- project_volumes
uic.p <- uic_projects
uic.w <- uic_wells
remove(disposal, project_volumes, uic_projects, uic_wells)

#-------------------------------------------------------------------------------
# Process data
#-------------------------------------------------------------------------------
# Merge fieldata field numbers with field names in uic.w
m <- merge(uic.w, fieldata, by.x = "field", by.y = "f_field_name")

# Select subset of m to keep which gives project numbers for Uintah & Duchense
# counties to determine water for injection wells
inj.list <- na.omit(sqldf("select distinct(project_unit_number), f_field_num
                          from m
                          where county = 'UINTAH' or county = 'DUCHESNE'"))
# Pull out list of unique fields in inj.list
inj.fields <- unique(inj.list[,2])
# Merge inj.list and v dataframes to add field numbers to v and select only
# desired injection project numbers
v <- merge(v, inj.list, by.x = "project_number", by.y = "project_unit_number")

# Create a sequence of dates from start of modeling period to final date in
# dataframe v (2013-11-01)
date <- seq(from = as.Date("1999-01-01"), to = as.Date("2013-11-01"),
            by = "months"))

# Determine water injection volumes as f(time) for each field
inj <- matrix(0, ncol = length(inj.fields), nrow = length(date))
for (i in 1:length(date)) {
  temp <- subset(v, report_date == date[i])
}

#-------------------------------------------------------------------------------

