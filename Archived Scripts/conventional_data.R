#-------------------------------------------------------------------------------
# Options 
#-------------------------------------------------------------------------------
# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

#-------------------------------------------------------------------------------
# Paths
#-------------------------------------------------------------------------------
# Change this to the folder where you keep your *.rda files
# For Mac:
# data_root <- "/Users/john/Dropbox/CLEAR/DOGM Data"
# For Windows:
data_root <- "D:/Dropbox/CLEAR/DOGM Data"
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
library(sqldf)

#-------------------------------------------------------------------------------
# Load required data files
#-------------------------------------------------------------------------------
# Load *.rda files
load(file.path(data_root, "proddata.rda"))
load(file.path(data_root, "histdata.rda"))
load(file.path(data_root, "welldata.rda"))
load(file.path(data_root, "production.rda"))

#-------------------------------------------------------------------------------
# Merge database files, select subset of wells in Uinta & Duchesne > 1999
#-------------------------------------------------------------------------------

m <- merge(proddata, histdata, by.x = "p_api", by.y = "h_api")
m <- merge(m, welldata, by.x = "p_api", by.y = "w_api")
m <- subset(m, h_apd_aprovd > as.Date("1999-01-01"))
m <- subset(m, w_county == "UINTAH" | w_county == "DUCHESNE")

#-------------------------------------------------------------------------------
# Save resultant dataframe as *.rda file
#-------------------------------------------------------------------------------
conventional.data <- m
save(conventional.data, file = "conventional_data.rda")