################################################################################
# Retrieve data from DOGM 
################################################################################

#-------------------------------------------------------------------------------
# Options 
#-------------------------------------------------------------------------------
options(stringsAsFactors=FALSE)

#-------------------------------------------------------------------------------
# Libraries
#-------------------------------------------------------------------------------
library(foreign)

#-------------------------------------------------------------------------------
# Paths
#-------------------------------------------------------------------------------
rdpath <- "/media/Data_Share/data/research/raw_data"
pdpath <- "/media/Data_Share/data/research/prepared_data"
dogm_url_base <- "https://oilgas.ogm.utah.gov/pub/Database"

#-------------------------------------------------------------------------------
# Delete old *.exe and *.DBF files 
#-------------------------------------------------------------------------------
files <- list.files(rdpath, pattern="\\.exe", ignore.case=TRUE, full.names=TRUE)
for (f in files){
  system(paste("rm -f ", f, sep=""))
}

files <- list.files(rdpath, pattern="\\.DBF",ignore.case=TRUE, full.names=TRUE)
for (f in files){
  system(paste("rm -f ", f, sep=""))
}

#-------------------------------------------------------------------------------
# Get latest "*.exe" files from DOGM
#-------------------------------------------------------------------------------
# tables to get, as named by DOGM
tables <- c("welldata",
            "histdata",
            "proddata"
            )

# append ".exe" to tables
tables <- paste(tables, ".exe", sep="")

urls <- file.path(dogm_url_base, tables)

setwd(rdpath)
for (u in urls) {
  system(paste("wget", " ", "'", u, "'", sep=""))
}

#-------------------------------------------------------------------------------
# Extract latest "*.exe"
#-------------------------------------------------------------------------------
files <- list.files(rdpath, pattern="\\.exe", ignore.case=TRUE, full.names=TRUE)
for (f in files){
  system(paste("7zr x ", f, sep=""))
}


#-------------------------------------------------------------------------------
# Read-in DBF as dataframes and prepare
#-------------------------------------------------------------------------------
# import the DBF files into R dataframes 
welldata <- read.dbf(file.path(rdpath, "welldata.dbf"))
welldata <- subset(welldata,select=-c(COMMENTS,MODIFYDATE))
proddata <- read.dbf(file.path(rdpath, "proddata.dbf"))
histdata <- read.dbf(file.path(rdpath, "histdata.dbf"))

#-------------------------------------------------------------------------------
# Write dataframes to comma-delimited files
#-------------------------------------------------------------------------------
write.table(welldata,file=file.path(rdpath, "welldata.txt"),sep=",",quote=TRUE,row.names=FALSE,na="NULL")
write.table(proddata,file=file.path(rdpath,"proddata.txt"),sep=",",quote=TRUE,row.names=FALSE,na="NULL")
write.table(histdata,file=file.path(rdpath, "histdata.txt"),sep=",",quote=TRUE,row.names=FALSE,na="NULL")
