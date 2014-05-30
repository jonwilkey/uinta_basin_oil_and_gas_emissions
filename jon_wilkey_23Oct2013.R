#-------------------------------------------------------------------------------
# Options 
#-------------------------------------------------------------------------------
# don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

#-------------------------------------------------------------------------------
# Paths
#-------------------------------------------------------------------------------
# Change this to the folder where you keep your *.rda files
data_root <- "D:/Google Drive/CLEAR/DOGM Data"

#-------------------------------------------------------------------------------
# Functions
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Libraries
#-------------------------------------------------------------------------------
# need 'zoo' for 'as.yearmon' - see below - install.packages("zoo") if not have
library(zoo)
# need 'plyr' for split-apply-combine operations - see below. 
library(plyr)

#-------------------------------------------------------------------------------
# Load DOGM data tables
#-------------------------------------------------------------------------------
# this will give you a dataframe named 'welldata'
load(file.path(data_root, "welldata.rda"))
# alias 'welldata' as 'w' for brevity
w <- welldata

# this will give you a dataframe named 'histdata'
load(file.path(data_root, "histdata.rda"))
# alias 'histdata' as 'h'
h <- histdata

# this will give you a dataframe named 'fieldata'
load(file.path(data_root, "fieldata.rda"))
# alias 'fieldata' as 'f'
f <- fieldata

# You only want records from histdata corresponding to
# 1. a newly drilled well
# 2. oil well
# 3. only where the spud_dry date is not NA (those are wells never drilled)
# 4. since 1999
h <- subset(h, subset = (h_work_type == "DRILL" &
                         h_well_type == "OW"    &
                         !is.na(h_spud_dry)     &
                         h_spud_dry > as.Date("1999-01-01")
                         )
            )

# Inner join h and w on h_api and w_api.
m <- merge(h, w, by.x = "h_api", by.y = "w_api")

# Only want certain columns of data (you may want more than just these ...).
keeps <- c("h_api", "h_spud_dry", "h_well_type", "w_field_num")
m <- subset(m, select = keeps)

# Inner join mm with f on w_field_num and f_field_num.
# This gets you the field names rather than just their number.
# Reassign result as m. 
m <- merge(m, f, by.x = "w_field_num", by.y = "f_field_num")

# Add column 'drill_date' as truncated-to-month version of h_spud_dry.
m$drill_date <- as.Date(as.yearmon(m[,"h_spud_dry"]))

# Create a complete set of months between 1999-01-01 and 2013-09-01.
all_months <- seq(from = as.Date("1999-01-01"),
                  to = as.Date("2013-09-01"),
                  by = "months"
                  )
# from a vector to a data frame
all_months <- data.frame(date = all_months)

# Loop through fields which have at least one well drilled since 1999. 
fields <- unique(f[,"f_field_num"])
mat <- matrix(nrow = length(fields), ncol=1)
for (k in 1:length(fields)) {
  sub <- subset(m, subset = (w_field_num == fields[k]))
  if (nrow(sub) == 0) {
    mat[k,1] <- FALSE
  }
  else {
    mat[k,1] <- TRUE
  }
}
# Only fields with wells drilled.
fields <- fields[mat[,1]]

# Start with an empty list.
df_list <- NULL
# Now loop through the (filtered) fields.
for (k in 1:length(fields)) {
# Extract records for fields[k] only; sub is a dataframe. 
  sub <- subset(m, subset = (w_field_num == fields[k]))
# Split the 'sub' dataframe by drill_date and count the number of records.
# This counts the number of wells drilled in fields[k] by month  
  z <- ddply(sub, c("drill_date"), summarize, wells_drilled = length(h_api))
# add the current field number to the all_months dataframe  
  all_months$field_num <- fields[k]
# now right-merge with the 'count' dataframe 'z'  
  x <- merge(z,
             all_months,
             by.x = "drill_date",
             by.y = "date",
             all.y = TRUE,
             all.x = FALSE
             )
# assign the resulting dataframe to the kth element of the list df_list  
  df_list[[k]] <- x
# remove the field_num column from all_months  
  all_months$field_num <- NULL
}

# Each pass through the loop creates a dataframe---one for each field and stores it as an element in a list
# Now stack the list elements into one combined dataframe
df <- do.call("rbind", df_list)

# An "NA" value means no records for that month - so replace NA with 0. 
ww <- which(is.na(df[,"wells_drilled"]))
df[ww, "wells_drilled"] <- 0



