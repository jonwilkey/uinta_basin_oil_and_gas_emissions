# 24hr production vs. First year total production correlation test


# Description -------------------------------------------------------------

# This script compares the reported 24hr test production amounts to the total
# annual produciton during the first year of a wells operation for oil and gas
# wells in the state of Utah as reported in the DOGM databases proddata.dbf and
# histdata.dbf.

# Method
# 1. The script begins by defining the directories for various items (data,
#    plots, etc.).
# 2. The formatted DOGM database files "production.rda" (proddata.dbf) and
#    "histdata.rda" (histdata.dbf) are loaded, along with a listing of field
#    names.
# 3. Every non-zero 24hr oil and gas test value in histdata is recorded.
#    Duplicate entries are deleted.
# 4. An SQL query is run (using the sqldf package) to get the total production
#    of oil and gas for each distinct API # in the proddata database.
# 5. The non-zero 24hr test values and SQL queries are merged together to give
#    the data.frames "m.oil" and "m.gas".
# 6. For each product type (oil and gas), the correlation coefficient between
#    the 24hr test and first years total production is determined using the
#    "cor" function - for all fields in the state and for each field
#    individually.
# 7. A linear model of (total annual production) = m * (24hr test production) + 
#    b is also determined for each product, for all fields in the state, and for
#    each field individually using the "lm" function.
# 8. The correlation coefficient and linear regression results are saved, along
#    with additional supporting data, to the data.frames "cor.oil" and "cor.gas"
# 9. For collections of data.points with >= 0.5 correlation coefficient values
#    and >= 10 data points, a hexbin scatterplot is generated and saved to the
#    plot directory.
# 10.Finally, the results can (optionally) be exported via clipboard or by
#    writing to a csv file.

# Instructions
# 1. Set directories for data_root, plot_root, etc.
# 2. Ensure that the sqldf package is installed.
# 3. Comment/Uncomment the lines for your desired SQL query to add or remove the
#    restriction on well type (oil prod. from oil wells or gas prod. from gas
#    wells).
# 4. If using a well restriction, note that you must manually repeat the SQL
#    query from step 3 for the opposite well type if you wish to analyze both
#    oil wells and gas wells.
# 5. Uncomment and run the desired export lines. "write.excel" copies the
#    correlation results to the system clipboard for pasting into a spreadsheet.
#    The second method writes the production history of every well to a csv file
#    (WARNING - this option takes ~ 30 minutes for each product type).


# Options -----------------------------------------------------------------

# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)


# Paths -------------------------------------------------------------------

# Set to match each of these to your directories

# Windows
data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"        # Prepared data
plot_root <- "D:/Dropbox/CLEAR/DOGM Data/Plots"                # Plots
work_root <- "C:/Users/Jon/Documents/R/ub_oilandgas/"          # Working dir.

# # Mac
# data_root <- "/Users/john/Dropbox/CLEAR/DOGM Data/Prepared Data"         # Prepared data
# plot_root <- "/Users/john/Dropbox/CLEAR/DOGM Data/Plots"                 # Plots
# work_root <- "/Users/john/Documents/ub_oilandgas/ub_oilandgas"           # Working dir.

# Set working directory
setwd(work_root)


# Functions ---------------------------------------------------------------

# Copy function for pasting data to clipboard (on Windows)
write.excel <- function(x,row.names=FALSE,col.names=FALSE,...) {
  write.table(x,"clipboard",sep="\t",row.names=row.names,col.names=col.names,...)
}

# # Same but for Mac:
# write.excel <- function (x,row.names=FALSE,col.names=FALSE,...) {
#   clip <- pipe("pbcopy", "w")
#   write.table(x,file=clip,sep="\t",row.names=row.names,col.names=col.names,...)
#   close(clip)
# }

# Libraries ---------------------------------------------------------------
library(sqldf)  # Install using "install.packages("sqldf") first!


# Load required data files ------------------------------------------------

# Load the dataframe named 'production'
load(file.path(data_root, "production.rda")) # DOGM production data
load(file.path(data_root, "histdata.rda"))   # DOGM historical well data
load(file.path(data_root, "fieldata.rda"))   # DOGM field names

# Rename
p <- production
h <- histdata
remove(production, histdata)


# Queries -----------------------------------------------------------------

# Create data.frame of well API#s and their 24hr production values
oil <- subset(h, subset = (h_oil_24hr != 0), select = c("h_api", "h_oil_24hr"))
gas <- subset(h, subset = (h_gas_24hr != 0), select = c("h_api", "h_gas_24hr"))

# Get list of row numbers with duplicate entries. Note: of the initial 11,356
# non-zero 24hr oil test observations, only 30 rows were true duplicates (same
# API and 24hr oil results), while there were 523 partial duplicates (same API
# #, different 24hr test results). Since there's no (easy) way to know when the
# second (or third, or fourth, etc.) test was done in the production history of
# the well, all duplicates (even partial duplicates) are to be deleted.
ind <- NULL
for (i in 2:nrow(oil)) {
  if (oil[i,1] == oil[(i-1),1]) {
    ind <- c(ind, i)
  }
}

# Remove duplicate oil entries
oil <- oil[-ind,]

# Repeat for gas. Note: there are 14,938 non-zero 24hr gas test observations, 11
# true duplicates, and 893 partial duplicates. Again, all duplicates (partial
# and true) are removed.
ind <- NULL
for (i in 2:nrow(gas)) {
  if (gas[i,1] == gas[(i-1),1]) {
    ind <- c(ind, i)
  }
}

# Remove duplicate gas entries
gas <- gas[-ind]

# Get total production from all wells in p over their first year of operation
# using an SQL query
annual.prod <- sqldf("select distinct(p_api), sum(p_oil_prod), sum(p_gas_prod),
                      w_well_type, w_field_num
                      from p
                      where time < 13
                      group by p_api")

# # If differentiating between oil wells and gas wells (i.e. 24hr test results vs
# # annual production for oil from oil wells or gas from gas wells), then use this
# # prompt and change "w_well_type" constraint to "OW" for oil wells or "GW" for
# # gas wells.
# annual.prod <- sqldf("select distinct(p_api), sum(p_oil_prod), sum(p_gas_prod),
#                       w_well_type, w_field_num
#                       from p
#                      where time < 13 and w_well_type = 'OW'
#                      group by p_api")

# Merge oil/gas 24hr test queries with the annual.prod data.frame. Also, drop
# irrelevant production data (i.e. remove first annual oil production sum from
# gas set, and first annual gas production sum from oil set). Note that this
# further reduces the number of observations to 8,281 (from 10,833) for oil and
# 12,966 (from 14,938) for gas. Not entirely sure why - maybe there are wells in
# data.frame h that aren't in data.frame p (???)
m.oil <- merge(x = oil, y = annual.prod[,-3], by.x = "h_api", by.y = "p_api")
m.gas <- merge(x = gas, y = annual.prod[,-2], by.x = "h_api", by.y = "p_api")

# Rename columns of merged tables. "day" is the column of the 24hr test data
# and "oilyr"/"gasyr" is the sum of the first years oil/gas production.
names(m.oil) <- c("api", "day", "oilyr", "wellType", "field")
names(m.gas) <- c("api", "day", "gasyr", "wellType", "field")


# Correlations ------------------------------------------------------------

# Get list of the unique field numbers for each production type, rename result
field.oil <- data.frame(unique(m.oil$field)); names(field.oil) <- c("field_num")
field.gas <- data.frame(unique(m.gas$field)); names(field.gas) <- c("field_num")

# Add field names from "fieldata" data.frame list of unique field numbers in
# dataset
field.oil <- merge(x = field.oil, y = fieldata, by.x = "field_num", by.y = "f_field_num")
field.gas <- merge(x = field.gas, y = fieldata, by.x = "field_num", by.y = "f_field_num")

# Create data.frame "cor.oil"/"cor.gas" to store for correlation results for
# each field. Data.frame will have six columns: 1- field name, 2- field number,
# 3- correlation coefficient, 4- number of data points, 5 - slope of line fit to
# data points by linear regression, 6 - intercept of line fit to data points by
# linear regression.
cor.oil <- data.frame(matrix(0, nrow = nrow(field.oil)+1, ncol = 6))
cor.gas <- data.frame(matrix(0, nrow = nrow(field.gas)+1, ncol = 6))

# Rename columns to match descriptions above
names(cor.oil) <- c("field_name", "field_num", "correlation", "count", "m", "b")
names(cor.gas) <- c("field_name", "field_num", "correlation", "count", "m", "b")

# Get correlation for oil for the entire state. "All" is inserted in place of
# field name, field number is "NA", the "cor" function gets the actual
# correlation coefficient for the entire dataset, "nrow" returns the number of
# data points (each datapoint is on a single row), and "lm" is a linear
# regression function which when called with the handle "$coefficients" returns
# the coefficients of regression's fit.

# Note: if there are only a few data points the "cor" function will give a
# warning message that the standard deviation is zero.

cor.oil[1,] <- c("All", NA, cor(x = m.oil$day, y = m.oil$oilyr),
                 nrow(m.oil), lm(m.oil[,2]~m.oil[,3])$coefficients)

# Repeat this process for oil on a field level. For each unique field number in
# "field.oil" the subset function selects only the data in "m.oil" for that
# particular field and stores it as "temp". The second command in the function
# repeats the process described above for the entire state.
for (i in 1:nrow(field.oil)) {
  temp <- subset(x = m.oil, subset = (field == field.oil[i,1]))
  cor.oil[(i+1),] <- c(field.oil[i,2], field.oil[i,1],
                       cor(x = temp$day, y = temp$oilyr), nrow(temp),
                       lm(temp[,2]~temp[,3])$coefficients)
}

# Repeat correlation process described above for gas for the entire state...
cor.gas[1,] <- c("All", NA, cor(x = m.gas$day, y = m.gas$gasyr),
                 nrow(m.gas), lm(m.gas[,2]~m.gas[,3])$coefficients)

# ... and for each field
for (i in 1:nrow(field.gas)) {
  temp <- subset(x = m.gas, subset = (field == field.gas[i,1]))
  cor.gas[(i+1),] <- c(field.gas[i,2], field.gas[i,1],
                       cor(x = temp$day, y = temp$gasyr), nrow(temp),
                       lm(temp[,2]~temp[,3])$coefficients)
}

# By default each column above is treated as a character string. Change the
# correlation coefficient and count columns to the numeric type so that they
# can be used in the plot section below.
cor.oil$correlation <- as.numeric(cor.oil$correlation)
cor.gas$correlation <- as.numeric(cor.gas$correlation)
cor.oil$count <- as.numeric(cor.oil$count)
cor.gas$count <- as.numeric(cor.gas$count)


# Correlation scatter plots for oil ---------------------------------------

# Get index of rows in cor.oil for fields with correlation coefficient >=
# abs(0.5) & more than 10 data points, as well as the first row containing the
# results for all wells
ind <- c(1, which(abs(cor.oil$correlation) >= 0.5 & cor.oil$count >= 10))

# Save plots to the following PDF
pdf(file.path(plot_root, "24hr Test vs 1st Year Production - Oil - All Wells.pdf"))

# Create a hexbin plot for each field in "ind"
for (i in 1:length(ind)) {
  print(hexbinplot(oilyr ~ day,
                   if (i == 1) {
                     data = m.oil
                   } else {
                     data = subset(m.oil, subset = (field == cor.oil$field_num[ind[i]]))
                   },
                   aspect = 1.05,
                   bins = 50,
                   type = "r",
                   xlab = "24hr Oil Production (bbl)",
                   ylab = "First Year Total Oil Production (bbl)",
                   main = if (i == 1) {
                     "Scatterplot of 24hr Test vs. 1st Year for Oil in All Fields"
                   } else {
                     paste("Scatterplot of 24hr Test vs. 1st Year for Oil in Field:",
                           cor.oil$field_num[ind[i]])
                   }
                   )
        )
}

# End plotting to PDF
dev.off()


# Correlation scatter plots for gas ---------------------------------------

# Repeat for gas
ind <- c(1, which(abs(cor.gas$correlation) >= 0.5 & cor.gas$count >= 10))

# Save plots to the following PDF
pdf(file.path(plot_root, "24hr Test vs 1st Year Production - Gas - All Wells.pdf"))

# Create a hexbin plot for each field in "ind"
for (i in 1:length(ind)) {
  print(hexbinplot(gasyr ~ day,
                   if (i == 1) {
                     data = m.gas
                   } else {
                     data = subset(m.gas, subset = (field == cor.gas$field_num[ind[i]]))
                   },
                   aspect = 1.05,
                   bins = 50,
                   type = "r",
                   xlab = "24hr Gas Production (MCF)",
                   ylab = "First Year Total Gas Production (MCF)",
                   main = if (i == 1) {
                     "Scatterplot of 24hr Test vs. 1st Year for Gas in All Fields"
                   } else {
                     paste("Scatterplot of 24hr Test vs. 1st Year for Gas in Field:",
                           cor.gas$field_num[ind[i]])
                   }
                   )
        )
}

# End plotting to PDF
dev.off()

# Data Export -------------------------------------------------------------

# This is the segment of code used to export data to Excel. Uncomment to use
# each segment as needed.

# # Copy cor.oil/cor.gas to clipboard using write.excel function
# write.excel(cor.oil)
# write.excel(cor.gas)

# Create matrix of production data for each oil/gas well in "oil" and "gas" list
# Warning - this is slow

# # ---For oil---
# # Preallocated space for results matrix with rows = oil wells that have reported
# # 24hr oil results and columns = oil production for each month since the well 
# # originally went online (i.e. time). Note that 'max(p$time)' returns 'NA' so
# # the actual maximum time step (248) is hardcoded here.
# oilprod <- matrix(0, nrow = nrow(oil), ncol = 248)
# 
# # Create smaller subset of p to get production data from
# sub <- subset(p, select = c("p_api", "p_oil_prod", "time"))
# 
# # Get production history for each well api # in oil
# for (i in 1:nrow(oil)) {
#   temp <- sub[which(sub$p_api == oil$h_api[i]),]
#   for (j in 1:248) {
#     temp1 <- temp$p_oil_prod[which(temp$time == j)]
#     if (length(temp1) > 0) {
#       oilprod[i,j] <- temp1
#     }
#   }
# }
# 
# # Add in API#s and 24hr test results from "oil" data.frame
# export.oil <- data.frame(oil, oilprod)
# 
# # Merge with annual.prod to get well type (oil well or gas well) and field num.
# export.oil <- merge(x = export.oil, y = annual.prod[,-3], by.x = "h_api", by.y = "p_api")
# 
# # Reorder columns
# export.oil <- export.oil[c(1, 2, 251:253, 3:250)]
# 
# # Write to csv file for import into Excel (too large for clipboard)
# write.csv(export.oil, file = file.path(data_root, "24hr oil comparison.csv"))

# # ---For gas---
# # Repeat steps above
# gasprod <- matrix(0, nrow = nrow(gas), ncol = 248)
# 
# # Create smaller subset of p to get production data from
# sub <- subset(p, select = c("p_api", "p_gas_prod", "time"))
# 
# # Get production history for each well api # in oil
# for (i in 1:nrow(gas)) {
#   temp <- sub[which(sub$p_api == gas$h_api[i]),]
#   for (j in 1:248) {
#     temp1 <- temp$p_gas_prod[which(temp$time == j)]
#     if (length(temp1) > 0) {
#       gasprod[i,j] <- temp1
#     }
#   }
# }
# 
# # Add in API#s and 24hr test results from "oil" data.frame
# export.gas <- data.frame(gas, gasprod)
# 
# # Merge with annual.prod to get well type (oil well or gas well) and field num.
# export.gas <- merge(x = export.gas, y = annual.prod[,-2], by.x = "h_api", by.y = "p_api")
# 
# # Reorder columns
# export.gas <- export.gas[c(1, 2, 251:253, 3:250)]
# 
# # Write to csv file for import into Excel (too large for clipboard)
# write.csv(export.gas, file = file.path(data_root, "24hr gas comparison.csv"))