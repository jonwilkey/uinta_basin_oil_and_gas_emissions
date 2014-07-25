#-------------------------------------------------------------------------------
# conventional_v4.R (Conventional Oil and Gas Model)
# Version 4
# 02/11/14
# Jon Wilkey
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Version History
#-------------------------------------------------------------------------------
# --- Version 1 ---
# 1. Loads *.rda files, finds APDs approved in each field at each timestep
#    according to Model 8, determines monthly production by field, plots
#    predicted vs. actual APD approvals, plots predicted production.
#
# --- Version 2 ---
# 1. Added sqldf query to pull actual production history of gas and oil in Basin.
#
# --- Version 3 ---
# 1. Added off-well type production (gas from oil wells, oil from gas wells).
# 2. Expanded data set from Michael's July 2012 analysis to Nov. 2013 dataset
#    with proddata, histdata, and welldata for h_apd_aprovd > "1999-01-01" and
#    w_county == "UINTAH" | w_county == "DUCHESNE". Named file conventional_data
#    and added value ptime = p_rpt_period - h_apd_aprovd, converted into units
#    of weeks and then divided by 4 to get months.
# 3. Updated m.fit.ow and m.fit.gw to cover full APD approval dataset.
#
# --- Version 4 ---
# 1. Added Monte-Carlo simulation of production rates taking the drilling
#    schedule (# of wells and field location) as a given.

#-------------------------------------------------------------------------------
# Options 
#-------------------------------------------------------------------------------
# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

#-------------------------------------------------------------------------------
# Paths
#-------------------------------------------------------------------------------
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
library(zoo)

#-------------------------------------------------------------------------------
# Load required data files
#-------------------------------------------------------------------------------
# Load *.rda files
load(file.path(data_root, "pdf_oow.rda"))
load(file.path(data_root, "cdf_oow.rda"))
load(file.path(data_root, "production.rda"))
load(file.path(data_root, "histdata.rda"))
# Rename some dataframes for brevity
p <- production
h <- histdata
remove(production, histdata)

#-------------------------------------------------------------------------------
# Determine well's date of 1st prod as f(time) and field they were drilled in
#-------------------------------------------------------------------------------
# Data Prep
# Merge p and h
m <- merge(p, h, by.x = "p_api", by.y = "h_api")

# Add column 'prod_date' as truncated-to-month version of h_first_prod
m$prod_date <- as.Date(as.yearmon(m[,"h_first_prod"]))

# Create dataframe containing dates of 1st production for each unique APD # and
# the field it is located in
well <- sqldf("select distinct(p_api), prod_date, w_field_num, w_well_type
              from m")

# Drop NA observations
well <- na.omit(well)

# Only wells since 1999-01-01
well <- subset(well, prod_date >= as.Date("1999-01-01"))

# Create a complete set of months between 1999-01-01 and 2014-01-01.
all_months <- seq(from = as.Date("1999-01-01"),
                  to = as.Date("2013-12-01"),
                  by = "months")

# Oil Wells
# Select only those wells in "well" dataframe which are oil wells
well.ow <- subset(well, w_well_type == "OW")

# List of oil well fields that account for 95% of APDs and 95% of production
field.ow <- c(105, 72, 590, 117, 718, 55, 60, 65, 665, 710, 80, 630, 605, 560,
              101, 610, 692, 695)

# Predefine space for matrix with oil well drilling schedule
schedule.ow <- matrix(0, nrow = length(all_months), ncol = length(field.ow))

# Create matrix with # of APDs approved by month (rows) in each field (columns)
for (i in 1:length(field.ow)) {
  temp <- subset(well.ow, w_field_num == field.ow[i])
  for (j in 1:length(all_months)) {
    schedule.ow[j,i] <- length(which(temp[,2] == all_months[j]))
  }  
}

# # Gas Wells
# # Select only those wells in "well" dataframe which are gas wells
# well.gw <- subset(well, w_well_type == "GW")
# # List of gas well fields that account for 95% of APDs and 95% of production
# field.gw <- c(630, 710, 600, 617, 640, 547, 670, 635, 618, 622, 610, 590, 791,
#               72, 105, 55)
# # Predefine space for matrix with gas well drilling schedule
# schedule.gw <- matrix(0, nrow = length(all_months), ncol = length(field.gw))
# 
# # Create matrix with # of APDs approved by month (rows) in each field (columns)
# for (i in 1:length(field.gw)) {
#   temp <- subset(well.gw, w_field_num == field.gw[i])
#   for (j in 1:length(all_months)) {
#     schedule.gw[j,i] <- length(which(temp[,2] == all_months[j]))
#   }  
# }

#-------------------------------------------------------------------------------
# Predict oil and gas production as f(time) for each field and entire Basin
#-------------------------------------------------------------------------------
# Oil production
# Pick number of Monte Carlo Runs
nrun <- 10^5
prod.oil <- matrix(0, nrow = length(all_months), ncol = nrun)

for (h in 1:nrun) {
  temp <- matrix(0, nrow = length(all_months), ncol = length(field.ow))
  for (i in 1:length(field.ow)) {
    for (j in 1:length(all_months)) {
      if (schedule.ow[j,i] > 0) {
        for (k in 1:schedule.ow[j,i]) {
          # Generate random numbers for each coefficient for this well
          rn <- runif(3, min = 0, max = 1)
          
          # Use findInterval to pick index and assign value to each variable
          a <- pdf.oow[findInterval(rn[1], c(0, cdf.oow[, 1*i])), 1*i]
          b <- pdf.oow[findInterval(rn[2], c(0, cdf.oow[, 2*i])), 2*i]
          c <- pdf.oow[findInterval(rn[3], c(0, cdf.oow[, 3*i])), 3*i]
          
          # Determine oil production over time from this well
          t <- 1:(length(all_months) - j + 1)
          oil <- a*(1+b*c*t)^(-1/b)
          
          # Add to total production history for this field
          t.step <- 1
          for (n in j:length(all_months)){
            temp[n,i] <- temp[n,i] + oil[t.step]
            t.step <- t.step + 1
          }
        }
      }
    }
  }
  prod.oil[,h] <- rowSums(temp)
}

percentile.ow <- matrix(0, nrow = length(all_months), ncol = 3)
for (i in 1:length(prod.oil[,1])) {
  temp <- quantile(prod.oil[i,], c(0.05, 0.95))
  percentile.ow[i,1] <- temp[1]
  percentile.ow[i,2] <- temp[2]
}

#-------------------------------------------------------------------------------
# Determine actual oil and gas production
#-------------------------------------------------------------------------------
poa <- subset(m, select = c("prod_date", "w_well_type", "p_oil_prod", "p_rpt_period"))
poa <- subset(poa, subset = (prod_date >= as.Date("1999-01-01") & w_well_type == "OW"))

prod.oil.actual <- matrix(0, nrow = length(all_months), ncol = 1)
for (i in 1:length(all_months)) {
  temp <- subset(poa, p_rpt_period == all_months[i])
  prod.oil.actual[i] <- sum(temp$p_oil_prod)
}

#-------------------------------------------------------------------------------
# Output - Plots and Tables
#-------------------------------------------------------------------------------
# Save plots to PDF
pdf(file = paste("conventional_v4 Results.pdf"))

# Plot of Predicted vs Actual Oil Production
plot(all_months, prod.oil.actual,
     type = "l",
     lty = 1,
     col = "black",
     xlab = "Date",
     ylab = "Oil Production (bbl per month)",
     main = "MC Predictions vs. Actual Oil Production")
lines(all_months, percentile.ow[,2], col = "blue")
lines(all_months, percentile.ow[,1], col = "red")
legend("topleft",
       c("Actual", "MC 95%", "MC 5%"),
       lty = c(1,1,1),
       col = c("black", "blue", "red"))


dev.off()