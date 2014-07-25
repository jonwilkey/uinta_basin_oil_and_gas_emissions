#-------------------------------------------------------------------------------
# Options 
#-------------------------------------------------------------------------------
# don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

#-------------------------------------------------------------------------------
# Paths
#-------------------------------------------------------------------------------
# Change this to the folder where you keep your *.rda files
data_root <- "D:/Dropbox/CLEAR/DOGM Data"

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
# need 'sqldf' for SQL queries
library(sqldf)

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

# Load oil price data
load(file.path(data_root, "bw.rda"))
# Load CPI data
load(file.path(data_root, "cpi.rda"))
# Inflation adjustment to 2013-07-1
bw <- bw.z * (cpi.z / 233.596)
# Make a new dataframe of inflation adjusted black wax prices (bwInfAd) out of zoo vector bw
price <- coredata(bw)
drill_date <- index(bw)
bwInfAd <- data.frame(drill_date, price)
names(bwInfAd)[2] <- "oil_price"
# Merge df and bwInfAd
df_bw <- merge(df, bwInfAd, by = "drill_date")

# Make an aggregate dataframe for entire basin (for comparison and alternative model)
compare <- sqldf("select drill_date, sum(wells_drilled) from df_bw group by drill_date")
compare <- cbind(compare, bwInfAd$oil_price)
names(compare)[2] <- "wells_drilled"
names(compare)[3] <- "oil_price"
lag_well <- compare$wells_drilled[2:length(compare$wells_drilled)]
lag_well <- c(lag_well, 0)
compare <- cbind(compare, lag_well)

#-------------------------------------------------------------------------------
# Fitting empircal well drilling model
#-------------------------------------------------------------------------------

# Model 1 (m1): Disaggregated fit to each field
# Model 2 (m2): Include two and three month price lags

# Define m1_coef and m2_coef, which will be used as matrix of fitted m1_coefion results
m1_coef <- NULL
m2_coef <- NULL
for (k in 1:length(fields)) {
  # Select field number
  j <- fields[k]
  # Select subset of current field number from df_bw
  df_bw_j <- df_bw[which(df_bw$field_num==j),]
  # Add column with one lagged number of wells drilled
  lag_well <- df_bw_j$wells_drilled[2:length(df_bw_j$wells_drilled)]
  lag_well <- c(lag_well, 0)
  # Same to create one month lagged oil price
  price_lag_1 <- df_bw_j$oil_price[2:length(df_bw_j$wells_drilled)]
  price_lag_1 <- c(price_lag_1, 0)
  # Same to create two month lagged oil price
  price_lag_2 <- df_bw_j$oil_price[3:length(df_bw_j$wells_drilled)]
  price_lag_2 <- c(price_lag_2, 0, 0)
  # Column join to df_bw_j
  df_bw_j <- cbind(df_bw_j, lag_well, price_lag_1, price_lag_2)
  # (Model 1) Fit well model with multiple linear m1_coefion
  m1_fit <- lm(df_bw_j$wells_drilled ~ df_bw_j$oil_price + df_bw_j$lag_well)
  # Extract coefficients and add to m1_coef
  m1_coef <- rbind(m1_coef,coef(m1_fit))
  # (Model 2)
  m2_fit <- lm(df_bw_j$wells_drilled ~ df_bw_j$oil_price + df_bw_j$price_lag_1 + df_bw_j$price_lag_2 + df_bw_j$lag_well)
  # Extract coefficients and add to m1_coef
  m2_coef <- rbind(m2_coef,coef(m2_fit))
}

# Model 3: Basin aggregate
m3_fit <- lm(compare$wells_drilled ~ compare$oil_price + compare$lag_well)
m3_coef <- coef(m3_fit)

#-------------------------------------------------------------------------------
# Compare to actual # of wells drilled
#-------------------------------------------------------------------------------

# Model 1 Predictions
m1_pred <- matrix(0, nrow = length(drill_date), ncol = length(fields))
for (k in 1:length(fields)) {
  Wo <- 0
 for (j in 1:length(drill_date)) {
   m1_pred[j,k] <- m1_coef[k,1] + m1_coef[k,2] * bwInfAd$oil_price[j] + m1_coef[k,3] * Wo
   Wo <- m1_pred[j,k]
 }  
}
m1_pred <- rowSums(round(m1_pred))

# Model 2 Predictions
m2_pred <- matrix(0, nrow = length(drill_date), ncol = length(fields))
for (k in 1:length(fields)) {
  a <- m2_coef[k,1]
  b <- m2_coef[k,2]
  c <- m2_coef[k,3]
  d <- m2_coef[k,4]
  e <- m2_coef[k,5]
  Wo <- 0
  for (j in 1:length(drill_date)) {
    if (j==1){
      m2_pred[j,k] <- a + b * bwInfAd$oil_price[j] + e * Wo
    } else{
      if (j==2){
        m2_pred[j,k] <- a + b * bwInfAd$oil_price[j] + c * bwInfAd$oil_price[j-1] + e * Wo
      }
      else{
        m2_pred[j,k] <- a + b * bwInfAd$oil_price[j] + c * bwInfAd$oil_price[j-1] + d * bwInfAd$oil_price[j-2] + e * Wo
      }
    }    
    Wo <- m2_pred[j,k]
  }  
}
m2_pred <- rowSums(round(m2_pred))

# Model 3 Predictions
m3_pred <- matrix(0, nrow = length(drill_date), ncol =1)
Wo <- 0
for (j in 1:length(drill_date)) {
  m3_pred[j] <- m3_coef[1] + m3_coef[2] * bwInfAd$oil_price[j] + m3_coef[3] * Wo
  Wo <- m3_pred[j]
}
m3_pred <- round(m3_pred)

# Plot output
plot(compare$drill_date, compare$wells_drilled, pch=1, xlab = "Date", ylab = "Number of Wells Drilled",
     main = "Comparison of Actual vs. Predicted Well Counts")
lines(compare$drill_date, m1_pred, col = "red")
lines(compare$drill_date, m2_pred, col = "blue")
lines(compare$drill_date, m3_pred, col = "green")
legend("topleft", c("Actual", "Model 1", "Model 2", "Model 3"), pch = c(1), lty = c(1,1,1), col = c("black", "red", "blue", "green"))