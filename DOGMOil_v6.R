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
# 1. Oil well
# 2. Since 1999
h <- subset(h, subset = (h_well_type == "OW"    &
                         h_apd_aprovd > as.Date("1999-01-01")
                         )
            )

# Inner join h and w on h_api and w_api.
m <- merge(h, w, by.x = "h_api", by.y = "w_api")

# Only want certain columns of data (you may want more than just these ...).
keeps <- c("h_api", "h_spud_dry", "h_well_type", "w_field_num", "h_apd_aprovd")
m <- subset(m, select = keeps)

# Inner join mm with f on w_field_num and f_field_num.
# This gets you the field names rather than just their number.
# Reassign result as m. 
m <- merge(m, f, by.x = "w_field_num", by.y = "f_field_num")

# Add column 'apd_date' as truncated-to-month version of h_apd_aprovd.
m$apd_date <- as.Date(as.yearmon(m[,"h_apd_aprovd"]))

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
# Split the 'sub' dataframe by apd_date and count the number of records.
# This counts the number of APDs approved in fields[k] by month  
  z <- ddply(sub, c("apd_date"), summarize, APDs_approved = length(h_api))
# add the current field number to the all_months dataframe  
  all_months$field_num <- fields[k]
# now right-merge with the 'count' dataframe 'z'  
  x <- merge(z,
             all_months,
             by.x = "apd_date",
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
ww <- which(is.na(df[,"APDs_approved"]))
df[ww, "APDs_approved"] <- 0

# Load oil price data
load(file.path(data_root, "bw.rda"))
# Load gas price data
load(file.path(data_root, "uswp.rda"))
# Load CPI data
load(file.path(data_root, "cpi.rda"))
# Inflation adjustment to 2012-12-01
cpi_index_pick <- 229.601
bw <- bw.z * (cpi.z / cpi_index_pick)
uswp <- uswp.z * (cpi.z / cpi_index_pick)
# Make a new dataframe of inflation adjusted gas prices (gasInfAd) out of zoo vector uswp
gas_price <- coredata(uswp)
apd_date <- index(uswp)
gasInfAd <- data.frame(apd_date, gas_price)
names(gasInfAd)[2] <- "gas_price"
# Make a new dataframe of inflation adjusted black wax prices (bwInfAd) out of zoo vector bw
oil_price <- coredata(bw)
apd_date <- index(bw)
bwInfAd <- data.frame(apd_date, oil_price)
names(bwInfAd)[2] <- "oil_price"
# Merge df, bwInfAd, and gasInfAd
df_bw <- merge(df, bwInfAd, by = "apd_date")
df_bw <- merge(df_bw, gasInfAd, by = "apd_date")

# Make an aggregate dataframe for entire basin (for comparison and alternative model)
compare <- sqldf("select apd_date, sum(APDs_approved) from df_bw group by apd_date")
compare <- merge(compare, bwInfAd, by = "apd_date")
compare <- merge(compare, gasInfAd, by = "apd_date")
names(compare)[2] <- "APDs_approved"
lag_apd <- compare$APDs_approved[2:length(compare$APDs_approved)]
lag_apd <- c(lag_apd, 0)
compare <- cbind(compare, lag_apd)

#-------------------------------------------------------------------------------
# Fitting empircal well drilling model
#-------------------------------------------------------------------------------

# Model 1 (m1): Disaggregated fit to each field with oil price and lagged APDs as predictor variables
# Model 2 (m2): Same as m1, but also include one and two month oil price lags as predictor variables
# Modle 4 (m4): Same as m1, but only oil price and lagged prices are used as predictor variables
# Model 5 (m5): Same as m1, but with gas prices as an additional predictor variable
# Model 6 (m6): Same as m2, but with gas prices and one and two month lagged gas prices as additional predictor variables
# Model 7 (m7): Same as m4, but with the addition of gas prices (current and one/two month lagged)

# Set cutoff date for data to train models on
train_time <- as.Date("2004-12-01")

# Define m1_coef and m2_coef, which will be used as matrix of fitted m1_coefion results
m1_coef <- NULL
m2_coef <- NULL
m4_coef <- NULL
m5_coef <- NULL
m6_coef <- NULL
m7_coef <- NULL

# Define m1_R2 - m8_R2 vectors
m1_R2 <- NULL
m2_R2 <- NULL
m4_R2 <- NULL
m5_R2 <- NULL
m6_R2 <- NULL
m7_R2 <- NULL

# Define m1_p - m8_p vectors
m1_p <- NULL
m2_p <- NULL
m4_p <- NULL
m5_p <- NULL
m6_p <- NULL
m7_p <- NULL

# # Gas fields
# fields_select <- c(630, 48, 710, 35, 18, 132, 40, 665)
# Oil fields
fields_select <- c(105, 72, 590, 55, 718, 117, 1, 60)

for (k in 1:length(fields_select)) {
  # Select field number
  j <- fields_select[k]
  # Select subset of current field number from df_bw that occured before the training date cutoff "train_time"
  df_bw_j <- df_bw[which(df_bw$field_num == j & df_bw$apd_date < train_time),]
  # Add column with one lagged number of wells drilled
  lag_apd <- df_bw_j$APDs_approved[2:length(df_bw_j$APDs_approved)]
  lag_apd <- c(lag_apd, 0)
  # Same to create one month lagged oil price
  price_lag_1 <- df_bw_j$oil_price[2:length(df_bw_j$APDs_approved)]
  price_lag_1 <- c(price_lag_1, 0)
  # Same to create two month lagged oil price
  price_lag_2 <- df_bw_j$oil_price[3:length(df_bw_j$APDs_approved)]
  price_lag_2 <- c(price_lag_2, 0, 0)
  # Same to create one month lagged gas price
  gas_lag_1 <- df_bw_j$gas_price[2:length(df_bw_j$APDs_approved)]
  gas_lag_1 <- c(gas_lag_1, 0)
  # Same to create two month lagged gas price
  gas_lag_2 <- df_bw_j$gas_price[3:length(df_bw_j$APDs_approved)]
  gas_lag_2 <- c(gas_lag_2, 0, 0)
  # Column join to df_bw_j
  df_bw_j <- cbind(df_bw_j, lag_apd, price_lag_1, price_lag_2, gas_lag_1, gas_lag_2)
  
  # (Model 1) Fit well model with multiple linear regression
  m1_fit <- lm(df_bw_j$APDs_approved ~ df_bw_j$oil_price + df_bw_j$lag_apd)
  # Extract coefficients and add to m2_coef
  m1_coef <- rbind(m1_coef,coef(m1_fit))
  m1_R2 <- rbind(m1_R2,summary(m1_fit)$adj.r.squared)
  m1_p <- rbind(m1_p,summary(m1_fit)$coefficients[,4])
  
  # (Model 2)
  m2_fit <- lm(df_bw_j$APDs_approved ~ df_bw_j$oil_price + df_bw_j$price_lag_1 + df_bw_j$price_lag_2 + df_bw_j$lag_apd)
  # Extract coefficients and add to m2_coef
  m2_coef <- rbind(m2_coef,coef(m2_fit))
  m2_R2 <- rbind(m2_R2,summary(m2_fit)$adj.r.squared)
  m2_p <- rbind(m2_p,summary(m2_fit)$coefficients[,4])
  
  # (Model 4)
  m4_fit <- lm(df_bw_j$APDs_approved ~ df_bw_j$oil_price + df_bw_j$price_lag_1 + df_bw_j$price_lag_2)
  # Extract coefficients and add to m4_coef
  m4_coef <- rbind(m4_coef,coef(m4_fit))
  m4_R2 <- rbind(m4_R2,summary(m4_fit)$adj.r.squared)
  m4_p <- rbind(m4_p,summary(m4_fit)$coefficients[,4])
  
  # (Model 5)
  m5_fit <- lm(df_bw_j$APDs_approved ~ df_bw_j$oil_price + df_bw_j$gas_price + df_bw_j$lag_apd)
  # Extract coefficients and add to m2_coef
  m5_coef <- rbind(m5_coef,coef(m5_fit))
  m5_R2 <- rbind(m5_R2,summary(m5_fit)$adj.r.squared)
  m5_p <- rbind(m5_p,summary(m5_fit)$coefficients[,4])
  
  # (Model 6)
  m6_fit <- lm(df_bw_j$APDs_approved ~
                 df_bw_j$oil_price + df_bw_j$price_lag_1 + df_bw_j$price_lag_2 +
                 df_bw_j$gas_price + df_bw_j$gas_lag_1 + df_bw_j$gas_lag_2+
                 df_bw_j$lag_apd)
  # Extract coefficients and add to m2_coef
  m6_coef <- rbind(m6_coef,coef(m6_fit))
  m6_R2 <- rbind(m6_R2,summary(m6_fit)$adj.r.squared)
  m6_p <- rbind(m6_p,summary(m6_fit)$coefficients[,4])
  
  # (Model 7)
  m7_fit <- lm(df_bw_j$APDs_approved ~
                 df_bw_j$oil_price + df_bw_j$price_lag_1 + df_bw_j$price_lag_2 +
                 df_bw_j$gas_price + df_bw_j$gas_lag_1 + df_bw_j$gas_lag_2)
  # Extract coefficients and add to m4_coef
  m7_coef <- rbind(m7_coef,coef(m7_fit))
  m7_R2 <- rbind(m7_R2,summary(m7_fit)$adj.r.squared)
  m7_p <- rbind(m7_p,summary(m7_fit)$coefficients[,4])
}

# In case the any of the fields have zero data before the time cutoff, omit NA values in model fits
m1_coef <- na.omit(m1_coef)
m2_coef <- na.omit(m2_coef)
m4_coef <- na.omit(m4_coef)
m5_coef <- na.omit(m5_coef)
m6_coef <- na.omit(m6_coef)
m7_coef <- na.omit(m7_coef)

# Subset aggregate data for training fit on data that occurs before time cutoff "train_time"
compare_train <- compare[which(compare$apd_date < train_time),]
# Add columns with lagged oil and gas prices, price deltas
oil_lag_1 <- compare_train$oil_price[2:length(compare_train$APDs_approved)]
oil_lag_1 <- c(oil_lag_1, 0)
oil_lag_2 <- compare_train$oil_price[3:length(compare_train$APDs_approved)]
oil_lag_2 <- c(oil_lag_2, 0, 0)
oil_d_1 <- compare_train$oil_price - oil_lag_1
oil_d_2 <- oil_lag_1 - oil_lag_2
oil_d_3 <- (compare_train$oil_price - oil_lag_2)/2
gas_lag_1 <- compare_train$gas_price[2:length(compare_train$APDs_approved)]
gas_lag_1 <- c(gas_lag_1, 0)
gas_lag_2 <- compare_train$gas_price[3:length(compare_train$APDs_approved)]
gas_lag_2 <- c(gas_lag_2, 0, 0)
gas_d_1 <- compare_train$gas_price - gas_lag_1
gas_d_2 <- gas_lag_1 - gas_lag_2
gas_d_3 <- (compare_train$gas_price - gas_lag_2)/2
compare_train <- cbind(compare_train, oil_lag_1, oil_lag_2, oil_d_1, oil_d_2, oil_d_3, gas_lag_1, gas_lag_2, gas_d_1, gas_d_2, gas_d_3)
compare_train <- compare_train[1:(length(compare_train[,1])-2),]

# Model 3: Basin aggregate
m3_fit <- lm(compare_train$APDs_approved ~ compare_train$oil_price + compare_train$lag_apd)
m3_coef <- coef(m3_fit)
m3_R2 <- summary(m3_fit)$adj.r.squared
m3_p <- summary(m3_fit)$coefficients[,4]

# Model 8: Basin aggregate with gas
m8_fit <- lm(compare_train$APDs_approved ~ compare_train$oil_price + compare_train$gas_price + compare_train$lag_apd)
m8_coef <- coef(m8_fit)
m8_R2 <- summary(m8_fit)$adj.r.squared
m8_p <- summary(m8_fit)$coefficients[,4]

# # Model 9: Basin aggregate with gas
# m9_fit <- lm(compare_train$APDs_approved ~
#                compare_train$oil_price + compare_train$oil_lag_1 + compare_train$oil_lag_2 +
#                compare_train$oil_d_1 + compare_train$oil_d_2 + compare_train$oil_d_3 +
#                compare_train$gas_price + compare_train$gas_lag_1 + compare_train$gas_lag_2 +
#                compare_train$gas_d_1 + compare_train$gas_d_2 + compare_train$gas_d_3 +
#                compare_train$lag_apd)
# m9_coef <- coef(m9_fit)
# m9_R2 <- summary(m9_fit)$adj.r.squared
# m9_p <- summary(m9_fit)$coefficients[,4]

#-------------------------------------------------------------------------------
# Compare to actual # of wells drilled
#-------------------------------------------------------------------------------

# Subset aggregate data for testing fit on data that occurs after time cutoff "train_time"
compare_test <- compare[which(compare$apd_date >= train_time),]

# Model 1 Predictions
m1_pred <- matrix(0, nrow = length(compare_test$apd_date), ncol = length(m1_coef[,1]))
for (k in 1:length(m1_coef[,1])) {
  Wo <- 0
 for (j in 1:length(compare_test$apd_date)) {
   m1_pred[j,k] <- m1_coef[k,1] + m1_coef[k,2] * compare_test$oil_price[j] + m1_coef[k,3] * Wo
   Wo <- m1_pred[j,k]
 }
}
m1_pred <- rowSums(round(m1_pred))
m1_error <- sum((compare_test$APDs_approved - m1_pred)^2)

# Model 2 Predictions
m2_pred <- matrix(0, nrow = length(compare_test$apd_date), ncol = length(m2_coef[,1]))
for (k in 1:length(m2_coef[,1])) {
  a <- m2_coef[k,1]
  b <- m2_coef[k,2]
  c <- m2_coef[k,3]
  d <- m2_coef[k,4]
  e <- m2_coef[k,5]
  Wo <- 0
  for (j in 1:length(compare_test$apd_date)) {
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
m2_error <- sum((compare_test$APDs_approved - m2_pred)^2)

# Model 3 Predictions
m3_pred <- matrix(0, nrow = length(compare_test$apd_date), ncol =1)
Wo <- 0
for (j in 1:length(compare_test$apd_date)) {
  m3_pred[j] <- m3_coef[1] + m3_coef[2] * bwInfAd$oil_price[j] + m3_coef[3] * Wo
  Wo <- m3_pred[j]
}
m3_pred <- round(m3_pred)
m3_error <- sum((compare_test$APDs_approved - m3_pred)^2)

# Model 4 Predictions
m4_pred <- matrix(0, nrow = length(compare_test$apd_date), ncol = length(m4_coef[,1]))
for (k in 1:length(m4_coef[,1])) {
  a <- m4_coef[k,1]
  b <- m4_coef[k,2]
  c <- m4_coef[k,3]
  d <- m4_coef[k,4]  
  Wo <- 0
  for (j in 1:length(compare_test$apd_date)) {
    if (j==1){
      m4_pred[j,k] <- a + b * bwInfAd$oil_price[j]
    } else{
      if (j==2){
        m4_pred[j,k] <- a + b * bwInfAd$oil_price[j] + c * bwInfAd$oil_price[j-1]
      }
      else{
        m4_pred[j,k] <- a + b * bwInfAd$oil_price[j] + c * bwInfAd$oil_price[j-1] + d * bwInfAd$oil_price[j-2]
      }
    }    
    Wo <- m4_pred[j,k]
  }  
}
m4_pred <- rowSums(round(m4_pred))
m4_error <- sum((compare_test$APDs_approved - m4_pred)^2)

# Model 5 Predictions
m5_pred <- matrix(0, nrow = length(compare_test$apd_date), ncol = length(m5_coef[,1]))
for (k in 1:length(m5_coef[,1])) {
  Wo <- 0
  for (j in 1:length(compare_test$apd_date)) {
    m5_pred[j,k] <- m5_coef[k,1] + m5_coef[k,2] * bwInfAd$oil_price[j] + m5_coef[k,3] * df_bw$gas_price[j] + m5_coef[k,4] * Wo
    Wo <- m5_pred[j,k]
  }  
}
m5_pred <- rowSums(round(m5_pred))
m5_error <- sum((compare_test$APDs_approved - m5_pred)^2)

# Model 6 Predictions
m6_pred <- matrix(0, nrow = length(compare_test$apd_date), ncol = length(m6_coef[,1]))
for (k in 1:length(m6_coef[,1])) {
  a <- m6_coef[k,1]
  b <- m6_coef[k,2]
  c <- m6_coef[k,3]
  d <- m6_coef[k,4]
  e <- m6_coef[k,5]
  f <- m6_coef[k,6]
  g <- m6_coef[k,7]
  h <- m6_coef[k,8]
  Wo <- 0
  for (j in 1:length(compare_test$apd_date)) {
    if (j==1){
      m6_pred[j,k] <- a + b * bwInfAd$oil_price[j] + e * compare_test$gas_price[j] + h * Wo
    } else{
      if (j==2){
        m6_pred[j,k] <- a + b * bwInfAd$oil_price[j] + c * bwInfAd$oil_price[j-1] +
          e * compare_test$gas_price[j] + f * compare_test$gas_price[j-1] + h * Wo
      }
      else{
        m6_pred[j,k] <- a + b * bwInfAd$oil_price[j] + c * bwInfAd$oil_price[j-1] + d * bwInfAd$oil_price[j-2] +
          e * compare_test$gas_price[j] + f * compare_test$gas_price[j-1] + g * compare_test$gas_price[j-2] + h * Wo
      }
    }    
    Wo <- m6_pred[j,k]
  }  
}
m6_pred <- rowSums(round(m6_pred))
m6_error <- sum((compare_test$APDs_approved - m6_pred)^2)

# Model 7 Predictions
m7_pred <- matrix(0, nrow = length(compare_test$apd_date), ncol = length(m7_coef[,1]))
for (k in 1:length(m7_coef[,1])) {
  a <- m7_coef[k,1]
  b <- m7_coef[k,2]
  c <- m7_coef[k,3]
  d <- m7_coef[k,4]
  e <- m7_coef[k,5]
  f <- m7_coef[k,6]
  g <- m7_coef[k,7]
  Wo <- 0
  for (j in 1:length(compare_test$apd_date)) {
    if (j==1){
      m7_pred[j,k] <- a + b * bwInfAd$oil_price[j] + e * compare_test$gas_price[j]
    } else{
      if (j==2){
        m7_pred[j,k] <- a + b * bwInfAd$oil_price[j] + c * bwInfAd$oil_price[j-1] +
          e * compare_test$gas_price[j] + f * compare_test$gas_price[j-1]
      }
      else{
        m7_pred[j,k] <- a + b * bwInfAd$oil_price[j] + c * bwInfAd$oil_price[j-1] + d * bwInfAd$oil_price[j-2] +
          e * compare_test$gas_price[j] + f * compare_test$gas_price[j-1] + g * compare_test$gas_price[j-2]
      }
    }    
    Wo <- m7_pred[j,k]
  }  
}
m7_pred <- rowSums(round(m7_pred))
m7_error <- sum((compare_test$APDs_approved - m7_pred)^2)

# Model 8 Predictions
m8_pred <- matrix(0, nrow = length(compare_test$apd_date), ncol =1)
Wo <- 0
for (j in 1:length(compare_test$apd_date)) {
  m8_pred[j] <- m8_coef[1] + m8_coef[2] * bwInfAd$oil_price[j] + m8_coef[3] * compare_test$gas_price[j] + m8_coef[4] * Wo
  Wo <- m8_pred[j]
}
m8_pred <- round(m8_pred)
m8_error <- sum((compare_test$APDs_approved - m8_pred)^2)

# Error Summary Table
Model <- c(1:8)
Error <- c(m1_error, m2_error, m3_error, m4_error, m5_error, m6_error, m7_error, m8_error)
Pred <- c(sum(m1_pred), sum(m2_pred), sum(m3_pred), sum(m4_pred), sum(m5_pred), sum(m6_pred), sum(m7_pred), sum(m8_pred))
error_summary <- data.frame(Model, Error, Pred)
write.csv(error_summary, file = "error.csv", row.names = FALSE)

# R2 Table
R2 <- data.frame(fields_select, m1_R2, m2_R2, m4_R2, m5_R2, m6_R2, m7_R2)
write.csv(R2, file = "R2.csv", row.names = FALSE)

# p-values Table
pvalue <- data.frame(fields_select, m1_p, m2_p, m4_p, m5_p, m6_p, m7_p)
write.csv(pvalue, file = "pvalue.csv", row.names = FALSE)

# Plot Output - Test
plot(compare_test$apd_date, compare_test$APDs_approved, type = "l", lty = 1, lwd = 2, xlab = "Date", ylab = "Number of APDs Approved",
     main = "Comparison of Actual vs. Predicted APD Approvals")
lines(compare_test$apd_date, m1_pred, col = "red")
lines(compare_test$apd_date, m2_pred, col = "blue")
lines(compare_test$apd_date, m3_pred, col = "green")
lines(compare_test$apd_date, m4_pred, col = "orange")
lines(compare_test$apd_date, m5_pred, col = "brown")
lines(compare_test$apd_date, m6_pred, col = "yellow")
lines(compare_test$apd_date, m7_pred, col = "purple")
lines(compare_test$apd_date, m8_pred, col = "gray")
legend("topleft", c("Actual", "Model 1", "Model 2", "Model 3", "Model 4", "Model 5", "Model 6", "Model 7", "Model 8"),
       lty = c(1,1,1,1,1,1,1,1), col = c("black", "red", "blue", "green", "orange", "brown", "yellow", "purple", "gray"))

# # Oil price plot
# plot(bwInfAd$apd_date, bwInfAd$oil_price, type = "l", lty = 1, xlab = "Date", ylab = "Oil Price (Dec 2012 $ / bbl)",
#      main = "Inflation Adjusted Oil Price History 1999 - 2012")