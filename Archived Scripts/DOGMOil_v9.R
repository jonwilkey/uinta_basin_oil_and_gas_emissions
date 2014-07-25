#-------------------------------------------------------------------------------
# Options 
#-------------------------------------------------------------------------------
# don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)

#-------------------------------------------------------------------------------
# Paths
#-------------------------------------------------------------------------------
# Change this to the folder where you keep your *.rda files
# data_root <- "/Users/john/Dropbox/CLEAR/DOGM Data/Prepared Data"
data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"
# Set working directory
work_root <- "D:/Dropbox/CLEAR/DOGM Data"
setwd(work_root)

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
# Load *.rda files
load(file.path(data_root, "welldata.rda"))
load(file.path(data_root, "histdata.rda"))
load(file.path(data_root, "fieldata.rda"))
load(file.path(data_root, "production.rda"))
# Rename for brevity
w <- welldata
h <- histdata
f <- fieldata
p <- production
remove(fieldata, histdata, production, welldata)

# Select records subject to the that are (1) given well type and (2) Since 1999
# For Oil Wells
h <- subset(h, subset = (h_well_type == "OW" &
                           h_apd_aprovd > as.Date("1999-01-01")))

# # For Gas Wells
# h <- subset(h, subset = (h_well_type == "GW" &
#                          h_apd_aprovd > as.Date("1999-01-01")))

# Inner join h and w on h_api and w_api.
m <- merge(h, w, by.x = "h_api", by.y = "w_api")

# Only want certain columns of data (you may want more than just these ...).
keeps <- c("h_api", "h_apd_aprovd", "h_spud_dry", "h_spud_rotry",
           "h_compl_date", "h_well_type", "h_first_prod", "h_wellstatus",
           "w_field_num", "w_abndondate")
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

# Each pass through the loop creates a dataframe---one for each field and stores
# it as an element in a list. Now stack the list elements into one combined
# dataframe
df <- do.call("rbind", df_list)

# An "NA" value means no records for that month - so replace NA with 0. 
ww <- which(is.na(df[,"APDs_approved"]))
df[ww, "APDs_approved"] <- 0

# Load oil price data, gas price data, and CPI data
load(file.path(data_root, "bw.rda"))
load(file.path(data_root, "uswp.rda"))
load(file.path(data_root, "cpi.rda"))

# Inflation adjustment to 2012-12-01
cpi_index_pick <- 229.601
bw <- bw.z * (cpi_index_pick / cpi.z)
uswp <- uswp.z * (cpi_index_pick / cpi.z)

# Make a new dataframe of inflation adjusted gas prices (gasInfAd) out of zoo
# vector uswp
gas_price <- coredata(uswp)
apd_date <- index(uswp)
gasInfAd <- data.frame(apd_date, gas_price)
names(gasInfAd)[2] <- "gas_price"

# Make a new dataframe of inflation adjusted black wax prices (bwInfAd) out of
# zoo vector bw
oil_price <- coredata(bw)
apd_date <- index(bw)
bwInfAd <- data.frame(apd_date, oil_price)
names(bwInfAd)[2] <- "oil_price"

# Merge df, bwInfAd, and gasInfAd
df_bw <- merge(df, bwInfAd, by = "apd_date")
df_bw <- merge(df_bw, gasInfAd, by = "apd_date")

# Make an aggregate dataframe for basin (for comparison and alternative model)
compare <- sqldf("select apd_date, sum(APDs_approved)
                 from df_bw
                 group by apd_date")
compare <- merge(compare, bwInfAd, by = "apd_date")
compare <- merge(compare, gasInfAd, by = "apd_date")
names(compare)[2] <- "APDs_approved"
lag_apd <- compare$APDs_approved[2:length(compare$APDs_approved)]
lag_apd <- c(lag_apd, 0)
compare <- cbind(compare, lag_apd)
# Drop last month since it could contain incomplete records
# compare <- compare[1:(length(compare[,1])-1),]

#-------------------------------------------------------------------------------
# Fitting empircal well drilling model
#-------------------------------------------------------------------------------

# Set # of months for moving fit window (i.e. # of time steps per fit)
fit.time <- 12
steps <- length(compare[,1])/fit.time
m8_coef <- matrix(0, nrow = steps, ncol = 4)
for (j in 1:steps) {
  temp <- compare[(1+fit.time*(j-1)):(fit.time*j),]
  m8_fit <- lm(APDs_approved ~ oil_price + gas_price + lag_apd,
               data = temp)
  m8_coef[j,] <- coef(m8_fit)
}

# Get probability density function (pdf) for each coefficient
# a = intercept, b = oil price, c = gas price, d = lag APD
a <- density(m8_coef[,1])
b <- density(m8_coef[,2])
c <- density(m8_coef[,3])
d <- density(m8_coef[,4])

# Determine normalized cumulative distribution function (ncdf) from pdfs. Note
# that the step size for x values in density() is constant, so only size of
# first step (diff(...$x[1:2])) needs to be determined
cdf.a <- cumsum(a$y * diff(a$x[1:2]))
cdf.b <- cumsum(b$y * diff(b$x[1:2]))
cdf.c <- cumsum(c$y * diff(c$x[1:2]))
cdf.d <- cumsum(d$y * diff(d$x[1:2]))
# Normalize in case of rounding error
cdf.a <- cdf.a / max(cdf.a)
cdf.b <- cdf.b / max(cdf.b)
cdf.c <- cdf.c / max(cdf.c)
cdf.d <- cdf.d / max(cdf.d)

#-------------------------------------------------------------------------------
# Compare to actual # of wells drilled
#-------------------------------------------------------------------------------

# Pick number of Monte Carlo Runs
nrun <- 10^4
m8_pred <- matrix(0, nrow = length(compare[,1]), ncol = nrun)

# Monte Carlo Simulation
for (i in 1:nrun) {
  
  # Initialize # of APDs approved as zero
  Wo <- 0
  
  # Each j loop is one full simulation
  for (j in 1:steps) {
    
    # Generate random numbers from a uniform distribution between 0 and 1
    rn <- runif(4, min = 0, max = 1)
    
    # Use findInterval to pick index of and assign value for each variable
    a.temp <- a$x[findInterval(rn[1],c(0,cdf.a))]
    b.temp <- b$x[findInterval(rn[2],c(0,cdf.b))]
    c.temp <- c$x[findInterval(rn[3],c(0,cdf.c))]
    d.temp <- d$x[findInterval(rn[4],c(0,cdf.d))]
        
    # Select required data for given time step
    temp <- compare[(1+fit.time*(j-1)):(fit.time*j),]
    
    # Calculate APDs approved given price data and random variable picks
    # Each k loop is one fit window period
    for (k in 1:length(temp[,1])) {
      m8_pred[(k+fit.time*(j-1)),i] <- a.temp +
                                       b.temp * temp$oil_price[k] +
                                       c.temp * temp$gas_price[k] +
                                       d.temp * Wo
      Wo <- m8_pred[(k+fit.time*(j-1)),i]
    }
  }
}

# Round values to get whole # of APDs approved
m8_pred <- round(m8_pred)

# Get confidence interval statistics
percentile <- matrix(0, nrow = length(m8_pred[,1]), ncol = 2)
for (i in 1:length(m8_pred[,1])) {
  temp <- quantile(m8_pred[i,], c(0.05, 0.95))
  percentile[i,1] <- temp[1]
  if (percentile[i,1] < 0) {
    percentile[i,1] = 0
  }
  percentile[i,2] <- temp[2]  
}

# Plot Output
pdf(file = "M8MC APD Approval Results from DOGMOil_v9 for Oil with CI.pdf")
hist(m8_coef[,1], breaks = 10, freq = FALSE,
     xlab = "Value of Intercept",
     ylab = "Probability",
     main = "Histogram and PDF for Intercept")
lines(a, col = "blue", lwd = 2)
plot(a$x, cdf.a,
     type = "l",
     xlab = "Value of Intercept",
     ylab = "Cumulative Probability",
     main = "CDF for Intercept")
hist(m8_coef[,2], breaks = 10, freq = FALSE,
     xlab = "Value of Oil Price Coefficient",
     ylab = "Probability",
     main = "Histogram and PDF for Oil Price Coefficient")
lines(b, col = "blue", lwd = 2)
plot(b$x, cdf.b,
     type = "l",
     xlab = "Value of Oil Price Coefficient",
     ylab = "Cumulative Probability",
     main = "CDF for Oil Price Coefficient")
hist(m8_coef[,3], breaks = 10, freq = FALSE,
     xlab = "Value of Gas Price Coefficient",
     ylab = "Probability",
     main = "Histogram and PDF for Gas Price Coefficient")
lines(c, col = "blue", lwd = 2)
plot(c$x, cdf.c,
     type = "l",
     xlab = "Value of Gas Price Coefficient",
     ylab = "Cumulative Probability",
     main = "CDF for Gas Price Coefficient")
hist(m8_coef[,4], breaks = 10, freq = FALSE,
     xlab = "Value of Lagged APD Approvals Coefficient",
     ylab = "Probability",
     main = "Histogram and PDF for Lagged APD Approvals Coefficient")
lines(d, col = "blue", lwd = 2)
plot(d$x, cdf.d,
     type = "l",
     xlab = "Value of Lagged APD Approvals Coefficient",
     ylab = "Cumulative Probability",
     main = "CDF for Lagged APD Approvals Coefficient")


pdf(file = "M8MC APD Approval Results from DOGMOil_v9 for Oil with CI.pdf")
plot(compare$apd_date, compare$APDs_approved,
     type = "l",
     lty = 1,
     lwd = 1,
     xlab = "Date",
     ylab = "Number of APDs Approved",
     main = "M8MC APD Approvals for Gas (n = 10^4)",
     ylim = c(0, 300))
lines(compare$apd_date, percentile[,2], col = "blue")
lines(compare$apd_date, percentile[,1], col = "red")
legend("topleft", c("Actual", "M8MC 95%", "M8MC 5%"),
       lty = c(1,1,1), col = c("black", "blue", "red"))

dev.off()