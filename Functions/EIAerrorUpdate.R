# Function Info -----------------------------------------------------------
# Name:      EIAerrorUpdate.R (EIA Annual Energy Outlook Error Analysis Update)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - List object containing directory paths for file I/O


# Outputs -----------------------------------------------------------------

# blah


# Description -------------------------------------------------------------

# This function blah


# Function ----------------------------------------------------------------

EIAerrorUpdate <- function(path,) {xq, tsteps, }


# Load EIA error *.csv files ----------------------------------------------

# Read in source data from *.csv files. The *.csv files should be structured as
# follows: columns = AEO reports, rows = time steps, values = % error between
# actual price and predicted price. Lower right of corner of export matrix
# should all be NA.
egas <- read.csv(file.path(path$raw, "EIA_gp_error_export.csv"), header = FALSE)
eoil <- read.csv(file.path(path$raw, "EIA_op_error_export.csv"), header = FALSE)


# Get CDF for each prediction year ----------------------------------------

# Assume that % relative error in each year is drawn from a normal distribution 
# with mean and sd given equal to that of the sample set in each year. We can
# then use qnorm() to get CDF

# Define quantiles at which to determine relative error values from sample
xq <- seq(from = 0.0001, to = 0.9999, by = 0.0001)

# Preallocate space for results
Eoil <- matrix(0, nrow = length(xq), ncol = tsteps)
Egas <- Eoil

# For each year into forecast, determine CDF
for (i in 1:tsteps){
  
  # Subset data to select only current year, omit any NAs
  otemp <- eoil[,i]
  gtemp <- egas[,i]
  otemp <- otemp[which(!is.na(otemp))]
  gtemp <- gtemp[which(!is.na(gtemp))]
  
  # Calculate CDF with qnorm
  Eoil[,i] <- qnorm(p = xq, mean = mean(otemp), sd = sd(otemp))
  Egas[,i] <- qnorm(p = xq, mean = mean(gtemp), sd = sd(gtemp))
}


# Test Plot ---------------------------------------------------------------

# CDF for % error in oil

# Line colors
linecolor <- rainbow(ncol(Eoil))

# Main plot
plot(Eoil[,1], xq,
     type = "l",
     col = linecolor[1],
     xlim = c(1.1*min(Eoil), 1.1*max(Eoil)),
     ylim = c(0, 1),
     xlab = "% Error",
     ylab = "Cumulative Probability",
     main = "CDF of Relative % Error of EIA Oil Price Forecasts")

# For all other timesteps
for (i in 2:ncol(Eoil)) {
  lines(Eoil[,i], xq, col = linecolor[i])
}

legend("topleft",
       c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7", "Y8", "Y9", "Y10"),
       ncol = 2, lty = 1, col = linecolor)

# Main plot for gas
plot(Egas[,1], xq,
     type = "l",
     col = linecolor[1],
     xlim = c(1.1*min(Egas), 1.1*max(Egas)),
     ylim = c(0, 1),
     xlab = "% Error",
     ylab = "Cumulative Probability",
     main = "CDF of Relative % Error of EIA Gas Price Forecasts")

# For all other timesteps
for (i in 2:ncol(Egas)) {
  lines(Egas[,i], xq, col = linecolor[i])
}

legend("topleft",
       c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7", "Y8", "Y9", "Y10"),
       ncol = 2, lty = 1, col = linecolor)


# Generate test runs
otest <- Eoil[round(runif(10e3, min = 1, max = nrow(Eoil))),]
gtest <- Egas[round(runif(10e3, min = 1, max = nrow(Egas))),]

# Space for results
o.err.q <- matrix(0, nrow = 9, ncol = tsteps)
g.err.q <- o.err.q

# Get quantiles of results
quant <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)
for (i in 1:tsteps) {
  o.err.q[,i] <- quantile(otest[,i], probs = quant)
  g.err.q[,i] <- quantile(gtest[,i], probs = quant)
}

linecolor <- rainbow(length(quant))

# Main plot with largest quantile result for oil
plot(o.err.q[1,],
     type = "l",
     col = linecolor[1],
     ylim = c(1.1*min(o.err.q), 1.1*max(o.err.q)),
     xlab = "Time Steps (years)",
     ylab = "% Relative Error",
     main = "% Relative Error EIA Forecast vs Actual Prices for Oil")

# All the other quantile lines
for (i in 2:length(quant)) {lines(o.err.q[i,], col = linecolor[i])}

# Add legend
legend("topright", c("90%", "80%", "70%", "60%", "50%", "40%", "30%", "20%", "10%"),
       ncol = 2, col = c(linecolor), lty = 1)