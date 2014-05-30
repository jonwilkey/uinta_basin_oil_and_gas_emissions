# Script Info -------------------------------------------------------------
# declineCurve_v9.R (DOGM Decline Curve Fitting)
# Version 9
# 05/17/14
# Jon Wilkey


# Script Info -------------------------------------------------------------
# v1 -Coded for fits for oil and gas for basin, eight most active oil and gas
#     fields, and individual wells in each of those fields.
#
# v2 -Updated to have field level fit all fields in production.rda, dropped
#     individual well fits.
#
# v3 -Commented out PDF outputs of v2.
#    -Added export segment to save hyperbolic fit coefficients for field level
#     as *.rda files for use in Conventional_v1.R.
#
# v4 -Rewrote script to fit individual oil well decline curves and determine 
#     probability and cumulative distribution functions for each coefficient in
#     hyperbolic decline curve equation for all oil fields at the 95% cutoff.
#
# v5 -Revised to fit PDF with 'fitdistr' from the MASS library so that PDF could
#     be fitted with a log-normal distribution. Motivation for change being that
#     'density' function generated negative x-values, which destroyed Monte-Carlo
#     simulation in conventional_v4.R.
#
# v6 -Added decline curves for off-type production (oil from gas wells, gas from
#     oil wells).
#    -Removed log-normal distribution fitting (wasn't working out)
#    -Revised outlier rejection criteria for fitted decline curve coefficients.
#     Now rejecting (1) any values of a > maximum monthly production in original
#     dataset, and (2) any negative values of a.
#    -Changed density() function calls to truncate PDF to values >= 0 for only
#     coefficients a and c.
#
# v7 -Added printing option to show plots of individual well fits for OOW to aid
#     with debugging MC simulation results and included additional goodness of
#     fit parameters (residual sum of squares, comparison of total production).
#
# v8 -Added fit parameters to GGW section, exported new PDF and CDF files for
#     GGW.
#
# v9 -Rewrote to only consider fields from top ten fields and "other" category.
#    -Also rewrote in function format.


# Options -----------------------------------------------------------------
# Don't want strings 'typed' as factors but as characters
options(stringsAsFactors=FALSE)


# Paths -------------------------------------------------------------------
# Prepared data directory
data_root <- "D:/Dropbox/CLEAR/DOGM Data/Prepared Data"
# Plot directory
plot_root <- "D:/Dropbox/CLEAR/DOGM Data/Plots"
# Functions directory
fin <- "D:/Dropbox/CLEAR/DOGM Data/Functions"
# Working directory
work_root <- "D:/Dropbox/CLEAR/DOGM Data"
setwd(work_root)


# Functions ---------------------------------------------------------------
# List of functions used in this script to be loaded here
flst <- file.path(fin,c("write_excel.R"))

# Load each function in list
for (f in flst) source(f)

# Remove temporary variables
remove(fin, flst, f)


# Libraries ---------------------------------------------------------------
# 'minpack.lm' for nonlinear least-squares regression using Levenberg-Marquardt
# algorithm
library(minpack.lm)
# 'sqldf' for SQL queries
library(sqldf)


# Load required data ------------------------------------------------------

# Load the dataframe named 'production'
p <- load(file.path(data_root, "production.rda"))

# Rename dataframe from brevity
p <- production
remove("production")

# Drop first month of production since it can be partial month
p <- subset(p, time != 0)

# Drop anything not in Uintah or Duchesne county
p <- subset(p, w_county == "UINTAH" | w_county == "DUCHESNE")

# Create subset dataframe which contains only API#s and oil prod. vs. time data
p.oow <- subset(p, w_well_type=="OW")
attach(p.oow)
p.oow <- data.frame(p_api, time, p_oil_prod)
detach(p.oow)

# Same for gas from oil wells
p.gow <- subset(p, w_well_type == "OW")
attach(p.gow)
p.gow <- data.frame(p_api, time, p_gas_prod)
detach(p.gow)

# Same for gas from gas wells
p.ggw <- subset(p, w_well_type == "GW")
attach(p.ggw)
p.ggw <- data.frame(p_api, time, p_gas_prod)
detach(p.ggw)

# Same for oil from gas wells
p.ogw <- subset(p, w_well_type == "GW")
attach(p.ogw)
p.ogw <- data.frame(p_api, time, p_oil_prod)
detach(p.ogw)


# Other Inputs ------------------------------------------------------------

# Field Listing
field <- c(630, 105, 72, 55, 65, 710, 665, 590, 60, 718)


# Regression to Oil Production from Individual Oil Wells by Field ---------

# Create list of unique API #'s and their Field # in p that are oil wells
api.ow <- sqldf("select distinct(p_api), w_field_num from p where w_well_type = 'OW'")

# Get api list for each field in field listing
api.list.630   <- subset(api.ow, w_field_num == 630, row.names = FALSE);   api.list.630   <- api.list.630[,1]
api.list.105   <- subset(api.ow, w_field_num == 105, row.names = FALSE);   api.list.105   <- api.list.105[,1]
api.list.72    <- subset(api.ow, w_field_num == 72, row.names = FALSE);    api.list.72    <- api.list.72[,1]
api.list.55    <- subset(api.ow, w_field_num == 55, row.names = FALSE);    api.list.55    <- api.list.55[,1]
api.list.65    <- subset(api.ow, w_field_num == 65, row.names = FALSE);    api.list.65    <- api.list.65[,1]
api.list.710   <- subset(api.ow, w_field_num == 710, row.names = FALSE);   api.list.710   <- api.list.710[,1]
api.list.665   <- subset(api.ow, w_field_num == 665, row.names = FALSE);   api.list.665   <- api.list.665[,1]
api.list.590   <- subset(api.ow, w_field_num == 590, row.names = FALSE);   api.list.590   <- api.list.590[,1]
api.list.60    <- subset(api.ow, w_field_num == 60, row.names = FALSE);    api.list.60    <- api.list.60[,1]
api.list.718   <- subset(api.ow, w_field_num == 718, row.names = FALSE);   api.list.718   <- api.list.718[,1]

# Get api list for all other fields by removing one field at a time from api.ow
api.list.other <- subset(api.ow, w_field_num != field[1], row.names = FALSE)
for (i in 2:length(field)) {
  api.list.other <- subset(api.list.other, w_field_num != field[i], row.names = FALSE)
}
api.list.other <- api.list.other[,1]

# Enter indices here of wells that fail to converge to skip them in subsequent
# iterations of for-loop
api.list.630 <- api.list.630[c(-
                               -39,   -43,   -52,   -58,   -65,   -74)]

api.list.105 <- api.list.105[c(-
                               -941,  -952,  -957,  -969,  -988,  -1005, -1011)]

api.list.72  <- api.list.72 [c(-
                               -364,  -368,  -391,  -399,  -403,  -450,  -567)]

api.list.55  <- api.list.55 [c(-
                               -311,  -323,  -329)]

api.list.65  <- api.list.65 [c(-
                               -144,  -244,  -245,  -250,  -268,  -279,  -295)]

api.list.710 <- api.list.710[c(-
                               -33,   -37,   -43,   -54,   -60,   -68)]

api.list.665 <- api.list.665[c(-
                               -102,  -104,  -105,  -108,  -114,  -125,  -130)]

api.list.590 <- api.list.590[c(-)]

api.list.60  <- api.list.60 [c(-)]

api.list.718 <- api.list.718[c(-)]

api.list.other <- api.list.other[c(-)]

# Create list with all the API field vectors
api.list.oow <- list(api.list.630,
                     api.list.705,
                     api.list.72,
                     api.list.55,
                     api.list.65,
                     api.list.710,
                     api.list.665,
                     api.list.590,
                     api.list.60,
                     api.list.718,
                     api.list.other)

for (j in 1:length(api.list.oow)) {
  ifelse(test = (j == lenght(api.list.oow)),
         yes  = pdf(file.path(plot_root, "Individual well fits for OOW in all other fields from declineCurve_v9.pdf")),
         no   = pdf(file.path(plot_root, paste("Individual well fits for OOW in Field ", field[j], " from declineCurve_v9.pdf", sep = ""))))
  api.list <- api.list.oow[[j]]

api.list <- api.list.630

test <- nlsLM(p_oil_prod ~ qo*exp(-(time/tau)^n),
              data = p.oow,
              start = list(qo = 870, tau = 73, n = 1),
              control = list(maxiter = 1e3))

test <- nlsLM(p_oil_prod ~ qo*exp(-(time/tau)),
              data = p.oow,
              start = list(qo = 1340, tau = 1),
              control = list(maxiter = 1e3))


  temp <- matrix(0, nrow = length(api.list), ncol = 7)
  for (a in 1:length(api.list)) {
    # Creates a subset 'w' from dataframe 'p.oow' whose API# is = to the API#
    # in step 'a' of the for-loop
    w <- subset(p.oow, p_api == api.list[a])
    if (length(w[,1]) >= 4) {
      # Smooth the dataset and redefine smooth spline as dataframe
      sm <- smooth.spline(w$time, w$p_oil_prod)
      sm <- data.frame(sm$x, sm$y)
      # Perform regression for oil from well
      fit.sepd.oow <- nlsLM(sm.y ~ qo*exp(-(sm.x/tau)^n),
                           data = sm,
                           start = list(qo = , tau = 1.78, delta = 1.16),
                           control = list(maxiter=1000))
      t <- w$time
      alpha <- coef(fit.hyp.oow)[1]
      theta <- coef(fit.hyp.oow)[2]
      delta <- coef(fit.hyp.oow)[3]
      y <- alpha*(1+theta*delta*t)^(-1/theta)
      RSS <- sum((w$p_oil_prod-y)^2)
      Sum <- sum(y)/sum(w$p_oil_prod)
      
      # Plot Result
      plot(t, w$p_oil_prod,
           type="l",
           ylab="Oil Production (bbl/month)",
           xlab="Time Since First Production (months)")
      title(paste("Oil Production Actual vs. Fit from API # ", api.list[a], sep = ""))
      lines(sm$sm.x, sm$sm.y, col = "black", lty = 2)
      lines(t, y, col = "blue")      
      mtext(paste("RSE: ", round(summary(fit.hyp.oow)$sigma, 2), " on ", length(t)-3,
                  " DOF,   RSS: ", round(RSS, 2),
                  ",   Sum: ", round(Sum, 4), sep = ""),
            side = 3)
      legend("topright",
             c("Actual", "Smooth", "Fit"),
             lty = c(1,2,1),
             col = c("black", "black", "blue"))
      
      # Save coefficients and goodness of fit results
      temp[a,1] <- alpha
      temp[a,2] <- theta
      temp[a,3] <- delta
      temp[a,4] <- summary(fit.hyp.oow)$sigma
      temp[a,5] <- RSS
      temp[a,6] <- Sum
      temp[a,7] <- length(t)
    }
  }
  coef.hyp.oow[[j]] <- temp
  dev.off()
}





# # Load results of loop below, or comment out and rerun to update
# load(file.path(data_root, "coef_hyp_oow.rda"))

# Define dataframes that will hold fit coefficients
coef.hyp.oow <- vector("list", length(api.list.oow))

# Begin for-loop
for (j in 1:length(api.list.oow)) {
  pdf(file = paste("Individual well fits for OOW in Field ", field.ow[j], " from declineCurve_v7.pdf", sep = ""))
  api.list <- api.list.oow[[j]]
  temp <- matrix(0, nrow = length(api.list), ncol = 7)
  for (a in 1:length(api.list)) {
    # Creates a subset 'w' from dataframe 'p.oow' whose API# is = to the API#
    # in step 'a' of the for-loop
    w <- subset(p.oow, p_api == api.list[a])
    if (length(w[,1]) >= 4) {
      # Smooth the dataset and redefine smooth spline as dataframe
      sm <- smooth.spline(w$time, w$p_oil_prod)
      sm <- data.frame(sm$x, sm$y)
      # Perform regression for oil from well
      fit.hyp.oow <- nlsLM(sm.y ~ alpha*(1+theta*delta*sm.x)^(-1/theta),
                           data = sm,
                           start = list(alpha = sm$sm.y[1], theta = 1.78, delta = 1.16),
                           control = list(maxiter=1000))
      t <- w$time
      alpha <- coef(fit.hyp.oow)[1]
      theta <- coef(fit.hyp.oow)[2]
      delta <- coef(fit.hyp.oow)[3]
      y <- alpha*(1+theta*delta*t)^(-1/theta)
      RSS <- sum((w$p_oil_prod-y)^2)
      Sum <- sum(y)/sum(w$p_oil_prod)
      
      # Plot Result
      plot(t, w$p_oil_prod,
           type="l",
           ylab="Oil Production (bbl/month)",
           xlab="Time Since First Production (months)")
           title(paste("Oil Production Actual vs. Fit from API # ", api.list[a], sep = ""))
      lines(sm$sm.x, sm$sm.y, col = "black", lty = 2)
      lines(t, y, col = "blue")      
      mtext(paste("RSE: ", round(summary(fit.hyp.oow)$sigma, 2), " on ", length(t)-3,
                  " DOF,   RSS: ", round(RSS, 2),
                  ",   Sum: ", round(Sum, 4), sep = ""),
            side = 3)
      legend("topright",
             c("Actual", "Smooth", "Fit"),
             lty = c(1,2,1),
             col = c("black", "black", "blue"))
      
      # Save coefficients and goodness of fit results
      temp[a,1] <- alpha
      temp[a,2] <- theta
      temp[a,3] <- delta
      temp[a,4] <- summary(fit.hyp.oow)$sigma
      temp[a,5] <- RSS
      temp[a,6] <- Sum
      temp[a,7] <- length(t)
    }
  }
  coef.hyp.oow[[j]] <- temp
  dev.off()
}

# # Save a list of the coefficient results so you don't have to do this again
# save(file=file.path(data_root, "coef_hyp_oow.rda"), list=c("coef.hyp.oow"))

# Reject outliers in coefficient dataset
for (i in 1:length(coef.hyp.oow)) {
  temp <- coef.hyp.oow[[i]]
  temp <- temp[which(temp[,1] <= max(p.oow$p_oil_prod) &
                       temp[,1] > 0),]
  coef.hyp.oow[[i]] <- temp
}

# Density criteria
# Number of points
n = 10^4
# Lower and upper limits for each coefficient
from.a = 0;
from.b = -10; to.b = 10;
from.c = 0; to.c = 5;

# Determine the PDF for each coefficient for each field
pdf.105a <- density(coef.hyp.oow[[1]][,1], from = from.a, n = n);  pdf.105b <- density(coef.hyp.oow[[1]][,2], from = from.b, to = to.b, n = n);  pdf.105c <- density(coef.hyp.oow[[1]][,3], from = from.c, to = to.c, n = n)
pdf.72a  <- density(coef.hyp.oow[[2]][,1], from = from.a, n = n);  pdf.72b  <- density(coef.hyp.oow[[2]][,2], from = from.b, to = to.b, n = n);  pdf.72c  <- density(coef.hyp.oow[[2]][,3], from = from.c, to = to.c, n = n)
pdf.590a <- density(coef.hyp.oow[[3]][,1], from = from.a, n = n);  pdf.590b <- density(coef.hyp.oow[[3]][,2], from = from.b, to = to.b, n = n);  pdf.590c <- density(coef.hyp.oow[[3]][,3], from = from.c, to = to.c, n = n)
pdf.117a <- density(coef.hyp.oow[[4]][,1], from = from.a, n = n);  pdf.117b <- density(coef.hyp.oow[[4]][,2], from = from.b, to = to.b, n = n);  pdf.117c <- density(coef.hyp.oow[[4]][,3], from = from.c, to = to.c, n = n)
pdf.718a <- density(coef.hyp.oow[[5]][,1], from = from.a, n = n);  pdf.718b <- density(coef.hyp.oow[[5]][,2], from = from.b, to = to.b, n = n);  pdf.718c <- density(coef.hyp.oow[[5]][,3], from = from.c, to = to.c, n = n)
pdf.55a  <- density(coef.hyp.oow[[6]][,1], from = from.a, n = n);  pdf.55b  <- density(coef.hyp.oow[[6]][,2], from = from.b, to = to.b, n = n);  pdf.55c  <- density(coef.hyp.oow[[6]][,3], from = from.c, to = to.c, n = n)
pdf.60a  <- density(coef.hyp.oow[[7]][,1], from = from.a, n = n);  pdf.60b  <- density(coef.hyp.oow[[7]][,2], from = from.b, to = to.b, n = n);  pdf.60c  <- density(coef.hyp.oow[[7]][,3], from = from.c, to = to.c, n = n)
pdf.65a  <- density(coef.hyp.oow[[8]][,1], from = from.a, n = n);  pdf.65b  <- density(coef.hyp.oow[[8]][,2], from = from.b, to = to.b, n = n);  pdf.65c  <- density(coef.hyp.oow[[8]][,3], from = from.c, to = to.c, n = n)
pdf.665a <- density(coef.hyp.oow[[9]][,1], from = from.a, n = n);  pdf.665b <- density(coef.hyp.oow[[9]][,2], from = from.b, to = to.b, n = n);  pdf.665c <- density(coef.hyp.oow[[9]][,3], from = from.c, to = to.c, n = n)
pdf.710a <- density(coef.hyp.oow[[10]][,1], from = from.a, n = n); pdf.710b <- density(coef.hyp.oow[[10]][,2], from = from.b, to = to.b, n = n); pdf.710c <- density(coef.hyp.oow[[10]][,3], from = from.c, to = to.c, n = n)
pdf.80a  <- density(coef.hyp.oow[[11]][,1], from = from.a, n = n); pdf.80b  <- density(coef.hyp.oow[[11]][,2], from = from.b, to = to.b, n = n); pdf.80c  <- density(coef.hyp.oow[[11]][,3], from = from.c, to = to.c, n = n)
pdf.630a <- density(coef.hyp.oow[[12]][,1], from = from.a, n = n); pdf.630b <- density(coef.hyp.oow[[12]][,2], from = from.b, to = to.b, n = n); pdf.630c <- density(coef.hyp.oow[[12]][,3], from = from.c, to = to.c, n = n)
pdf.605a <- density(coef.hyp.oow[[13]][,1], from = from.a, n = n); pdf.605b <- density(coef.hyp.oow[[13]][,2], from = from.b, to = to.b, n = n); pdf.605c <- density(coef.hyp.oow[[13]][,3], from = from.c, to = to.c, n = n)
pdf.560a <- density(coef.hyp.oow[[14]][,1], from = from.a, n = n); pdf.560b <- density(coef.hyp.oow[[14]][,2], from = from.b, to = to.b, n = n); pdf.560c <- density(coef.hyp.oow[[14]][,3], from = from.c, to = to.c, n = n)
pdf.101a <- density(coef.hyp.oow[[15]][,1], from = from.a, n = n); pdf.101b <- density(coef.hyp.oow[[15]][,2], from = from.b, to = to.b, n = n); pdf.101c <- density(coef.hyp.oow[[15]][,3], from = from.c, to = to.c, n = n)
pdf.610a <- density(coef.hyp.oow[[16]][,1], from = from.a, n = n); pdf.610b <- density(coef.hyp.oow[[16]][,2], from = from.b, to = to.b, n = n); pdf.610c <- density(coef.hyp.oow[[16]][,3], from = from.c, to = to.c, n = n)
pdf.692a <- density(coef.hyp.oow[[17]][,1], from = from.a, n = n); pdf.692b <- density(coef.hyp.oow[[17]][,2], from = from.b, to = to.b, n = n); pdf.692c <- density(coef.hyp.oow[[17]][,3], from = from.c, to = to.c, n = n)
pdf.695a <- density(coef.hyp.oow[[18]][,1], from = from.a, n = n); pdf.695b <- density(coef.hyp.oow[[18]][,2], from = from.b, to = to.b, n = n); pdf.695c <- density(coef.hyp.oow[[18]][,3], from = from.c, to = to.c, n = n)

# Create dataframe of pdf x-values
pdf.oow <- data.frame(pdf.105a$x, pdf.105b$x, pdf.105c$x,
                      pdf.72a$x,  pdf.72b$x,  pdf.72c$x,
                      pdf.590a$x, pdf.590b$x, pdf.590c$x,
                      pdf.117a$x, pdf.117b$x, pdf.117c$x,
                      pdf.718a$x, pdf.718b$x, pdf.718c$x,
                      pdf.55a$x,  pdf.55b$x,  pdf.55c$x,
                      pdf.60a$x,  pdf.60b$x,  pdf.60c$x,
                      pdf.65a$x,  pdf.65b$x,  pdf.65c$x,
                      pdf.665a$x, pdf.665b$x, pdf.665c$x,
                      pdf.710a$x, pdf.710b$x, pdf.710c$x,
                      pdf.80a$x,  pdf.80b$x,  pdf.80c$x,
                      pdf.630a$x, pdf.630b$x, pdf.630c$x,
                      pdf.605a$x, pdf.605b$x, pdf.605c$x,
                      pdf.560a$x, pdf.560b$x, pdf.560c$x,
                      pdf.101a$x, pdf.101b$x, pdf.101c$x,
                      pdf.610a$x, pdf.610b$x, pdf.610c$x,
                      pdf.692a$x, pdf.692b$x, pdf.692c$x,
                      pdf.695a$x, pdf.695b$x, pdf.695c$x)

# Determine the CDF for each coefficient for each field
cdf.105a <- cumsum(pdf.105a$y * diff(pdf.105a$x[1:2])); cdf.105b <- cumsum(pdf.105b$y * diff(pdf.105b$x[1:2])); cdf.105c <- cumsum(pdf.105c$y * diff(pdf.105c$x[1:2]))
cdf.72a  <- cumsum(pdf.72a$y  * diff(pdf.72a$x[1:2]));  cdf.72b  <- cumsum(pdf.72b$y  * diff(pdf.72b$x[1:2]));  cdf.72c  <- cumsum(pdf.72c$y  * diff(pdf.72c$x[1:2]))
cdf.590a <- cumsum(pdf.590a$y * diff(pdf.590a$x[1:2])); cdf.590b <- cumsum(pdf.590b$y * diff(pdf.590b$x[1:2])); cdf.590c <- cumsum(pdf.590c$y * diff(pdf.590c$x[1:2]))
cdf.117a <- cumsum(pdf.117a$y * diff(pdf.117a$x[1:2])); cdf.117b <- cumsum(pdf.117b$y * diff(pdf.117b$x[1:2])); cdf.117c <- cumsum(pdf.117c$y * diff(pdf.117c$x[1:2]))
cdf.718a <- cumsum(pdf.718a$y * diff(pdf.718a$x[1:2])); cdf.718b <- cumsum(pdf.718b$y * diff(pdf.718b$x[1:2])); cdf.718c <- cumsum(pdf.718c$y * diff(pdf.718c$x[1:2]))
cdf.55a  <- cumsum(pdf.55a$y  * diff(pdf.55a$x[1:2]));  cdf.55b  <- cumsum(pdf.55b$y  * diff(pdf.55b$x[1:2]));  cdf.55c  <- cumsum(pdf.55c$y  * diff(pdf.55c$x[1:2]))
cdf.60a  <- cumsum(pdf.60a$y  * diff(pdf.60a$x[1:2]));  cdf.60b  <- cumsum(pdf.60b$y  * diff(pdf.60b$x[1:2]));  cdf.60c  <- cumsum(pdf.60c$y  * diff(pdf.60c$x[1:2]))
cdf.65a  <- cumsum(pdf.65a$y  * diff(pdf.65a$x[1:2]));  cdf.65b  <- cumsum(pdf.65b$y  * diff(pdf.65b$x[1:2]));  cdf.65c  <- cumsum(pdf.65c$y  * diff(pdf.65c$x[1:2]))
cdf.665a <- cumsum(pdf.665a$y * diff(pdf.665a$x[1:2])); cdf.665b <- cumsum(pdf.665b$y * diff(pdf.665b$x[1:2])); cdf.665c <- cumsum(pdf.665c$y * diff(pdf.665c$x[1:2]))
cdf.710a <- cumsum(pdf.710a$y * diff(pdf.710a$x[1:2])); cdf.710b <- cumsum(pdf.710b$y * diff(pdf.710b$x[1:2])); cdf.710c <- cumsum(pdf.710c$y * diff(pdf.710c$x[1:2]))
cdf.80a  <- cumsum(pdf.80a$y  * diff(pdf.80a$x[1:2]));  cdf.80b  <- cumsum(pdf.80b$y  * diff(pdf.80b$x[1:2]));  cdf.80c  <- cumsum(pdf.80c$y  * diff(pdf.80c$x[1:2]))
cdf.630a <- cumsum(pdf.630a$y * diff(pdf.630a$x[1:2])); cdf.630b <- cumsum(pdf.630b$y * diff(pdf.630b$x[1:2])); cdf.630c <- cumsum(pdf.630c$y * diff(pdf.630c$x[1:2]))
cdf.605a <- cumsum(pdf.605a$y * diff(pdf.605a$x[1:2])); cdf.605b <- cumsum(pdf.605b$y * diff(pdf.605b$x[1:2])); cdf.605c <- cumsum(pdf.605c$y * diff(pdf.605c$x[1:2]))
cdf.560a <- cumsum(pdf.560a$y * diff(pdf.560a$x[1:2])); cdf.560b <- cumsum(pdf.560b$y * diff(pdf.560b$x[1:2])); cdf.560c <- cumsum(pdf.560c$y * diff(pdf.560c$x[1:2]))
cdf.101a <- cumsum(pdf.101a$y * diff(pdf.101a$x[1:2])); cdf.101b <- cumsum(pdf.101b$y * diff(pdf.101b$x[1:2])); cdf.101c <- cumsum(pdf.101c$y * diff(pdf.101c$x[1:2]))
cdf.610a <- cumsum(pdf.610a$y * diff(pdf.610a$x[1:2])); cdf.610b <- cumsum(pdf.610b$y * diff(pdf.610b$x[1:2])); cdf.610c <- cumsum(pdf.610c$y * diff(pdf.610c$x[1:2]))
cdf.692a <- cumsum(pdf.692a$y * diff(pdf.692a$x[1:2])); cdf.692b <- cumsum(pdf.692b$y * diff(pdf.692b$x[1:2])); cdf.692c <- cumsum(pdf.692c$y * diff(pdf.692c$x[1:2]))
cdf.695a <- cumsum(pdf.695a$y * diff(pdf.695a$x[1:2])); cdf.695b <- cumsum(pdf.695b$y * diff(pdf.695b$x[1:2])); cdf.695c <- cumsum(pdf.695c$y * diff(pdf.695c$x[1:2]))

# Create CDF matrix for export
cdf.oow <- matrix(0, nrow = n, ncol = 54)

# Assign cdf results to each column
cdf.oow[, 1] <- cdf.105a; cdf.oow[, 2] <- cdf.105b; cdf.oow[, 3] <- cdf.105c
cdf.oow[, 4] <- cdf.72a;  cdf.oow[, 5] <- cdf.72b;  cdf.oow[, 6] <- cdf.72c
cdf.oow[, 7] <- cdf.590a; cdf.oow[, 8] <- cdf.590b; cdf.oow[, 9] <- cdf.590c
cdf.oow[,10] <- cdf.117a; cdf.oow[,11] <- cdf.117b; cdf.oow[,12] <- cdf.117c
cdf.oow[,13] <- cdf.718a; cdf.oow[,14] <- cdf.718b; cdf.oow[,15] <- cdf.718c
cdf.oow[,16] <- cdf.55a;  cdf.oow[,17] <- cdf.55b;  cdf.oow[,18] <- cdf.55c
cdf.oow[,19] <- cdf.60a;  cdf.oow[,20] <- cdf.60b;  cdf.oow[,21] <- cdf.60c
cdf.oow[,22] <- cdf.65a;  cdf.oow[,23] <- cdf.65b;  cdf.oow[,24] <- cdf.65c
cdf.oow[,25] <- cdf.665a; cdf.oow[,26] <- cdf.665b; cdf.oow[,27] <- cdf.665c
cdf.oow[,28] <- cdf.710a; cdf.oow[,29] <- cdf.710b; cdf.oow[,30] <- cdf.710c
cdf.oow[,31] <- cdf.80a;  cdf.oow[,32] <- cdf.80b;  cdf.oow[,33] <- cdf.80c
cdf.oow[,34] <- cdf.630a; cdf.oow[,35] <- cdf.630b; cdf.oow[,36] <- cdf.630c
cdf.oow[,37] <- cdf.605a; cdf.oow[,38] <- cdf.605b; cdf.oow[,39] <- cdf.605c
cdf.oow[,40] <- cdf.560a; cdf.oow[,41] <- cdf.560b; cdf.oow[,42] <- cdf.560c
cdf.oow[,43] <- cdf.101a; cdf.oow[,44] <- cdf.101b; cdf.oow[,45] <- cdf.101c
cdf.oow[,46] <- cdf.610a; cdf.oow[,47] <- cdf.610b; cdf.oow[,48] <- cdf.610c
cdf.oow[,49] <- cdf.692a; cdf.oow[,50] <- cdf.692b; cdf.oow[,51] <- cdf.692c
cdf.oow[,52] <- cdf.695a; cdf.oow[,53] <- cdf.695b; cdf.oow[,54] <- cdf.695c

# Normalize in case of rounding error
for (a in 1:length(cdf.oow[1,])) {
  cdf.oow[,a] <- cdf.oow[,a] / max(cdf.oow[,a])
}

# Redefine as dataframe and name columns for each field
cdf.oow <- data.frame(cdf.oow)
names(cdf.oow) <- c("105a", "105b", "105c",
                    "72a",  "72b",   "72c",
                    "590a", "590b", "590c",
                    "117a", "117b", "117c",
                    "718a", "718b", "718c",
                    "55a",  "55b",  "55c",
                    "60a",  "60b",  "60c",
                    "65a",  "65b",  "65c",
                    "665a", "665b", "665c",
                    "710a", "710b", "710c",
                    "80a",  "80b",  "80c",
                    "630a", "630b", "630c",
                    "605a", "605b", "605c",
                    "560a", "560b", "560c",
                    "101a", "101b", "101c",
                    "610a", "610b", "610c",
                    "692a", "692b", "692c",
                    "695a", "695b", "695c")

# # Export PDF and CDF dataframes
# save(file=file.path(data_root, "pdf_oow.rda"), list=c("pdf.oow"))
# save(file=file.path(data_root, "cdf_oow.rda"), list=c("cdf.oow"))

#-------------------------------------------------------------------------------
# Regression to Gas Production from Individual Gas Wells by Field
#-------------------------------------------------------------------------------
# Field listing
field.gw <- c(630, 710, 600, 617, 640, 547, 670, 635, 618, 622, 610, 590, 791, 72, 105, 55)

# Create list of unique API #'s and their Field # in p that are gas wells
api.gw <- sqldf("select distinct(p_api), w_field_num from p where w_well_type = 'GW'")

# Get API list for each field (cutoff of 95% of APD count & 95% of production)
api.list.630 <- subset(api.gw, w_field_num == 630, row.names = FALSE); api.list.630 <- api.list.630[,1]
api.list.710 <- subset(api.gw, w_field_num == 710, row.names = FALSE); api.list.710 <- api.list.710[,1]
api.list.600 <- subset(api.gw, w_field_num == 600, row.names = FALSE); api.list.600 <- api.list.600[,1]
api.list.617 <- subset(api.gw, w_field_num == 617, row.names = FALSE); api.list.617 <- api.list.617[,1]
api.list.640 <- subset(api.gw, w_field_num == 640, row.names = FALSE); api.list.640 <- api.list.640[,1]
api.list.547 <- subset(api.gw, w_field_num == 547, row.names = FALSE); api.list.547 <- api.list.547[,1]
api.list.670 <- subset(api.gw, w_field_num == 670, row.names = FALSE); api.list.670 <- api.list.670[,1]
api.list.635 <- subset(api.gw, w_field_num == 635, row.names = FALSE); api.list.635 <- api.list.635[,1]
api.list.618 <- subset(api.gw, w_field_num == 618, row.names = FALSE); api.list.618 <- api.list.618[,1]
api.list.622 <- subset(api.gw, w_field_num == 622, row.names = FALSE); api.list.622 <- api.list.622[,1]
api.list.610 <- subset(api.gw, w_field_num == 610, row.names = FALSE); api.list.610 <- api.list.610[,1]
api.list.590 <- subset(api.gw, w_field_num == 590, row.names = FALSE); api.list.590 <- api.list.590[,1]
api.list.791 <- subset(api.gw, w_field_num == 791, row.names = FALSE); api.list.791 <- api.list.791[,1]
api.list.72  <- subset(api.gw, w_field_num == 72,  row.names = FALSE); api.list.72  <- api.list.72[,1]
api.list.105 <- subset(api.gw, w_field_num == 105, row.names = FALSE); api.list.105 <- api.list.105[,1]
api.list.55  <- subset(api.gw, w_field_num == 55,  row.names = FALSE); api.list.55  <- api.list.55[,1]

# Enter indices here of wells that fail to converge to skip them in subsequent
# iterations of for-loop
api.list.630 <- api.list.630[c(-3,    -44,   -69,   -88,   -118,  -162,  -187,
                               -259,  -286,  -306,  -384,  -509,  -681,  -1089,
                               -1592, -2787, -3173, -3608, -4034, -4063, -4339,
                               -4714, -4826, -5008, -5012, -5013, -5027, -5028,
                               -5046, -5106, -5142, -5147)]
api.list.600 <- api.list.600[c(-2,    -21,  -22,    -25,   -54)]
api.list.635 <- api.list.635[c(-29)]
api.list.105 <- api.list.105[c(-2)]
# No skips for Fields 710, 617, 640, 547, 670, 618, 622, 610, 590, 791, 72, 55

# Create list with all the API field vectors
api.list.ggw <- list(api.list.630,
                     api.list.710,
                     api.list.600,
                     api.list.617,
                     api.list.640,
                     api.list.547,
                     api.list.670,
                     api.list.635,
                     api.list.618,
                     api.list.622,
                     api.list.610,
                     api.list.590,
                     api.list.791,
                     api.list.72,
                     api.list.105,
                     api.list.55)

# Load results of loop below, or comment out and rerun to update
load(file.path(data_root, "coef_hyp_ggw.rda"))

# # Define dataframes that will hold fit coefficients
# coef.hyp.ggw <- vector("list", length(api.list.ggw))
# 
# # Begin for-loop
# for (j in 1:length(api.list.ggw)) {
#   pdf(file = paste("Individual well fits for GGW in Field ", field.gw[j], " from declineCurve_v8.pdf", sep = ""))
#   api.list <- api.list.ggw[[j]]
#   temp <- matrix(0, nrow = length(api.list), ncol = 8)
#   for (a in 1:length(api.list)) {
#     # Creates a subset 'w' from dataframe 'p.ggw' whose API# is = to the API#
#     # in step 'a' of the for-loop
#     w <- subset(p.ggw, p_api == api.list[a])
#     if (length(w[,1]) >= 4) {
#       # Smooth the dataset and redefine smooth spline as dataframe
#       sm <- smooth.spline(w$time, w$p_gas_prod)
#       sm <- data.frame(sm$x, sm$y)
#       # Perform regression for oil from well
#       fit.hyp.ggw <- nlsLM(sm.y ~ alpha*(1+theta*delta*sm.x)^(-1/theta),
#                            data = sm,
#                            start = list(alpha = 33000, theta = 1.32, delta = 0.24),
#                            control = list(maxiter=1000))
#       t <- w$time
#       alpha <- coef(fit.hyp.ggw)[1]
#       theta <- coef(fit.hyp.ggw)[2]
#       delta <- coef(fit.hyp.ggw)[3]
#       y <- alpha*(1+theta*delta*t)^(-1/theta)
#       RSS <- sum((w$p_gas_prod-y)^2)
#       Sum <- sum(y)
#       frac <- Sum/sum(w$p_gas_prod)      
#       
#       # Plot Result
#       plot(t, w$p_gas_prod,
#            type="l",
#            ylab="Gas Production (bbl/month)",
#            xlab="Time Since First Production (months)")
#       title(paste("Gas Production Actual vs. Fit from API # ", api.list[a], sep = ""))
#       lines(sm$sm.x, sm$sm.y, col = "black", lty = 2)
#       lines(t, y, col = "blue")      
#       mtext(paste("RSE: ", round(summary(fit.hyp.ggw)$sigma, 2), " on ", length(t)-3,
#                   " DOF,   RSS: ", round(RSS, 2),
#                   ",   Sum: ", round(Sum, 4), sep = ""),
#             side = 3)
#       legend("topright",
#              c("Actual", "Smooth", "Fit"),
#              lty = c(1,2,1),
#              col = c("black", "black", "blue"))
#       
#       # Save coefficients and goodness of fit results
#       temp[a,1] <- alpha
#       temp[a,2] <- theta
#       temp[a,3] <- delta
#       temp[a,4] <- summary(fit.hyp.ggw)$sigma
#       temp[a,5] <- RSS
#       temp[a,6] <- frac
#       temp[a,7] <- length(t)
#       temp[a,8] <- Sum
#     }
#   }
#   coef.hyp.ggw[[j]] <- temp
#   dev.off()
# }
# 
# # Save a list of the coefficient results so you don't have to do this again
# save(file=file.path(data_root, "coef_hyp_ggw.rda"), list=c("coef.hyp.ggw"))

summation <- matrix(0, nrow = 16, ncol = 1)
# Reject outliers in coefficient dataset
for (i in 1:length(coef.hyp.ggw)) {
  temp <- coef.hyp.ggw[[i]]
  temp <- temp[which(temp[,1] <= max(p.ggw$p_gas_prod) &
                       temp[,1] > 0),]
  coef.hyp.ggw[[i]] <- temp
  summation[i] <- sum(temp[,8])
}

# Density criteria
# Number of points
n = 10^4
# Lower and upper limits for each coefficient
from.a = 0;
from.b = -10; to.b = 10;
from.c = 0; to.c = 5;

# Determine the PDF for each coefficient for each field
pdf.630a <- density(coef.hyp.ggw[[1]][,1],  from = from.a, n = n); pdf.630b <- density(coef.hyp.ggw[[1]][,2],  from = from.b, to = to.b, n = n);  pdf.630c <- density(coef.hyp.ggw[[1]][,3],  from = from.c, to = to.c, n = n)
pdf.710a <- density(coef.hyp.ggw[[2]][,1],  from = from.a, n = n); pdf.710b <- density(coef.hyp.ggw[[2]][,2],  from = from.b, to = to.b, n = n);  pdf.710c <- density(coef.hyp.ggw[[2]][,3],  from = from.c, to = to.c, n = n)
pdf.600a <- density(coef.hyp.ggw[[3]][,1],  from = from.a, n = n); pdf.600b <- density(coef.hyp.ggw[[3]][,2],  from = from.b, to = to.b, n = n);  pdf.600c <- density(coef.hyp.ggw[[3]][,3],  from = from.c, to = to.c, n = n)
pdf.617a <- density(coef.hyp.ggw[[4]][,1],  from = from.a, n = n); pdf.617b <- density(coef.hyp.ggw[[4]][,2],  from = from.b, to = to.b, n = n);  pdf.617c <- density(coef.hyp.ggw[[4]][,3],  from = from.c, to = to.c, n = n)
pdf.640a <- density(coef.hyp.ggw[[5]][,1],  from = from.a, n = n); pdf.640b <- density(coef.hyp.ggw[[5]][,2],  from = from.b, to = to.b, n = n);  pdf.640c <- density(coef.hyp.ggw[[5]][,3],  from = from.c, to = to.c, n = n)
pdf.547a <- density(coef.hyp.ggw[[6]][,1],  from = from.a, n = n); pdf.547b <- density(coef.hyp.ggw[[6]][,2],  from = from.b, to = to.b, n = n);  pdf.547c <- density(coef.hyp.ggw[[6]][,3],  from = from.c, to = to.c, n = n)
pdf.670a <- density(coef.hyp.ggw[[7]][,1],  from = from.a, n = n); pdf.670b <- density(coef.hyp.ggw[[7]][,2],  from = from.b, to = to.b, n = n);  pdf.670c <- density(coef.hyp.ggw[[7]][,3],  from = from.c, to = to.c, n = n)
pdf.635a <- density(coef.hyp.ggw[[8]][,1],  from = from.a, n = n); pdf.635b <- density(coef.hyp.ggw[[8]][,2],  from = from.b, to = to.b, n = n);  pdf.635c <- density(coef.hyp.ggw[[8]][,3],  from = from.c, to = to.c, n = n)
pdf.618a <- density(coef.hyp.ggw[[9]][,1],  from = from.a, n = n); pdf.618b <- density(coef.hyp.ggw[[9]][,2],  from = from.b, to = to.b, n = n);  pdf.618c <- density(coef.hyp.ggw[[9]][,3],  from = from.c, to = to.c, n = n)
pdf.622a <- density(coef.hyp.ggw[[10]][,1], from = from.a, n = n); pdf.622b <- density(coef.hyp.ggw[[10]][,2], from = from.b, to = to.b, n = n);  pdf.622c <- density(coef.hyp.ggw[[10]][,3], from = from.c, to = to.c, n = n)
pdf.610a <- density(coef.hyp.ggw[[11]][,1], from = from.a, n = n); pdf.610b <- density(coef.hyp.ggw[[11]][,2], from = from.b, to = to.b, n = n);  pdf.610c <- density(coef.hyp.ggw[[11]][,3], from = from.c, to = to.c, n = n)
pdf.590a <- density(coef.hyp.ggw[[12]][,1], from = from.a, n = n); pdf.590b <- density(coef.hyp.ggw[[12]][,2], from = from.b, to = to.b, n = n);  pdf.590c <- density(coef.hyp.ggw[[12]][,3], from = from.c, to = to.c, n = n)
pdf.791a <- density(coef.hyp.ggw[[13]][,1], from = from.a, n = n); pdf.791b <- density(coef.hyp.ggw[[13]][,2], from = from.b, to = to.b, n = n);  pdf.791c <- density(coef.hyp.ggw[[13]][,3], from = from.c, to = to.c, n = n)
pdf.72a  <- density(coef.hyp.ggw[[14]][,1], from = from.a, n = n); pdf.72b  <- density(coef.hyp.ggw[[14]][,2], from = from.b, to = to.b, n = n);  pdf.72c  <- density(coef.hyp.ggw[[14]][,3], from = from.c, to = to.c, n = n)
pdf.105a <- density(coef.hyp.ggw[[15]][,1], from = from.a, n = n); pdf.105b <- density(coef.hyp.ggw[[15]][,2], from = from.b, to = to.b, n = n);  pdf.105c <- density(coef.hyp.ggw[[15]][,3], from = from.c, to = to.c, n = n)
pdf.55a  <- density(coef.hyp.ggw[[16]][,1], from = from.a, n = n); pdf.55b  <- density(coef.hyp.ggw[[16]][,2], from = from.b, to = to.b, n = n);  pdf.55c  <- density(coef.hyp.ggw[[16]][,3], from = from.c, to = to.c, n = n)

# Create dataframe of pdf x-values
pdf.ggw <- data.frame(pdf.630a$x, pdf.630b$x, pdf.630c$x,
                      pdf.710a$x, pdf.710b$x, pdf.710c$x,
                      pdf.600a$x, pdf.600b$x, pdf.600c$x,
                      pdf.617a$x, pdf.617b$x, pdf.617c$x,
                      pdf.640a$x, pdf.640b$x, pdf.640c$x,
                      pdf.547a$x, pdf.547b$x, pdf.547c$x,
                      pdf.670a$x, pdf.670b$x, pdf.670c$x,
                      pdf.635a$x, pdf.635b$x, pdf.635c$x,
                      pdf.618a$x, pdf.618b$x, pdf.618c$x,
                      pdf.622a$x, pdf.622b$x, pdf.622c$x,
                      pdf.610a$x, pdf.610b$x, pdf.610c$x,
                      pdf.590a$x, pdf.590b$x, pdf.590c$x,
                      pdf.791a$x, pdf.791b$x, pdf.791c$x,
                      pdf.72a$x,  pdf.72b$x,  pdf.72c$x,
                      pdf.105a$x, pdf.105b$x, pdf.105c$x,
                      pdf.55a$x,  pdf.55b$x,  pdf.55c$x)

# Determine the CDF for each coefficient for each field
cdf.630a <- cumsum(pdf.630a$y * diff(pdf.630a$x[1:2])); cdf.630b <- cumsum(pdf.630b$y * diff(pdf.630b$x[1:2])); cdf.630c <- cumsum(pdf.630c$y * diff(pdf.630c$x[1:2]))
cdf.710a <- cumsum(pdf.710a$y * diff(pdf.710a$x[1:2])); cdf.710b <- cumsum(pdf.710b$y * diff(pdf.710b$x[1:2])); cdf.710c <- cumsum(pdf.710c$y * diff(pdf.710c$x[1:2]))
cdf.600a <- cumsum(pdf.600a$y * diff(pdf.600a$x[1:2])); cdf.600b <- cumsum(pdf.600b$y * diff(pdf.600b$x[1:2])); cdf.600c <- cumsum(pdf.600c$y * diff(pdf.600c$x[1:2]))
cdf.617a <- cumsum(pdf.617a$y * diff(pdf.617a$x[1:2])); cdf.617b <- cumsum(pdf.617b$y * diff(pdf.617b$x[1:2])); cdf.617c <- cumsum(pdf.617c$y * diff(pdf.617c$x[1:2]))
cdf.640a <- cumsum(pdf.640a$y * diff(pdf.640a$x[1:2])); cdf.640b <- cumsum(pdf.640b$y * diff(pdf.640b$x[1:2])); cdf.640c <- cumsum(pdf.640c$y * diff(pdf.640c$x[1:2]))
cdf.547a <- cumsum(pdf.547a$y * diff(pdf.547a$x[1:2])); cdf.547b <- cumsum(pdf.547b$y * diff(pdf.547b$x[1:2])); cdf.547c <- cumsum(pdf.547c$y * diff(pdf.547c$x[1:2]))
cdf.670a <- cumsum(pdf.670a$y * diff(pdf.670a$x[1:2])); cdf.670b <- cumsum(pdf.670b$y * diff(pdf.670b$x[1:2])); cdf.670c <- cumsum(pdf.670c$y * diff(pdf.670c$x[1:2]))
cdf.635a <- cumsum(pdf.635a$y * diff(pdf.635a$x[1:2])); cdf.635b <- cumsum(pdf.635b$y * diff(pdf.635b$x[1:2])); cdf.635c <- cumsum(pdf.635c$y * diff(pdf.635c$x[1:2]))
cdf.618a <- cumsum(pdf.618a$y * diff(pdf.618a$x[1:2])); cdf.618b <- cumsum(pdf.618b$y * diff(pdf.618b$x[1:2])); cdf.618c <- cumsum(pdf.618c$y * diff(pdf.618c$x[1:2]))
cdf.622a <- cumsum(pdf.622a$y * diff(pdf.622a$x[1:2])); cdf.622b <- cumsum(pdf.622b$y * diff(pdf.622b$x[1:2])); cdf.622c <- cumsum(pdf.622c$y * diff(pdf.622c$x[1:2]))
cdf.610a <- cumsum(pdf.610a$y * diff(pdf.610a$x[1:2])); cdf.610b <- cumsum(pdf.610b$y * diff(pdf.610b$x[1:2])); cdf.610c <- cumsum(pdf.610c$y * diff(pdf.610c$x[1:2]))
cdf.590a <- cumsum(pdf.590a$y * diff(pdf.590a$x[1:2])); cdf.590b <- cumsum(pdf.590b$y * diff(pdf.590b$x[1:2])); cdf.590c <- cumsum(pdf.590c$y * diff(pdf.590c$x[1:2]))
cdf.791a <- cumsum(pdf.791a$y * diff(pdf.791a$x[1:2])); cdf.791b <- cumsum(pdf.791b$y * diff(pdf.791b$x[1:2])); cdf.791c <- cumsum(pdf.791c$y * diff(pdf.791c$x[1:2]))
cdf.72a  <- cumsum(pdf.72a$y  * diff(pdf.72a$x[1:2]));  cdf.72b  <- cumsum(pdf.72b$y  * diff(pdf.72b$x[1:2]));  cdf.72c  <- cumsum(pdf.72c$y  * diff(pdf.72c$x[1:2]))
cdf.105a <- cumsum(pdf.105a$y * diff(pdf.105a$x[1:2])); cdf.105b <- cumsum(pdf.105b$y * diff(pdf.105b$x[1:2])); cdf.105c <- cumsum(pdf.105c$y * diff(pdf.105c$x[1:2]))
cdf.55a  <- cumsum(pdf.55a$y  * diff(pdf.55a$x[1:2]));  cdf.55b  <- cumsum(pdf.55b$y  * diff(pdf.55b$x[1:2]));  cdf.55c  <- cumsum(pdf.55c$y  * diff(pdf.55c$x[1:2]))


# Create CDF matrix for export
cdf.ggw <- matrix(0, nrow = n, ncol = 3*length(api.list.ggw))

# Assign cdf results to each column
cdf.ggw[, 1] <- cdf.630a; cdf.ggw[, 2] <- cdf.630b; cdf.ggw[, 3] <- cdf.630c
cdf.ggw[, 4] <- cdf.710a; cdf.ggw[, 5] <- cdf.710b; cdf.ggw[, 6] <- cdf.710c
cdf.ggw[, 7] <- cdf.600a; cdf.ggw[, 8] <- cdf.600b; cdf.ggw[, 9] <- cdf.600c
cdf.ggw[,10] <- cdf.617a; cdf.ggw[,11] <- cdf.617b; cdf.ggw[,12] <- cdf.617c
cdf.ggw[,13] <- cdf.640a; cdf.ggw[,14] <- cdf.640b; cdf.ggw[,15] <- cdf.640c
cdf.ggw[,16] <- cdf.547a; cdf.ggw[,17] <- cdf.547b; cdf.ggw[,18] <- cdf.547c
cdf.ggw[,19] <- cdf.670a; cdf.ggw[,20] <- cdf.670b; cdf.ggw[,21] <- cdf.670c
cdf.ggw[,22] <- cdf.635a; cdf.ggw[,23] <- cdf.635b; cdf.ggw[,24] <- cdf.635c
cdf.ggw[,25] <- cdf.618a; cdf.ggw[,26] <- cdf.618b; cdf.ggw[,27] <- cdf.618c
cdf.ggw[,28] <- cdf.622a; cdf.ggw[,29] <- cdf.622b; cdf.ggw[,30] <- cdf.622c
cdf.ggw[,31] <- cdf.610a; cdf.ggw[,32] <- cdf.610b; cdf.ggw[,33] <- cdf.610c
cdf.ggw[,34] <- cdf.590a; cdf.ggw[,35] <- cdf.590b; cdf.ggw[,36] <- cdf.590c
cdf.ggw[,37] <- cdf.791a; cdf.ggw[,38] <- cdf.791b; cdf.ggw[,39] <- cdf.791c
cdf.ggw[,40] <- cdf.72a;  cdf.ggw[,41] <- cdf.72b;  cdf.ggw[,42] <- cdf.72c
cdf.ggw[,43] <- cdf.105a; cdf.ggw[,44] <- cdf.105b; cdf.ggw[,45] <- cdf.105c
cdf.ggw[,46] <- cdf.55a;  cdf.ggw[,47] <- cdf.55b;  cdf.ggw[,48] <- cdf.55c

# Normalize in case of rounding error
for (a in 1:length(cdf.ggw[1,])) {
  cdf.ggw[,a] <- cdf.ggw[,a] / max(cdf.ggw[,a])
}

# Redefine as dataframe and name columns for each field
cdf.ggw <- data.frame(cdf.ggw)
names(cdf.ggw) <- c("630a", "630b", "630c",
                    "710a", "710b", "710c",
                    "600a", "600b", "600c",
                    "617a", "617b", "617c",
                    "640a", "640b", "640c",
                    "547a", "547b", "547c",
                    "670a", "670b", "670c",
                    "635a", "635b", "635c",
                    "618a", "618b", "618c",
                    "622a", "622b", "622c",
                    "610a", "610b", "610c",
                    "590a", "590b", "590c",
                    "791a", "791b", "791c",
                    "72a",  "72b",  "72c",
                    "105a", "105b", "105c",
                    "55a",  "55b",  "55c")

# # Export PDF and CDF dataframes
# save(file=file.path(data_root, "pdf_ggw.rda"), list=c("pdf.ggw"))
# save(file=file.path(data_root, "cdf_ggw.rda"), list=c("cdf.ggw"))

#-------------------------------------------------------------------------------
# Regression to Gas Production from Individual Oil Wells by Field
#-------------------------------------------------------------------------------

# Create list of unique API #'s and their Field # in p that are oil wells
api.ow <- sqldf("select distinct(p_api), w_field_num from p where w_well_type = 'OW'")

# Get api list for each field (cutoff of 95% of APD count & 95% of production)
api.list.105 <- subset(api.ow, w_field_num == 105, row.names = FALSE); api.list.105 <- api.list.105[,1]
api.list.72  <- subset(api.ow, w_field_num == 72,  row.names = FALSE); api.list.72  <- api.list.72[,1]
api.list.590 <- subset(api.ow, w_field_num == 590, row.names = FALSE); api.list.590 <- api.list.590[,1]
api.list.117 <- subset(api.ow, w_field_num == 117, row.names = FALSE); api.list.117 <- api.list.117[,1]
api.list.718 <- subset(api.ow, w_field_num == 718, row.names = FALSE); api.list.718 <- api.list.718[,1]
api.list.55  <- subset(api.ow, w_field_num == 55,  row.names = FALSE); api.list.55  <- api.list.55[,1]
api.list.60  <- subset(api.ow, w_field_num == 60,  row.names = FALSE); api.list.60  <- api.list.60[,1]
api.list.65  <- subset(api.ow, w_field_num == 65,  row.names = FALSE); api.list.65  <- api.list.65[,1]
api.list.665 <- subset(api.ow, w_field_num == 665, row.names = FALSE); api.list.665 <- api.list.665[,1]
api.list.710 <- subset(api.ow, w_field_num == 710, row.names = FALSE); api.list.710 <- api.list.710[,1]
api.list.80  <- subset(api.ow, w_field_num == 80,  row.names = FALSE); api.list.80  <- api.list.80[,1]
api.list.630 <- subset(api.ow, w_field_num == 630, row.names = FALSE); api.list.630 <- api.list.630[,1]
api.list.605 <- subset(api.ow, w_field_num == 605, row.names = FALSE); api.list.605 <- api.list.605[,1]
api.list.560 <- subset(api.ow, w_field_num == 560, row.names = FALSE); api.list.560 <- api.list.560[,1]
api.list.101 <- subset(api.ow, w_field_num == 101, row.names = FALSE); api.list.101 <- api.list.101[,1]
api.list.610 <- subset(api.ow, w_field_num == 610, row.names = FALSE); api.list.610 <- api.list.610[,1]
api.list.692 <- subset(api.ow, w_field_num == 692, row.names = FALSE); api.list.692 <- api.list.692[,1]
api.list.695 <- subset(api.ow, w_field_num == 695, row.names = FALSE); api.list.695 <- api.list.695[,1]

# Enter indices here of wells that fail to converge to skip them in subsequent
# iterations of for-loop
api.list.105 <- api.list.105[c(-22,   -57,   -104,  -119,  -170,  -172,  -180,
                               -184,  -263,  -287,  -292,  -367,  -410,  -429,
                               -434,  -471,  -518,  -597,  -644,  -671,  -688,
                               -690,  -696,  -721,  -733,  -751,  -783,  -794,
                               -802,  -830,  -843,  -852,  -860,  -878,  -918,
                               -921,  -922,  -936,  -964,  -970,  -983,  -985,
                               -1009, -1013, -1014, -1034, -1042, -1058, -1060,
                               -1061, -1062, -1063, -1072, -1074, -1079, -1080,
                               -1083, -1098, -1106, -1112, -1118, -1153, -1159,
                               -1163, -1164, -1221, -1266, -1277, -1278)]

api.list.72  <- api.list.72 [c(-18,   -206,  -272,  -305,  -555,  -608,  -611,
                               -612,  -616,  -620,  -622,  -637,  -654,  -662)]

api.list.590 <- api.list.590[c(-5,    -46,   -49,   -87,   -91,   -112,  -115,
                               -178,  -184,  -196,  -232)]

api.list.117 <- api.list.117[c(-3,    -43,   -45,   -46,   -87,   -89,   -106,
                               -109,  -133,  -136,  -141)]

api.list.718 <- api.list.718[c(-9,    -11,   -22,   -25,   -75,   -106,  -133,
                               -135,  -138,  -150,  -151,  -153,  -154,  -155,
                               -156,  -158)]

api.list.55  <- api.list.55 [c(-46,   -58,   -72,   -88,   -89,   -95,   -103,
                               -112,  -170,  -173,  -192,  -202,  -208,  -211,
                               -219,  -256,  -263,  -329,  -379,  -386,  -408,
                               -416,  -422,  -438,  -448,  -450,  -457,  -461,
                               -467,  -493)]

api.list.60  <- api.list.60 [c(-2,    -15,   -47,   -105,  -114,  -162,  -164,
                               -168,  -169)]

api.list.65  <- api.list.65 [c(-10,   -15,   -33,   -36,   -44,   -52,   -99,
                               -102,  -119,  -120,  -123,  -138,  -149,  -151,
                               -183,  -199,  -205,  -232,  -288,  -295,  -322,
                               -330)]

api.list.665 <- api.list.665[c(-34,   -67,   -89,   -144,  -163)]

api.list.710 <- api.list.710[c(-16,   -49,   -50,   -51,   -53,   -58,   -59,
                               -71,   -77,   -90,   -93)]

api.list.80  <- api.list.80 [c(-37,   -61)]

api.list.630 <- api.list.630[c(-13,   -46)]

api.list.605 <- api.list.605[c(-23)]

api.list.560 <- api.list.560[c(-8,    -11,   -18,   -32)]

api.list.101 <- api.list.101[c(-6,    -12,   -29,   -60,   -71)]

api.list.610 <- api.list.610[c(-12)]

api.list.692 <- api.list.692[c(-6)]

api.list.695 <- api.list.692[c(-19,   -21,   -24,   -36)]

# Create list with all the API field vectors
api.list.gow <- list(api.list.105,
                     api.list.72,
                     api.list.590,
                     api.list.117,
                     api.list.718,
                     api.list.55,
                     api.list.60,
                     api.list.65,
                     api.list.665,
                     api.list.710,
                     api.list.80,
                     api.list.630,
                     api.list.605,
                     api.list.560,
                     api.list.101,
                     api.list.610,
                     api.list.692,
                     api.list.695)

# Load results of loop below, or comment out and rerun to update
load(file.path(data_root, "coef_hyp_gow.rda"))

# # Define dataframes that will hold fit coefficients
# coef.hyp.gow <- vector("list", length(api.list.gow))
# 
# # Begin for-loop
# for (j in 1:length(api.list.gow)) {
#   api.list <- api.list.gow[[j]]
#   temp <- matrix(0, nrow = length(api.list), ncol = 4)
#   for (a in b:length(api.list)) {
#     # Creates a subset 'w' from dataframe 'p.gow' whose API# is = to the API#
#     # in step 'a' of the for-loop
#     w <- subset(p.gow, p_api == api.list[a])
#     if (length(w[,1]) >= 4) {
#       # Smooth the dataset and redefine smooth spline as dataframe
#       sm <- smooth.spline(w$time, w$p_gas_prod)
#       sm <- data.frame(sm$x, sm$y)
#       # Perform regression for gas from well
#       fit.hyp.gow <- nlsLM(sm.y ~ alpha*(1+theta*delta*sm.x)^(-1/theta),
#                            data = sm,
#                            start = list(alpha = 3000, theta = 1.45, delta = 0.06),
#                            control = list(maxiter=1000))
#       temp[a,1] <- coef(fit.hyp.gow)[1]
#       temp[a,2] <- coef(fit.hyp.gow)[2]
#       temp[a,3] <- coef(fit.hyp.gow)[3]
#       temp[a,4] <- summary(fit.hyp.gow)$sigma
#     }
#   }
#   coef.hyp.gow[[j]] <- temp
# }
# 
# # Save a list of the coefficient results so you don't have to do this again
# save(file=file.path(data_root, "coef_hyp_gow.rda"), list=c("coef.hyp.gow"))

# Reject outliers in coefficient dataset
# Note - Fields 605, 560, 610, 692, 695 return no APIs that meet rejection
# filter, so only select fields from field list that will meet rejection
# criteria
field.select <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 15)
for (i in 1:length(field.select)) {
  j <- field.select[i]
  temp <- coef.hyp.gow[[j]]
  temp <- temp[which(temp[,1] <= max(p.gow$p_gas_prod) &
                       temp[,1] > 0),]
  coef.hyp.gow[[j]] <- temp
}

# Determine the PDF for each coefficient for each field
pdf.105a <- density(coef.hyp.gow[[1]][,1], from = 0);  pdf.105b <- density(coef.hyp.gow[[1]][,2], from = 0);  pdf.105c <- density(coef.hyp.gow[[1]][,3], from = 0)
pdf.72a  <- density(coef.hyp.gow[[2]][,1], from = 0);  pdf.72b  <- density(coef.hyp.gow[[2]][,2], from = 0);  pdf.72c  <- density(coef.hyp.gow[[2]][,3], from = 0)
pdf.590a <- density(coef.hyp.gow[[3]][,1], from = 0);  pdf.590b <- density(coef.hyp.gow[[3]][,2], from = 0);  pdf.590c <- density(coef.hyp.gow[[3]][,3], from = 0)
pdf.117a <- density(coef.hyp.gow[[4]][,1], from = 0);  pdf.117b <- density(coef.hyp.gow[[4]][,2], from = 0);  pdf.117c <- density(coef.hyp.gow[[4]][,3], from = 0)
pdf.718a <- density(coef.hyp.gow[[5]][,1], from = 0);  pdf.718b <- density(coef.hyp.gow[[5]][,2], from = 0);  pdf.718c <- density(coef.hyp.gow[[5]][,3], from = 0)
pdf.55a  <- density(coef.hyp.gow[[6]][,1], from = 0);  pdf.55b  <- density(coef.hyp.gow[[6]][,2], from = 0);  pdf.55c  <- density(coef.hyp.gow[[6]][,3], from = 0)
pdf.60a  <- density(coef.hyp.gow[[7]][,1], from = 0);  pdf.60b  <- density(coef.hyp.gow[[7]][,2], from = 0);  pdf.60c  <- density(coef.hyp.gow[[7]][,3], from = 0)
pdf.65a  <- density(coef.hyp.gow[[8]][,1], from = 0);  pdf.65b  <- density(coef.hyp.gow[[8]][,2], from = 0);  pdf.65c  <- density(coef.hyp.gow[[8]][,3], from = 0)
pdf.665a <- density(coef.hyp.gow[[9]][,1], from = 0);  pdf.665b <- density(coef.hyp.gow[[9]][,2], from = 0);  pdf.665c <- density(coef.hyp.gow[[9]][,3], from = 0)
pdf.710a <- density(coef.hyp.gow[[10]][,1], from = 0); pdf.710b <- density(coef.hyp.gow[[10]][,2], from = 0); pdf.710c <- density(coef.hyp.gow[[10]][,3], from = 0)
pdf.80a  <- density(coef.hyp.gow[[11]][,1], from = 0); pdf.80b  <- density(coef.hyp.gow[[11]][,2], from = 0); pdf.80c  <- density(coef.hyp.gow[[11]][,3], from = 0)
pdf.630a <- density(coef.hyp.gow[[12]][,1], from = 0); pdf.630b <- density(coef.hyp.gow[[12]][,2], from = 0); pdf.630c <- density(coef.hyp.gow[[12]][,3], from = 0)
pdf.605a <- density(coef.hyp.gow[[13]][,1], from = 0); pdf.605b <- density(coef.hyp.gow[[13]][,2], from = 0); pdf.605c <- density(coef.hyp.gow[[13]][,3], from = 0)
pdf.560a <- density(coef.hyp.gow[[14]][,1], from = 0); pdf.560b <- density(coef.hyp.gow[[14]][,2], from = 0); pdf.560c <- density(coef.hyp.gow[[14]][,3], from = 0)
pdf.101a <- density(coef.hyp.gow[[15]][,1], from = 0); pdf.101b <- density(coef.hyp.gow[[15]][,2], from = 0); pdf.101c <- density(coef.hyp.gow[[15]][,3], from = 0)
pdf.610a <- density(coef.hyp.gow[[16]][,1], from = 0); pdf.610b <- density(coef.hyp.gow[[16]][,2], from = 0); pdf.610c <- density(coef.hyp.gow[[16]][,3], from = 0)
pdf.692a <- density(coef.hyp.gow[[17]][,1], from = 0); pdf.692b <- density(coef.hyp.gow[[17]][,2], from = 0); pdf.692c <- density(coef.hyp.gow[[17]][,3], from = 0)
pdf.695a <- density(coef.hyp.gow[[18]][,1], from = 0); pdf.695b <- density(coef.hyp.gow[[18]][,2], from = 0); pdf.695c <- density(coef.hyp.gow[[18]][,3], from = 0)

# Create dataframe of pdf x-values
pdf.gow <- data.frame(pdf.105a$x, pdf.105b$x, pdf.105c$x,
                      pdf.72a$x,  pdf.72b$x,  pdf.72c$x,
                      pdf.590a$x, pdf.590b$x, pdf.590c$x,
                      pdf.117a$x, pdf.117b$x, pdf.117c$x,
                      pdf.718a$x, pdf.718b$x, pdf.718c$x,
                      pdf.55a$x,  pdf.55b$x,  pdf.55c$x,
                      pdf.60a$x,  pdf.60b$x,  pdf.60c$x,
                      pdf.65a$x,  pdf.65b$x,  pdf.65c$x,
                      pdf.665a$x, pdf.665b$x, pdf.665c$x,
                      pdf.710a$x, pdf.710b$x, pdf.710c$x,
                      pdf.80a$x,  pdf.80b$x,  pdf.80c$x,
                      pdf.630a$x, pdf.630b$x, pdf.630c$x,
                      pdf.605a$x, pdf.605b$x, pdf.605c$x,
                      pdf.560a$x, pdf.560b$x, pdf.560c$x,
                      pdf.101a$x, pdf.101b$x, pdf.101c$x,
                      pdf.610a$x, pdf.610b$x, pdf.610c$x,
                      pdf.692a$x, pdf.692b$x, pdf.692c$x,
                      pdf.695a$x, pdf.695b$x, pdf.695c$x)

# Determine the CDF for each coefficient for each field
cdf.105a <- cumsum(pdf.105a$y * diff(pdf.105a$x[1:2])); cdf.105b <- cumsum(pdf.105b$y * diff(pdf.105b$x[1:2])); cdf.105c <- cumsum(pdf.105c$y * diff(pdf.105c$x[1:2]))
cdf.72a  <- cumsum(pdf.72a$y  * diff(pdf.72a$x[1:2]));  cdf.72b  <- cumsum(pdf.72b$y  * diff(pdf.72b$x[1:2]));  cdf.72c  <- cumsum(pdf.72c$y  * diff(pdf.72c$x[1:2]))
cdf.590a <- cumsum(pdf.590a$y * diff(pdf.590a$x[1:2])); cdf.590b <- cumsum(pdf.590b$y * diff(pdf.590b$x[1:2])); cdf.590c <- cumsum(pdf.590c$y * diff(pdf.590c$x[1:2]))
cdf.117a <- cumsum(pdf.117a$y * diff(pdf.117a$x[1:2])); cdf.117b <- cumsum(pdf.117b$y * diff(pdf.117b$x[1:2])); cdf.117c <- cumsum(pdf.117c$y * diff(pdf.117c$x[1:2]))
cdf.718a <- cumsum(pdf.718a$y * diff(pdf.718a$x[1:2])); cdf.718b <- cumsum(pdf.718b$y * diff(pdf.718b$x[1:2])); cdf.718c <- cumsum(pdf.718c$y * diff(pdf.718c$x[1:2]))
cdf.55a  <- cumsum(pdf.55a$y  * diff(pdf.55a$x[1:2]));  cdf.55b  <- cumsum(pdf.55b$y  * diff(pdf.55b$x[1:2]));  cdf.55c  <- cumsum(pdf.55c$y  * diff(pdf.55c$x[1:2]))
cdf.60a  <- cumsum(pdf.60a$y  * diff(pdf.60a$x[1:2]));  cdf.60b  <- cumsum(pdf.60b$y  * diff(pdf.60b$x[1:2]));  cdf.60c  <- cumsum(pdf.60c$y  * diff(pdf.60c$x[1:2]))
cdf.65a  <- cumsum(pdf.65a$y  * diff(pdf.65a$x[1:2]));  cdf.65b  <- cumsum(pdf.65b$y  * diff(pdf.65b$x[1:2]));  cdf.65c  <- cumsum(pdf.65c$y  * diff(pdf.65c$x[1:2]))
cdf.665a <- cumsum(pdf.665a$y * diff(pdf.665a$x[1:2])); cdf.665b <- cumsum(pdf.665b$y * diff(pdf.665b$x[1:2])); cdf.665c <- cumsum(pdf.665c$y * diff(pdf.665c$x[1:2]))
cdf.710a <- cumsum(pdf.710a$y * diff(pdf.710a$x[1:2])); cdf.710b <- cumsum(pdf.710b$y * diff(pdf.710b$x[1:2])); cdf.710c <- cumsum(pdf.710c$y * diff(pdf.710c$x[1:2]))
cdf.80a  <- cumsum(pdf.80a$y  * diff(pdf.80a$x[1:2]));  cdf.80b  <- cumsum(pdf.80b$y  * diff(pdf.80b$x[1:2]));  cdf.80c  <- cumsum(pdf.80c$y  * diff(pdf.80c$x[1:2]))
cdf.630a <- cumsum(pdf.630a$y * diff(pdf.630a$x[1:2])); cdf.630b <- cumsum(pdf.630b$y * diff(pdf.630b$x[1:2])); cdf.630c <- cumsum(pdf.630c$y * diff(pdf.630c$x[1:2]))
cdf.605a <- cumsum(pdf.605a$y * diff(pdf.605a$x[1:2])); cdf.605b <- cumsum(pdf.605b$y * diff(pdf.605b$x[1:2])); cdf.605c <- cumsum(pdf.605c$y * diff(pdf.605c$x[1:2]))
cdf.560a <- cumsum(pdf.560a$y * diff(pdf.560a$x[1:2])); cdf.560b <- cumsum(pdf.560b$y * diff(pdf.560b$x[1:2])); cdf.560c <- cumsum(pdf.560c$y * diff(pdf.560c$x[1:2]))
cdf.101a <- cumsum(pdf.101a$y * diff(pdf.101a$x[1:2])); cdf.101b <- cumsum(pdf.101b$y * diff(pdf.101b$x[1:2])); cdf.101c <- cumsum(pdf.101c$y * diff(pdf.101c$x[1:2]))
cdf.610a <- cumsum(pdf.610a$y * diff(pdf.610a$x[1:2])); cdf.610b <- cumsum(pdf.610b$y * diff(pdf.610b$x[1:2])); cdf.610c <- cumsum(pdf.610c$y * diff(pdf.610c$x[1:2]))
cdf.692a <- cumsum(pdf.692a$y * diff(pdf.692a$x[1:2])); cdf.692b <- cumsum(pdf.692b$y * diff(pdf.692b$x[1:2])); cdf.692c <- cumsum(pdf.692c$y * diff(pdf.692c$x[1:2]))
cdf.695a <- cumsum(pdf.695a$y * diff(pdf.695a$x[1:2])); cdf.695b <- cumsum(pdf.695b$y * diff(pdf.695b$x[1:2])); cdf.695c <- cumsum(pdf.695c$y * diff(pdf.695c$x[1:2]))

# Create CDF matrix for export
cdf.gow <- matrix(0, nrow = 512, ncol = 54)

# Assign cdf results to each column
cdf.gow[, 1] <- cdf.105a; cdf.gow[, 2] <- cdf.105b; cdf.gow[, 3] <- cdf.105c
cdf.gow[, 4] <- cdf.72a;  cdf.gow[, 5] <- cdf.72b;  cdf.gow[, 6] <- cdf.72c
cdf.gow[, 7] <- cdf.590a; cdf.gow[, 8] <- cdf.590b; cdf.gow[, 9] <- cdf.590c
cdf.gow[,10] <- cdf.117a; cdf.gow[,11] <- cdf.117b; cdf.gow[,12] <- cdf.117c
cdf.gow[,13] <- cdf.718a; cdf.gow[,14] <- cdf.718b; cdf.gow[,15] <- cdf.718c
cdf.gow[,16] <- cdf.55a;  cdf.gow[,17] <- cdf.55b;  cdf.gow[,18] <- cdf.55c
cdf.gow[,19] <- cdf.60a;  cdf.gow[,20] <- cdf.60b;  cdf.gow[,21] <- cdf.60c
cdf.gow[,22] <- cdf.65a;  cdf.gow[,23] <- cdf.65b;  cdf.gow[,24] <- cdf.65c
cdf.gow[,25] <- cdf.665a; cdf.gow[,26] <- cdf.665b; cdf.gow[,27] <- cdf.665c
cdf.gow[,28] <- cdf.710a; cdf.gow[,29] <- cdf.710b; cdf.gow[,30] <- cdf.710c
cdf.gow[,31] <- cdf.80a;  cdf.gow[,32] <- cdf.80b;  cdf.gow[,33] <- cdf.80c
cdf.gow[,34] <- cdf.630a; cdf.gow[,35] <- cdf.630b; cdf.gow[,36] <- cdf.630c
cdf.gow[,37] <- cdf.605a; cdf.gow[,38] <- cdf.605b; cdf.gow[,39] <- cdf.605c
cdf.gow[,40] <- cdf.560a; cdf.gow[,41] <- cdf.560b; cdf.gow[,42] <- cdf.560c
cdf.gow[,43] <- cdf.101a; cdf.gow[,44] <- cdf.101b; cdf.gow[,45] <- cdf.101c
cdf.gow[,46] <- cdf.610a; cdf.gow[,47] <- cdf.610b; cdf.gow[,48] <- cdf.610c
cdf.gow[,49] <- cdf.692a; cdf.gow[,50] <- cdf.692b; cdf.gow[,51] <- cdf.692c
cdf.gow[,52] <- cdf.695a; cdf.gow[,53] <- cdf.695b; cdf.gow[,54] <- cdf.695c

# Normalize in case of rounding error
for (a in 1:length(cdf.gow[1,])) {
  cdf.gow[,a] <- cdf.gow[,a] / max(cdf.gow[,a])
}

# Redefine as dataframe and name columns for each field
cdf.gow <- data.frame(cdf.gow)
names(cdf.gow) <- c("105a", "105b", "105c",
                    "72a",  "72b",   "72c",
                    "590a", "590b", "590c",
                    "117a", "117b", "117c",
                    "718a", "718b", "718c",
                    "55a",  "55b",  "55c",
                    "60a",  "60b",  "60c",
                    "65a",  "65b",  "65c",
                    "665a", "665b", "665c",
                    "710a", "710b", "710c",
                    "80a",  "80b",  "80c",
                    "630a", "630b", "630c",
                    "605a", "605b", "605c",
                    "560a", "560b", "560c",
                    "101a", "101b", "101c",
                    "610a", "610b", "610c",
                    "692a", "692b", "692c",
                    "695a", "695b", "695c")

# # Export PDF and CDF dataframes
# save(file=file.path(data_root, "pdf_gow.rda"), list=c("pdf.gow"))
# save(file=file.path(data_root, "cdf_gow.rda"), list=c("cdf.gow"))

#-------------------------------------------------------------------------------
# Regression to Oil Production from Individual Gas Wells by Field
#-------------------------------------------------------------------------------

# Create list of unique API #'s and their Field # in p that are gas wells
api.gw <- sqldf("select distinct(p_api), w_field_num from p where w_well_type = 'GW'")

# Get API list for each field (cutoff of 95% of APD count & 95% of production)
api.list.630 <- subset(api.gw, w_field_num == 630, row.names = FALSE); api.list.630 <- api.list.630[,1]
api.list.710 <- subset(api.gw, w_field_num == 710, row.names = FALSE); api.list.710 <- api.list.710[,1]
api.list.600 <- subset(api.gw, w_field_num == 600, row.names = FALSE); api.list.600 <- api.list.600[,1]
api.list.617 <- subset(api.gw, w_field_num == 617, row.names = FALSE); api.list.617 <- api.list.617[,1]
api.list.640 <- subset(api.gw, w_field_num == 640, row.names = FALSE); api.list.640 <- api.list.640[,1]
api.list.547 <- subset(api.gw, w_field_num == 547, row.names = FALSE); api.list.547 <- api.list.547[,1]
api.list.670 <- subset(api.gw, w_field_num == 670, row.names = FALSE); api.list.670 <- api.list.670[,1]
api.list.635 <- subset(api.gw, w_field_num == 635, row.names = FALSE); api.list.635 <- api.list.635[,1]
api.list.618 <- subset(api.gw, w_field_num == 618, row.names = FALSE); api.list.618 <- api.list.618[,1]
api.list.622 <- subset(api.gw, w_field_num == 622, row.names = FALSE); api.list.622 <- api.list.622[,1]
api.list.610 <- subset(api.gw, w_field_num == 610, row.names = FALSE); api.list.610 <- api.list.610[,1]
api.list.590 <- subset(api.gw, w_field_num == 590, row.names = FALSE); api.list.590 <- api.list.590[,1]
api.list.791 <- subset(api.gw, w_field_num == 791, row.names = FALSE); api.list.791 <- api.list.791[,1]
api.list.72  <- subset(api.gw, w_field_num == 72,  row.names = FALSE); api.list.72  <- api.list.72[,1]
api.list.105 <- subset(api.gw, w_field_num == 105, row.names = FALSE); api.list.105 <- api.list.105[,1]
api.list.55  <- subset(api.gw, w_field_num == 55,  row.names = FALSE); api.list.55  <- api.list.55[,1]

# Enter indices here of wells that fail to converge to skip them in subsequent
# iterations of for-loop
api.list.630 <- api.list.630[c(-26,   -80,   -106,  -128,  -154,  -156,  -159,
                               -163,  -171,  -176,  -188,  -189,  -197,  -218,
                               -237,  -239,  -257,  -281,  -291,  -310,  -331,
                               -362,  -378,  -384,  -387,  -414,  -418,  -423,
                               -424,  -429,  -459,  -475,  -510,  -526,  -539,
                               -545,  -555,  -565,  -577,  -582,  -603,  -656,
                               -710,  -752,  -786,  -816,  -904,  -914,  -966,
                               -973,  -1006, -1035, -1036, -1039, -1040, -1050,
                               -1068, -1102, -1125, -1141, -1160, -1162, -1165,
                               -1201, -1254, -1264, -1288, -1327, -1341, -1354,
                               -1367, -1373, -1374, -1402, -1409, -1412, -1434,
                               -1437, -1449, -1463, -1465, -1467, -1469, -1504,
                               -1505, -1514, -1637, -1681, -1686, -1692, -1739,
                               -1752, -1774, -1797, -1852, -1853, -1880, -1895,
                               -1896, -1918, -1934, -2021, -2056, -2094, -2164,
                               -2186, -2251, -2297, -2387, -2457, -2478, -2485,
                               -2513, -2604, -2605, -2615, -2653, -2668, -2808,
                               -2958, -2989, -3000, -3068, -3082, -3234, -3451,
                               -3452, -3757, -3830, -3893, -4006, -4026, -4036,
                               -4118, -4152, -4179, -4207, -4209, -4389, -4454,
                               -4477, -4647, -4668, -4753, -4765, -4773, -4783,
                               -4828, -4843, -4844, -4866, -4873, -4887, -4957,
                               -4984, -5048, -5077, -5094, -5120, -5123)]
api.list.710 <- api.list.710[c(-25,   -73,   -151,  -166)]
api.list.600 <- api.list.600[c(-18,   -53,   -69)]
api.list.617 <- api.list.617[c(-23)]
api.list.640 <- api.list.640[c(-33,   -55)]
api.list.547 <- api.list.547[c(-1,    -5,    -18)]
api.list.670 <- api.list.670[c(-21,   -54)]
api.list.622 <- api.list.622[c(-25)]
api.list.590 <- api.list.590[c(-28)]
api.list.791 <- api.list.791[c(-2)]
api.list.105 <- api.list.105[c(-4)]
api.list.55  <- api.list.55[c(-7)]
# No skips for Field 635, 618, 610, 72

# Create list with all the API field vectors
api.list.ogw <- list(api.list.630,
                     api.list.710,
                     api.list.600,
                     api.list.617,
                     api.list.640,
                     api.list.547,
                     api.list.670,
                     api.list.635,
                     api.list.618,
                     api.list.622,
                     api.list.610,
                     api.list.590,
                     api.list.791,
                     api.list.72,
                     api.list.105,
                     api.list.55)

# Load results of loop below, or comment out and rerun to update
load(file.path(data_root, "coef_hyp_ogw.rda"))

# # Define dataframes that will hold fit coefficients
# coef.hyp.ogw <- vector("list", length(api.list.ogw))
# 
# # Begin for-loop
# for (j in 1:length(api.list.ogw)) {
#   api.list <- api.list.ogw[[j]]
#   temp <- matrix(0, nrow = length(api.list), ncol = 4)
#   for (a in b:length(api.list)) {
#     # Creates a subset 'w' from dataframe 'p.ogw' whose API# is = to the API#
#     # in step 'a' of the for-loop
#     w <- subset(p.ogw, p_api == api.list[a])
#     if (length(w[,1]) >= 4) {
#       # Smooth the dataset and redefine smooth spline as dataframe
#       sm <- smooth.spline(w$time, w$p_oil_prod)
#       sm <- data.frame(sm$x, sm$y)
#       # Perform regression for oil from well
#       fit.hyp.ogw <- nlsLM(sm.y ~ alpha*(1+theta*delta*sm.x)^(-1/theta),
#                            data = sm,
#                            start = list(alpha = 360, theta = 1.15, delta = 0.28),
#                            control = list(maxiter=1000))
#       temp[a,1] <- coef(fit.hyp.ogw)[1]
#       temp[a,2] <- coef(fit.hyp.ogw)[2]
#       temp[a,3] <- coef(fit.hyp.ogw)[3]
#       temp[a,4] <- summary(fit.hyp.ogw)$sigma
#     }
#   }
#   coef.hyp.ogw[[j]] <- temp
# }
# 
# # Save a list of the coefficient results so you don't have to do this again
# save(file=file.path(data_root, "coef_hyp_ogw.rda"), list=c("coef.hyp.ogw"))

# Reject outliers in coefficient dataset
# Skip rejection of outliers in Fields 72, 105, and 55 because filtering results
# in zero datapoints to fit density() to
field.select <- c(1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13)
for (i in 1:length(field.select)) {
  j <- field.select[i]
  temp <- coef.hyp.ogw[[j]]
  temp <- temp[which(temp[,1] <= max(p.ogw$p_oil_prod) &
                     temp[,1] > 0),]
  coef.hyp.ogw[[j]] <- temp
}

# Determine the PDF for each coefficient for each field
pdf.630a <- density(coef.hyp.ogw[[1]][,1], from = 0);  pdf.630b <- density(coef.hyp.ogw[[1]][,2], from = 0);  pdf.630c <- density(coef.hyp.ogw[[1]][,3], from = 0)
pdf.710a <- density(coef.hyp.ogw[[2]][,1], from = 0);  pdf.710b <- density(coef.hyp.ogw[[2]][,2], from = 0);  pdf.710c <- density(coef.hyp.ogw[[2]][,3], from = 0)
pdf.600a <- density(coef.hyp.ogw[[3]][,1], from = 0);  pdf.600b <- density(coef.hyp.ogw[[3]][,2], from = 0);  pdf.600c <- density(coef.hyp.ogw[[3]][,3], from = 0)
pdf.617a <- density(coef.hyp.ogw[[4]][,1], from = 0);  pdf.617b <- density(coef.hyp.ogw[[4]][,2], from = 0);  pdf.617c <- density(coef.hyp.ogw[[4]][,3], from = 0)
pdf.640a <- density(coef.hyp.ogw[[5]][,1], from = 0);  pdf.640b <- density(coef.hyp.ogw[[5]][,2], from = 0);  pdf.640c <- density(coef.hyp.ogw[[5]][,3], from = 0)
pdf.547a <- density(coef.hyp.ogw[[6]][,1], from = 0);  pdf.547b <- density(coef.hyp.ogw[[6]][,2], from = 0);  pdf.547c <- density(coef.hyp.ogw[[6]][,3], from = 0)
pdf.670a <- density(coef.hyp.ogw[[7]][,1], from = 0);  pdf.670b <- density(coef.hyp.ogw[[7]][,2], from = 0);  pdf.670c <- density(coef.hyp.ogw[[7]][,3], from = 0)
pdf.635a <- density(coef.hyp.ogw[[8]][,1], from = 0);  pdf.635b <- density(coef.hyp.ogw[[8]][,2], from = 0);  pdf.635c <- density(coef.hyp.ogw[[8]][,3], from = 0)
pdf.618a <- density(coef.hyp.ogw[[9]][,1], from = 0);  pdf.618b <- density(coef.hyp.ogw[[9]][,2], from = 0);  pdf.618c <- density(coef.hyp.ogw[[9]][,3], from = 0)
pdf.622a <- density(coef.hyp.ogw[[10]][,1], from = 0);  pdf.622b <- density(coef.hyp.ogw[[10]][,2], from = 0);  pdf.622c <- density(coef.hyp.ogw[[10]][,3], from = 0)
pdf.610a <- density(coef.hyp.ogw[[11]][,1], from = 0);  pdf.610b <- density(coef.hyp.ogw[[11]][,2], from = 0);  pdf.610c <- density(coef.hyp.ogw[[11]][,3], from = 0)
pdf.590a <- density(coef.hyp.ogw[[12]][,1], from = 0);  pdf.590b <- density(coef.hyp.ogw[[12]][,2], from = 0);  pdf.590c <- density(coef.hyp.ogw[[12]][,3], from = 0)
pdf.791a <- density(coef.hyp.ogw[[13]][,1], from = 0);  pdf.791b <- density(coef.hyp.ogw[[13]][,2], from = 0);  pdf.791c <- density(coef.hyp.ogw[[13]][,3], from = 0)
pdf.72a  <- density(coef.hyp.ogw[[14]][,1], from = 0);  pdf.72b  <- density(coef.hyp.ogw[[14]][,2], from = 0);  pdf.72c  <- density(coef.hyp.ogw[[14]][,3], from = 0)
pdf.105a <- density(coef.hyp.ogw[[15]][,1], from = 0);  pdf.105b <- density(coef.hyp.ogw[[15]][,2], from = 0);  pdf.105c <- density(coef.hyp.ogw[[15]][,3], from = 0)
pdf.55a  <- density(coef.hyp.ogw[[16]][,1], from = 0);  pdf.55b  <- density(coef.hyp.ogw[[16]][,2], from = 0);  pdf.55c  <- density(coef.hyp.ogw[[16]][,3], from = 0)

# Create dataframe of pdf x-values
pdf.ogw <- data.frame(pdf.630a$x, pdf.630b$x, pdf.630c$x,
                      pdf.710a$x, pdf.710b$x, pdf.710c$x,
                      pdf.600a$x, pdf.600b$x, pdf.600c$x,
                      pdf.617a$x, pdf.617b$x, pdf.617c$x,
                      pdf.640a$x, pdf.640b$x, pdf.640c$x,
                      pdf.547a$x, pdf.547b$x, pdf.547c$x,
                      pdf.670a$x, pdf.670b$x, pdf.670c$x,
                      pdf.635a$x, pdf.635b$x, pdf.635c$x,
                      pdf.618a$x, pdf.618b$x, pdf.618c$x,
                      pdf.622a$x, pdf.622b$x, pdf.622c$x,
                      pdf.610a$x, pdf.610b$x, pdf.610c$x,
                      pdf.590a$x, pdf.590b$x, pdf.590c$x,
                      pdf.791a$x, pdf.791b$x, pdf.791c$x,
                      pdf.72a$x,  pdf.72b$x,  pdf.72c$x,
                      pdf.105a$x, pdf.105b$x, pdf.105c$x,
                      pdf.55a$x,  pdf.55b$x,  pdf.55c$x)

# Determine the CDF for each coefficient for each field
cdf.630a <- cumsum(pdf.630a$y * diff(pdf.630a$x[1:2])); cdf.630b <- cumsum(pdf.630b$y * diff(pdf.630b$x[1:2])); cdf.630c <- cumsum(pdf.630c$y * diff(pdf.630c$x[1:2]))
cdf.710a <- cumsum(pdf.710a$y * diff(pdf.710a$x[1:2])); cdf.710b <- cumsum(pdf.710b$y * diff(pdf.710b$x[1:2])); cdf.710c <- cumsum(pdf.710c$y * diff(pdf.710c$x[1:2]))
cdf.600a <- cumsum(pdf.600a$y * diff(pdf.600a$x[1:2])); cdf.600b <- cumsum(pdf.600b$y * diff(pdf.600b$x[1:2])); cdf.600c <- cumsum(pdf.600c$y * diff(pdf.600c$x[1:2]))
cdf.617a <- cumsum(pdf.617a$y * diff(pdf.617a$x[1:2])); cdf.617b <- cumsum(pdf.617b$y * diff(pdf.617b$x[1:2])); cdf.617c <- cumsum(pdf.617c$y * diff(pdf.617c$x[1:2]))
cdf.640a <- cumsum(pdf.640a$y * diff(pdf.640a$x[1:2])); cdf.640b <- cumsum(pdf.640b$y * diff(pdf.640b$x[1:2])); cdf.640c <- cumsum(pdf.640c$y * diff(pdf.640c$x[1:2]))
cdf.547a <- cumsum(pdf.547a$y * diff(pdf.547a$x[1:2])); cdf.547b <- cumsum(pdf.547b$y * diff(pdf.547b$x[1:2])); cdf.547c <- cumsum(pdf.547c$y * diff(pdf.547c$x[1:2]))
cdf.670a <- cumsum(pdf.670a$y * diff(pdf.670a$x[1:2])); cdf.670b <- cumsum(pdf.670b$y * diff(pdf.670b$x[1:2])); cdf.670c <- cumsum(pdf.670c$y * diff(pdf.670c$x[1:2]))
cdf.635a <- cumsum(pdf.635a$y * diff(pdf.635a$x[1:2])); cdf.635b <- cumsum(pdf.635b$y * diff(pdf.635b$x[1:2])); cdf.635c <- cumsum(pdf.635c$y * diff(pdf.635c$x[1:2]))
cdf.618a <- cumsum(pdf.618a$y * diff(pdf.618a$x[1:2])); cdf.618b <- cumsum(pdf.618b$y * diff(pdf.618b$x[1:2])); cdf.618c <- cumsum(pdf.618c$y * diff(pdf.618c$x[1:2]))
cdf.622a <- cumsum(pdf.622a$y * diff(pdf.622a$x[1:2])); cdf.622b <- cumsum(pdf.622b$y * diff(pdf.622b$x[1:2])); cdf.622c <- cumsum(pdf.622c$y * diff(pdf.622c$x[1:2]))
cdf.610a <- cumsum(pdf.610a$y * diff(pdf.610a$x[1:2])); cdf.610b <- cumsum(pdf.610b$y * diff(pdf.610b$x[1:2])); cdf.610c <- cumsum(pdf.610c$y * diff(pdf.610c$x[1:2]))
cdf.590a <- cumsum(pdf.590a$y * diff(pdf.590a$x[1:2])); cdf.590b <- cumsum(pdf.590b$y * diff(pdf.590b$x[1:2])); cdf.590c <- cumsum(pdf.590c$y * diff(pdf.590c$x[1:2]))
cdf.791a <- cumsum(pdf.791a$y * diff(pdf.791a$x[1:2])); cdf.791b <- cumsum(pdf.791b$y * diff(pdf.791b$x[1:2])); cdf.791c <- cumsum(pdf.791c$y * diff(pdf.791c$x[1:2]))
cdf.72a  <- cumsum(pdf.72a$y  * diff(pdf.72a$x[1:2]));  cdf.72b  <- cumsum(pdf.72b$y  * diff(pdf.72b$x[1:2]));  cdf.72c  <- cumsum(pdf.72c$y  * diff(pdf.72c$x[1:2]))
cdf.105a <- cumsum(pdf.105a$y * diff(pdf.105a$x[1:2])); cdf.105b <- cumsum(pdf.105b$y * diff(pdf.105b$x[1:2])); cdf.105c <- cumsum(pdf.105c$y * diff(pdf.105c$x[1:2]))
cdf.55a  <- cumsum(pdf.55a$y  * diff(pdf.55a$x[1:2]));  cdf.55b  <- cumsum(pdf.55b$y  * diff(pdf.55b$x[1:2]));  cdf.55c  <- cumsum(pdf.55c$y  * diff(pdf.55c$x[1:2]))


# Create CDF matrix for export
cdf.ogw <- matrix(0, nrow = 512, ncol = 3*length(api.list.ogw))

# Assign cdf results to each column
cdf.ogw[, 1] <- cdf.630a; cdf.ogw[, 2] <- cdf.630b; cdf.ogw[, 3] <- cdf.630c
cdf.ogw[, 4] <- cdf.710a; cdf.ogw[, 5] <- cdf.710b; cdf.ogw[, 6] <- cdf.710c
cdf.ogw[, 7] <- cdf.600a; cdf.ogw[, 8] <- cdf.600b; cdf.ogw[, 9] <- cdf.600c
cdf.ogw[,10] <- cdf.617a; cdf.ogw[,11] <- cdf.617b; cdf.ogw[,12] <- cdf.617c
cdf.ogw[,13] <- cdf.640a; cdf.ogw[,14] <- cdf.640b; cdf.ogw[,15] <- cdf.640c
cdf.ogw[,16] <- cdf.547a; cdf.ogw[,17] <- cdf.547b; cdf.ogw[,18] <- cdf.547c
cdf.ogw[,19] <- cdf.670a; cdf.ogw[,20] <- cdf.670b; cdf.ogw[,21] <- cdf.670c
cdf.ogw[,22] <- cdf.635a; cdf.ogw[,23] <- cdf.635b; cdf.ogw[,24] <- cdf.635c
cdf.ogw[,25] <- cdf.618a; cdf.ogw[,26] <- cdf.618b; cdf.ogw[,27] <- cdf.618c
cdf.ogw[,28] <- cdf.622a; cdf.ogw[,29] <- cdf.622b; cdf.ogw[,30] <- cdf.622c
cdf.ogw[,31] <- cdf.610a; cdf.ogw[,32] <- cdf.610b; cdf.ogw[,33] <- cdf.610c
cdf.ogw[,34] <- cdf.590a; cdf.ogw[,35] <- cdf.590b; cdf.ogw[,36] <- cdf.590c
cdf.ogw[,37] <- cdf.791a; cdf.ogw[,38] <- cdf.791b; cdf.ogw[,39] <- cdf.791c
cdf.ogw[,40] <- cdf.72a;  cdf.ogw[,41] <- cdf.72b;  cdf.ogw[,42] <- cdf.72c
cdf.ogw[,43] <- cdf.105a; cdf.ogw[,44] <- cdf.105b; cdf.ogw[,45] <- cdf.105c
cdf.ogw[,46] <- cdf.55a;  cdf.ogw[,47] <- cdf.55b;  cdf.ogw[,48] <- cdf.55c

# Normalize in case of rounding error
for (a in 1:length(cdf.ogw[1,])) {
  cdf.ogw[,a] <- cdf.ogw[,a] / max(cdf.ogw[,a])
}

# Redefine as dataframe and name columns for each field
cdf.ogw <- data.frame(cdf.ogw)
names(cdf.ogw) <- c("630a", "630b", "630c",
                    "710a", "710b", "710c",
                    "600a", "600b", "600c",
                    "617a", "617b", "617c",
                    "640a", "640b", "640c",
                    "547a", "547b", "547c",
                    "670a", "670b", "670c",
                    "635a", "635b", "635c",
                    "618a", "618b", "618c",
                    "622a", "622b", "622c",
                    "610a", "610b", "610c",
                    "590a", "590b", "590c",
                    "791a", "791b", "791c",
                    "72a",  "72b",  "72c",
                    "105a", "105b", "105c",
                    "55a",  "55b",  "55c")

# # Export PDF and CDF dataframes
# save(file=file.path(data_root, "pdf_ogw.rda"), list=c("pdf.ogw"))
# save(file=file.path(data_root, "cdf_ogw.rda"), list=c("cdf.ogw"))

#-------------------------------------------------------------------------------
# Plot results
#-------------------------------------------------------------------------------
labels.ow <- c("105 Coefficient a", "105 Coefficient b", "105 Coefficient c",
               "72 Coefficient a",  "72 Coefficient b",  "72 Coefficient c",
               "590 Coefficient a", "590 Coefficient b", "590 Coefficient c",
               "117 Coefficient a", "117 Coefficient b", "117 Coefficient c",
               "718 Coefficient a", "718 Coefficient b", "718 Coefficient c",
               "55 Coefficient a",  "55 Coefficient b",  "55 Coefficient c",
               "60 Coefficient a",  "60 Coefficient b",  "60 Coefficient c",
               "65 Coefficient a",  "65 Coefficient b",  "65 Coefficient c",
               "665 Coefficient a", "665 Coefficient b", "665 Coefficient c",
               "710 Coefficient a", "710 Coefficient b", "710 Coefficient c",
               "80 Coefficient a",  "80 Coefficient b",  "80 Coefficient c",
               "630 Coefficient a", "630 Coefficient b", "630 Coefficient c",
               "605 Coefficient a", "605 Coefficient b", "605 Coefficient c",
               "560 Coefficient a", "560 Coefficient b", "560 Coefficient c",
               "101 Coefficient a", "101 Coefficient b", "101 Coefficient c",
               "610 Coefficient a", "610 Coefficient b", "610 Coefficient c",
               "692 Coefficient a", "692 Coefficient b", "692 Coefficient c",
               "695 Coefficient a", "695 Coefficient b", "695 Coefficient c")

labels.gw <- c("630 Coefficient a", "630 Coefficient b", "630 Coefficient c",
               "710 Coefficient a", "710 Coefficient b", "710 Coefficient c",
               "600 Coefficient a", "600 Coefficient b", "600 Coefficient c",
               "617 Coefficient a", "617 Coefficient b", "617 Coefficient c",
               "640 Coefficient a", "640 Coefficient b", "640 Coefficient c",
               "547 Coefficient a", "547 Coefficient b", "547 Coefficient c",
               "670 Coefficient a", "670 Coefficient b", "670 Coefficient c",
               "635 Coefficient a", "635 Coefficient b", "635 Coefficient c",
               "618 Coefficient a", "618 Coefficient b", "618 Coefficient c",
               "622 Coefficient a", "622 Coefficient b", "622 Coefficient c",
               "610 Coefficient a", "610 Coefficient b", "610 Coefficient c",
               "590 Coefficient a", "590 Coefficient b", "590 Coefficient c",
               "791 Coefficient a", "791 Coefficient b", "791 Coefficient c",
               "72 Coefficient a",  "72 Coefficient b",  "72 Coefficient c",
               "105 Coefficient a", "105 Coefficient b", "105 Coefficient c",
               "55 Coefficient a",  "55 Coefficient b",  "55 Coefficient c")

pdf.plot.list.ow <- list(pdf.105a, pdf.105b, pdf.105c,
                         pdf.72a,  pdf.72b,  pdf.72c,
                         pdf.590a, pdf.590b, pdf.590c,
                         pdf.117a, pdf.117b, pdf.117c,
                         pdf.718a, pdf.718b, pdf.718c,
                         pdf.55a,  pdf.55b,  pdf.55c,
                         pdf.60a,  pdf.60b,  pdf.60c,
                         pdf.65a,  pdf.65b,  pdf.65c,
                         pdf.665a, pdf.665b, pdf.665c,
                         pdf.710a, pdf.710b, pdf.710c,
                         pdf.80a,  pdf.80b,  pdf.80c,
                         pdf.630a, pdf.630b, pdf.630c,
                         pdf.605a, pdf.605b, pdf.605c,
                         pdf.560a, pdf.560b, pdf.560c,
                         pdf.101a, pdf.101b, pdf.101c,
                         pdf.610a, pdf.610b, pdf.610c,
                         pdf.692a, pdf.692b, pdf.692c,
                         pdf.695a, pdf.695b, pdf.695c)

cdf.plot.list.ow <- list(cdf.105a, cdf.105b, cdf.105c,
                         cdf.72a,  cdf.72b,  cdf.72c,
                         cdf.590a, cdf.590b, cdf.590c,
                         cdf.117a, cdf.117b, cdf.117c,
                         cdf.718a, cdf.718b, cdf.718c,
                         cdf.55a,  cdf.55b,  cdf.55c,
                         cdf.60a,  cdf.60b,  cdf.60c,
                         cdf.65a,  cdf.65b,  cdf.65c,
                         cdf.665a, cdf.665b, cdf.665c,
                         cdf.710a, cdf.710b, cdf.710c,
                         cdf.80a,  cdf.80b,  cdf.80c,
                         cdf.630a, cdf.630b, cdf.630c,
                         cdf.605a, cdf.605b, cdf.605c,
                         cdf.560a, cdf.560b, cdf.560c,
                         cdf.101a, cdf.101b, cdf.101c,
                         cdf.610a, cdf.610b, cdf.610c,
                         cdf.692a, cdf.692b, cdf.692c,
                         cdf.695a, cdf.695b, cdf.695c)

pdf.plot.list.gw <- list(pdf.630a, pdf.630b, pdf.630c,
                         pdf.710a, pdf.710b, pdf.710c,
                         pdf.600a, pdf.600b, pdf.600c,
                         pdf.617a, pdf.617b, pdf.617c,
                         pdf.640a, pdf.640b, pdf.640c,
                         pdf.547a, pdf.547b, pdf.547c,
                         pdf.670a, pdf.670b, pdf.670c,
                         pdf.635a, pdf.635b, pdf.635c,
                         pdf.618a, pdf.618b, pdf.618c,
                         pdf.622a, pdf.622b, pdf.622c,
                         pdf.610a, pdf.610b, pdf.610c,
                         pdf.590a, pdf.590b, pdf.590c,
                         pdf.791a, pdf.791b, pdf.791c,
                         pdf.72a,  pdf.72b,  pdf.72c,
                         pdf.105a, pdf.105b, pdf.105c,
                         pdf.55a,  pdf.55b,  pdf.55c)

hist.list <- list(coef.hyp.gow[[1]][,1], coef.hyp.gow[[1]][,2], coef.hyp.gow[[1]][,3],
                  coef.hyp.gow[[2]][,1], coef.hyp.gow[[2]][,2], coef.hyp.gow[[2]][,3],
                  coef.hyp.gow[[3]][,1], coef.hyp.gow[[3]][,2], coef.hyp.gow[[3]][,3],
                  coef.hyp.gow[[4]][,1], coef.hyp.gow[[4]][,2], coef.hyp.gow[[4]][,3],
                  coef.hyp.gow[[5]][,1], coef.hyp.gow[[5]][,2], coef.hyp.gow[[5]][,3],
                  coef.hyp.gow[[6]][,1], coef.hyp.gow[[6]][,2], coef.hyp.gow[[6]][,3],
                  coef.hyp.gow[[7]][,1], coef.hyp.gow[[7]][,2], coef.hyp.gow[[7]][,3],
                  coef.hyp.gow[[8]][,1], coef.hyp.gow[[8]][,2], coef.hyp.gow[[8]][,3],
                  coef.hyp.gow[[9]][,1], coef.hyp.gow[[9]][,2], coef.hyp.gow[[9]][,3],
                  coef.hyp.gow[[10]][,1], coef.hyp.gow[[10]][,2], coef.hyp.gow[[10]][,3],
                  coef.hyp.gow[[11]][,1], coef.hyp.gow[[11]][,2], coef.hyp.gow[[11]][,3],
                  coef.hyp.gow[[12]][,1], coef.hyp.gow[[12]][,2], coef.hyp.gow[[12]][,3],
                  coef.hyp.gow[[13]][,1], coef.hyp.gow[[13]][,2], coef.hyp.gow[[13]][,3],
                  coef.hyp.gow[[14]][,1], coef.hyp.gow[[14]][,2], coef.hyp.gow[[14]][,3],
                  coef.hyp.gow[[15]][,1], coef.hyp.gow[[15]][,2], coef.hyp.gow[[15]][,3],
                  coef.hyp.gow[[16]][,1], coef.hyp.gow[[16]][,2], coef.hyp.gow[[16]][,3],
                  coef.hyp.gow[[17]][,1], coef.hyp.gow[[17]][,2], coef.hyp.gow[[17]][,3],
                  coef.hyp.gow[[18]][,1], coef.hyp.gow[[18]][,2], coef.hyp.gow[[18]][,3])

# Comparison of PDF/CDF values for OOW
pdf(file = "CDF results for OOW from declineCurve_v7.pdf")
# Comparison of coefficient b values
linecolor <- rainbow(18)
plot(pdf.oow[,2], cdf.oow[,2],
     xlab = "Coefficient Value",
     ylab = "Probability",
     type = "l",
     col = linecolor[1],
     ylim = c(0, 1),
     xlim = c(-10, 10),
     main = "CDF of Oil from Oil Wells for Coefficient b for all fields")
m <- 3
for (j in 2:18) {
  lines(pdf.oow[,2], cdf.oow[,((m*j)-1)], col = linecolor[j])
}
legend("bottomright", as.character(field.ow), ncol = 2, col = linecolor, lty = 1)
# Comparison of coefficient c values
plot(pdf.oow[,3], cdf.oow[,3],
     xlab = "Coefficient Value",
     ylab = "Probability",
     type = "l",
     col = linecolor[1],
     ylim = c(0, 1),
     xlim = c(0, 5),
     main = "CDF of Oil from Oil Wells for Coefficient c for all fields")
m <- 3
for (j in 2:18) {
  lines(pdf.oow[,3], cdf.oow[,m*j], col = linecolor[j])
}
legend("bottomright", as.character(field.ow), ncol = 2, col = linecolor, lty = 1)
# Individual CDF Plots
for (i in 1:length(cdf.oow)) {
  plot(pdf.oow[,i], cdf.oow[,i],
       xlab = "Coefficient Value",
       ylab = "Probability",
       type = "l",
       main = paste("CDF of Oil from Oil Wells for Field", labels.ow[i], sep = " "))
}
dev.off()

# Comparison of PDF/CDF values for GGW
pdf(file = "CDF results for GGW from declineCurve_v8.pdf")
# Comparison of coefficient b values
linecolor <- rainbow(16)
plot(pdf.ggw[,2], cdf.ggw[,2],
     xlab = "Coefficient Value",
     ylab = "Probability",
     type = "l",
     col = linecolor[1],
     ylim = c(0, 1),
     xlim = c(-10, 10),
     main = "CDF of Gas from Gas Wells for Coefficient b for all fields")
m <- 3
for (j in 2:16) {
  lines(pdf.ggw[,2], cdf.ggw[,((m*j)-1)], col = linecolor[j])
}
legend("bottomright", as.character(field.gw), ncol = 2, col = linecolor, lty = 1)
# Comparison of coefficient c values
plot(pdf.ggw[,3], cdf.ggw[,3],
     xlab = "Coefficient Value",
     ylab = "Probability",
     type = "l",
     col = linecolor[1],
     ylim = c(0, 1),
     xlim = c(0, 5),
     main = "CDF of Gas from Gas Wells for Coefficient c for all fields")
m <- 3
for (j in 2:16) {
  lines(pdf.ggw[,3], cdf.ggw[,m*j], col = linecolor[j])
}
legend("bottomright", as.character(field.gw), ncol = 2, col = linecolor, lty = 1)
# Individual CDF Plots
for (i in 1:length(cdf.ggw)) {
  plot(pdf.ggw[,i], cdf.ggw[,i],
       xlab = "Coefficient Value",
       ylab = "Probability",
       type = "l",
       main = paste("CDF of Gas from Gas Wells for Field", labels.gw[i], sep = " "))
}
dev.off()

pdf(file = "PDF results for GGW from declineCurve_v6.pdf")
for (i in 1:length(pdf.plot.list.gw)) {
  plot(pdf.plot.list.gw[[i]],
       main = paste("PDF of Gas from Gas Wells for Field", labels.gw[i], sep = " "))
}
dev.off()

pdf(file = "PDF results for GOW from declineCurve_v6.pdf")
for (i in 1:length(pdf.plot.list.ow)) {
  hist(hist.list[[i]],
       freq = FALSE,
       breaks = 20,
       xlab = "Coefficient Value",
       main = paste("PDF of Gas from Oil Wells for Field", labels.ow[i], sep = " "))
  lines(pdf.plot.list.ow[[i]], col = "blue")
  legend("topright", c("density() PDF"), lty = c(1), col = c("blue"))
}
dev.off()

# Box plots of results from coef.hyp.oow
name <- c("Coefficient a", "Coefficient b", "coefficient c", "RSE", "RSS", "Sum", "Datapoints per Well")
pdf(file = "Box plots of results for OOW from declineCurve_v7 no outliers.pdf")
for (j in 1:7) {
  boxplot(coef.hyp.oow[[1]][,j], coef.hyp.oow[[2]][,j], coef.hyp.oow[[3]][,j],
          coef.hyp.oow[[4]][,j], coef.hyp.oow[[5]][,j], coef.hyp.oow[[6]][,j],
          coef.hyp.oow[[7]][,j], coef.hyp.oow[[8]][,j], coef.hyp.oow[[9]][,j],
          coef.hyp.oow[[10]][,j], coef.hyp.oow[[11]][,j], coef.hyp.oow[[12]][,j],
          coef.hyp.oow[[13]][,j], coef.hyp.oow[[14]][,j], coef.hyp.oow[[15]][,j],
          coef.hyp.oow[[16]][,j], coef.hyp.oow[[17]][,j], coef.hyp.oow[[18]][,j],
          names = field.ow,
          las = 3,
          varwidth = FALSE,
          outline = FALSE,
          xlab = "Field Number",
          ylab = "Value",
          main = paste("Boxplot (w/o outliers) of ", name[j], " for OOW", sep = ""))
}
dev.off()

# Box plots of results from coef.hyp.ggw
name <- c("Coefficient a", "Coefficient b", "coefficient c", "RSE", "RSS", "Sum", "Datapoints per Well")
pdf(file = "Box plots of results for GGW from declineCurve_v8 no outliers.pdf")
for (j in 1:7) {
  boxplot(coef.hyp.ggw[[1]][,j], coef.hyp.ggw[[2]][,j], coef.hyp.ggw[[3]][,j],
          coef.hyp.ggw[[4]][,j], coef.hyp.ggw[[5]][,j], coef.hyp.ggw[[6]][,j],
          coef.hyp.ggw[[7]][,j], coef.hyp.ggw[[8]][,j], coef.hyp.ggw[[9]][,j],
          coef.hyp.ggw[[10]][,j], coef.hyp.ggw[[11]][,j], coef.hyp.ggw[[12]][,j],
          coef.hyp.ggw[[13]][,j], coef.hyp.ggw[[14]][,j], coef.hyp.ggw[[15]][,j],
          coef.hyp.ggw[[16]][,j],
          names = field.gw,
          las = 3,
          varwidth = FALSE,
          outline = FALSE,
          xlab = "Field Number",
          ylab = "Value",
          main = paste("Boxplot (w/o outliers) of ", name[j], " for GGW", sep = ""))
}
dev.off()

# 3D Scatterplot of coefficients in OOW fits
pdf(file.path(plot_root, "OOW 3D Scatterplots from declineCurve_v8.pdf"))
for (i in 1:length(field.ow)) {
  s3d <- scatterplot3d(x = coef.hyp.oow[[i]][,2],
                       y = coef.hyp.oow[[i]][,3],
                       z = coef.hyp.oow[[i]][,1]/1e3,
                       xlim = c(-10,10),
                       ylim = c(-10,10),
                       xlab = "Coefficient b",
                       ylab = "Coefficient c",
                       zlab = "Coefficient a (10^3)",
                       main = paste("3D Scatterplot for OOW coefficients in Field ", field.ow[i], sep = ""))
  fit <- lm(coef.hyp.oow[[i]][,1]/1e3 ~ coef.hyp.oow[[i]][,2] + coef.hyp.oow[[i]][,3])
  s3d$plane3d(fit)
}
dev.off()

# 3D Scatterplot of coefficients in GGW fits
pdf(file.path(plot_root, "GGW 3D Scatterplots from declineCurve_v8.pdf"))
for (i in 1:length(field.gw)) {
  s3d <- scatterplot3d(x = coef.hyp.ggw[[i]][,2],
                       y = coef.hyp.ggw[[i]][,3],
                       z = coef.hyp.ggw[[i]][,1]/1e3,
                       xlim = c(-10,10),
                       ylim = c(-10,10),
                       xlab = "Coefficient b",
                       ylab = "Coefficient c",
                       zlab = "Coefficient a (10^3)",
                       main = paste("3D Scatterplot for GGW coefficients in Field ", field.gw[i], sep = ""))
  fit <- lm(coef.hyp.ggw[[i]][,1]/1e3 ~ coef.hyp.ggw[[i]][,2] + coef.hyp.ggw[[i]][,3])
  s3d$plane3d(fit)
}
dev.off()