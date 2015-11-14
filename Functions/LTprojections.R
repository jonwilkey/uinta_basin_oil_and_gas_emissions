# Script Info -------------------------------------------------------------
# Name:      LTprojections.R (Long-term projections)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Description -------------------------------------------------------------

# This script generates plots of long-term projection results under different
# assumptions.



# GBM Fit -----------------------------------------------------------------

tGBMfit <- function(path, eia.hp, tstart, tstop) {
  
  # Define vectors from eia.hp columns
  dates <- eia.hp$month
  OP <- eia.hp$OP
  GP <- eia.hp$GP
  
  # Create an ordered time series object out of OP and GP so that we can use time
  # series functions such as diff().
  OP.z <- zoo(OP, order.by=dates)
  GP.z <- zoo(GP, order.by=dates)
  
  # Subset to specified time range
  OP.z <- window(OP.z, start = as.yearmon(tstart), end = as.yearmon(tstop))
  GP.z <- window(GP.z, start = as.yearmon(tstart), end = as.yearmon(tstop))
  
  
  # Fit GBM parameters ------------------------------------------------------
  
  # Calculate the mean of the first differences of the log prices.
  m.OP <- mean(diff(log(OP.z)))
  m.GP <- mean(diff(log(GP.z)))
  
  # Following are the Maximum Likelihood Estimation (MLE) for GBM parameters
  
  # [1] 'v' is the squared volatility
  v.OP <- mean((diff(log(OP.z))-m.OP)**2)
  v.GP <- mean((diff(log(GP.z))-m.GP)**2)
  
  # [2] 'mu' is drift
  mu.OP <- m.OP + v.OP/2
  mu.GP <- m.GP + v.GP/2
  
  # Create data.frame with fit results for export
  GBMfitOP <- data.frame(v.OP, mu.OP); names(GBMfitOP) <- c("v", "mu")
  GBMfitGP <- data.frame(v.GP, mu.GP); names(GBMfitGP) <- c("v", "mu")
  
  
  # Export results ----------------------------------------------------------
  
  return(list(GBMfitOP, GBMfitGP))
}

GBMfit <- tGBMfit(path, eia.hp, tstart = as.Date("1977-01-01"), tstop = as.Date("1994-12-01"))

# Queries and Calculations ------------------------------------------------

tepsim <- GBMsim(path =         path,
                 oil.fpp.init = 25.89,
                 gas.fpp.init = 2.83,
                 timesteps =    20*12,
                 nrun =         1e4,
                 GBMfitOP =     GBMfit[[1]],
                 GBMfitGP =     GBMfit[[2]])

# Extract individual data.frames from list object
top <- tepsim$op
tgp <- tepsim$gp

# Actual energy prices for oil and gas
ep.act <- subset(eia.hp,
                 subset = (month >= as.yearmon("1995-01-01") &
                             month <= as.yearmon("2014-12-01")),
                 select = c("OP", "GP"))

# Predefine quantile matrices
op.q <- matrix(0, nrow = length(opt$quant), ncol = (20*12))
gp.q <- op.q

# For each timestep, get quantiles
for (i in 1:ncol(op.q)) {
  op.q[,i] <-     quantile(top[,i],                      opt$quant)
  gp.q[,i] <-     quantile(tgp[,i],                      opt$quant)
}

# Set line colors for quantiles used in quant
qlinecolor <- rainbow(length(opt$quant))
qlinetype <-  rep(1, length(opt$quant))
qlinewidth <- rep(1, length(opt$quant))

# Set line options for actual lines (for cross-validation plots)
alinecolor <- "black"
alinetype <-  1
alinewidth <- 1

tsteps <- seq(as.Date("1995-01-01"), as.Date("2014-12-01"), by = "months")

pdf(file.path(path$plot, "GBM sim 95-14.pdf"))

# Main plot with largest quantile result
plot(tsteps, op.q[1,],
     ylim = c(0.9*min(op.q), 1.1*max(ep.act$OP)),
     type = "l",
     lty = qlinetype[1],
     lwd = qlinewidth[1],
     col = qlinecolor[1],
     xlab = "Time (months)",
     ylab = paste("Oil First Purchase Price (", opt$cpiDate, " $/bbl)", sep = ""))#,
#main = "Oil Price")

# All the other quantile lines
for (i in 2:length(opt$quant)) {
  lines(tsteps, op.q[i,], col = qlinecolor[i], lty = qlinetype[i], lwd = qlinewidth[i])
}

# Add the line for the actual value
lines(tsteps, ep.act$OP, col = alinecolor, lty = alinetype, lwd = alinewidth)

# Add legend
legend("topleft",
       c("Actual", "90%", "70%", "50%", "30%", "10%"),
       ncol = 1,
       bg = "white",
       col = c(alinecolor, qlinecolor),
       lwd = c(alinewidth, qlinewidth),
       lty = c(alinetype,  qlinetype))

# Main plot with largest quantile result
plot(tsteps, gp.q[1,],
     ylim = c(0.9*min(gp.q), 1.1*max(ep.act$GP)),
     type = "l",
     lty = qlinetype[1],
     lwd = qlinewidth[1],
     col = qlinecolor[1],
     xlab = "Time (months)",
     ylab = paste("Gas First Purchase Price (", opt$cpiDate, " $/MCF)", sep = ""))#,
#main = "Oil Price")

# All the other quantile lines
for (i in 2:length(opt$quant)) {
  lines(tsteps, gp.q[i,], col = qlinecolor[i], lty = qlinetype[i], lwd = qlinewidth[i])
}

# Add the line for the actual value
lines(tsteps, ep.act$GP, col = alinecolor, lty = alinetype, lwd = alinewidth)

# Add legend
legend("topleft",
       c("Actual", "90%", "70%", "50%", "30%", "10%"),
       ncol = 1,
       bg = "white",
       col = c(alinecolor, qlinecolor),
       lwd = c(alinewidth, qlinewidth),
       lty = c(alinetype,  qlinetype))

dev.off()


# Drilling Model ----------------------------------------------------------

tsteps <- seq(as.Date("1999-12-01"), as.Date("2009-12-01"), by = "year")

pdf(file.path(path$plot, "Figure 9 Drill model test 1.pdf"))

for (i in 1:length(tsteps)) {
  
  # Function call
  drillingModelUpdate(path =      path,
                      p =         p,
                      min.depth = opt$min.well.depth,
                      tstart =    as.Date("1977-08-01"),
                      tstop =     tsteps[i],
                      ver =       "test",
                      eia.hp =    eia.hp)
  
  # Load economic drilling model fits (drillModel) and data (drillModelData)
  load(file.path(path$data, paste("drillModel_test.rda", sep = "")))
  
  # Prior well model function
  PWM <- function(OP, GP, par, init) {
    
    # Initial wells drilled
    w <- round(par[1]*OP[1]+par[2]*GP[1]+par[3]*init+par[4])
    w <- ifelse(w < 0, 0, w)
    
    for(i in 2:length(OP)) {
      
      # Wells drilled
      w <- c(w, round(par[1]*OP[i]+par[2]*GP[i]+par[3]*w[length(w)]+par[4]))
      w <- ifelse(w < 0, 0, w)
    }
    
    return(w)
  }
  
  # Other models
  EPM <- function(fit, OP, GP) {temp <- round(fit[2]*OP+fit[3]*GP+fit[1]); ifelse(temp > 0, temp, 0)} # Energy price model
  OPM <- function(fit, OP)     {temp <- round(fit[2]*OP+fit[1]); ifelse(temp > 0, temp, 0)}           # Oil price model
  GPM <- function(fit, GP)     {temp <- round(fit[2]*GP+fit[1]); ifelse(temp > 0, temp, 0)}
  
  # For simplicity, make copy of drillModelData just labeled as "d"
  d <- drillModelData
  
  # Temporary time index
  ind <- which(d$month >= tsteps[i]+31 &
                 d$month <= opt$tstop)
  
  # Main plot for cross-validation
  plot(d$month[ind], d$wells[ind],
       ylim = c(0, 100),
       lwd =  alinewidth,
       col =  alinecolor,
       lty =  alinetype,
       type = "l",
       xlab = "Time (months)",
       ylab = "Wells Drilled",
       main = paste("Train July 1977 to", as.yearmon(tsteps[i])))
  
  # Add line for model predictions
  lines(d$month[ind],
        PWM(OP = d$OP[ind], GP = d$GP[ind], par = drillModel$pwm, init = d$prior[ind[1]]),
        col = qlinecolor[1], lwd = qlinewidth[1], lty = qlinetype[1])
  lines(d$month[ind],
        EPM(fit = coefficients(drillModel$epm), OP = d$OP[ind-1], GP = d$GP[ind-1]),
        col = qlinecolor[2], lwd = qlinewidth[2], lty = qlinetype[2])
  lines(d$month[ind],
        OPM(fit = coefficients(drillModel$opm), OP = d$OP[ind-1]),
        col = qlinecolor[3], lwd = qlinewidth[3], lty = qlinetype[3])
  lines(d$month[ind],
        GPM(fit = coefficients(drillModel$gpm), GP = d$GP[ind-1]),
        col = qlinecolor[4], lwd = qlinewidth[4], lty = qlinetype[4])
  
  # Legend
  #legend("bottomleft", c("Actual", "PWM", "EPM", "OPM", "GPM"), lty = c(1,rep(1,4)),
  legend("bottomleft", c("Actual", "Eq. (7)", "Eq. (8)", "Eq. (9)", "Eq. (10)"),
         bg = "white",
         lty = c(alinetype,  qlinetype[1:4]),
         lwd = c(alinewidth, qlinewidth[1:4]),
         col = c(alinecolor, qlinecolor[1:4]),
         ncol = 5, cex = 1/opt$defFontSize)
  
  # Remove everything
  remove(PWM, EPM, OPM, GPM, d, ind)
}

dev.off()

pdf(file.path(path$plot, "drillData.pdf"))

# Drilling data
plot(wells~month, drillModelData,
     #pch = 20,
     col = "grey",
     ylim = c(0, 125),
     xlab = "Date (months)",
     ylab = "Well Count (well / month) & OP ($ / bbl) & 10*GP ($ / MCF)")
lines(OP~month, drillModelData, col = "blue")
lines(I(GP*10)~month, drillModelData, col = "red")
legend("topleft", c("Wells", "OP", "GP"), pch = c(1, NA, NA), lty = c(NA, 1, 1), col = c("grey", "blue", "red"))

dev.off()


# Well types --------------------------------------------------------------

# Add column 'drill_month' as truncated to month version of h_first_prod
p$drill_month <- as.yearmon(p[,"h_first_prod"])

# Create dataframe containing dates of 1st production for each unique APD #
# and the field it is located in
well <- sqldf("select distinct(p_api), drill_month, h_well_type, h_td_md
                from p")

# Drop NA observations
well <- na.omit(well)

# Only wells (1) inside price history timeframe (and prior month), (2) with
# depths > 0, and (3) that were drilled with the intention of being producing
# wells (i.e. oil wells, gas wells, or dry wells).
well <- subset(well, subset = (drill_month >= (as.yearmon(min(eia.hp$month))-1/12) &
                                 drill_month <= as.yearmon(max(eia.hp$month)) &
                                 h_td_md >= 0 &
                                 (h_well_type == "OW" |
                                    h_well_type == "GW" |
                                    h_well_type == "D")))

# Determine total number of wells drilled each year
drill.ow <- sqldf("select drill_month, count(p_api)
                 from well
                 where h_well_type = 'OW' group by drill_month")
drill.gw <- sqldf("select drill_month, count(p_api)
                 from well
                  where h_well_type = 'GW' group by drill_month")

names(drill.ow) <- c("month", "ow")
names(drill.gw) <- c("month", "gw")

m <- merge(x = drill.ow, y = drill.gw, all = TRUE)
m$ow[which(is.na(m$ow))] <- 0
m$gw[which(is.na(m$gw))] <- 0

pdf(file.path(path$plot, "Figure 12 Well counts by type.pdf"))

# Drilling data
plot(ow~month, m,
     type = "l",
     lwd = 1,
     col = "blue",
     ylim = c(0, 1.1*max(m[,2:3])),
     xlab = "Date (months)",
     ylab = "Wells Drilled")
lines(gw~month, m, col = "red", lwd = 1)
legend("topleft", c("Oil Wells", "Gas Wells"), lty = 1, col = c("blue", "red"))

dev.off()

