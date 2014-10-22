# Read in EIA forecast prices for oil and gas
eia <- read.csv(file.path(path$raw, "EIAforecast.csv"))
names(eia) <- c("date", "OPr", "OPh", "OPl", "GPr", "GPh", "GPl")
eia$date <- as.Date(eia$date)

# Adjust for inflation and mutliply by FPP conversion factor
OPr <- inf_adj(eia$OPr, opt$EP.CPI.basis, opt$cpi)*opt$WTI.to.Oilfpp
OPh <- inf_adj(eia$OPh, opt$EP.CPI.basis, opt$cpi)*opt$WTI.to.Oilfpp
OPl <- inf_adj(eia$OPl, opt$EP.CPI.basis, opt$cpi)*opt$WTI.to.Oilfpp
GPr <- inf_adj(eia$GPr, opt$EP.CPI.basis, opt$cpi)*opt$HH.to.Gasfpp
GPh <- inf_adj(eia$GPh, opt$EP.CPI.basis, opt$cpi)*opt$HH.to.Gasfpp
GPl <- inf_adj(eia$GPl, opt$EP.CPI.basis, opt$cpi)*opt$HH.to.Gasfpp

# Set date as index
times.init <-as.POSIXct(strptime(eia$date, "%Y-%m-%d"))

# Then turn each forecast into zoo object
OPr <- zoo(OPr,times.init); OPh <- zoo(OPh,times.init); OPl <- zoo(OPl,times.init)
GPr <- zoo(GPr,times.init); GPh <- zoo(GPh,times.init); GPl <- zoo(GPl,times.init)

# Finally, use na.approx to linearly interpolate missing points
OPr <- na.approx(object = OPr, xout = seq(min(times.init), max(times.init), "month"))
OPh <- na.approx(object = OPh, xout = seq(min(times.init), max(times.init), "month"))
OPl <- na.approx(object = OPl, xout = seq(min(times.init), max(times.init), "month"))
GPr <- na.approx(object = GPr, xout = seq(min(times.init), max(times.init), "month"))
GPh <- na.approx(object = GPh, xout = seq(min(times.init), max(times.init), "month"))
GPl <- na.approx(object = GPl, xout = seq(min(times.init), max(times.init), "month"))

# Load GBM fitting results
load(file.path(path$data, "GBMfit_v2.rda"))

# Simulate N paths of monthly GBM for 2013-2040 time period with an initial
# price equal to the last recorded oil/gas price.
timesteps <- length(OPr)-1
N <- 1e3
simOP <- GBMsim(p.init = inf_adj(opt$oil.fpp.init, opt$EP.CPI.basis, opt$cpi),
                mu = GBMfitOP$mu,
                v = GBMfitOP$v,
                timesteps = timesteps,
                N = N)
simGP <- GBMsim(p.init = inf_adj(opt$gas.fpp.init, opt$EP.CPI.basis, opt$cpi),
                mu = GBMfitGP$mu,
                v = GBMfitGP$v,
                timesteps = timesteps,
                N = N)

# Quantiles
quant <- c(0.9, 0.75, 0.5, 0.25, 0.1)
OPquant <- matrix(0, nrow = length(quant), ncol = length(OPr))
GPquant <- OPquant
for (i in 1:length(OPr)) {
  OPquant[,i] <- quantile(simOP[i,], quant)
  GPquant[,i] <- quantile(simGP[i,], quant)
}

# Plots

# Set line colors
linecolor <- rainbow(length(quant))

# Cutoff Date
date <- seq(from = as.Date("2013-01-01"), to = as.Date("2020-01-01"), by = "month")

# Trim data
simOP <- simOP[1:(length(date)),]
simGP <- simGP[1:(length(date)),]
OPquant <- OPquant[,1:(length(date))]
GPquant <- GPquant[,1:(length(date))]

# Oil
# pdf(file.path(path$plot, file = "GBM Oil Price Paths.pdf"))
# Plot paths
plot(date, simOP[,1],
     ylim = c(0, 1.1*max(simOP)),
     xlab = "Date",
     ylab = "UT Oil FPP (2014 $/bbl)",
     main = "GBM Oil Price Paths vs. EIA Forecasts",
     type = "n")
for (k in 1:ncol(simOP)) {
  lines(date, simOP[,k], col="lightgrey")
}
# Plot quantiles
for (i in 1:length(quant)) {
  lines(date, OPquant[i,], col = linecolor[i], lwd = 2)
}
# Plot EIA Forecasts
lines(date, coredata(OPr)[1:length(date)], lwd = 2, col = "black", lty = 2)
lines(date, coredata(OPh)[1:length(date)], lwd = 2, col = "black", lty = 2)
lines(date, coredata(OPl)[1:length(date)], lwd = 2, col = "black", lty = 2)
# Legend
legend("topleft",
      c("90%", "75%", "50%", "25%", "10%", "EIA", "GBM"),
      ncol = 2,
      col = c(linecolor,"black","lightgrey"),
      lwd = c(rep(2,length(quant)), 2, 2),
      lty = c(rep(1,length(quant)), 2, 1))
# dev.off()

# Gas
# pdf(file.path(path$plot, file = "GBM Gas Price Paths.pdf"))
# Plot paths
plot(date, simGP[,1],
     ylim = c(0, 1.1*max(simGP)),
     xlab = "Date",
     ylab = "UT Gas FPP (2014 $/MMBtu)",
     main = "GBM Gas Price Paths vs. EIA Forecasts",
     type = "n")
for (k in 1:ncol(simGP)) {
  lines(date, simGP[,k], col="lightgrey")
}
# Plot quantiles
for (i in 1:length(quant)) {
  lines(date, GPquant[i,], col = linecolor[i], lwd = 2)
}
# Plot EIA Forecasts
lines(date, coredata(GPr)[1:length(date)], lwd = 2, col = "black", lty = 2)
lines(date, coredata(GPh)[1:length(date)], lwd = 2, col = "black", lty = 2)
lines(date, coredata(GPl)[1:length(date)], lwd = 2, col = "black", lty = 2)
# Legend
legend("topleft",
       c("90%", "75%", "50%", "25%", "10%", "EIA", "GBM"),
       ncol = 2,
       col = c(linecolor,"black","lightgrey"),
       lwd = c(rep(2,length(quant)), 2, 2),
       lty = c(rep(1,length(quant)), 2, 1))
# dev.off()
