# Function Info -----------------------------------------------------------
# Name:      postProcess.R (Post processing and plot function)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# plot.PDF - TRUE/FALSE flag indicating whether or not to print to PDF

# pdfName - character string for file name of PDF plot (if applicable)

# wsim - simulated well information data.frame

# osim - oil production volume matrix

# wsim.actual - actual well information data.frame

# osim.actual - actual oil production volume matrix

# timeseteps - numeric vector of timesteps in simulation/actual datasets


# Outputs -----------------------------------------------------------------

# blah


# Description -------------------------------------------------------------

# This script generates plots the results from the Monte-Carlo simulation.


# Function ---------------------------------------------------------------- 
postProcess <- function(plot.PDF, pdfName, wsim, osim, wsim.actual, osim.actual,
                        timesteps) {
  
}


# Queries and Calculations ------------------------------------------------

# Actual energy prices for oil and gas
ep.act <- subset(eia.hp,
                 subset = (month >= as.yearmon(opt$tstart) &
                           month <= as.yearmon(opt$tstop)),
                 select = c("OP", "GP"))

# Actual number of wells drilled as timeseries. sqldf returns result as a
# data.frame, so brackets extract numerical results as a vector.
Drilled.act <- sqldf("select count(wellID) from 'wsim.actual' group by tDrill")[[1]]

# Total oil/gas production as timeseries for each simulation run
tot.oil <- matrix(0, nrow = opt$nrun, ncol = opt$MC.tsteps)
tot.gas <- tot.oil
for (i in 1:opt$nrun) {
  ind <- which(wsim$runID == i)
  for (j in 1:max(wsim$tDrill)) {
    tot.oil[i,j] <- sum(osim[ind,j])
    tot.gas[i,j] <- sum(gsim[ind,j])
  }
}

# Actual total oil/gas production as timeseries
tot.oil.act <- colSums(osim.actual, na.rm = TRUE)
tot.gas.act <- colSums(gsim.actual, na.rm = TRUE)

# Quantiles
quant <- c(0.9, 0.7, 0.5, 0.3, 0.1)
Drilled.quant <- matrix(0, nrow = length(quant), ncol = opt$MC.tsteps)
tot.oil.quant <- Drilled.quant
tot.gas.quant <- Drilled.quant
op.quant <- Drilled.quant
gp.quant <- Drilled.quant
for (i in 1:max(wsim$tDrill)) {
  Drilled.quant[,i] <- quantile(Drilled[,i], quant)
  tot.oil.quant[,i] <- quantile(tot.oil[,i], quant)
  tot.gas.quant[,i] <- quantile(tot.gas[,i], quant)
  
  # Energy price matrices have extra column for prices at timestep prior to
  # start of simulation period, so pick column i+1 to adjust
  op.quant[,i] <- quantile(op[,i+1], quant)
  gp.quant[,i] <- quantile(gp[,i+1], quant)
}


# Plotting ----------------------------------------------------------------

# Save plots to PDF as pdfName if pdfFLag == "yes"
if(opt$exportFlag == TRUE) pdf(file.path(path$plot, file = opt$pdfName))

# Set line colors for quantiles used in quant
linecolor <- rainbow(length(quant))

# # Oil prices as timeseries simulated vs actual
# plot(opt$tsteps, op.quant[1,],
#      type = "l",
#      col = linecolor[1],
#      xlab = "Time",
#      ylab = "Oil First Purchase Price (2012 $/bbl)",
#      main = "Oil Price - Simulation vs. Actual")
# for (i in 2:length(quant)) {
#   lines(opt$tsteps, op.quant[i,], col = linecolor[i])
# }
# lines(opt$tsteps, ep.act$OP)
# legend("topleft",
#        c("90%", "70%", "50%", "30%", "10%", "Actual"),
#        ncol = 2,
#        col = c(linecolor,"black"),
#        lty = 1)
# 
# # Gas prices as timeseries simulated vs actual
# plot(opt$tsteps, gp.quant[1,],
#      type = "l",
#      col = linecolor[1],
#      xlab = "Time",
#      ylab = "Gas First Purchase Price (2012 $/bbl)",
#      main = "Gas Price - Simulation vs. Actual")
# for (i in 2:length(quant)) {
#   lines(opt$tsteps, gp.quant[i,], col = linecolor[i])
# }
# lines(opt$tsteps, ep.act$GP)
# legend("topleft",
#        c("90%", "70%", "50%", "30%", "10%", "Actual"),
#        ncol = 2,
#        col = c(linecolor,"black"),
#        lty = 1)
# 
# # Number of wells drilled as timeseries simulated vs actual
# plot(opt$tsteps, Drilled.quant[1,],
#      type = "l",
#      col = linecolor[1],
#      xlab = "Time",
#      ylab = "Wells Drilled",
#      main = "Drilling Schedule - Simulation vs. Actual")
# for (i in 2:length(quant)) {
#   lines(opt$tsteps, Drilled.quant[i,], col = linecolor[i])
# }
# lines(opt$tsteps, Drilled.act)
# legend("topleft",
#        c("90%", "70%", "50%", "30%", "10%", "Actual"),
#        ncol = 2,
#        col = c(linecolor,"black"),
#        lty = 1)


# Boxplots of Decline curve coefficients by field

# Production timeseries for oil vs actual
plot(opt$tsteps, tot.oil.quant[1,],
     type = "l",
     col = linecolor[1],
     xlab = "Time (months)",
     ylab = "Oil Production (bbl)",
     main = "Total Oil Production - Simulated vs. Actual")
for (i in 2:length(quant)) {
  lines(opt$tsteps,tot.oil.quant[i,], col = linecolor[i])
}
lines(opt$tsteps, tot.oil.act)
legend("topleft",
       c("90%", "70%", "50%", "30%", "10%", "Actual"),
       ncol = 2,
       col = c(linecolor,"black"),
       lty = 1)

# Production timeseries for gas vs actual
plot(opt$tsteps, tot.gas.quant[1,],
     type = "l",
     col = linecolor[1],
     xlab = "Time (months)",
     ylab = "Gas Production (MCF)",
     main = "Total Gas Production - Simulated vs. Actual")
for (i in 2:length(quant)) {
  lines(opt$tsteps,tot.gas.quant[i,], col = linecolor[i])
}
lines(opt$tsteps, tot.gas.act)
legend("topleft",
       c("90%", "70%", "50%", "30%", "10%", "Actual"),
       ncol = 2,
       col = c(linecolor,"black"),
       lty = 1)


# # Production time series for gas vs actual
# 
# # Production from individual well by field vs actual
# 
# # Royalties, severance taxes, property taxes, corporate income taxes (federal
# # and state) as timeseries
# 
# # RIMS vs. BEA mining jobs data
# 
# 
# # Workload vs. DWFS oil and gas jobs data
# 
# 
# # GHG CO2e timeseries
# plot(timesteps, ghg.quant[1,],
#      type = "l",
#      col = linecolor[1],
#      xlab = "Time (months)",
#      ylab = "CO2e Emissions (1e3 kg)",
#      main = "Total CO2 Equivalent Emissions Estimate")
# for (i in 2:length(quant)) {
#   lines(timesteps,ghg.quant[i,], col = linecolor[i])
# }
# legend("topleft",
#        c("90%", "70%", "50%", "30%", "10%"),
#        ncol = 2, col = c(linecolor), lty = 1)
# 
# 
# # GHG CH4 timeseries
# plot(timesteps, ch4.quant[1,],
#      type = "l",
#      col = linecolor[1],
#      xlab = "Time (months)",
#      ylab = "CH4 Emissions (MCF CH4)",
#      main = "Total CH4 Emissions Estimate from Gas Wells")
# for (i in 2:length(quant)) {
#   lines(timesteps,ch4.quant[i,], col = linecolor[i])
# }
# legend("topleft",
#        c("90%", "70%", "50%", "30%", "10%"),
#        ncol = 2, col = c(linecolor), lty = 1)

# End PDF
if(opt$exportFlag == TRUE) dev.off()