# Script Info -------------------------------------------------------------
# Name:      postProcess.R (Post processing and plot function)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Description -------------------------------------------------------------

# This script generates plots of the results from the Monte-Carlo simulation.


# Queries and Calculations ------------------------------------------------

# Actual energy prices for oil and gas
ep.act <- subset(eia.hp,
                 subset = (month >= as.yearmon(opt$tstart) &
                             month <= as.yearmon(opt$tstop)),
                 select = c("OP", "GP"))

# Actual number of wells drilled as timeseries. sqldf returns result as a
# data.frame, so brackets extract numerical results as a vector.
Drilled.act <- drillModelData$wells[which(drillModelData$month >= opt$tstart &
                                            drillModelData$month <= opt$tstop)]

# Actual total oil/gas production as timeseries...
all.p <- sqldf("select p_rpt_period, sum(p_oil_prod), sum(p_gas_prod)
              from p
              group by p_rpt_period")
names(all.p) <- c("date", "oil", "gas")
all.p <- all.p[which(all.p$date >= opt$tstart & all.p$date <= opt$tstop),]

# ...and just from new wells
temp <- subset(p,
               subset = (h_first_prod >= opt$tstart),
               select = c("p_rpt_period", "p_oil_prod", "p_gas_prod"))
new.p <- sqldf("select p_rpt_period, sum(p_oil_prod), sum(p_gas_prod)
              from temp
              group by p_rpt_period")
remove(temp)
names(new.p) <- c("date", "oil", "gas")
new.p <- new.p[which(new.p$date >= opt$tstart & new.p$date <= opt$tstop),]

# ...and just from existing wells
prior.p <- data.frame(date = all.p$date,
                      oil = (all.p$oil-new.p$oil),
                      gas = (all.p$gas-new.p$gas))

# ---Quantiles---

# Predefine quantile matrices
Drilled.q <- matrix(0, nrow = length(opt$quant), ncol = length(opt$tsteps))
oil.q <-     Drilled.q
gas.q <-     Drilled.q
poil.q <-    Drilled.q
pgas.q <-    Drilled.q
op.q <-      Drilled.q
gp.q <-      Drilled.q
CO2.q <-     Drilled.q
CH4.q <-     Drilled.q
VOC.q <-     Drilled.q
rCO2.q <-    Drilled.q
rCH4.q <-    Drilled.q
rVOC.q <-    Drilled.q
w.pw.q <-    Drilled.q
w.disp.q <-  Drilled.q
w.evap.q <-  Drilled.q
w.rec.q <-   Drilled.q
w.dw.q <-    Drilled.q
w.fw.q <-    Drilled.q
w.inj.q <-   Drilled.q
w.in.q <-    Drilled.q
w.r.q <-     Drilled.q

# For each timestep, get quantiles
for (i in 1:ncol(Drilled.q)) {
  Drilled.q[,i] <- quantile(Drilled[,i], opt$quant)
  oil.q[,i] <-     quantile(osim[,i],    opt$quant)
  gas.q[,i] <-     quantile(gsim[,i],    opt$quant)
  poil.q[,i] <-    quantile(posim[,i],   opt$quant)
  pgas.q[,i] <-    quantile(pgsim[,i],   opt$quant)
  op.q[,i] <-      quantile(op[,i],      opt$quant)
  gp.q[,i] <-      quantile(gp[,i],      opt$quant)
  CO2.q[,i] <-     quantile(CO2[,i],     opt$quant)
  CH4.q[,i] <-     quantile(CH4[,i],     opt$quant)
  VOC.q[,i] <-     quantile(VOC[,i],     opt$quant)
  rCO2.q[,i] <-    quantile(rCO2[,i],    opt$quant)
  rCH4.q[,i] <-    quantile(rCH4[,i],    opt$quant)
  rVOC.q[,i] <-    quantile(rVOC[,i],    opt$quant)
  w.pw.q[,i] <-    quantile(w.pw[,i],    opt$quant)
  w.disp.q[,i] <-  quantile(w.disp[,i],  opt$quant)
  w.evap.q[,i] <-  quantile(w.evap[,i],  opt$quant)
  w.rec.q[,i] <-   quantile(w.rec[,i],   opt$quant)
  w.dw.q[,i] <-    quantile(w.dw[,i],    opt$quant)
  w.fw.q[,i] <-    quantile(w.fw[,i],    opt$quant)
  w.inj.q[,i] <-   quantile(w.inj[,i],   opt$quant)
  w.in.q[,i] <-    quantile(w.in[,i],    opt$quant)
  w.r.q[,i] <-     quantile(w.r[,i],     opt$quant)
}


# Global plotting options -------------------------------------------------

# Set line colors for quantiles used in quant
linecolor <- rainbow(length(opt$quant))

# Set line colors for fields
flinecolor <- rainbow(length(field))

# Plot counter
j <- 1

# Oil prices simulated vs actual ------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, op.q[1,],
       ylim = c(0.9*min(c(min(op.q), min(ep.act$OP))), 1.1*max(c(max(op.q), max(ep.act$OP)))),
       type = "l",
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = paste("Oil First Purchase Price (", opt$cpiDate, " $/bbl)", sep = ""),
       main = "Oil Price - Simulation vs. Actual")
  
  # All the other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, op.q[i,], col = linecolor[i])}
  
  # Add the line for the actual value
  lines(opt$tsteps, ep.act$OP)
  
  # Add the forecast line
  lines(opt$tsteps, op.FC, lty = 2)
  
  # Add legend
  legend("topleft",
         c("Actual", "Forecast", "90%", "70%", "50%", "30%", "10%"),
         ncol = 2,
         col = c("black", "black", linecolor),
         lty = c(1, 2, rep(1, times = 5)))
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Gas prices simulated vs actual ------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, gp.q[1,],
       type = "l",
       ylim = c(0.9*min(c(min(gp.q), min(ep.act$GP))), 1.1*max(c(max(gp.q), max(ep.act$GP)))),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = paste("Gas First Purchase Price (", opt$cpiDate, " $/MCF)", sep = ""),
       main = "Gas Price - Simulation vs. Actual")
  
  # All the other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, gp.q[i,], col = linecolor[i])}
  
  # Add the line for the actual value
  lines(opt$tsteps, ep.act$GP)
  
  # Add the forecast line
  lines(opt$tsteps, gp.FC, lty = 2)
  
  # Add legend
  legend("topleft",
         c("Actual", "Forecast", "90%", "70%", "50%", "30%", "10%"),
         ncol = 2,
         col = c("black", "black", linecolor),
         lty = c(1, 2, rep(1, times = 5)))
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Drilling schedule simulated vs actual -----------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, Drilled.q[1,],
       type = "l",
       ylim = c(0, 1.1*max(c(max(Drilled.q),max(Drilled.act)))),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "Wells Drilled",
       main = "Drilling Schedule - Simulation vs. Actual")
  
  # All the other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, Drilled.q[i,], col = linecolor[i])}
  
  # Add the line for the actual value
  lines(opt$tsteps, Drilled.act)
  
  # Add legend
  legend("topleft", c("Actual", "90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c("black", linecolor), lty = 1)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Drilling fit vs actual --------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
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
  ind <- which(d$month >= opt$DMU.tstart &
               d$month <= opt$DMU.tstop)
  
  # Main plot for training fit
  plot(d$month[ind], d$wells[ind],
       lwd = 2,
       type = "l",
       xlab = "Time (months)",
       ylab = "Total Wells Drilled (oil, gas, or dry)",
       main = "Drilling Schedule Models")
  
  # Add line for model fit results
  lines(d$month[ind],
        PWM(OP = d$OP[ind], GP = d$GP[ind], par = drillModel$pwm, init = d$prior[ind[1]]),
        col = "red", lty = 1)
  lines(d$month[ind], fitted(drillModel$epm), col = "blue", lty = 1)
  lines(d$month[ind], fitted(drillModel$opm), col = "green", lty = 1)
  lines(d$month[ind], fitted(drillModel$gpm), col = "purple", lty = 1)
  
  # Legend
  legend("topleft", c("Actual", "PWM", "EPM", "OPM", "GPM"), lty = c(1,rep(1,4)),
         lwd = c(2, rep(1,4)), col = c("black", "red", "blue", "green", "purple"))
  
  # Temporary time index
  ind <- which(d$month >= opt$tstart &
               d$month <= opt$tstop)
  
  # Main plot for cross-validation
  plot(d$month[ind], d$wells[ind],
       ylim = c(0, 100),
       lwd = 2,
       type = "l",
       xlab = "Time (months)",
       ylab = "Total Wells Drilled (oil, gas, or dry)",
       main = "Cross-Validation of Drilling Schedule Models")
  
  # Add line for model predictions
  lines(d$month[ind],
        PWM(OP = d$OP[ind], GP = d$GP[ind], par = drillModel$pwm, init = d$prior[ind[1]]),
        col = "red", lty = 1)
  lines(d$month[ind],
        EPM(fit = coefficients(drillModel$epm), OP = d$OP[ind-1], GP = d$GP[ind-1]),
        col = "blue", lty = 1)
  lines(d$month[ind],
        OPM(fit = coefficients(drillModel$opm), OP = d$OP[ind-1]),
        col = "green", lty = 1)
  lines(d$month[ind],
        GPM(fit = coefficients(drillModel$gpm), GP = d$GP[ind-1]),
        col = "purple", lty = 1)
  
  # Legend
  legend("bottomleft", c("Actual", "PWM", "EPM", "OPM", "GPM"), lty = c(1,rep(1,4)),
         lwd = c(2, rep(1,4)), col = c("black", "red", "blue", "green", "purple"), ncol = 2, cex = 1/opt$defFontSize)
  
  # Remove everything
  remove(PWM, EPM, OPM, GPM, d, ind)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Boxplots of decline curve coefficients by field -------------------------
if(opt$plist$plot[j] == TRUE) {
  
# Plots below are broken because of dynamic field selection in scheduleUpdate - fix me
#   # If exporting to PDF
#   if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
#   
#   # Source hyperbolic and cumulative boxplot functions
#   source(file.path(path$plotfun, "bplotHypDCAcoef.R"))
#   source(file.path(path$plotfun, "bplotQfitDCAcoef.R"))
#   
#   # Run plotting functions
#   bplotHypDCAcoef()
#   bplotQfitDCAcoef()  
#   
#   # If exporting to PDF, close PDF
#   if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# CDF for decline curve coefficients --------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Source hyperbolic and cumulative boxplot functions
  source(file.path(path$plotfun, "cdfDCAcoef.R"))
  
  # Run plotting functions
  cdfDCAcoef(hyp = F, Qfit = T)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Total oil production simulated vs actual --------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, oil.q[1,]+poil.q[1,],
       type = "l",
       ylim = c(min(c(min(oil.q+poil.q),min(all.p$oil))),
                max(c(max(oil.q+poil.q),max(all.p$oil)))),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "Oil Production (bbl)",
       main = "Oil Production - Simulated vs. Actual")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, oil.q[i,]+poil.q[i,], col = linecolor[i])}
  
  # Actual oil production
  lines(opt$tsteps, all.p$oil)
  
  # Legend
  legend("topleft", c("Actual", "90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c("black", linecolor), lty = 1)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Total oil production from new wells simulated vs actual -----------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, oil.q[1,],
       type = "l",
       ylim = c(min(c(min(oil.q),min(new.p$oil))),
                max(c(max(oil.q),max(new.p$oil)))),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "Oil Production (bbl)",
       main = "Oil Production from New Wells - Simulated vs. Actual")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, oil.q[i,], col = linecolor[i])}
  
  # Actual oil production
  lines(opt$tsteps, new.p$oil)
  
  # Legend
  legend("topleft", c("Actual", "90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c("black", linecolor), lty = 1)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Total oil production from existing wells simulated vs actual ------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, poil.q[1,],
       type = "l",
       ylim = c(min(c(min(poil.q),min(prior.p$oil))),
                max(c(max(poil.q),max(prior.p$oil)))),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "Oil Production (bbl)",
       main = "Oil Production from Existing Wells - Simulated vs. Actual")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, poil.q[i,], col = linecolor[i])}
  
  # Actual oil production
  lines(opt$tsteps, prior.p$oil)
  
  # Legend
  legend("topright", c("Actual", "90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c("black", linecolor), lty = 1)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Total gas production simulated vs actual --------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, gas.q[1,]+pgas.q[1,],
       type = "l",
       ylim = c(min(c(min(gas.q+pgas.q),min(all.p$gas))),
                max(c(max(gas.q+pgas.q),max(all.p$gas)))),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "Gas Production (MCF)",
       main = "Gas Production - Simulated vs. Actual")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, gas.q[i,]+pgas.q[i,], col = linecolor[i])}
  
  # Actual gas production
  lines(opt$tsteps, all.p$gas)
  
  # Legend
  legend("topleft", c("Actual", "90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c("black", linecolor), lty = 1)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Total gas production from new wells simulated vs actual -----------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, gas.q[1,],
       type = "l",
       ylim = c(min(c(min(gas.q),min(new.p$gas))),
                max(c(max(gas.q),max(new.p$gas)))),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "Gas Production (MCF)",
       main = "Gas Production from New Wells - Simulated vs. Actual")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, gas.q[i,], col = linecolor[i])}
  
  # Actual gas production
  lines(opt$tsteps, new.p$gas)
  
  # Legend
  legend("topleft", c("Actual", "90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c("black", linecolor), lty = 1)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Total gas production from existing wells simulated vs actual -------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, pgas.q[1,],
       type = "l",
       ylim = c(min(c(min(pgas.q),min(prior.p$gas))),
                max(c(max(pgas.q),max(prior.p$gas)))),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "Gas Production (MCF)",
       main = "Gas Production from Existing Wells - Simulated vs. Actual")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, pgas.q[i,], col = linecolor[i])}
  
  # Actual gas production
  lines(opt$tsteps, prior.p$gas)
  
  # Legend
  legend("topright", c("Actual", "90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c("black", linecolor), lty = 1)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# CO2e Emissions ----------------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, CO2.q[1,],
       type = "l",
       lty = 2,
       ylim = c(0.9*min(CO2.q),
                1.1*max(CO2.q)),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "CO2e Emissions (metric tons)",
       main = "Total CO2e Emissions")
  mtext("Solid Lines = Reduced Emissions, Dotted Lines = Base Emissions")
  lines(opt$tsteps, rCO2.q[1,], col = linecolor[1], lty = 1)
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {
    lines(opt$tsteps, CO2.q[i,], col = linecolor[i], lty = 2)
    lines(opt$tsteps, rCO2.q[i,], col = linecolor[i], lty = 1)
  }
  
  # Legend
  legend("topleft", c("90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c(linecolor), lty = 1)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# CH4 Emissions -----------------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, CH4.q[1,],
       type = "l",
       lty = 2,
       ylim = c(0.9*min(CH4.q),
                1.1*max(CH4.q)),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "CH4 Emissions (metric tons)",
       main = "Total CH4 Emissions")
  mtext("Solid Lines = Reduced Emissions, Dotted Lines = Base Emissions")
  lines(opt$tsteps, rCH4.q[1,], col = linecolor[1], lty = 1)
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {
    lines(opt$tsteps, CH4.q[i,], col = linecolor[i], lty = 2)
    lines(opt$tsteps, rCH4.q[i,], col = linecolor[i], lty = 1)
  }
  
  # Legend
  legend("topleft", c("90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c(linecolor), lty = 1)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# VOC Emissions -----------------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, VOC.q[1,],
       type = "l",
       lty = 2,
       ylim = c(0.9*min(VOC.q),
                1.1*max(VOC.q)),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "VOC Emissions (metric tons/month)",
       main = "Total VOC Emissions")
  mtext("Solid Lines = Reduced Emissions, Dotted Lines = Base Emissions")
  lines(opt$tsteps, rVOC.q[1,], col = linecolor[1], lty = 1)
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {
    lines(opt$tsteps, VOC.q[i,], col = linecolor[i], lty = 2)
    lines(opt$tsteps, rVOC.q[i,], col = linecolor[i], lty = 1)
  }
  
  # Legend
  legend("topleft", c("90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c(linecolor), lty = 1)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Field Fractions ---------------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Get well counts by field from well.actual
  fcount <- round(c(cdf.ff$CDF[1], diff(cdf.ff$CDF))*nrow(well.actual))
  
  # Main Bar Chart
  bp <- barplot(height = fcount,
                names.arg = as.character(cdf.ff$Field),
                #log = "y",
                #ylim = c(0, 3.5e3),
                ylab = "Well Count",
                xlab = "Field Number",
                main = "Well Counts by Field")
  
  # Add grid lines
  # abline(h = c(5, 10, 50, 100, 500, 1e3, 5e3), lty = 3, col = "grey")
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Field Fractions - OW ----------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Get well counts by field from well.actual
  fcount <- round(c(cdf.ff$CDF[1], diff(cdf.ff$CDF))*nrow(well.actual)*(1-prob$gas))
  
  # Main Bar Chart
  barplot(height = fcount,
          names.arg = as.character(cdf.ff$Field),
          #log = "y",
          #ylim = c(1, 5e3),
          ylab = "Well Count",
          xlab = "Field Number",
          main = "Oil Well Counts by Field")
  
  # Add grid lines
  #abline(h = c(5, 10, 50, 100, 500, 1e3, 5e3), lty = 3, col = "grey")
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Field Fractions - GW ----------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Get well counts by field from well.actual
  fcount <- round(c(cdf.ff$CDF[1], diff(cdf.ff$CDF))*nrow(well.actual)*prob$gas)
  
  # Since some fields have zero gas wells, make those fields NA
  fcount[which(fcount == 0)] <- NA
  
  # Main Bar Chart
  barplot(height = fcount,
          names.arg = as.character(cdf.ff$Field),
          #log = "y",
          #ylim = c(1, 5e3),
          ylab = "Well Count",
          xlab = "Field Number",
          main = "Gas Well Counts by Field")
  
  # Add grid lines
  #abline(h = c(5, 10, 50, 100, 500, 1e3, 5e3), lty = 3, col = "grey")
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Well Capital Cost -------------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot
  plot(cost ~ depth,
       data = drillCost.data,
       xlab = "Measured Well Depth (ft)",
       ylab = paste("Capital Cost (in", opt$cpiDate, "dollars)"),
       main = "Well Capital Cost Model")
  
  # Add line for model fit results
  lines(seq(0, 16e3, 100),
        exp(coef(drillCost.fit)["(Intercept)"]+coef(drillCost.fit)["depth"]*seq(0, 16e3, 100)),
        col = "red")
  
  # Add text line with equation
  mtext(expression(log(C)==a+b%.%D))
  
  # Legend
  legend("topleft", c("Actual", "Fit"), lty = c(NA,1), pch = c(1,NA), col = c("black","red"))
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Surface Lease Ownership -------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Build temporary data.frame with individual (not cumulative) probabilities
  # of each surface lease type by field and then transpose
  temp <- data.frame(cdf.flt[,2],
                     cdf.flt[,3]-cdf.flt[,2],
                     cdf.flt[,4]-cdf.flt[,3],
                     cdf.flt[,5]-cdf.flt[,4], row.names = cdf.flt[,1])
  names(temp) <- c("Federal", "Indian", "State", "Fee")
  temp <- t(as.matrix(temp))
  
  # Plot with barplot()
  barplot(
    temp,
    beside = TRUE,
    xlab = "Field Number",
    ylab = "Probability",
    main = "Surface Lease Ownership Type by Field",
    legend = c("Federal", "Indian", "State", "Fee"),
    args.legend = list(x = "topright", cex = 0.75)
  )
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Well Depth -------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main Plot
  plot(cdf.depth.ow,
       type = "l",
       col = "blue",
       xlab = "Well Depth (ft)",
       ylab = "Cumulative Probability",
       main = "CDFs for Well Depth by Well Type")
  
  # Line plot for GW
  lines(cdf.depth.gw, col = "red")
  
  # Legend
  legend = legend("topleft", c("Oil Wells", "Gas Wells"), lty = 1, col = c("blue","red"))
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Lease Capital & Operating Costs Fit ---------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Oil LOC equip plot
  ind <- which(LOC.data$wellType == "OW" & LOC.data$LOCtype == "equip")
  temp <-scatterplot3d(x = LOC.data$op[ind],
                       y = LOC.data$depth[ind]/1e3,
                       z = LOC.data$cost[ind]/1e3,
                       highlight.3d=TRUE,
                       xlim = c(0, 100),
                       ylim = c(0, 12),
                       zlim = c(0, 300),
                       xlab = paste("Oil price in", opt$cpiDate, "$/bbl"),
                       ylab = "Well Depth (1e3 ft)",
                       zlab = paste("Capital Cost in thousands of", opt$cpiDate, "USD"),
                       main="Lease Capital Cost Fit for Oil Wells")
  
  # Plane fit for oil LOC
  fitdata <- LOC.data[ind,]
  fitdata$cost <- fitdata$cost/1e3
  fitdata$depth <- fitdata$depth/1e3
  fitdata <- lm(cost~op+depth-1, data = fitdata)
  temp$plane3d(Intercept = 0,
               x.coef = coefficients(fitdata)[1],
               y.coef = coefficients(fitdata)[2])
  
  # Gas LOC equip plot - 250 MCFD gas production rate
  ind <- which(LOC.data$wellType == "GW" & LOC.data$LOCtype == "equip" & LOC.data$gasRate == 250)
  temp <-scatterplot3d(x = LOC.data$gp[ind],
                       y = LOC.data$depth[ind]/1e3,
                       z = LOC.data$cost[ind]/1e3,
                       highlight.3d=TRUE,
                       xlim = c(0, 10),
                       ylim = c(0, 12),
                       zlim = c(0, 150),
                       xlab = paste("Gas price in", opt$cpiDate, "$/MCF"),
                       ylab = "Well Depth (1e3 ft)",
                       zlab = paste("Capital Cost in thousands of", opt$cpiDate, "USD"),
                       main="Lease Capital Cost Fit for Gas Wells - Gas Production 250 MCFD")
  
  # Plane fit for oil LOC
  fitdata <- LOC.data[ind,]
  fitdata$cost <- fitdata$cost/1e3
  fitdata$depth <- fitdata$depth/1e3
  fitdata <- lm(cost~op+depth-1, data = fitdata)
  temp$plane3d(Intercept = 0,
               x.coef = coefficients(fitdata)[1],
               y.coef = coefficients(fitdata)[2])
  
  # Oil LOC op plot
  ind <- which(LOC.data$wellType == "OW" & LOC.data$LOCtype == "op")
  temp <-scatterplot3d(x = LOC.data$op[ind],
                       y = LOC.data$depth[ind]/1e3,
                       z = LOC.data$cost[ind]/1e3,
                       highlight.3d=TRUE,
                       xlim = c(0, 100),
                       ylim = c(0, 12),
                       zlim = c(0, 6),
                       xlab = paste("Oil price in", opt$cpiDate, "$/bbl"),
                       ylab = "Well Depth (1e3 ft)",
                       zlab = paste("Operating Cost in thousands of", opt$cpiDate, "USD per month"),
                       main="Lease Operating Cost Fit for Oil Wells")
  
  # Plane fit for oil LOC
  fitdata <- LOC.data[ind,]
  fitdata$cost <- fitdata$cost/1e3
  fitdata$depth <- fitdata$depth/1e3
  fitdata <- lm(cost~op+depth-1, data = fitdata)
  temp$plane3d(Intercept = 0,
               x.coef = coefficients(fitdata)[1],
               y.coef = coefficients(fitdata)[2])
  
  # Gas LOC op plot
  ind <- which(LOC.data$wellType == "GW" & LOC.data$LOCtype == "op" & LOC.data$gasRate == 250)
  temp <-scatterplot3d(x = LOC.data$gp[ind],
                       y = LOC.data$depth[ind]/1e3,
                       z = LOC.data$cost[ind]/1e3,
                       highlight.3d=TRUE,
                       xlim = c(0, 10),
                       ylim = c(0, 12),
                       zlim = c(0, 8),
                       xlab = paste("Gas price in", opt$cpiDate, "$/bbl"),
                       ylab = "Well Depth (1e3 ft)",
                       zlab = paste("Operating Cost in thousands of", opt$cpiDate, "USD per month"),
                       main="Lease Operating Cost Fit for Gas Wells - Gas Production 250 MCFD")
  
  # Plane fit for oil LOC
  fitdata <- LOC.data[ind,]
  fitdata$cost <- fitdata$cost/1e3
  fitdata$depth <- fitdata$depth/1e3
  fitdata <- lm(cost~op+depth-1, data = fitdata)
  temp$plane3d(Intercept = 0,
               x.coef = coefficients(fitdata)[1],
               y.coef = coefficients(fitdata)[2])
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Energy FPP History -------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Oil FPP
  plot(eia.hp$month, eia.hp$OP,
       type = "l",
       xlab = "Date - July 1978 to Nov. 2014 (by month)",
       ylab = paste("Price in", opt$cpiDate, "$/bbl"),
       main = "Utah Oil First Purchase Price History")
  
  # Gas FPP
  plot(eia.hp$month, eia.hp$GP,
       type = "l",
       xlab = "Date - July 1978 to Nov. 2014 (by month)",
       ylab = paste("Price in", opt$cpiDate, "$/MCF"),
       main = "Utah Gas First Purchase Price History")
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# NTI CDF ---------------------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Plot
  plot(x = qnorm(p = opt$xq, mean = corpNTIfrac["mean"], sd = corpNTIfrac["sd"]),
       y = opt$xq,
       type = "l",
       xlab = "NTI Revenue Fraction (dimensionless)",
       ylab = "Cumulative Probability",
       main = "CDF for NTI as Fraction of Revenue")
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Property Tax CDF ---------------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Plot
  plot(x = qnorm(p = opt$xq, mean = pTaxRate["mean"], sd = pTaxRate["sd"]),
       y = opt$xq,
       type = "l",
       xlab = "Property Tax Revenue Fraction (dimensionless)",
       ylab = "Cumulative Probability",
       main = "CDF for Property Taxes as Fraction of Revenue")
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# EIA AEO Error CDFs -------------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # CDF for % error in oil  
  # Line colors
  elinecolor <- rainbow(ncol(Eoil)/12)
  
  # Main plot
  plot(Eoil[,12], opt$xq,
       type = "l",
       col = elinecolor[1],
       xlim = c(1.1*min(Eoil), 1.1*max(Eoil)),
       ylim = c(0, 1),
       xlab = "% Error",
       ylab = "Cumulative Probability",
       main = "CDF of Relative % Error of EIA Oil Price Forecasts")
  
  # For all other timesteps
  for (i in seq(from = 24, to = ncol(Eoil), by = 12)) {
    lines(Eoil[,i], opt$xq, col = elinecolor[i/12])
  }
  
  legend("topleft",
         c("Y1", "Y2", "Y3", "Y4", "Y5"),
         ncol = 2, lty = 1, col = elinecolor)
  
  # Main plot for gas
  plot(Egas[,12], opt$xq,
       type = "l",
       col = elinecolor[1],
       xlim = c(1.1*min(Egas), 1.1*max(Egas)),
       ylim = c(0, 1),
       xlab = "% Error",
       ylab = "Cumulative Probability",
       main = "CDF of Relative % Error of EIA Gas Price Forecasts")
  
  # For all other timesteps
  for (i in seq(from = 24, to = ncol(Egas), by = 12)) {
    lines(Egas[,i], opt$xq, col = elinecolor[i/12])
  }
  
  legend("topleft",
         c("Y1", "Y2", "Y3", "Y4", "Y5"),
         ncol = 2, lty = 1, col = elinecolor)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Water balance terms CDFs and regression models ----------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # CDF of produced water from oil wells
  plot(cdf ~ pw.oil, data = cdf.water,
       type = "l",
       xlab = "Ratio of (Produced Water) / (Oil Production)",
       ylab = "Cumulative Probability",
       main = "CDF of Produced Water Ratio for Oil Wells")
  
  # CDF of produced water from gas wells
  plot(cdf ~ pw.gas, data = cdf.water,
       type = "l",
       xlab = "Ratio of (Produced Water) / (Gas Production)",
       ylab = "Cumulative Probability",
       main = "CDF of Produced Water Ratio for Gas Wells")
  
  # CDF of disposal water vs produced water
  plot(cdf ~ disp, data = cdf.water,
       type = "l",
       xlab = "Ratio of (Disposal Well Water) / (Produced Water)",
       ylab = "Cumulative Probability",
       main = "CDF of Disposal Well Water Ratio")
  
  # CDF of evaporation water vs produced water
  plot(cdf ~ evap, data = cdf.water,
       type = "l",
       xlab = "Ratio of (Evaporation Pond Water) / (Produced Water)",
       ylab = "Cumulative Probability",
       main = "CDF of Evaporation Pond Water Ratio")
  
  # CDF of fracking water by well type
  plot(cdf ~ fw.ow, data = cdf.water,
       col = "blue",
       type = "l",
       xlab = "Hydraulic Fracturing Water Usage (bbl)",
       ylab = "Cumulative Probability",
       main = "CDF of Hydraulic Fracturing Water Usage by Well Type")
  
  # Add line for gas wells
  lines(cdf~fw.gw, data = cdf.water, col = "red")
  
  # Add legend
  legend("bottomright", c("Oil Wells", "Gas Wells"), lty = 1, col = c("blue", "red"))
  
  # CDF for flooding water vs oil production
  plot(cdf ~ inj, data = cdf.water,
       type = "l",
       xlab = "Ratio of (Flooding Water) / (Oil Production)",
       ylab = "Cumulative Probability",
       main = "CDF of Water Flooding Ratio")
  
  # Linear regression model for drilling water usage
  plot(water~depth, data = water.lm$dw.lm$model,
       xlab = "Well Depth (ft)",
       ylab = "Drilling Water Usage (bbl)",
       main = "Drilling (Mud and Cement) Water Usage Model")
  lines(water.lm$dw.lm$model$depth, fitted(water.lm$dw.lm), col = "blue")
  legend("topleft", c("Data", "Fit"), pch = c(1, NA), lty = c(NA, 1), col = c("black", "blue"))
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Water balance results ---------------------------------------------------

if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Produced water - Main plot with largest quantile result
  plot(opt$tsteps, w.pw.q[1,],
       type = "l",
       ylim = c(0.9*min(w.pw.q),
                1.1*max(w.pw.q)),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "Produced Water (bbl)",
       main = "Total Produced Water")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, w.pw.q[i,], col = linecolor[i])}
  
  # Legend
  legend("topleft", c("90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c(linecolor), lty = 1)
  
  # Disposal water - Main plot with largest quantile result
  plot(opt$tsteps, w.disp.q[1,],
       type = "l",
       ylim = c(0.9*min(w.disp.q),
                1.1*max(w.disp.q)),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "Disposal Well Water (bbl)",
       main = "Total Disposal Well Water")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, w.disp.q[i,], col = linecolor[i])}
  
  # Legend
  legend("topleft", c("90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c(linecolor), lty = 1)
  
  # Evaporation water - Main plot with largest quantile result
  plot(opt$tsteps, w.evap.q[1,],
       type = "l",
       ylim = c(0.9*min(w.evap.q),
                1.1*max(w.evap.q)),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "Water Evaporated in Ponds (bbl)",
       main = "Total Evaporation Pond Water")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, w.evap.q[i,], col = linecolor[i])}
  
  # Legend
  legend("topleft", c("90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c(linecolor), lty = 1)
  
  # Water recycle - Main plot with largest quantile result
  plot(opt$tsteps, w.rec.q[1,],
       type = "l",
       ylim = c(0.9*min(w.rec.q),
                1.1*max(w.rec.q)),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "Water Recycled (bbl)",
       main = "Total Water Recycle")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, w.rec.q[i,], col = linecolor[i])}
  
  # Legend
  legend("topleft", c("90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c(linecolor), lty = 1)
  
  # Drilling water - Main plot with largest quantile result
  plot(opt$tsteps, w.dw.q[1,],
       type = "l",
       ylim = c(0.9*min(w.dw.q),
                1.1*max(w.dw.q)),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "Drilling Water Usage (bbl)",
       main = "Total Drilling Water Usage")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, w.dw.q[i,], col = linecolor[i])}
  
  # Legend
  legend("topleft", c("90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c(linecolor), lty = 1)
  
  # Fracking water - Main plot with largest quantile result
  plot(opt$tsteps, w.fw.q[1,],
       type = "l",
       ylim = c(0.9*min(w.fw.q),
                1.1*max(w.fw.q)),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "Hydraulic Fracturing Water Usage (bbl)",
       main = "Total Hydraulic Fracturing Water Usage")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, w.fw.q[i,], col = linecolor[i])}
  
  # Legend
  legend("topleft", c("90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c(linecolor), lty = 1)
  
  # Water Flooding - Main plot with largest quantile result
  plot(opt$tsteps, w.inj.q[1,],
       type = "l",
       ylim = c(0.9*min(w.inj.q),
                1.1*max(w.inj.q)),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "Water Flooding (bbl)",
       main = "Total Water Flooding")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, w.inj.q[i,], col = linecolor[i])}
  
  # Legend
  legend("topleft", c("90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c(linecolor), lty = 1)
  
  # Water In - Main plot with largest quantile result
  plot(opt$tsteps, w.in.q[1,],
       type = "l",
       ylim = c(0.9*min(w.in.q),
                1.1*max(w.in.q)),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "Water In (bbl)",
       main = "Total Water In")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, w.in.q[i,], col = linecolor[i])}
  
  # Legend
  legend("topleft", c("90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c(linecolor), lty = 1)
  
  # Water Intensity Ratio - Main plot with largest quantile result
  plot(opt$tsteps, w.r.q[1,],
       type = "l",
       ylim = c(0.9*min(w.r.q),
                1.1*max(w.r.q)),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "Water Intensity Ratio (water_in / oil)",
       main = "Water Intensity Ratio")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, w.r.q[i,], col = linecolor[i])}
  
  # Legend
  legend("topright", c("90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c(linecolor), lty = 1)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# CDF Well Reworks --------------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot - CDF for oil
  plot(oil~month, cdf.rework,
       type = "l",
       col = "blue",
       ylim = c(0, 1),
       xlab = "Time Since Drilling Completion (months)",
       ylab = "Cumulative Probability",
       main = "CDF for Well Reworks as f(time)")
  
  # CDF for gas
  lines(gas~month, cdf.rework, col = "red")
  
  # Legend
  legend("topleft", c("Oil Wells", "Gas Wells"), lty = 1, col = c("blue", "red"))
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# EIA AEO Relative Error --------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Get data for plot
  din <- read.csv(file.path(path$raw, "EIA_op_error_export.csv"))
  
  # Restructure
  din <- rbind(data.frame(year = "Year 1", err = din[,1]),
               data.frame(year = "Year 2", err = din[,2]),
               data.frame(year = "Year 3", err = din[,3]),
               data.frame(year = "Year 4", err = din[,4]),
               data.frame(year = "Year 5", err = din[,5]))
  
  # Main plot - oil
  boxplot(err~year, din,
          range = 0,
          ylim = c(-250, 50),
          ylab = "Relative Error (%)",
          main = "Error in EIA Oil Price Forecasts")
  
  # Repeat for gas
  din <- read.csv(file.path(path$raw, "EIA_gp_error_export.csv"))
  
  # Restructure
  din <- rbind(data.frame(year = "Year 1", err = din[,1]),
               data.frame(year = "Year 2", err = din[,2]),
               data.frame(year = "Year 3", err = din[,3]),
               data.frame(year = "Year 4", err = din[,4]),
               data.frame(year = "Year 5", err = din[,5]))
  
  # Main plot - oil
  boxplot(err~year, din,
          range = 0,
          ylim = c(-150, 100),
          ylab = "Relative Error (%)",
          main = "Error in EIA Gas Price Forecasts")
  
  # Remove data
  remove(din)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1
