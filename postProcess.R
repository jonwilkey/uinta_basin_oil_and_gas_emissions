# Script Info -------------------------------------------------------------
# Name:      postProcess.R (Post processing and plot function)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Description -------------------------------------------------------------

# This script generates plots of the results from the Monte-Carlo simulation.


# Queries and Calculations ------------------------------------------------

# If cross-validating
if(opt$crossvalid == T) {
  
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
}


# Quantiles ---------------------------------------------------------------

# Predefine quantile matrices
Drilled.q <- matrix(0, nrow = length(opt$quant), ncol = length(opt$tsteps))
oil.q <-     Drilled.q
gas.q <-     Drilled.q
poil.q <-    Drilled.q
pgas.q <-    Drilled.q
op.q <-      Drilled.q
gp.q <-      Drilled.q

# Activity-based emissions
aE$CO2.q <-  Drilled.q
aE$CH4.q <-  Drilled.q
aE$VOC.q <-  Drilled.q
aE$rCO2.q <- Drilled.q
aE$rCH4.q <- Drilled.q
aE$rVOC.q <- Drilled.q

# Equipment-based emissions
eE$pm10.q <- Drilled.q
eE$pm25.q <- Drilled.q
eE$sox.q <-  Drilled.q
eE$nox.q <-  Drilled.q
eE$voc.q <-  Drilled.q
eE$co.q <-   Drilled.q
eE$ch2o.q <- Drilled.q

# For each timestep, get quantiles
for (i in 1:ncol(Drilled.q)) {
  Drilled.q[, i] <- quantile(Drilled[, i], opt$quant)
  oil.q    [, i] <- quantile(osim   [, i], opt$quant)
  gas.q    [, i] <- quantile(gsim   [, i], opt$quant)
  poil.q   [, i] <- quantile(posim  [, i], opt$quant)
  pgas.q   [, i] <- quantile(pgsim  [, i], opt$quant)
  op.q     [, i] <- quantile(op     [, i], opt$quant)
  gp.q     [, i] <- quantile(gp     [, i], opt$quant)
  
  # Activity-based emissions
  aE$CO2.q [, i] <- quantile(aE$CO2 [, i], opt$quant)
  aE$CH4.q [, i] <- quantile(aE$CH4 [, i], opt$quant)
  aE$VOC.q [, i] <- quantile(aE$VOC [, i], opt$quant)
  aE$rCO2.q[, i] <- quantile(aE$rCO2[, i], opt$quant)
  aE$rCH4.q[, i] <- quantile(aE$rCH4[, i], opt$quant)
  aE$rVOC.q[, i] <- quantile(aE$rVOC[, i], opt$quant)
  
  # Equipment-based emissions
  eE$pm10.q[, i] <- quantile(eE$pm10[, i], opt$quant)
  eE$pm25.q[, i] <- quantile(eE$pm25[, i], opt$quant)
  eE$sox.q [, i] <- quantile(eE$sox [, i], opt$quant)
  eE$nox.q [, i] <- quantile(eE$nox [, i], opt$quant)
  eE$voc.q [, i] <- quantile(eE$voc [, i], opt$quant)
  eE$co.q  [, i] <- quantile(eE$co  [, i], opt$quant)
  eE$ch2o.q[, i] <- quantile(eE$ch2o[, i], opt$quant)
}


# Global plotting options -------------------------------------------------

# Set line colors for quantiles used in quant
qlinecolor <- gray(1:5/7) # gray(1:5/7)  #rep("grey", length(opt$quant))  #rainbow(length(opt$quant))
qlinetype <-  2:6#rep(1,5)#2:6
qlinewidth <- rep(2,5)

# Set line options for actual lines (for cross-validation plots)
alinecolor <- "black"
alinetype <-  1
alinewidth <- 3#2

# Set line options for forecast lines (for price plots)
forlinecolor <- c("grey50", "grey75", "grey25")
forlinetype <-  c(2,2,2)
forlinewidth <- c(1,1,1)

# Plot counter
j <- 1


# 01 - Oil prices simulated vs actual -------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, op.q[1,],
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
    lines(opt$tsteps, op.q[i,], col = qlinecolor[i], lty = qlinetype[i], lwd = qlinewidth[i])
  }
  
  # Add the forecast line
  lines(opt$tsteps, op.FC.ref,  col = forlinecolor[1], lty = forlinetype[1], lwd = forlinewidth[1])
  lines(opt$tsteps, op.FC.low,  col = forlinecolor[2], lty = forlinetype[2], lwd = forlinewidth[2])
  lines(opt$tsteps, op.FC.high, col = forlinecolor[3], lty = forlinetype[3], lwd = forlinewidth[3])
  
  if(opt$crossvalid == T) {
    
    # Add the line for the actual value
    lines(opt$tsteps, ep.act$OP, col = alinecolor, lty = alinetype, lwd = alinewidth)
    
    # Add legend
    legend("topleft",
           c("Actual", "EIA Ref.", "EIA Low", "EIA High", "90%", "70%", "50%", "30%", "10%"),
           ncol = 2,
           bg = "white",
           col = c(alinecolor, forlinecolor, qlinecolor),
           lwd = c(alinewidth, forlinewidth, qlinewidth),
           lty = c(alinetype,  forlinetype,  qlinetype))
  } else {
    
    # Add legend
    legend("topleft",
           c("EIA Ref.", "EIA Low", "EIA High", "90%", "70%", "50%", "30%", "10%"),
           ncol = 2,
           bg = "white",
           col = c(forlinecolor, qlinecolor),
           lwd = c(forlinewidth, qlinewidth),
           lty = c(forlinetype,  qlinetype))
  }
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 02 - Gas prices simulated vs actual -------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # What term to use as lower ylim value depends on whether or not
  # cross-validation is occurring
  if(opt$crossvalid == T) {
    
    # Main plot with largest quantile result using ylim on actual price
    plot(opt$tsteps, gp.q[1,],
         ylim = c(0.9*min(ep.act$GP), 1.1*max(ep.act$GP)),
         type = "l",
         lty = qlinetype[1],
         lwd = qlinewidth[1],
         col = qlinecolor[1],
         xlab = "Time (months)",
         ylab = paste("Gas First Purchase Price (", opt$cpiDate, " $/MCF)", sep = ""))#,
         #main = "Gas Price")
    
  } else {
    
    # Main plot with largest quantile result using ylim on simulated price
    plot(opt$tsteps, gp.q[1,],
         ylim = c(0.9*min(gp.q), 1.1*max(gp.q)),
         type = "l",
         lty = qlinetype[1],
         lwd = qlinewidth[1],
         col = qlinecolor[1],
         xlab = "Time (months)",
         ylab = paste("Gas First Purchase Price (", opt$cpiDate, " $/MCF)", sep = ""))#,
         #main = "Gas Price")
  }
  
  # All the other quantile lines
  for (i in 2:length(opt$quant)) {
    lines(opt$tsteps, gp.q[i,], col = qlinecolor[i], lty = qlinetype[i], lwd = qlinewidth[i])
  }
  
  # Add the forecast line
  lines(opt$tsteps, gp.FC.ref,  col = forlinecolor[1], lty = forlinetype[1], lwd = forlinewidth[1])
  lines(opt$tsteps, gp.FC.low,  col = forlinecolor[2], lty = forlinetype[2], lwd = forlinewidth[2])
  lines(opt$tsteps, gp.FC.high, col = forlinecolor[3], lty = forlinetype[3], lwd = forlinewidth[3])
  
  if(opt$crossvalid == T) {
    
    # Add the line for the actual value
    lines(opt$tsteps, ep.act$GP, col = alinecolor, lty = alinetype, lwd = alinewidth)
    
    # Add legend
    legend("topleft",
           c("Actual", "EIA Ref.", "EIA Low", "EIA High", "90%", "70%", "50%", "30%", "10%"),
           bg = "white",
           ncol = 2,
           col = c(alinecolor, forlinecolor, qlinecolor),
           lwd = c(alinewidth, forlinewidth, qlinewidth),
           lty = c(alinetype,  forlinetype,  qlinetype))
  } else {
    
    # Add legend
    legend("topleft",
           c("EIA Ref.", "EIA Low", "EIA High", "90%", "70%", "50%", "30%", "10%"),
           ncol = 2,
           bg = "white",
           col = c(forlinecolor, qlinecolor),
           lwd = c(forlinewidth, qlinewidth),
           lty = c(forlinetype,  qlinetype))
  }
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 03 - Drilling schedule simulated vs actual ------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, Drilled.q[1,],
       type = "l",
       ylim = c(0, 1.1*max(Drilled.q)),
       col = qlinecolor[1],
       lty = qlinetype[1],
       lwd = qlinewidth[1],
       xlab = "Time (months)",
       ylab = "Wells Drilled")#,
       #main = "Drilling Schedule")
  
  # All the other quantile lines
  for (i in 2:length(opt$quant)) {
    lines(opt$tsteps, Drilled.q[i,], col = qlinecolor[i], lty = qlinetype[i], lwd = qlinewidth[i])
  }
  
  if(opt$crossvalid == T) {
    
    # Add the line for the actual value
    lines(opt$tsteps, Drilled.act, col = alinecolor, lty = alinetype, lwd = alinewidth)
    
    # Add legend
    legend("topleft",
           c("Actual", "90%", "70%", "50%", "30%", "10%"),
           ncol = 2,
           col = c(alinecolor, qlinecolor),
           lwd = c(alinewidth, qlinewidth),
           lty = c(alinetype,  qlinetype))
  } else {
    
    # Add legend
    legend("topleft",
           c("90%", "70%", "50%", "30%", "10%"),
           ncol = 2,
           col = qlinecolor,
           lwd = qlinewidth,
           lty = qlinetype)
  }
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 04 - Drilling fit vs actual ---------------------------------------------
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
  
  # 2x2 multiplot
  par(mfrow = c(2,2),
      oma = c(5,4,0,0) + 0.1,
      mar = c(0,0,3,3) + 0.1)
  
  # names for character switch
  char.switch <- c("Eq. (7)", "Eq. (8)", "Eq. (9)", "Eq. (10)")
  
  for (k in 1:4) {
    
    # Main plot for training fit
    plot(d$month[ind], d$wells[ind],
         lwd =  2,
         col =  alinecolor,
         lty =  alinetype,
         type = "l",
         xlab = "Time (months)",
         ylab = "Wells Drilled")#,
    #main = "Drilling Schedule Models")
    
    #Cswitch <- 
    
    # Add line for model fit results
    switch(char.switch[k],
           
           "Eq. (7)" =  lines(d$month[ind],
                              PWM(OP = d$OP[ind], GP = d$GP[ind], par = drillModel$pwm, init = d$prior[ind[1]]),
                              col = "grey", lwd = 2, lty = 1),
           "Eq. (8)" =  lines(d$month[ind], fitted(drillModel$epm), col = "grey", lwd = 2, lty = 1),
           "Eq. (9)" =  lines(d$month[ind], fitted(drillModel$opm), col = "grey", lwd = 2, lty = 1),
           "Eq. (10)" = lines(d$month[ind], fitted(drillModel$gpm), col = "grey", lwd = 2, lty = 1))
    
    # Legend
    legend("topleft",
           c("Actual", char.switch[k]),
           lty = c(alinetype,  1),
           lwd = c(alinewidth, 2),
           col = c(alinecolor, "grey"))
  }
  
  title(xlab = "Time (months)",
        ylab = "Wells Drilled",
        outer = TRUE, line = 3)
  
  # Remove everything
  remove(PWM, EPM, OPM, GPM, d, ind, char.switch, k)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 05 - Drilling fit cross-validation --------------------------------------
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
  ind <- which(d$month >= opt$tstart &
                 d$month <= opt$tstop)
  
  # Main plot for cross-validation
  plot(d$month[ind], d$wells[ind],
       ylim = c(0, 100),
       lwd =  alinewidth,
       col =  alinecolor,
       lty =  alinetype,
       type = "l",
       xlab = "Time (months)",
       ylab = "Wells Drilled")#,
  #main = "Cross-Validation of Drilling Schedule Models")
  
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
         lty = c(alinetype,  qlinetype[1:4]),
         lwd = c(alinewidth, qlinewidth[1:4]),
         col = c(alinecolor, qlinecolor[1:4]),
         ncol = 3, cex = 1/opt$defFontSize)
  
  # Remove everything
  remove(PWM, EPM, OPM, GPM, d, ind)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 06 - Boxplots of decline curve coefficients by field --------------------
if(opt$plist$plot[j] == TRUE) {

  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Source hyperbolic and cumulative boxplot functions
  source(file.path(path$plotfun, "bplotHypDCAcoef.R"))
  source(file.path(path$plotfun, "bplotQfitDCAcoef.R"))
  
  # Run plotting functions
  bplotHypDCAcoef()
  bplotQfitDCAcoef()
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 07 - CDF for decline curve coefficients ---------------------------------
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


# 08 - otal oil production simulated vs actual ----------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, oil.q[1,]+poil.q[1,],
       type = "l",
       ylim = c(0.9*min(oil.q+poil.q), 1.1*max(oil.q+poil.q)),
       col = qlinecolor[1],
       lty = qlinetype[1],
       lwd = qlinewidth[1],
       xlab = "Time (months)",
       ylab = "Oil Production (bbl)")#,
       #main = "Total Oil Production")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {
    lines(opt$tsteps, oil.q[i,]+poil.q[i,], col = qlinecolor[i], lty = qlinetype[i], lwd = qlinewidth[i])
    }
  
  if(opt$crossvalid == T) {
    
    # Actual oil production
    lines(opt$tsteps, all.p$oil, col = alinecolor, lty = alinetype, lwd = alinewidth)
    
    # Legend
    legend("topleft",
           c("Actual", "90%", "70%", "50%", "30%", "10%"),
           ncol = 2,
           col = c(alinecolor, qlinecolor),
           lwd = c(alinewidth, qlinewidth),
           lty = c(alinetype,  qlinetype))
  } else {
    
    # Legend
    legend("topleft",
           c("90%", "70%", "50%", "30%", "10%"),
           ncol = 2,
           col = qlinecolor,
           lwd = qlinewidth,
           lty = qlinetype)
  }
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 09 - Total oil production from new wells simulated vs actual ------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, oil.q[1,],
       type = "l",
       ylim = c(0.9*min(oil.q), 1.1*max(oil.q)),
       col = qlinecolor[1],
       lty = qlinetype[1],
       lwd = qlinewidth[1],
       xlab = "Time (months)",
       ylab = "Oil Production (bbl)")#,
       #main = "Oil Production from New Wells")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {
    lines(opt$tsteps, oil.q[i,], col = qlinecolor[i], lty = qlinetype[i], lwd = qlinewidth[i])
    }
  
  if(opt$crossvalid == T) {
    
    # Actual oil production
    lines(opt$tsteps, new.p$oil, col = alinecolor, lty = alinetype, lwd = alinewidth)
    
    # Legend
    legend("topleft",
           c("Actual", "90%", "70%", "50%", "30%", "10%"),
           ncol = 2,
           col = c(alinecolor, qlinecolor),
           lwd = c(alinewidth, qlinewidth),
           lty = c(alinetype,  qlinetype))
  } else {
    
    # Legend
    legend("topleft",
           c("90%", "70%", "50%", "30%", "10%"),
           ncol = 2,
           col = qlinecolor,
           lwd = qlinewidth,
           lty = qlinetype)
  }
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 10 - Total oil production from existing wells simulated vs actual -------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, poil.q[1,],
       type = "l",
       ylim = c(0.9*min(poil.q), 1.1*max(poil.q)),
       col = qlinecolor[1],
       lty = qlinetype[1],
       lwd = qlinewidth[1],
       xlab = "Time (months)",
       ylab = "Oil Production (bbl)")#,
       #main = "Oil Production from Existing Wells")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {
    lines(opt$tsteps, poil.q[i,], col = qlinecolor[i], lty = qlinetype[i], lwd = qlinewidth[i])
    }
  
  if(opt$crossvalid == T) {
    
    # Actual oil production
    lines(opt$tsteps, prior.p$oil, col = alinecolor, lty = alinetype, lwd = alinewidth)
    
    # Legend
    legend("topright",
           c("Actual", "90%", "70%", "50%", "30%", "10%"),
           ncol = 2,
           col = c(alinecolor, qlinecolor),
           lwd = c(alinewidth, qlinewidth),
           lty = c(alinetype,  qlinetype))
  } else {
    
    # Legend
    legend("topleft",
           c("90%", "70%", "50%", "30%", "10%"),
           ncol = 2,
           col = qlinecolor,
           lwd = qlinewidth,
           lty = qlinetype)
  }
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 11 - Total gas production simulated vs actual ---------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, gas.q[1,]+pgas.q[1,],
       type = "l",
       ylim = c(0.9*min(gas.q+pgas.q), 1.1*max(gas.q+pgas.q)),
       col = qlinecolor[1],
       lty = qlinetype[1],
       lwd = qlinewidth[1],
       xlab = "Time (months)",
       ylab = "Gas Production (MCF)")#,
       #main = "Total Gas Production")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {
    lines(opt$tsteps, gas.q[i,]+pgas.q[i,], col = qlinecolor[i], lty = qlinetype[i], lwd = qlinewidth[i])
    }
  
  if(opt$crossvalid == T) {
    
    # Actual oil production
    lines(opt$tsteps, all.p$gas, col = alinecolor, lty = alinetype, lwd = alinewidth)
    
    # Legend
    legend("topleft",
           c("Actual", "90%", "70%", "50%", "30%", "10%"),
           ncol = 2,
           col = c(alinecolor, qlinecolor),
           lwd = c(alinewidth, qlinewidth),
           lty = c(alinetype,  qlinetype))
  } else {
    
    # Legend
    legend("topleft",
           c("90%", "70%", "50%", "30%", "10%"),
           ncol = 2,
           col = qlinecolor,
           lwd = qlinewidth,
           lty = qlinetype)
  }
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 12 - Total gas production from new wells simulated vs actual ------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, gas.q[1,],
       type = "l",
       ylim = c(0.9*min(gas.q), 1.1*max(gas.q)),
       col = qlinecolor[1],
       lty = qlinetype[1],
       lwd = qlinewidth[1],
       xlab = "Time (months)",
       ylab = "Gas Production (MCF)")#,
       #main = "Gas Production from New Wells")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {
    lines(opt$tsteps, gas.q[i,], col = qlinecolor[i], lty = qlinetype[i], lwd = qlinewidth[i])
    }
  
  if(opt$crossvalid == T) {
    
    # Actual oil production
    lines(opt$tsteps, new.p$gas, col = alinecolor, lty = alinetype, lwd = alinewidth)
    
    # Legend
    legend("topleft",
           c("Actual", "90%", "70%", "50%", "30%", "10%"),
           ncol = 2,
           col = c(alinecolor, qlinecolor),
           lwd = c(alinewidth, qlinewidth),
           lty = c(alinetype,  qlinetype))
  } else {
    
    # Legend
    legend("topleft",
           c("90%", "70%", "50%", "30%", "10%"),
           ncol = 2,
           col = qlinecolor,
           lwd = qlinewidth,
           lty = qlinetype)
  }
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 13 - Total gas production from existing wells simulated vs actual -------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, pgas.q[1,],
       type = "l",
       ylim = c(0.9*min(pgas.q), 1.1*max(pgas.q)),
       col = qlinecolor[1],
       lty = qlinetype[1],
       lwd = qlinewidth[1],
       xlab = "Time (months)",
       ylab = "Gas Production (MCF)")#,
       #main = "Gas Production from Existing Wells")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {
    lines(opt$tsteps, pgas.q[i,], col = qlinecolor[i], lty = qlinetype[i], lwd = qlinewidth[i])
    }
  
  if(opt$crossvalid == T) {
    
    # Actual oil production
    lines(opt$tsteps, prior.p$gas, col = alinecolor, lty = alinetype, lwd = alinewidth)
    
    # Legend
    legend("topright",
           c("Actual", "90%", "70%", "50%", "30%", "10%"),
           ncol = 2,
           col = c(alinecolor, qlinecolor),
           lwd = c(alinewidth, qlinewidth),
           lty = c(alinetype,  qlinetype))
  } else {
    
    # Legend
    legend("topleft",
           c("90%", "70%", "50%", "30%", "10%"),
           ncol = 2,
           col = qlinecolor,
           lwd = qlinewidth,
           lty = qlinetype)
  }
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 14 - CO2e Emissions -----------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, aE$CO2.q[1,],
       type = "l",
       ylim = c(0.9*min(aE$rCO2.q),
                1.1*max(aE$CO2.q)),
       col = qlinecolor[1],
       lty = 1,
       lwd = qlinewidth[1],
       xlab = "Time (months)",
       ylab = "CO2e Emissions (metric tons)")#,
       #main = "Total CO2e Emissions")
  #mtext("Solid Lines = Reduced Emissions, Dotted Lines = Base Emissions")
  lines(opt$tsteps, aE$rCO2.q[1,], col = qlinecolor[1], lty = qlinetype[1], lwd = qlinewidth[1])
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {
    lines(opt$tsteps, aE$CO2.q[i,],  col = qlinecolor[i], lty = 1,            lwd = qlinewidth[i])
    lines(opt$tsteps, aE$rCO2.q[i,], col = qlinecolor[i], lty = qlinetype[i], lwd = qlinewidth[i])
  }
  
  # Legend
  legend("topleft",
         c("90%", "70%", "50%", "30%", "10%"),
         ncol = 2,
         col = qlinecolor,
         lty = qlinetype,
         lwd = qlinewidth)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 15 - CH4 Emissions ------------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, aE$CH4.q[1,],
       type = "l",
       ylim = c(0.9*min(aE$rCH4.q),
                1.1*max(aE$CH4.q)),
       col = qlinecolor[1],
       lty = 1,
       lwd = qlinewidth[1],
       xlab = "Time (months)",
       ylab = "CH4 Emissions (metric tons)")#,
       #main = "Total CH4 Emissions")
  #mtext("Solid Lines = Reduced Emissions, Dotted Lines = Base Emissions")
  lines(opt$tsteps, aE$rCH4.q[1,], col = qlinecolor[1], lty = qlinetype[1], lwd = qlinewidth[1])
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {
    lines(opt$tsteps, aE$CH4.q[i,],  col = qlinecolor[i], lty = 1,            lwd = qlinewidth[i])
    lines(opt$tsteps, aE$rCH4.q[i,], col = qlinecolor[i], lty = qlinetype[i], lwd = qlinewidth[i])
  }
  
  # Legend
  legend("topleft",
         c("90%", "70%", "50%", "30%", "10%"),
         ncol = 2,
         col = qlinecolor,
         lty = qlinetype,
         lwd = qlinewidth)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 16 - VOC Emissions ------------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, aE$VOC.q[1,] * 1.10231,
       type = "l",
       ylim = c(0.9*min(aE$rVOC.q),
                1.1*max(aE$VOC.q)),
       col = qlinecolor[1],
       lty = 1,
       lwd = qlinewidth[1],
       xlab = "Time (months)",
       ylab = "VOC Emissions (tons)",
       main = "Total VOC Emissions (Activity-Based)")
  mtext("Solid Lines = Base Emissions, Dotted Lines = NSPS Emissions")
  lines(opt$tsteps, aE$rVOC.q[1,] * 1.10231, col = qlinecolor[1], lty = qlinetype[1], lwd = qlinewidth[1])
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {
    lines(opt$tsteps, aE$VOC.q[i,] * 1.10231,  col = qlinecolor[i], lty = 1,            lwd = qlinewidth[i])
    lines(opt$tsteps, aE$rVOC.q[i,] * 1.10231, col = qlinecolor[i], lty = qlinetype[i], lwd = qlinewidth[i])
  }
  
  # Legend
  legend("topleft",
         c("90%", "70%", "50%", "30%", "10%"),
         ncol = 2,
         col = qlinecolor,
         lty = qlinetype,
         lwd = qlinewidth)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 17 - Field Fractions ----------------------------------------------------
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


# 18 - Field Fractions - OW -----------------------------------------------
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


# 19 - Field Fractions - GW -----------------------------------------------
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


# 20 - Well Capital Cost --------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot
  plot(cost ~ depth,
       data = drillCost.data,
       col =  "grey40",
       xlab = "Measured Well Depth (ft)",
       ylab = paste("Well Capital Cost (in", opt$cpiDate, "dollars)"))#,
       #main = "Well Capital Cost Model")
  
  # Add line for model fit results
  lines(seq(0, 16e3, 100),
        exp(coef(drillCost.fit)["(Intercept)"]+coef(drillCost.fit)["depth"]*seq(0, 16e3, 100)))
  
  # Add text line with equation
  # mtext(expression(log(C)==a+b%.%D))
  
  # Legend
  legend("topleft", c("Actual", "Fit"), lty = c(NA,1), pch = c(1,NA), col = c("grey40","black"))
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 21 - Surface Lease Ownership --------------------------------------------
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


# 22 - Well Depth ---------------------------------------------------------
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


# 23 - Lease Capital & Operating Costs Fit --------------------------------
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


# 24 - Energy FPP History -------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Oil FPP
  plot(eia.hp$month, eia.hp$OP,
       type = "l",
       xlab = "Date (by month)",
       ylab = paste("Price in", opt$cpiDate, "$/bbl"),
       main = "Utah Oil First Purchase Price History")
  
  # Gas FPP
  plot(eia.hp$month, eia.hp$GP,
       type = "l",
       xlab = "Date (by month)",
       ylab = paste("Price in", opt$cpiDate, "$/MCF"),
       main = "Utah Gas First Purchase Price History")
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 25 - EIA AEO Error CDFs -------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # CDF for % error in oil  
  # Line colors
  eqlinecolor <- rainbow(ncol(Eoil)/12)
  
  # Main plot
  plot(Eoil[,12], opt$xq,
       type = "l",
       col = eqlinecolor[1],
       xlim = c(1.1*min(Eoil), 1.1*max(Eoil)),
       ylim = c(0, 1),
       xlab = "% Error",
       ylab = "Cumulative Probability",
       main = "CDF of Relative % Error of EIA Oil Price Forecasts")
  
  # For all other timesteps
  for (i in seq(from = 24, to = ncol(Eoil), by = 12)) {
    lines(Eoil[,i], opt$xq, col = eqlinecolor[i/12])
  }
  
  legend("topleft",
         c("Y1", "Y2", "Y3", "Y4", "Y5"),
         ncol = 2, lty = 1, col = eqlinecolor)
  
  # Main plot for gas
  plot(Egas[,12], opt$xq,
       type = "l",
       col = eqlinecolor[1],
       xlim = c(1.1*min(Egas), 1.1*max(Egas)),
       ylim = c(0, 1),
       xlab = "% Error",
       ylab = "Cumulative Probability",
       main = "CDF of Relative % Error of EIA Gas Price Forecasts")
  
  # For all other timesteps
  for (i in seq(from = 24, to = ncol(Egas), by = 12)) {
    lines(Egas[,i], opt$xq, col = eqlinecolor[i/12])
  }
  
  legend("topleft",
         c("Y1", "Y2", "Y3", "Y4", "Y5"),
         ncol = 2, lty = 1, col = eqlinecolor)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 26 - CDF Well Reworks ---------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot - CDF for oil
  plot(oil~month, cdf.rework,
       type = "l",
       col = qlinecolor[1],
       lwd = 2,
       ylim = c(0, 1),
       xlab = "Well Age (months)",
       ylab = "Cumulative Probability")#,
       #main = "CDF for Well Reworks as f(time)")
  
  # CDF for gas
  lines(gas~month, cdf.rework, col = qlinecolor[3], lwd = 2, lty = qlinetype[1])
  
  # Legend
  legend("topleft", c("Oil Wells", "Gas Wells"), lty = c(1, qlinetype[1]), lwd = 2, col = qlinecolor[c(1,3)])
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 27 - EIA AEO Relative Error ---------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Error collector function
  din <- function(fname) {
    
    # Get data for plot
    din <- read.csv(file.path(path$raw, fname))
    
    # Restructure
    din <- rbind(data.frame(year = "FY 1", err = din[,1]),
                 data.frame(year = "FY 2", err = din[,2]),
                 data.frame(year = "FY 3", err = din[,3]),
                 data.frame(year = "FY 4", err = din[,4]),
                 data.frame(year = "FY 5", err = din[,5]))
  }
  
#   # Main plot - oil directional relative error
#   boxplot(err~year, din(fname = "EIA_op_error_export.csv"),
#           range = 0,
#           ylim = c(-250, 100),
#           xlab = "Future-Year",
#           ylab = "Relative Error (%)")#,
#           #main = "Error in EIA Oil Price Forecasts (RE = (FP - AP) / AP)")
#   
#   # Main plot - gas directional relative error
#   boxplot(err~year, din(fname = "EIA_gp_error_export.csv"),
#           range = 0,
#           ylim = c(-250, 100),
#           xlab = "Future-Year",
#           ylab = "Relative Error (%)")#,
#           #main = "Error in EIA Gas Price Forecasts (RE = (FP - AP) / AP)")
  
  # Main plot - oil fractional relative error
  boxplot(r[year <= 5]~year[year <= 5], read.csv(file.path(path$raw, "EIA AEO frac error op export.csv")),
          range = 0,
          ylim = c(0, 1),
          xlab = "Future-Year",
          ylab = "Relative Error (%)")#,
          #main = "Error in EIA Oil Price Forecasts (RE = FP/AP | RE = AP/FP)")
  
  # Main plot - gas fractional relative error
  boxplot(r[year <= 5]~year[year <= 5], read.csv(file.path(path$raw, "EIA AEO frac error gp export.csv")),
          range = 0,
          ylim = c(0, 1),
          xlab = "Future-Year",
          ylab = "Relative Error (%)")#,
          #main = "Error in EIA Gas Price Forecasts (RE = FP/AP | RE = AP/FP)")
  
  # Remove function
  remove(din)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 28 - CO2 Emission totals Barplot ----------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  tCO2 <- NULL
  
  # Median emissions activity fractions
  nfCO2.med <-  apply(aE$nfCO2,  2, median) # CO2 emissions fractions by activity for new wells
  pfCO2.med <-  apply(aE$pfCO2,  2, median) # CO2 emissions fractions by activity for prior wells
  rnfCO2.med <- apply(aE$rnfCO2, 2, median) # CO2 emissions fractions by activity for new wells with reductions
  rpfCO2.med <- apply(aE$rpfCO2, 2, median) # CO2 emissions fractions by activity for prior wells with reductions
  
  # Median emissions from new vs. prior wells for CO2s
  fnvp.CO2 <-  median(aE$fnvp[, 3])
  rfnvp.CO2 <- median(aE$rfnvp[, 3])
  
  # Conversion factor
  fconv <-  fnvp.CO2  * (nfCO2.med)  + (1 - fnvp.CO2)  * (pfCO2.med)
  rfconv <- rfnvp.CO2 * (rnfCO2.med) + (1 - rfnvp.CO2) * (rpfCO2.med)
  
  # Get total median CO2 production by year for (a) base and (b) reduced emissions
  for(i in seq(from = 12, to = 60, by = 12)-11) {
    
    tCO2 <- cbind(tCO2, (sum(apply(aE$CO2[,  i:(i+11)], 2, median))*fconv))
    tCO2 <- cbind(tCO2, (sum(apply(aE$rCO2[, i:(i+11)], 2, median))*rfconv))
  }
  
  # Sum together small stuff
  ind <- c(3, 5, 7, 8)
  tCO2 <- rbind(tCO2, colSums(tCO2[ind,]))
  tCO2 <- tCO2[-ind, ]
  
  # Names
  fCO2.names <- c("Drill", "Completion", "Gas Production", "Gas Transmission", "Other")
  fCO2.lab <- c("BY1", "RY1", "BY2", "RY2", "BY3", "RY3", "BY4", "RY4", "BY5", "RY5")
  fCO2.xlab <- c("Year (2010 - 2014)")
  
  # Barplot
  barplot(height = tCO2/1e6,
          names.arg = fCO2.lab,
          las = 2,
          ylim = c(0, 7.5),
          xlab = fCO2.xlab,
          ylab = "CO2e Emissions (1E+09 kg / yr)",
          legend.text = fCO2.names,
          args.legend = list("top", ncol = 3, cex = 1/opt$defFontSize))
  
  abline(h = seq(1, 6, 1), col = "lightgrey")
  box()
  
  # Barplot
  barplot(height = tCO2/1e6,
          add = TRUE,
          names.arg = fCO2.lab,
          las = 2,
          ylim = c(0, 7.5),
          xlab = fCO2.xlab,
          ylab = "CO2e Emissions (1E+09 kg / yr)",
          legend.text = fCO2.names,
          args.legend = list("top", ncol = 3, cex = 1/opt$defFontSize))
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 29 - CH4 Emission totals Barplot ----------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  tCH4 <- NULL
  
  # Median emissions activity fractions
  nfCH4.med <-  apply(aE$nfCH4,  2, median) # CH4 emissions fractions by activity for new wells
  pfCH4.med <-  apply(aE$pfCH4,  2, median) # CH4 emissions fractions by activity for prior wells
  rnfCH4.med <- apply(aE$rnfCH4, 2, median) # CH4 emissions fractions by activity for new wells with reductions
  rpfCH4.med <- apply(aE$rpfCH4, 2, median) # CH4 emissions fractions by activity for prior wells with reductions
  
  # Median emissions from new vs. prior wells for CH4s
  fnvp.CH4 <-  median(aE$fnvp[, 3])
  rfnvp.CH4 <- median(aE$rfnvp[, 3])
  
  # Conversion factor
  fconv <-  fnvp.CH4  * (nfCH4.med)  + (1 - fnvp.CH4)  * (pfCH4.med)
  rfconv <- rfnvp.CH4 * (rnfCH4.med) + (1 - rfnvp.CH4) * (rpfCH4.med)
  
  # Get total median CH4 production by year for (a) base and (b) reduced emissions
  for(i in seq(from = 12, to = 60, by = 12)-11) {
    
    tCH4 <- cbind(tCH4, (sum(apply(aE$CH4[,  i:(i+11)], 2, median))*fconv))
    tCH4 <- cbind(tCH4, (sum(apply(aE$rCH4[, i:(i+11)], 2, median))*rfconv))
  }
  
  # Sum together small stuff
  ind <- c(3, 5, 7, 8)
  tCH4 <- rbind(tCH4, colSums(tCH4[ind,]))
  tCH4 <- tCH4[-ind, ]
  
  # Names
  fCH4.names <- c("Drill", "Completion", "Gas Production", "Gas Transmission", "Other")
  fCH4.lab <- c("BY1", "RY1", "BY2", "RY2", "BY3", "RY3", "BY4", "RY4", "BY5", "RY5")
  fCH4.xlab <- c("Year (2010 - 2014)")
  
  # Barplot
  barplot(height = tCH4/1e4,
          names.arg = fCH4.lab,
          las = 2,
          ylim = c(0, 35),
          xlab = fCH4.xlab,
          ylab = "CH4 Emissions (1E+07 kg / yr)",
          legend.text = fCH4.names,
          args.legend = list(x = 12.3, y = 34.75, ncol = 3, cex = 1/opt$defFontSize))
  
  abline(h = seq(5, 30, 5), col = "lightgrey")
  box()
  
  # Barplot
  barplot(height = tCH4/1e4,
          add = TRUE,
          names.arg = fCH4.lab,
          las = 2,
          ylim = c(0, 35),
          xlab = fCH4.xlab,
          ylab = "CH4 Emissions (1E+07 kg / yr)",
          legend.text = fCH4.names,
          args.legend = list(x = 12.3, y = 34.75, ncol = 3, cex = 1/opt$defFontSize))
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 30 - VOC Emission totals Barplot ----------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  tVOC <- NULL
  
  # Median emissions activity fractions
  nfVOC.med <-  apply(aE$nfVOC,  2, median) # VOC emissions fractions by activity for new wells
  pfVOC.med <-  apply(aE$pfVOC,  2, median) # VOC emissions fractions by activity for prior wells
  rnfVOC.med <- apply(aE$rnfVOC, 2, median) # VOC emissions fractions by activity for new wells with reductions
  rpfVOC.med <- apply(aE$rpfVOC, 2, median) # VOC emissions fractions by activity for prior wells with reductions
  
  # Median emissions from new vs. prior wells for VOCs
  fnvp.VOC <-  median(aE$fnvp[, 3])
  rfnvp.VOC <- median(aE$rfnvp[, 3])
  
  # Conversion factor
  fconv <-  fnvp.VOC  * (nfVOC.med)  + (1 - fnvp.VOC)  * (pfVOC.med)
  rfconv <- rfnvp.VOC * (rnfVOC.med) + (1 - rfnvp.VOC) * (rpfVOC.med)
  
  # Get total median VOC production by year for (a) base and (b) reduced emissions
  for(i in seq(from = 12, to = 60, by = 12)-11) {
    
    tVOC <- cbind(tVOC, (sum(apply(aE$VOC[,  i:(i+11)], 2, median))*fconv))
    tVOC <- cbind(tVOC, (sum(apply(aE$rVOC[, i:(i+11)], 2, median))*rfconv))
  }
  
  # Sum together small stuff
  ind <- c(3, 5, 7, 8)
  tVOC <- rbind(tVOC, colSums(tVOC[ind,]))
  tVOC <- tVOC[-ind, ]
  
  # Names
  fVOC.names <- c("Drill", "Completion", "Gas Production", "Gas Transmission", "Other")
  fVOC.lab <- c("BY1", "RY1", "BY2", "RY2", "BY3", "RY3", "BY4", "RY4", "BY5", "RY5")
  fVOC.xlab <- c("Year (2010 - 2014)")
  
  # Barplot
  barplot(height = tVOC/1e3,
          names.arg = fVOC.lab,
          las = 2,
          ylim = c(0, 60),
          xlab = fVOC.xlab,
          ylab = "VOC Emissions (1E+06 kg / yr)",
          legend.text = fVOC.names,
          args.legend = list("top", ncol = 3, cex = 1/opt$defFontSize))
  
  abline(h = seq(10, 50, 10), col = "lightgrey")
  box()
  
  # Barplot
  barplot(height = tVOC/1e3,
          add = TRUE,
          names.arg = fVOC.lab,
          las = 2,
          ylim = c(0, 60),
          xlab = fVOC.xlab,
          ylab = "VOC Emissions (1E+06 kg / yr)",
          legend.text = fVOC.names,
          args.legend = list("top", ncol = 3, cex = 1/opt$defFontSize))
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 31 - Production ratio new wells vs. existing wells ----------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Get ratios
  sPR <- data.frame(oil = oil.q[3, ] / (oil.q[3, ] + poil.q[3, ]),
                    gas = gas.q[3, ] / (gas.q[3, ] + pgas.q[3, ]))
  
  # Main plot with largest quantile result
  plot(opt$tsteps, sPR$oil,
       type = "l",
       ylim = c(0, 1),
       col = qlinecolor[1],
       lty = qlinetype[1],
       lwd = qlinewidth[1],
       xlab = "Time (months)",
       ylab = "New Well Production Fraction")#,
       #main = "Fraction of Total Production from New Wells")
  
  # Other quantile lines
  lines(opt$tsteps, sPR$gas, col = qlinecolor[2], lty = qlinetype[2], lwd = qlinewidth[2])
  
  if(opt$crossvalid == T) {
    
    # Get actual ratio
    aPR <- data.frame(oil = new.p$oil / (new.p$oil + prior.p$oil),
                      gas = new.p$gas / (new.p$gas + prior.p$gas))
    
    # Actual oil production
    lines(opt$tsteps, aPR$oil, col = qlinecolor[1], lty = alinetype, lwd = alinewidth)
    lines(opt$tsteps, aPR$gas, col = qlinecolor[2], lty = alinetype, lwd = alinewidth)
    
    # Legend
    legend("topleft",
           c("Oil - Actual", "Oil - Sim", "Gas - Actual", "Gas - Sim"),
           ncol = 1,
           col = c(qlinecolor[1], qlinecolor[1], qlinecolor[2], qlinecolor[2]),
           lwd = c(alinewidth, qlinewidth[1]),
           lty = c(alinetype,  qlinetype[1], alinetype, qlinetype[2]))
  } else {
    
    # Legend
    legend("topleft",
           c("Oil - Sim", "Gas - Sim"),
           ncol = 1,
           col = qlinecolor[1:2],
           lwd = qlinewidth[1:2],
           lty = qlinetype[1:2])
  }
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 32 - EQ-Based PM10 Emissions --------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, eE$pm10.q[1,],
       type = "l",
       ylim = c(0.9*min(eE$pm10.q),
                1.1*max(eE$pm10.q)),
       col = qlinecolor[1],
       lty = 1,
       lwd = qlinewidth[1],
       xlab = "Time (months)",
       ylab = "PM10 Emissions (tons)")
  lines(opt$tsteps, eE$pm10.q[1,], col = qlinecolor[1], lty = qlinetype[1], lwd = qlinewidth[1])
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {
    lines(opt$tsteps, eE$pm10.q[i,], col = qlinecolor[i], lty = 1, lwd = qlinewidth[i])
  }
  
  # Legend
  legend("topleft",
         c("90%", "70%", "50%", "30%", "10%"),
         ncol = 2,
         col = qlinecolor,
         lty = qlinetype,
         lwd = qlinewidth)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# 33 - EQ-Based VOC Emissions ---------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Set font size
  par(cex = opt$defFontSize)
  
  # Main plot with largest quantile result
  plot(opt$tsteps, eE$voc.q[1,],
       type = "l",
       ylim = c(0.9*min(eE$voc.q),
                1.1*max(eE$voc.q)),
       col = qlinecolor[1],
       lty = 1,
       lwd = qlinewidth[1],
       xlab = "Time (months)",
       ylab = "VOC Emissions (tons)",
       main = "Total VOC Emissions (Equipment-Based)")
  lines(opt$tsteps, eE$voc.q[1,], col = qlinecolor[1], lty = qlinetype[1], lwd = qlinewidth[1])
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {
    lines(opt$tsteps, eE$voc.q[i,], col = qlinecolor[i], lty = 1, lwd = qlinewidth[i])
  }
  
  # Legend
  legend("topleft",
         c("90%", "70%", "50%", "30%", "10%"),
         ncol = 2,
         col = qlinecolor,
         lty = 1,
         lwd = qlinewidth)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1