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
Drilled.act <- sqldf("select count(wellID)
                     from 'wsim.actual'
                     group by tDrill")[[1]]

# Actual total oil/gas production as timeseries
oil.act <- colSums(osim.actual, na.rm = TRUE)
gas.act <- colSums(gsim.actual, na.rm = TRUE)

# ---Quantiles---

# Predefine quantile matrices
Drilled.q <- matrix(0, nrow = length(opt$quant), ncol = length(opt$tsteps))
oil.q <-     Drilled.q
gas.q <-     Drilled.q
op.q <-      Drilled.q
gp.q <-      Drilled.q
CO2.q <-     Drilled.q
CH4.q <-     Drilled.q
VOC.q <-     Drilled.q

# For each timestep, get quantiles
for (i in 1:ncol(Drilled.q)) {
  Drilled.q[,i] <- quantile(Drilled[,i], opt$quant)
  oil.q[,i] <-     quantile(osim[,i], opt$quant)
  gas.q[,i] <-     quantile(gsim[,i], opt$quant)
  op.q[,i] <-      quantile(op[,i], opt$quant)
  gp.q[,i] <-      quantile(gp[,i], opt$quant)
  CO2.q[,i] <-     quantile(CO2[,i], opt$quant)
  CH4.q[,i] <-     quantile(CH4[,i], opt$quant)
  VOC.q[,i] <-     quantile(gp[,i], opt$quant)
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
  
  # Main plot with largest quantile result
  plot(opt$tsteps, op.q[1,],
       ylim = c(0.9*min(c(min(op.q), min(ep.act$OP))), 1.1*max(c(max(op.q), max(ep.act$OP)))),
       type = "l",
       col = linecolor[1],
       xlab = "Time",
       ylab = paste("Oil First Purchase Price (", opt$cpiDate, " $/bbl)", sep = ""),
       main = "Oil Price - Simulation vs. Actual")
  
  # All the other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, op.q[i,], col = linecolor[i])}
  
  # Add the line for the actual value
  lines(opt$tsteps, ep.act$OP)
  
  # Add the forecast line
  lines(opt$tsteps, op.FC, lty = 2)
  
  # Add legend
  legend("topright",
         c("90%", "70%", "50%", "30%", "10%", "Actual", "Forecast"),
         ncol = 2,
         col = c(linecolor,"black", "black"),
         lty = c(rep(1, times = 6), 2))
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Gas prices simulated vs actual ------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Main plot with largest quantile result
  plot(opt$tsteps, gp.q[1,],
       type = "l",
       ylim = c(0.9*min(c(min(gp.q), min(ep.act$GP))), 1.1*max(c(max(gp.q), max(ep.act$GP)))),
       col = linecolor[1],
       xlab = "Time",
       ylab = paste("Gas First Purchase Price (", opt$cpiDate, " $/MCF)", sep = ""),
       main = "Gas Price - Simulation vs. Actual")
  
  # All the other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, gp.q[i,], col = linecolor[i])}
  
  # Add the line for the actual value
  lines(opt$tsteps, ep.act$GP)
  
  # Add the forecast line
  lines(opt$tsteps, gp.FC, lty = 2)
  
  # Add legend
  legend("topright",
         c("90%", "70%", "50%", "30%", "10%", "Actual", "Forecast"),
         ncol = 2,
         col = c(linecolor,"black", "black"),
         lty = c(rep(1, times = 6), 2))
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Drilling schedule simulated vs actual -----------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Main plot with largest quantile result
  plot(opt$tsteps, Drilled.q[1,],
       type = "l",
       ylim = c(0.9*min(c(min(Drilled.q),min(Drilled.act))),
                1.1*max(c(max(Drilled.q),max(Drilled.act)))),
       col = linecolor[1],
       xlab = "Time",
       ylab = "Wells Drilled",
       main = "Drilling Schedule - Simulation vs. Actual")
  
  # All the other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, Drilled.q[i,], col = linecolor[i])}
  
  # Add the line for the actual value
  lines(opt$tsteps, Drilled.act)
  
  # Add legend
  legend("topright", c("90%", "70%", "50%", "30%", "10%", "Actual"), ncol = 2, col = c(linecolor,"black"), lty = 1)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Drilling fit vs actual --------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Main plot with actual drilling schedule
  plot(drillModelData$month, drillModelData$wells,
       type = "l",
       xlab = "Year",
       ylab = "Total Wells Drilled (oil, gas, or dry)",
       main = "Drilling Schedule Model")
  
  # Add line for model fit results
  lines(drillModelData$month, fitted(drillModel), col = "red")
  
  # Add text line with equation
  mtext(expression(W==a%.%OP+b%.%GP+c%.%W[n-1]+d))
  
  # Legend
  legend("topleft", c("Actual", "Fit"), lty = c(1,1), col = c("black","red"))
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Boxplots of decline curve coefficients by field -------------------------
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


# CDF for decline curve coefficients --------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Source hyperbolic and cumulative boxplot functions
  source(file.path(path$plotfun, "cdfDCAcoef.R"))
  
  # Run plotting functions
  cdfDCAcoef()
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Total oil production simulated vs actual --------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Main plot with largest quantile result
  plot(opt$tsteps, oil.q[1,],
       type = "l",
       ylim = c(min(c(min(oil.q),min(oil.act))),
                max(c(max(oil.q),max(oil.act)))),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "Oil Production (bbl)",
       main = "Total Oil Production - Simulated vs. Actual")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, oil.q[i,], col = linecolor[i])}
  
  # Actual oil production
  lines(opt$tsteps, oil.act)
  
  # Legend
  legend("topleft", c("90%", "70%", "50%", "30%", "10%", "Actual"), ncol = 2, col = c(linecolor,"black"), lty = 1)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Total gas production simulated vs actual --------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Main plot with largest quantile result
  plot(opt$tsteps, gas.q[1,],
       type = "l",
       ylim = c(min(c(min(gas.q),min(gas.act))),
                max(c(max(gas.q),max(gas.act)))),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "Gas Production (MCF)",
       main = "Total Gas Production - Simulated vs. Actual")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, gas.q[i,], col = linecolor[i])}
  
  # Actual gas production
  lines(opt$tsteps, gas.act)
  
  # Legend
  legend("topleft", c("90%", "70%", "50%", "30%", "10%", "Actual"), ncol = 2, col = c(linecolor,"black"), lty = 1)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1  


# CO2e Emissions ----------------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Main plot with largest quantile result
  plot(opt$tsteps, CO2.q[1,],
       type = "l",
       ylim = c(0.9*min(CO2.q),
                1.1*max(CO2.q)),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "CO2e Emissions (metric tons)",
       main = "Total CO2e Emissions")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, CO2.q[i,], col = linecolor[i])}
  
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
  
  # Main plot with largest quantile result
  plot(opt$tsteps, CH4.q[1,],
       type = "l",
       ylim = c(0.9*min(CH4.q),
                1.1*max(CH4.q)),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "CH4 Emissions (metric tons)",
       main = "Total CH4 Emissions")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, CH4.q[i,], col = linecolor[i])}
  
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
  
  # Main plot with largest quantile result
  plot(opt$tsteps, VOC.q[1,],
       type = "l",
       ylim = c(0.9*min(VOC.q),
                1.1*max(VOC.q)),
       col = linecolor[1],
       xlab = "Time (months)",
       ylab = "VOC Emissions (metric tons)",
       main = "Total VOC Emissions")
  
  # Other quantile lines
  for (i in 2:length(opt$quant)) {lines(opt$tsteps, VOC.q[i,], col = linecolor[i])}
  
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
  
  # Get well counts by field from well.actual
  fcount <- round(c(cdf.ff$CDF[1], diff(cdf.ff$CDF))*nrow(well.actual))
  
  # Main Bar Chart
  bp <- barplot(height = fcount,
                names.arg = as.character(cdf.ff$Field),
                #log = "y",
                ylim = c(0, 3.5e3),
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
  
  # Main plot
  plot(log(cost) ~ depth,
       data = drillCost.data,
       xlab = "Measured Well Depth (ft)",
       ylab = paste("Capital Cost (in", opt$cpiDate, "dollars)"),
       main = "Well Capital Cost Model")
  
  # Add line for model fit results
  lines(drillCost.data$depth, fitted(drillCost.fit), col = "red")
  
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


# Lease Operating Costs Fit -------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
  # Oil LOC plot
  temp <-scatterplot3d(x = LOC.oil$real.price,
                       y = LOC.oil$depth/1e3,
                       z = LOC.oil$cost/1e3,
                       highlight.3d=TRUE,
                       xlab = paste("Oil price in", opt$cpiDate, "$/bbl"),
                       ylab = "Well Depth (1e3 ft)",
                       zlab = paste("Operating Cost in", opt$cpiDate, "$1e3/year"),
                       main="Lease Opearting Cost Fit for Oil Wells")
  
  # Plane fit for oil LOC
  fitdata <- LOC.oil
  fitdata$cost <- fitdata$cost/1e3
  fitdata$depth <- fitdata$depth/1e3
  temp$plane3d(lm(cost ~ real.price + depth, data = fitdata))
  
  # Select only the data points which have a production rate == 250 MCF/day
  # (production rate with the largest number of data points)
  tdata <- LOC.gas[which(LOC.gas$prodrate == 250),]
  
  # Gas LOC plot
  temp <-scatterplot3d(x = tdata$real.price,
                       y = tdata$depth/1e3,
                       z = tdata$cost/1e3,
                       highlight.3d=TRUE,
                       xlab = paste("Gas price in", opt$cpiDate, "$/MCF"),
                       ylab = "Well Depth (1e3 ft)",
                       zlab = paste("Operating Cost in", opt$cpiDate, "$1e3/year"),
                       main="Lease Opearting Cost Fit for Gas Wells - Prod. Rate 250 MCFD")
  
  # Plane fit for gas LOC
  fitdata <- tdata
  fitdata$cost <- fitdata$cost/1e3
  fitdata$depth <- fitdata$depth/1e3
  temp$plane3d(lm(cost ~ real.price + depth, data = fitdata))
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1


# Energy FPP History -------------------------------------------------
if(opt$plist$plot[j] == TRUE) {
  
  # If exporting to PDF
  if(opt$exportFlag == TRUE) {pdf(file.path(path$plot, file = paste(opt$prefix, opt$plist$name[j], opt$affix, sep = "")))}
  
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
  
  # Plot
  plot(x = qnorm(p = opt$DCA.CDF.xq, mean = corpNTIfrac["mean"], sd = corpNTIfrac["sd"]),
       y = opt$DCA.CDF.xq,
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
  
  # Plot
  plot(x = qnorm(p = opt$DCA.CDF.xq, mean = pTaxRate["mean"], sd = pTaxRate["sd"]),
       y = opt$DCA.CDF.xq,
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
  
  # CDF for % error in oil
  
  # Line colors
  linecolor <- rainbow(ncol(Eoil))
  
  # Main plot
  plot(Eoil[,1], opt$EIAExq,
       type = "l",
       col = linecolor[1],
       xlim = c(1.1*min(Eoil), 1.1*max(Eoil)),
       ylim = c(0, 1),
       xlab = "% Error",
       ylab = "Cumulative Probability",
       main = "CDF of Relative % Error of EIA Oil Price Forecasts")
  
  # For all other timesteps
  for (i in 2:ncol(Eoil)) {
    lines(Eoil[,i], opt$EIAExq, col = linecolor[i])
  }
  
  legend("topleft",
         c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7", "Y8", "Y9", "Y10"),
         ncol = 2, lty = 1, col = linecolor)
  
  # Main plot for gas
  plot(Egas[,1], opt$EIAExq,
       type = "l",
       col = linecolor[1],
       xlim = c(1.1*min(Egas), 1.1*max(Egas)),
       ylim = c(0, 1),
       xlab = "% Error",
       ylab = "Cumulative Probability",
       main = "CDF of Relative % Error of EIA Gas Price Forecasts")
  
  # For all other timesteps
  for (i in 2:ncol(Egas)) {
    lines(Egas[,i], opt$EIAExq, col = linecolor[i])
  }
  
  legend("topleft",
         c("Y1", "Y2", "Y3", "Y4", "Y5", "Y6", "Y7", "Y8", "Y9", "Y10"),
         ncol = 2, lty = 1, col = linecolor)
  
  # If exporting to PDF, close PDF
  if(opt$exportFlag == TRUE) {dev.off()}
}

# Increment counter
j <- j+1
