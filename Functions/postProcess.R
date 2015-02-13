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

# timesteps - numeric vector of timesteps in simulation/actual datasets


# Outputs -----------------------------------------------------------------

# blah


# Description -------------------------------------------------------------

# This script generates plots the results from the Monte-Carlo simulation.


# Function ---------------------------------------------------------------- 
postProcess <- function(quant, tstart, tstop, tsteps, eia.hp, wsim.actual,
                        osim.actual, gsim.actual, path, export, plist, prefix,
                        affix, osim, gsim, field, CO2, CH4, VOC, op, gp,
                        drillModel, drillModelData, mo, mg, DCA.cdf.coef.gas,
                        DCA.cdf.coef.oil, Q.DCA.cdf.coef.gas, Q.DCA.cdf.coef.oil,
                        cdf.ff, well.actual, cpiDate, drillCost.data,
                        drillCost.fit) {
  
  # Internal values - uncomment to debug ------------------------------------
  
  # quant <-   opt$quant
  # tstart <-  opt$tstart
  # tstop <-   opt$tstop
  # tsteps <-  opt$tsteps
  # export <-  opt$exportFlag
  # plist <-   opt$plist
  # prefix <-  opt$prefix
  # affix <-   opt$affix
  # field <-   opt$field
  # cpiDate <- opt$cpiDate
  
  # Queries and Calculations ------------------------------------------------
  
  # Actual energy prices for oil and gas
  ep.act <- subset(eia.hp,
                   subset = (month >= as.yearmon(tstart) &
                               month <= as.yearmon(tstop)),
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
  Drilled.q <- matrix(0, nrow = length(quant), ncol = length(tsteps))
  oil.q <-     Drilled.q
  gas.q <-     Drilled.q
  op.q <-      Drilled.q
  gp.q <-      Drilled.q
  CO2.q <-     Drilled.q
  CH4.q <-     Drilled.q
  VOC.q <-     Drilled.q
  
  # For each timestep, get quantiles
  for (i in 1:ncol(Drilled.q)) {
    Drilled.q[,i] <- quantile(Drilled[,i], quant)
    oil.q[,i] <-     quantile(osim[,i], quant)
    gas.q[,i] <-     quantile(gsim[,i], quant)
    op.q[,i] <-      quantile(op[,i], quant)
    gp.q[,i] <-      quantile(gp[,i], quant)
    CO2.q[,i] <-     quantile(CO2[,i], quant)
    CH4.q[,i] <-     quantile(CH4[,i], quant)
    VOC.q[,i] <-     quantile(gp[,i], quant)
  }
  
  
  # Global plotting options -------------------------------------------------
  
  # Set line colors for quantiles used in quant
  linecolor <- rainbow(length(quant))
  
  # Set line colors for fields
  flinecolor <- rainbow(length(field))
  
  # Plot counter
  j <- 1
  
  # Oil prices simulated vs actual ------------------------------------------
  if(plist$plot[j] == TRUE) {
    
    # If exporting to PDF
    if(export == TRUE) {pdf(file.path(path$plot, file = paste(prefix, plist$name[j], affix, sep = "")))}
    
    # Main plot with largest quantile result
    plot(tsteps, op.q[1,],
         type = "l",
         col = linecolor[1],
         xlab = "Time",
         ylab = "Oil First Purchase Price (2012 $/bbl)",
         main = "Oil Price - Simulation vs. Actual")
    
    # All the other quantile lines
    for (i in 2:length(quant)) {lines(tsteps, op.q[i,], col = linecolor[i])}
    
    # Add the line for the actual value
    lines(tsteps, ep.act$OP)
    
    # Add legend
    legend("topleft", c("90%", "70%", "50%", "30%", "10%", "Actual"), ncol = 2, col = c(linecolor,"black"), lty = 1)
    
    # If exporting to PDF, close PDF
    if(export == TRUE) {dev.off()}
  }
  
  # Increment counter
  j <- j+1
  
  
  # Gas prices simulated vs actual ------------------------------------------
  if(plist$plot[j] == TRUE) {
    
    # If exporting to PDF
    if(export == TRUE) {pdf(file.path(path$plot, file = paste(prefix, plist$name[j], affix, sep = "")))}
    
    # Main plot with largest quantile result
    plot(tsteps, gp.q[1,],
         type = "l",
         col = linecolor[1],
         xlab = "Time",
         ylab = "Gas First Purchase Price (2012 $/MCF)",
         main = "Gas Price - Simulation vs. Actual")
    
    # All the other quantile lines
    for (i in 2:length(quant)) {lines(tsteps, gp.q[i,], col = linecolor[i])}
    
    # Add the line for the actual value
    lines(tsteps, ep.act$GP)
    
    # Add legend
    legend("topleft", c("90%", "70%", "50%", "30%", "10%", "Actual"), ncol = 2, col = c(linecolor,"black"), lty = 1)
    
    # If exporting to PDF, close PDF
    if(export == TRUE) {dev.off()}
  }
  
  # Increment counter
  j <- j+1
  
  
  # Drilling schedule simulated vs actual -----------------------------------
  if(plist$plot[j] == TRUE) {
    
    # If exporting to PDF
    if(export == TRUE) {pdf(file.path(path$plot, file = paste(prefix, plist$name[j], affix, sep = "")))}
    
    # Main plot with largest quantile result
    plot(tsteps, Drilled.q[1,],
         type = "l",
         col = linecolor[1],
         xlab = "Time",
         ylab = "Wells Drilled",
         main = "Drilling Schedule - Simulation vs. Actual")
    
    # All the other quantile lines
    for (i in 2:length(quant)) {lines(tsteps, Drilled.q[i,], col = linecolor[i])}
    
    # Add the line for the actual value
    lines(tsteps, Drilled.act)
    
    # Add legend
    legend("topleft", c("90%", "70%", "50%", "30%", "10%", "Actual"), ncol = 2, col = c(linecolor,"black"), lty = 1)
    
    # If exporting to PDF, close PDF
    if(export == TRUE) {dev.off()}
  }
  
  # Increment counter
  j <- j+1
  
  
  # Drilling fit vs actual --------------------------------------------------
  if(plist$plot[j] == TRUE) {
    
    # If exporting to PDF
    if(export == TRUE) {pdf(file.path(path$plot, file = paste(prefix, plist$name[j], affix, sep = "")))}
    
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
    if(export == TRUE) {dev.off()}
  }
  
  # Increment counter
  j <- j+1
  
  
  # Boxplots of decline curve coefficients by field -------------------------
  if(plist$plot[j] == TRUE) {
    
    # If exporting to PDF
    if(export == TRUE) {pdf(file.path(path$plot, file = paste(prefix, plist$name[j], affix, sep = "")))}
    
    # Source hyperbolic and cumulative boxplot functions
    source(file.path(path$plotfun, "bplotHypDCAcoef.R"))
    source(file.path(path$plotfun, "bplotQfitDCAcoef.R"))
    
    # Run plotting functions
    bplotHypDCAcoef()
    bplotQfitDCAcoef()  
    
    # If exporting to PDF, close PDF
    if(export == TRUE) {dev.off()}
  }
  
  # Increment counter
  j <- j+1
  
  
  # CDF for decline curve coefficients --------------------------------------
  if(plist$plot[j] == TRUE) {
    
    # If exporting to PDF
    if(export == TRUE) {pdf(file.path(path$plot, file = paste(prefix, plist$name[j], affix, sep = "")))}
    
    # Source hyperbolic and cumulative boxplot functions
    source(file.path(path$plotfun, "cdfDCAcoef.R"))
    
    # Run plotting functions
    cdfDCAcoef()
    
    # If exporting to PDF, close PDF
    if(export == TRUE) {dev.off()}
  }
  
  # Increment counter
  j <- j+1
  
  
  # Total oil production simulated vs actual --------------------------------
  if(plist$plot[j] == TRUE) {
    
    # If exporting to PDF
    if(export == TRUE) {pdf(file.path(path$plot, file = paste(prefix, plist$name[j], affix, sep = "")))}
    
    # Main plot with largest quantile result
    plot(tsteps, oil.q[1,],
         type = "l",
         col = linecolor[1],
         xlab = "Time (months)",
         ylab = "Oil Production (bbl)",
         main = "Total Oil Production - Simulated vs. Actual")
    
    # Other quantile lines
    for (i in 2:length(quant)) {lines(tsteps, oil.q[i,], col = linecolor[i])}
    
    # Actual oil production
    lines(tsteps, oil.act)
    
    # Legend
    legend("topleft", c("90%", "70%", "50%", "30%", "10%", "Actual"), ncol = 2, col = c(linecolor,"black"), lty = 1)
    
    # If exporting to PDF, close PDF
    if(export == TRUE) {dev.off()}
  }
  
  # Increment counter
  j <- j+1
  
  
  # Total gas production simulated vs actual --------------------------------
  if(plist$plot[j] == TRUE) {
    
    # If exporting to PDF
    if(export == TRUE) {pdf(file.path(path$plot, file = paste(prefix, plist$name[j], affix, sep = "")))}
    
    # Main plot with largest quantile result
    plot(tsteps, gas.q[1,],
         type = "l",
         col = linecolor[1],
         xlab = "Time (months)",
         ylab = "Gas Production (MCF)",
         main = "Total Gas Production - Simulated vs. Actual")
    
    # Other quantile lines
    for (i in 2:length(quant)) {lines(tsteps, gas.q[i,], col = linecolor[i])}
    
    # Actual gas production
    lines(tsteps, gas.act)
    
    # Legend
    legend("topleft", c("90%", "70%", "50%", "30%", "10%", "Actual"), ncol = 2, col = c(linecolor,"black"), lty = 1)
    
    # If exporting to PDF, close PDF
    if(export == TRUE) {dev.off()}
  }
  
  # Increment counter
  j <- j+1
  
  
  
  # CO2e Emissions ----------------------------------------------------------
  if(plist$plot[j] == TRUE) {
    
    # If exporting to PDF
    if(export == TRUE) {pdf(file.path(path$plot, file = paste(prefix, plist$name[j], affix, sep = "")))}
    
    # Main plot with largest quantile result
    plot(tsteps, CO2.q[1,],
         type = "l",
         col = linecolor[1],
         xlab = "Time (months)",
         ylab = "CO2e Emissions (metric tons)",
         main = "Total CO2e Emissions")
    
    # Other quantile lines
    for (i in 2:length(quant)) {lines(tsteps, CO2.q[i,], col = linecolor[i])}
    
    # Legend
    legend("topleft", c("90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c(linecolor), lty = 1)
    
    # If exporting to PDF, close PDF
    if(export == TRUE) {dev.off()}
  }
  
  # Increment counter
  j <- j+1
  
  
  # CH4 Emissions -----------------------------------------------------------
  if(plist$plot[j] == TRUE) {
    
    # If exporting to PDF
    if(export == TRUE) {pdf(file.path(path$plot, file = paste(prefix, plist$name[j], affix, sep = "")))}
    
    # Main plot with largest quantile result
    plot(tsteps, CH4.q[1,],
         type = "l",
         col = linecolor[1],
         xlab = "Time (months)",
         ylab = "CH4 Emissions (metric tons)",
         main = "Total CH4 Emissions")
    
    # Other quantile lines
    for (i in 2:length(quant)) {lines(tsteps, CH4.q[i,], col = linecolor[i])}
    
    # Legend
    legend("topleft", c("90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c(linecolor), lty = 1)
    
    # If exporting to PDF, close PDF
    if(export == TRUE) {dev.off()}
  }
  
  # Increment counter
  j <- j+1
  
  
  # VOC Emissions -----------------------------------------------------------
  if(plist$plot[j] == TRUE) {
    
    # If exporting to PDF
    if(export == TRUE) {pdf(file.path(path$plot, file = paste(prefix, plist$name[j], affix, sep = "")))}
    
    # Main plot with largest quantile result
    plot(tsteps, VOC.q[1,],
         type = "l",
         col = linecolor[1],
         xlab = "Time (months)",
         ylab = "VOC Emissions (metric tons)",
         main = "Total VOC Emissions")
    
    # Other quantile lines
    for (i in 2:length(quant)) {lines(tsteps, VOC.q[i,], col = linecolor[i])}
    
    # Legend
    legend("topleft", c("90%", "70%", "50%", "30%", "10%"), ncol = 2, col = c(linecolor), lty = 1)
    
    # If exporting to PDF, close PDF
    if(export == TRUE) {dev.off()}
  }
  
  # Increment counter
  j <- j+1
  
  
  # Field Fractions ---------------------------------------------------------
  if(plist$plot[j] == TRUE) {
    
    # If exporting to PDF
    if(export == TRUE) {pdf(file.path(path$plot, file = paste(prefix, plist$name[j], affix, sep = "")))}
    
    # Get well counts by field from well.actual
    fcount <- round(c(cdf.ff$CDF[1], diff(cdf.ff$CDF))*nrow(well.actual))
    
    # Main Bar Chart
    barplot(height = fcount,
            names.arg = as.character(cdf.ff$Field),
            log = "y",
            ylim = c(1, 5e3),
            ylab = "Well Count (log-scale)",
            xlab = "Field Number",
            main = "Well Counts by Field")
    
    # Add grid lines
    abline(h = c(5, 10, 50, 100, 500, 1e3, 5e3), lty = 3, col = "grey")
    
    # If exporting to PDF, close PDF
    if(export == TRUE) {dev.off()}
  }
  
  # Increment counter
  j <- j+1
  
  
  # Field Fractions - OW ----------------------------------------------------
  if(plist$plot[j] == TRUE) {
    
    # If exporting to PDF
    if(export == TRUE) {pdf(file.path(path$plot, file = paste(prefix, plist$name[j], affix, sep = "")))}
    
    # Get well counts by field from well.actual
    fcount <- round(c(cdf.ff$CDF[1], diff(cdf.ff$CDF))*nrow(well.actual)*(1-prob$gas))
    
    # Main Bar Chart
    barplot(height = fcount,
            names.arg = as.character(cdf.ff$Field),
            log = "y",
            ylim = c(1, 5e3),
            ylab = "Well Count (log-scale)",
            xlab = "Field Number",
            main = "Oil Well Counts by Field")
    
    # Add grid lines
    abline(h = c(5, 10, 50, 100, 500, 1e3, 5e3), lty = 3, col = "grey")
    
    # If exporting to PDF, close PDF
    if(export == TRUE) {dev.off()}
  }
  
  # Increment counter
  j <- j+1
  
  
  # Field Fractions - GW ----------------------------------------------------
  if(plist$plot[j] == TRUE) {
    
    # If exporting to PDF
    if(export == TRUE) {pdf(file.path(path$plot, file = paste(prefix, plist$name[j], affix, sep = "")))}
    
    # Get well counts by field from well.actual
    fcount <- round(c(cdf.ff$CDF[1], diff(cdf.ff$CDF))*nrow(well.actual)*prob$gas)
    
    # Since some fields have zero gas wells, make those fields NA
    fcount[which(fcount == 0)] <- NA
    
    # Main Bar Chart
    barplot(height = fcount,
            names.arg = as.character(cdf.ff$Field),
            log = "y",
            ylim = c(1, 5e3),
            ylab = "Well Count (log-scale)",
            xlab = "Field Number",
            main = "Gas Well Counts by Field")
    
    # Add grid lines
    abline(h = c(5, 10, 50, 100, 500, 1e3, 5e3), lty = 3, col = "grey")
    
    # If exporting to PDF, close PDF
    if(export == TRUE) {dev.off()}
  }
  
  # Increment counter
  j <- j+1
  
  
  # Well Capital Cost -------------------------------------------------------
  if(plist$plot[j] == TRUE) {
    
    # If exporting to PDF
    if(export == TRUE) {pdf(file.path(path$plot, file = paste(prefix, plist$name[j], affix, sep = "")))}
    
    # Main plot
    plot(log(cost) ~ depth,
         data = drillCost.data,
         xlab = "Measured Well Depth (ft)",
         ylab = paste("Capital Cost (in", cpiDate, "dollars)"),
         main = "Well Capital Cost Model")
    
    # Add line for model fit results
    lines(drillCost.data$depth, fitted(drillCost.fit), col = "red")
    
    # Add text line with equation
    mtext(expression(log(C)==a+b%.%D))
    
    # Legend
    legend("topleft", c("Actual", "Fit"), lty = c(NA,1), pch = c(1,NA), col = c("black","red"))
    
    # If exporting to PDF, close PDF
    if(export == TRUE) {dev.off()}
  }
  
  # Increment counter
  j <- j+1
}