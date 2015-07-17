# Function Info -----------------------------------------------------------
# Name:      DCAlnormUpdate.R (DCA Log-Normal CDF update)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# min.rec.count - minimum number of fits required in a given year in order to
# attempt to fit distribution

# plot.flag - T/F flag indicating whether or not to plot distribution and
# trendline fits

# mo/mg - DCA results data.frame for oil/gas wells

# Q.cdf.oil/gas.from - lower limit in cumulative fit for CDF function for (Cp, c1)

# Q.cdf.oil/gas.to - upper limit in cumulative fit for CDF function for (Cp, c1)

# tstart - start date (in years) for fitting trendline data (i.e. 2000 or 2005, etc.)

# tstop - end date (in years) for fitting trendline data

# path - file path listing

# ver - file version number

# field - vector of field numbers to be analyzed individually


# Outputs -----------------------------------------------------------------

# DCAlnormFit - data.frame listing for each field and each DCA coefficient the
# trendline fit parameters that can be used for estimating future CDFs for that
# DCA coefficients based on what year it is (in years since 1983)


# Description -------------------------------------------------------------

# This function does what the output says.


# Function ----------------------------------------------------------------

DCAlnormUpdate <- function(min.rec.count, plot.flag, mo, mg, Q.cdf.oil.to,
                           Q.cdf.oil.from, Q.cdf.gas.to, Q.cdf.gas.from, tstart,
                           tstop, path, ver, field) {
  
  # Data prep ---------------------------------------------------------------
  
  # Select subset of mo/mg that succesfully fitted both first/last curves, that
  # have dates of first production, and whose Cp values are within the CDF limits
  # of Cp for oil/gas
  v1.ow <- subset(mo, subset = (Qfit.1 == 1 & Qfit.2 == 1 & !is.na(firstprod) & Cp.1 <= Q.cdf.oil.to[1] & c1.1 >= Q.cdf.oil.from[2] & c1.1 <= Q.cdf.oil.to[2]))
  v1.gw <- subset(mg, subset = (Qfit.1 == 1 & Qfit.2 == 1 & !is.na(firstprod) & Cp.1 <= Q.cdf.gas.to[1] & c1.1 >= Q.cdf.gas.from[2] & c1.1 <= Q.cdf.gas.to[1]))
  
  # Add diff time value in months since Dec. 1983
  v1.ow$dt <- with(v1.ow, round(as.numeric(difftime(firstprod, as.Date("1983-12-01"), units = "days"))*(12/365.25)))
  v1.gw$dt <- with(v1.gw, round(as.numeric(difftime(firstprod, as.Date("1983-12-01"), units = "days"))*(12/365.25)))
  
  # Aggregate to annual basis
  vow <- v1.ow
  vgw <- v1.gw
  for(i in 1:ceiling(c(max(v1.ow$dt),max(v1.gw$dt))/12)) {
    ind.ow <- which(vow$dt >= (12*(i-1)+1) & vow$dt <= (12*i))
    ind.gw <- which(vgw$dt >= (12*(i-1)+1) & vgw$dt <= (12*i))
    vow$dt[ind.ow] <- i
    vgw$dt[ind.gw] <- i
  }
  
  
  # Fit DCA coefficients distribution parameters ----------------------------
  
  # Predefine objects inside loop
  year <- 1984:(1984+max(c(max(vow$dt), max(vgw$dt)))-1)
  
  # Fit function
  fitf <- function(data, plot.flag, type) {
    
    # Predefine result objects
    Cp <- NULL
    c1 <- Cp
    ind999 <- Cp
    
    # For each field being analyzed individually
    for (i in 1:(length(field)-1)) {
      
      # For each field being analyzed individually
      ind <- which(data$w_field_num == field[i])
      ftemp <- data[ind,]
      
      # Add row numbers to ind999 exclusion list
      ind999 <- c(ind999, ind)
      
      # For each year (i.e. value of dt)
      for (j in 1:max(data$dt)) {
        
        # Subset to year
        ttemp <- subset(ftemp, subset = (dt == j))
        
        # If subset of fits is large enough
        if (length(ttemp$dt) > min.rec.count) {
          
          # Fit distribution for Cp (log-normal) and c1 (normal)
          tparm.Cp <- fitdist(ttemp$Cp.1, "lnorm")
          tparm.c1t1 <- fitdist(ttemp$c1.1, "norm")
          tparm.c1t2 <- fitdist(ttemp$c1.1, "cauchy")
          tparm.c1t3 <- fitdist(ttemp$c1.1, "logis")
          
          # If plotting
          if (plot.flag == T) {
            cdfcomp(tparm.Cp, main = paste(type, "Cp values for Field", field[i], "for", year[j]), xlab = "Cp value", legendtext = "Log-Normal")
            cdfcomp(list(tparm.c1t1, tparm.c1t2, tparm.c1t3), main = paste(type, "c1 values for Field", field[i], "for", year[j]), xlab = "c1 value", legendtext = c("Normal", "Cauchy", "Logistic"))
          }
          
          # Save out result
          Cp <- rbind(Cp, c(j, field[i], tparm.Cp$estimate))
          c1 <- rbind(c1, c(j, field[i], tparm.c1t1$estimate, tparm.c1t2$estimate, tparm.c1t3$estimate))
        }
      }
    }
    
    # For Field 999
    ftemp <- data[-ind999,]
    
    # For each year (i.e. value of dt)
    for (j in 1:max(data$dt)) {
      
      # Subset to year
      ttemp <- subset(ftemp, subset = (dt == j))
      
      # If subset of fits is large enough
      if (length(ttemp$dt) > min.rec.count) {
        
        # Fit distribution for Cp (log-normal) and c1 (normal)
        tparm.Cp <- fitdist(ttemp$Cp.1, "lnorm")
        tparm.c1t1 <- fitdist(ttemp$c1.1, "norm")
        tparm.c1t2 <- fitdist(ttemp$c1.1, "cauchy")
        tparm.c1t3 <- fitdist(ttemp$c1.1, "logis")
        
        # If plotting
        if (plot.flag == T) {
          cdfcomp(tparm.Cp, main = paste(type, "Cp values for Field", field[i+1], "for", year[j]), xlab = "Cp value", legendtext = "Log-Normal")
          cdfcomp(list(tparm.c1t1, tparm.c1t2, tparm.c1t3), main = paste(type, "c1 values for Field", field[i+1], "for", year[j]), xlab = "c1 value", legendtext = c("Normal", "Cauchy", "Logistic"))
        }
        
        # Save out result
        Cp <- rbind(Cp, c(j, field[i+1], tparm.Cp$estimate))
        c1 <- rbind(c1, c(j, field[i+1], tparm.c1t1$estimate, tparm.c1t2$estimate, tparm.c1t3$estimate))
      }
    }
    
    # Return results
    return(list(Cp, c1))
  }
  
  # If plotting, save to PDF
  if (plot.flag == T) {
    pdf(file.path(path$plot, "Field level DCC distribution fits.pdf"), width = 14, height = 7)
    par(mfcol = c(1, 2))
  }
  
  # Run fit function
  par.ow <- fitf(vow, plot.flag, "Oil")
  par.gw <- fitf(vgw, plot.flag, "Gas")
  
  # If plotting, close PDF
  if (plot.flag == T) {
    dev.off()
  }
  
  # Extract results, change to data.frame, and rename
  Cp.ow <- par.ow[[1]]; Cp.ow <- data.frame(Cp.ow); names(Cp.ow) <- c("year", "field", "meanlog", "sdlog")
  Cp.gw <- par.gw[[1]]; Cp.gw <- data.frame(Cp.gw); names(Cp.gw) <- c("year", "field", "meanlog", "sdlog")
  c1.ow <- par.ow[[2]]; c1.ow <- data.frame(c1.ow); names(c1.ow) <- c("year", "field", "mean", "sd", "loc1", "scale1", "loc2", "scale2")
  c1.gw <- par.gw[[2]]; c1.gw <- data.frame(c1.gw); names(c1.gw) <- c("year", "field", "mean", "sd", "loc1", "scale1", "loc2", "scale2")
  
  
  # Distribution parameter trendline fitting --------------------------------
  
  min.expRSS <- function(time, obs, par) {
    
    # Initial wells drilled
    y <- par[1]*exp(-par[2]*time)
    
    # RSS
    RSS <- sum((y-obs)^2)
    
    # Return RSS
    return(RSS)
  }
  
  # Trend fit function
  fitt <- function(data, type) {
    
    tfmn <- NULL
    tfsd <- NULL
    
    # If fitting Cp
    if (type == "Cp") {
      
      # For each field
      for (i in 1:length(field)) {
        
        # Subset data for fit
        temp <- data[data$field == field[i],]
        
        # Fit trendlines
        tfmn <- rbind(tfmn, coefficients(lm(meanlog~year, temp)))
        tfsd <- rbind(tfsd, optim(par = c(1,0.02), fn = min.expRSS, time = temp$year, obs = temp$sdlog)$par)
      }
      
      # Change to data.frame and rename columns
      tfmn <- data.frame(field, tfmn); names(tfmn) <- c("field", "b", "m")
      tfsd <- data.frame(field, tfsd); names(tfsd) <- c("field", "a", "b")
      
      # Return results
      return(list(meanlog = tfmn, sdlog = tfsd))
    }
    
    # If fitting c1
    if (type == "c1") {
      
      # For each field
      for (i in 1:length(field)) {
        
        # Subset data for fit
        temp <- data[data$field == field[i],]
        
        # Fit trendlines
        tfmn <- rbind(tfmn, coefficients(lm(mean~year, temp)))
        tfsd <- rbind(tfsd, coefficients(lm(sd~year, temp)))
      }
      
      # Change to data.frame and rename columns
      tfmn <- data.frame(field, tfmn); names(tfmn) <- c("field", "b", "m")
      tfsd <- data.frame(field, tfsd); names(tfsd) <- c("field", "b", "m")
      
      # Return results
      return(list(mean = tfmn, sd = tfsd))
    }
  }
  
  # Run trendline fit function
  tf.ow <- fitt(Cp.ow[which(Cp.ow$year >= tstart-1983 & Cp.ow$year <= tstop-1983),], type = "Cp")
  tf.gw <- fitt(Cp.gw[which(Cp.gw$year >= tstart-1983 & Cp.gw$year <= tstop-1983),], type = "Cp")
  tf.c1.ow <- fitt(c1.ow[which(c1.ow$year >= tstart-1983 & c1.ow$year <= tstop-1983),], type = "c1")
  tf.c1.gw <- fitt(c1.gw[which(c1.gw$year >= tstart-1983 & c1.gw$year <= tstop-1983),], type = "c1")
  
  # If plotting
  if (plot.flag == T) {
    
    # Start PDF
    pdf(file.path(path$plot, "Field level DCC distribution scatterplots 99-09.pdf"))
    par(mfrow = c(2, 2))
    
    # For each field
    for (i in 1:length(field)) {
      
      # Plot scatter and fits
      plot(meanlog~I(year+1983), xlim = c(1984, 2015), ylim = c(4,12), Cp.ow[Cp.ow$field == field[i],], xlab = "Year", ylab = "meanlog", main = paste("meanlog oil Cp value for field", field[i]))
      lines(year, with(tf.ow, meanlog$m[i]*(year-1983)+meanlog$b[i]), col = "red")
      
      plot(sdlog~I(year+1983),   xlim = c(1984, 2015), ylim = c(0,2), Cp.ow[Cp.ow$field == field[i],], xlab = "Year", ylab = "sdlog", main = paste("sdlog oil Cp value for field", field[i]))
      lines(year, with(tf.ow, sdlog$a[i]*exp(-sdlog$b[i]*(year-1983))), col = "red")
      
      plot(meanlog~I(year+1983), xlim = c(1984, 2015), ylim = c(4,12), Cp.gw[Cp.gw$field == field[i],], xlab = "Year", ylab = "meanlog", main = paste("meanlog gas Cp value for field", field[i]))
      lines(year, with(tf.gw, meanlog$m[i]*(year-1983)+meanlog$b[i]), col = "red")
      
      plot(sdlog~I(year+1983),   xlim = c(1984, 2015), ylim = c(0,2), Cp.gw[Cp.gw$field == field[i],], xlab = "Year", ylab = "sdlog", main = paste("sdlog gas Cp value for field", field[i]))
      lines(year, with(tf.gw, sdlog$a[i]*exp(-sdlog$b[i]*(year-1983))), col = "red")
      
      plot(mean~I(year+1983), xlim = c(1984, 2015), c1.ow[c1.ow$field == field[i],], xlab = "Year", ylab = "Mean", main = paste("Mean oil c1 value for field", field[i]))
      lines(year, with(tf.c1.ow, mean$m[i]*(year-1983)+mean$b[i]), col = "red")
      
      plot(sd~I(year+1983),   xlim = c(1984, 2015), c1.ow[c1.ow$field == field[i],], xlab = "Year", ylab = "SD", main = paste("SD oil c1 value for field", field[i]))
      lines(year, with(tf.c1.ow, sd$m[i]*(year-1983)+sd$b[i]), col = "red")
      
      plot(mean~I(year+1983), xlim = c(1984, 2015), c1.gw[c1.gw$field == field[i],], xlab = "Year", ylab = "Mean", main = paste("Mean gas c1 value for field", field[i]))
      lines(year, with(tf.c1.gw, mean$m[i]*(year-1983)+mean$b[i]), col = "red")
      
      plot(sd~I(year+1983),   xlim = c(1984, 2015), c1.gw[c1.gw$field == field[i],], xlab = "Year", ylab = "SD", main = paste("SD gas c1 value for field", field[i]))
      lines(year, with(tf.c1.gw, sd$m[i]*(year-1983)+sd$b[i]), col = "red")
    }
    
    # Close PDF
    dev.off()
  }
  
  
  # Global fit --------------------------------------------------------------
  
  # Repeat as above, but w/o differentiating between fields
  
  # Step 1: Fit distributions
  
  # Global distribution fit function
  gfitf <- function(data, plot.flag, type) {
    
    # Predefine output variables
    Cp <- NULL
    c1 <- NULL
    
    # For each year (i.e. value of dt)
    for (j in 1:max(data$dt)) {
      
      # Get subset to year
      ttemp <- subset(data, subset = (dt == j))
      
      # If subset of fits is large enough
      if (length(ttemp$dt) > min.rec.count) {
        
        # Fit distribution for Cp (log-normal) and c1 (normal)
        tparm.Cp <-   fitdist(ttemp$Cp.1, "lnorm")
        tparm.c1t1 <- fitdist(ttemp$c1.1, "norm")
        tparm.c1t2 <- fitdist(ttemp$c1.1, "cauchy")
        tparm.c1t3 <- fitdist(ttemp$c1.1, "logis")
        
        # If plotting
        if (plot.flag == T) {
          cdfcomp(tparm.Cp, main = paste(type, "Cp values for Basin for", year[j]), xlab = "Cp value", legendtext = "Log-Normal")
          cdfcomp(list(tparm.c1t1, tparm.c1t2, tparm.c1t3), main = paste(type, "c1 values for Basin for", year[j]), xlab = "c1 value", legendtext = c("Normal", "Cauchy", "Logistic"))
        }
        
        # Save out result
        Cp <- rbind(Cp, c(j, tparm.Cp$estimate))
        c1 <- rbind(c1, c(j, tparm.c1t1$estimate, tparm.c1t2$estimate, tparm.c1t3$estimate))
      }
    }
    
    # Return results
    return(list(Cp = Cp, c1 = c1))
  }
  
  # Call global fit function
  # If plotting, save to PDF
  if (plot.flag == T) {
    pdf(file.path(path$plot, "Basin level DCC distribution fits.pdf"), width = 14, height = 7)
    par(mfcol = c(1, 2))
  }
  
  # Run fit function
  gpar.ow <- gfitf(vow, plot.flag, "Oil")
  gpar.gw <- gfitf(vgw, plot.flag, "Gas")
  
  # If plotting, close PDF
  if (plot.flag == T) {
    dev.off()
  }
  
  # Extract results, change to data.frame, and rename
  gCp.ow <- gpar.ow$Cp; gCp.ow <- data.frame(gCp.ow); names(gCp.ow) <- c("year", "meanlog", "sdlog")
  gCp.gw <- gpar.gw$Cp; gCp.gw <- data.frame(gCp.gw); names(gCp.gw) <- c("year", "meanlog", "sdlog")
  gc1.ow <- gpar.ow$c1; gc1.ow <- data.frame(gc1.ow); names(gc1.ow) <- c("year", "mean", "sd", "loc1", "scale1", "loc2", "scale2")
  gc1.gw <- gpar.gw$c1; gc1.gw <- data.frame(gc1.gw); names(gc1.gw) <- c("year", "mean", "sd", "loc1", "scale1", "loc2", "scale2")
  
  
  # Step 2: Fit trendlines to distribution parameters
  
  # Subset data
  tgCp.ow <- gCp.ow[which(gCp.ow$year >= tstart-1983 & gCp.ow$year <= tstop-1983),]
  tgCp.gw <- gCp.gw[which(gCp.gw$year >= tstart-1983 & gCp.gw$year <= tstop-1983),]
  tgc1.ow <- gc1.ow[which(gc1.ow$year >= tstart-1983 & gc1.ow$year <= tstop-1983),]
  tgc1.gw <- gc1.gw[which(gc1.gw$year >= tstart-1983 & gc1.gw$year <= tstop-1983),]
  
  # Fit trendlines
  gtfmn.ow <- coefficients(lm(meanlog~year, tgCp.ow))
  gtfmn.gw <- coefficients(lm(meanlog~year, tgCp.gw))
  gtfsd.ow <- optim(par = c(1,0.02), fn = min.expRSS, time = tgCp.ow$year, obs = tgCp.ow$sdlog)$par
  gtfsd.gw <- optim(par = c(1,0.02), fn = min.expRSS, time = tgCp.gw$year, obs = tgCp.gw$sdlog)$par
  gtfmn.c1.ow <- coefficients(lm(mean~year, tgc1.ow))
  gtfmn.c1.gw <- coefficients(lm(mean~year, tgc1.gw))
  gtfsd.c1.ow <- coefficients(lm(sd~year, tgc1.ow))
  gtfsd.c1.gw <- coefficients(lm(sd~year, tgc1.gw))
  
  # Rename
  names(gtfmn.ow) <- c("b", "m")
  names(gtfmn.gw) <- c("b", "m")
  names(gtfsd.ow) <- c("a", "b")
  names(gtfsd.gw) <- c("a", "b")
  names(gtfmn.c1.ow) <- c("b", "m")
  names(gtfmn.c1.gw) <- c("b", "m")
  names(gtfsd.c1.ow) <- c("b", "m")
  names(gtfsd.c1.gw) <- c("b", "m")
  
  # If plotting
  if (plot.flag == T) {
    
    # Start PDF
    pdf(file.path(path$plot, "Basin level DCC distribution scatterplots 99-09.pdf"))
    par(mfrow = c(2, 2))
    
    # Plot scatter and fits
    plot(meanlog~I(year+1983), xlim = c(1984, 2015), ylim = c(4,12), gCp.ow, xlab = "Year", ylab = "meanlog", main = "meanlog oil Cp value for Basin")
    lines(year, (gtfmn.ow["m"]*(year-1983)+gtfmn.ow["b"]), col = "red")
    
    plot(sdlog~I(year+1983),   xlim = c(1984, 2015), ylim = c(0,2), gCp.ow, xlab = "Year", ylab = "sdlog", main = "sdlog oil Cp value for Basin")
    lines(year, (gtfsd.ow["a"]*exp(-gtfsd.ow["b"]*(year-1983))), col = "red")
    
    plot(meanlog~I(year+1983), xlim = c(1984, 2015), ylim = c(4,12), gCp.gw, xlab = "Year", ylab = "meanlog", main = "meanlog gas Cp value for Basin")
    lines(year, (gtfmn.gw["m"]*(year-1983)+gtfmn.gw["b"]), col = "red")
    
    plot(sdlog~I(year+1983),   xlim = c(1984, 2015), ylim = c(0,2), gCp.gw, xlab = "Year", ylab = "sdlog", main = "sdlog gas Cp value for Basin")
    lines(year, (gtfsd.gw["a"]*exp(-gtfsd.gw["b"]*(year-1983))), col = "red")
    
    plot(mean~I(year+1983), xlim = c(1984, 2015), gc1.ow, xlab = "Year", ylab = "Mean", main = "Mean oil c1 value for Basin")
    lines(year, (gtfmn.c1.ow["m"]*(year-1983)+gtfmn.c1.ow["b"]), col = "red")
    
    plot(sd~I(year+1983), xlim = c(1984, 2015), gc1.ow, xlab = "Year", ylab = "SD", main = "SD oil c1 value for Basin")
    lines(year, (gtfsd.c1.ow["m"]*(year-1983)+gtfsd.c1.ow["b"]), col = "red")
    
    plot(mean~I(year+1983), xlim = c(1984, 2015), gc1.gw, xlab = "Year", ylab = "Mean", main = "Mean gas c1 value for Basin")
    lines(year, (gtfmn.c1.gw["m"]*(year-1983)+gtfmn.c1.gw["b"]), col = "red")
    
    plot(sd~I(year+1983), xlim = c(1984, 2015), gc1.gw, xlab = "Year", ylab = "SD", main = "SD gas c1 value for Basin")
    lines(year, (gtfsd.c1.gw["m"]*(year-1983)+gtfsd.c1.gw["b"]), col = "red")
    
    # Close PDF
    dev.off()
  }
  
  
  # Save results ------------------------------------------------------------
  
  # Make results data.frame
  DCAlnormFit <- data.frame(field = 998, type = "oil", var = "Cp", param = "meanlog", p1 = gtfmn.ow[2], p2 = gtfmn.ow[1])
  DCAlnormFit <- rbind(DCAlnormFit,
                       c(998, "oil", "Cp", "sdlog",   gtfsd.ow),
                       c(998, "gas", "Cp", "meanlog", gtfmn.gw[2], gtfmn.gw[1]),
                       c(998, "gas", "Cp", "sdlog",   gtfsd.gw),
                       c(998, "oil", "c1", "mean",    gtfmn.c1.ow[2], gtfmn.c1.ow[1]),
                       c(998, "oil", "c1", "sd",      gtfsd.c1.ow[2], gtfsd.c1.ow[1]),
                       c(998, "gas", "c1", "mean",    gtfmn.c1.gw[2], gtfmn.c1.gw[1]),
                       c(998, "gas", "c1", "sd",      gtfsd.c1.gw[2], gtfsd.c1.gw[1]),
                       data.frame(field = tf.ow$meanlog$field, type = "oil", var = "Cp", param = "meanlog", p1 = tf.ow$meanlog$m, p2 = tf.ow$meanlog$b),
                       data.frame(field = tf.ow$sdlog$field,   type = "oil", var = "Cp", param = "sdlog",   p1 = tf.ow$sdlog$a,   p2 = tf.ow$sdlog$b),
                       data.frame(field = tf.gw$meanlog$field, type = "gas", var = "Cp", param = "meanlog", p1 = tf.gw$meanlog$m, p2 = tf.gw$meanlog$b),
                       data.frame(field = tf.gw$sdlog$field,   type = "gas", var = "Cp", param = "sdlog",   p1 = tf.gw$sdlog$a,   p2 = tf.gw$sdlog$b),
                       data.frame(field = tf.c1.ow$mean$field, type = "oil", var = "c1", param = "mean",    p1 = tf.c1.ow$mean$m, p2 = tf.c1.ow$mean$b),
                       data.frame(field = tf.c1.ow$sd$field,   type = "oil", var = "c1", param = "sd",      p1 = tf.c1.ow$sd$m,   p2 = tf.c1.ow$sd$b),
                       data.frame(field = tf.c1.gw$mean$field, type = "gas", var = "c1", param = "mean",    p1 = tf.c1.gw$mean$m, p2 = tf.c1.gw$mean$b),
                       data.frame(field = tf.c1.gw$sd$field,   type = "gas", var = "c1", param = "sd",      p1 = tf.c1.gw$sd$m,   p2 = tf.c1.gw$sd$b))
  
  # Change variable types
  DCAlnormFit$field <- as.integer(DCAlnormFit$field)
  DCAlnormFit$p1 <- as.numeric(DCAlnormFit$p1)
  DCAlnormFit$p2 <- as.numeric(DCAlnormFit$p2)
  
  save(file=file.path(path$data,
                      paste("DCAlnormFit_", ver, ".rda", sep = "")),
       list=c("DCAlnormFit"))
}