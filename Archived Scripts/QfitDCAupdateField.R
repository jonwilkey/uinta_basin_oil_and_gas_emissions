# Function Info -----------------------------------------------------------
# Name:      QfitDCAupdateField.R (Cumulative Production Field-Level Decline Curve Analysis)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# minDayProd - Minimum number of days of a well produced in a given month
# required to include production data point

# DCAplot - True/False flag indicating whether or not to print

# Cp.start.oil/gas - Initial guess value for Cp coefficient for oil/gas

# c1.start.oil/gas - Initial guess value for c1 coefficient for oil/gas

# Qlower.oil/gas - Lower limits for NLS for oil/gas decline curve for (Cp, c1) 
# coefficients

# Qupper.oil/gas - Upper limits for NLS for oil/gas decline curve for (Cp, c1)
# coefficients

# field - List of fields to be analyzed individually

# ver - Version number for results version tracking

# path - path names for file directoires (data, plotting, etc.)

# p - production database

# quant - numeric vector giving the probabilities at which to get quantile
# estimates for production levels in each field (used for plotting only)

# tstart - start time cutoff, only p_rpt_period values >= this date will be
# included in DCA

# tstop - stop time cutoff, only p_rpt_period values <= this date will be
# included in DCA


# Outputs -----------------------------------------------------------------

# QFF - data.frame containing cumulative production coefficients for oil and gas
# production for each field


# Description -------------------------------------------------------------

# This function fits the cumulative production function:

# Q(t) = Cp * sqrt(t) + c1

# to all oil and gas production records in the Uinta Basin by field.


# Function ----------------------------------------------------------------
QDCAupdateField <- function(minDayProd, DCAplot, Cp.start.oil, c1.start.oil,
                            Qlower.oil, Qupper.oil, Cp.start.gas, c1.start.gas,
                            Qlower.gas, Qupper.gas, field, ver, path, p, quant,
                            tstart, tstop) {
  
  # Internal Debug Variables  -----------------------------------------------
  
  # # Uncomment if running as script
  # minDayProd   <- opt$minDayProd
  # field        <- field
  # DCAplot      <- opt$DCAplot
  # ver          <- opt$file_ver
  # quant        <- opt$quant
  # tstart       <- opt$FDC.tstart
  # tstop        <- opt$FDC.tstop
  # Cp.start.oil <- opt$Cp.start.oil
  # Cp.start.gas <- opt$Cp.start.gas
  # c1.start.oil <- opt$c1.start.oil
  # c1.start.gas <- opt$c1.start.gas
  # Qlower.oil   <- opt$Qlower.oil
  # Qupper.oil   <- opt$Qupper.oil
  # Qlower.gas   <- opt$Qlower.gas
  # Qupper.gas   <- opt$Qupper.gas
  
  
  # Subset data -------------------------------------------------------------
  
  ps <- subset(p,
               subset = (time != 0 &
                           (p$h_well_type == "OW" |
                              p$h_well_type == "GW") &
                           p_rpt_period >= tstart &
                           p_rpt_period <= tstop),
               select = c("p_api",
                          "coil_prod",
                          "cgas_prod",
                          "time",
                          "w_field_num"))
  
  
  # Plotting to PDF ---------------------------------------------------------
  
  # If printing, initialize PDF devices and set global printing options
  if (DCAplot == TRUE) {
    
    # Call PDF printer
    pdf(file.path(path$plot, paste("Field level DCA Qfits ", ver, ".pdf", sep = "")))
    
    # Set linecolors for quantile outputs
    linecolor <- rainbow(length(quant))
    
    # Create internal plot function
    DCAfieldplot <- function(type, field) {
      
      # If type is oil
      if (type == "oil") {
        label.y <-    "Cumulative Oil Production (bbl)"
        label.main <- paste("Cumulative Fit for Oil from Field ", field)
      } else {
        label.y <-    "Cumulative Gas Production (MCF)"
        label.main <- paste("Cumulative Fit for Gas from Field ", field)
      }
      
      # Set time vector
      tv <- 1:max(w$time)
      
      # Get quantiles for field
      wq <- matrix(0, nrow = length(quant), ncol = max(w$time))
      for (j in 1:max(w$time)) {
        wq[,j] <- quantile(w[,2][w$time == j], quant)
      }
      
      # Main Plot
      plot(tv, wq[1,],
           ylim = c(0.9*min(wq, na.rm = T), 1.1*max(wq, na.rm = T)),
           type = "l",
           col = linecolor[1],
           xlab = "Time since first production (months)",
           ylab = label.y,
           main = label.main)
      
      # Other quantiles
      for (k in 2:length(quant)) {
        lines(tv, wq[k,], col = linecolor[k])
      }
      
      # Cumulative fit
      lines(tv, (coef(Qfit)[1]*sqrt(tv)+coef(Qfit)[2]),
            col = "black",
            lwd = 1.5)
      
      # Legend
      legend("topleft", c("90%", "70%", "50%", "30%", "10%", "Fit"),
             lty = 1,
             lwd = c(rep(1, times = length(quant)), 1.5),
             col = c(linecolor, "black"),
             ncol = 2)
    }
  }
  
  
  # DCA Fitting - Oil -------------------------------------------------------
  
  # Predefine index of wells to skip for generating list of wells located in
  # Field 999
  ind999 <- NULL
  
  # Predefine fit results vectors
  Cp.oil <- NULL
  c1.oil  <- NULL
  
  # For each field to be analyzed individually
  for (i in 1:(length(field)-1)) {
    
    # Get row indices of wells located just in field[i]
    ind <- which(ps$w_field_num == field[i])
    
    # Pull subset of production records on rows in 'ind'
    w <- ps[ind, c("time", "coil_prod")]
    
    # Only keep productions records which are > 0
    w <- w[w$coil_prod > 0,]
    
    # Run nlsLM for cumulative production curve fit
    Qfit <- nlsLM(formula = coil_prod ~ Cp*sqrt(time)+c1,
                  data =    w,
                  start =   list(Cp = Cp.start.oil, c1 = c1.start.oil),
                  lower =   Qlower.oil,
                  upper =   Qupper.oil,
                  control = list(maxiter=1000))
    
    # Save fit coefficients
    Cp.oil <- c(Cp.oil, coef(Qfit)[1])
    c1.oil <- c(c1.oil, coef(Qfit)[2])
    
    # Build index of non-Field 999 rows
    ind999 <- c(ind999, ind)
    
    # If plotting, do so now
    if (DCAplot == TRUE) {
      
      # Plot using custom print function
      DCAfieldplot(type = "oil", field = field[i])
    }
  }
  
  # For Field 999, use ind999 to exclude all rows associated with field being
  # analyzed individually.
  ind <- 1:nrow(ps)
  ind <- ind[-ind999]
  
  # Pull subset of production records on rows in 'ind'
  w <- ps[ind, c("time", "coil_prod")]
  
  # Only keep productions records which are > 0
  w <- w[w$coil_prod > 0,]
  
  # Run nlsLM for cumulative production curve fit
  Qfit <- nlsLM(formula = coil_prod ~ Cp*sqrt(time)+c1,
                data =    w,
                start =   list(Cp = Cp.start.oil, c1 = c1.start.oil),
                lower =   Qlower.oil,
                upper =   Qupper.oil,
                control = list(maxiter=1000))
  
  # Save fit coefficients
  Cp.oil <- c(Cp.oil, coef(Qfit)[1])
  c1.oil <- c(c1.oil, coef(Qfit)[2])
  
  # If plotting, do so now
  if (DCAplot == TRUE) {
    
    # Plot using custom print function
    DCAfieldplot(type = "oil", field = field[i+1])
  }
  
  
  # DCA Fitting - Gas -------------------------------------------------------
  
  # Predefine index of wells to skip for generating list of wells located in
  # Field 999
  ind999 <- NULL
  
  # Predefine fit results vectors
  Cp.gas <- NULL
  c1.gas <- NULL
  
  # For each field to be analyzed individually
  for (i in 1:(length(field)-1)) {
    
    # Get row indices of wells located just in field[i]
    ind <- which(ps$w_field_num == field[i])
    
    # Pull subset of production records on rows in 'ind'
    w <- ps[ind, c("time", "cgas_prod")]
    
    # Only keep productions records which are > 0
    w <- w[w$cgas_prod > 0,]
    
    # Run nlsLM for cumulative production curve fit
    Qfit <- nlsLM(formula = cgas_prod ~ Cp*sqrt(time)+c1,
                  data =    w,
                  start =   list(Cp = Cp.start.gas, c1 = c1.start.gas),
                  lower =   Qlower.gas,
                  upper =   Qupper.gas,
                  control = list(maxiter=1000))
    
    # Save fit coefficients
    Cp.gas <- c(Cp.gas, coef(Qfit)[1])
    c1.gas <- c(c1.gas, coef(Qfit)[2])
    
    # Build index of non-Field 999 rows
    ind999 <- c(ind999, ind)
    
    # If plotting, do so now
    if (DCAplot == TRUE) {
      
      # Plot using custom print function
      DCAfieldplot(type = "gas", field = field[i])
    }
  }
  
  # For Field 999, use ind999 to exclude all rows associated with field being
  # analyzed individually.
  ind <- 1:nrow(ps)
  ind <- ind[-ind999]
  
  # Pull subset of production records on rows in 'ind'
  w <- ps[ind, c("time", "cgas_prod")]
  
  # Only keep productions records which are > 0
  w <- w[w$cgas_prod > 0,]
  
  # Run nlsLM for cumulative production curve fit
  Qfit <- nlsLM(formula = cgas_prod ~ Cp*sqrt(time)+c1,
                data =    w,
                start =   list(Cp = Cp.start.gas, c1 = c1.start.gas),
                lower =   Qlower.gas,
                upper =   Qupper.gas,
                control = list(maxiter=1000))
  
  # Save fit coefficients
  Cp.gas <- c(Cp.gas, coef(Qfit)[1])
  c1.gas <- c(c1.gas, coef(Qfit)[2])
  
  # If plotting, do so now
  if (DCAplot == TRUE) {
    
    # Plot using custom print function
    DCAfieldplot(type = "gas", field = field[i+1])
    
    # Since this is the last plot, close the printer
    dev.off()
  }
  
  
  # Export Results ----------------------------------------------------------
  
  # Make results data.frame
  QFF <- data.frame(field, Cp.oil, c1.oil, Cp.gas, c1.gas)
  
  # Save fit results
  save(file=file.path(path$data,
                      paste("DCA_field_Qfits_", ver, ".rda", sep = "")),
       list=c("QFF"))
}