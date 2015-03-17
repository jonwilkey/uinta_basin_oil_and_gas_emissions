# Function Info -----------------------------------------------------------
# Name:      DCAupdateField.R (Field-Level Decline Curve Analysis)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# minDayProd - Minimum number of days of a well produced in a given month
# required to include production data point

# DCAplot - True/False flag indicating whether or not to print

# b.start.oil - Initial guess value for b coefficient for oil decline curve

# Di.start.oil - Initial guess value for Di coefficient for oil decline curve

# lower.oil - Lower limits for NLS for oil decline curve for (qo, b, Di)
# coefficients

# upper.oil - Upper limits for NLS for oil decline curve for (qo, b, Di)
# coefficients

# b.start.gas - Initial guess value for b coefficient for gas decline curve

# Di.start.gas - Initial guess value for Di coefficient for gas decline curve

# lower.gas - Lower limits for NLS for gas decline curve for (qo, b, Di)
# coefficients

# upper.gas - Upper limits for NLS for gas decline curve for (qo, b, Di)
# coefficients

# field - List of fields to be analyzed individually

# ver - Version number for results version tracking

# path - path names for file directoires (data, plotting, etc.)

# p - production database

# quant - numeric vector giving the probabilities at which to get quantile
# estimates for production levels in each field (used for plotting only)


# Outputs -----------------------------------------------------------------

# hypFF - data.frame containing hyperbolic decline curve coefficients for oil
# and gas production for each field


# Description -------------------------------------------------------------

# This function fits the hyperbolic decline curve function:

# q(t) = qo * (1 + b * Di * t) ^ (-1 / b)

# to all oil and gas production records in the Uinta Basin by field.


# Function ----------------------------------------------------------------
DCAupdateField <- function(minDayProd, DCAplot, b.start.oil, Di.start.oil,
                           lower.oil, upper.oil, b.start.gas, Di.start.gas,
                           lower.gas, upper.gas, field, ver, path, p, quant) {
  
  # Internal Debug Variables  -----------------------------------------------
  
  # # Uncomment if running as script
  # minDayProd   <- opt$minDayProd
  # field        <- field
  # DCAplot      <- opt$DCAplot
  # ver          <- opt$file_ver
  # b.start.oil  <- opt$b.start.oil
  # Di.start.oil <- opt$Di.start.oil
  # lower.oil    <- opt$lower.oil
  # upper.oil    <- opt$upper.oil
  # quant        <- opt$quant
  # b.start.gas  <- opt$b.start.gas
  # Di.start.gas <- opt$Di.start.gas
  # lower.gas    <- opt$lower.gas
  # upper.gas    <- opt$upper.gas
  # tstart       <- opt$tstart
  
  
  # Subset data -------------------------------------------------------------
  
  ps <- subset(p,
               subset = (time != 0 &
                           (p$h_well_type == "OW" |
                              p$h_well_type == "GW")),
               select = c("p_api",
                          "p_oil_prod",
                          "p_gas_prod",
                          "time",
                          "w_field_num"))
  
  
  # Plotting to PDF ---------------------------------------------------------
  
  # If printing, initialize PDF devices and set global printing options
  if (DCAplot == TRUE) {
    
    # Call PDF printer
    pdf(file.path(path$plot, paste("Field level DCA fits ", ver, ".pdf", sep = "")))
    
    # Set linecolors for quantile outputs
    linecolor <- rainbow(length(quant))
    
    # Create internal plot function
    DCAfieldplot <- function(type, field) {
      
      # If type is oil
      if (type == "oil") {
        label.y <-    "Oil Production (bbl)"
        label.main <- paste("Hyperbolic Fit for Oil from Field ", field)
      } else {
        label.y <-    "Gas Production (MCF)"
        label.main <- paste("Hyperbolic Fit for Gas from Field ", field)
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
      
      # Hyperbolic fit
      lines(tv, (coef(hyp)[1]*(1+coef(hyp)[2]*coef(hyp)[3]*tv)^(-1/coef(hyp)[2])),
            col = "black",
            lwd = 1.5)
      
      # Legend
      legend("topright", c("90%", "70%", "50%", "30%", "10%", "Fit"),
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
  qo.oil <- NULL
  b.oil  <- NULL
  Di.oil <- NULL
  
  # For each field to be analyzed individually
  for (i in 1:(length(field)-1)) {
    
    # Get row indices of wells located just in field[i]
    ind <- which(ps$w_field_num == field[i])
    
    # Pull subset of production records on rows in 'ind'
    w <- ps[ind, c("time", "p_oil_prod")]
    
    # Only keep productions records which are > 0
    w <- w[w$p_oil_prod > 0,]
    
    # Run nlsLM for hyperbolic decline curve fit
    hyp <- nlsLM(formula = p_oil_prod ~ qo*(1+b*Di*time)^(-1/b),
                 data =    w,
                 start =   list(qo = max(w$p_oil_prod), b = b.start.oil, Di = Di.start.oil),
                 lower =   lower.oil,
                 upper =   upper.oil,
                 control = list(maxiter=1000))
    
    # Save fit coefficients
    qo.oil <- c(qo.oil, coef(hyp)[1])
    b.oil  <- c(b.oil,  coef(hyp)[2])
    Di.oil <- c(Di.oil, coef(hyp)[3])
    
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
  w <- ps[ind, c("time", "p_oil_prod")]
  
  # Only keep productions records which are > 0
  w <- w[w$p_oil_prod > 0,]
  
  # Run nlsLM for hyperbolic decline curve fit
  hyp <- nlsLM(formula = p_oil_prod ~ qo*(1+b*Di*time)^(-1/b),
               data =    w,
               start =   list(qo = max(w$p_oil_prod), b = b.start.oil, Di = Di.start.oil),
               lower =   lower.oil,
               upper =   upper.oil,
               control = list(maxiter=1000))
  
  # Save fit coefficients
  qo.oil <- c(qo.oil, coef(hyp)[1])
  b.oil  <- c(b.oil,  coef(hyp)[2])
  Di.oil <- c(Di.oil, coef(hyp)[3])
  
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
  qo.gas <- NULL
  b.gas  <- NULL
  Di.gas <- NULL
  
  # For each field to be analyzed individually
  for (i in 1:(length(field)-1)) {
    
    # Get row indices of wells located just in field[i]
    ind <- which(ps$w_field_num == field[i])
    
    # Pull subset of production records on rows in 'ind'
    w <- ps[ind, c("time", "p_gas_prod")]
    
    # Only keep productions records which are > 0
    w <- w[w$p_gas_prod > 0,]
    
    # Run nlsLM for hyperbolic decline curve fit
    hyp <- nlsLM(formula = p_gas_prod ~ qo*(1+b*Di*time)^(-1/b),
                 data =    w,
                 start =   list(qo = max(w$p_gas_prod), b = b.start.gas, Di = Di.start.gas),
                 lower =   lower.gas,
                 upper =   upper.gas,
                 control = list(maxiter=1000))
    
    # Save fit coefficients
    qo.gas <- c(qo.gas, coef(hyp)[1])
    b.gas  <- c(b.gas,  coef(hyp)[2])
    Di.gas <- c(Di.gas, coef(hyp)[3])
    
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
  w <- ps[ind, c("time", "p_gas_prod")]
  
  # Only keep productions records which are > 0
  w <- w[w$p_gas_prod > 0,]
  
  # Run nlsLM for hyperbolic decline curve fit
  hyp <- nlsLM(formula = p_gas_prod ~ qo*(1+b*Di*time)^(-1/b),
               data =    w,
               start =   list(qo = max(w$p_gas_prod), b = b.start.gas, Di = Di.start.gas),
               lower =   lower.gas,
               upper =   upper.gas,
               control = list(maxiter=1000))
  
  # Save fit coefficients
  qo.gas <- c(qo.gas, coef(hyp)[1])
  b.gas  <- c(b.gas,  coef(hyp)[2])
  Di.gas <- c(Di.gas, coef(hyp)[3])
  
  # If plotting, do so now
  if (DCAplot == TRUE) {
    
    # Plot using custom print function
    DCAfieldplot(type = "gas", field = field[i+1])
    
    # Since this is the last plot, close the printer
    dev.off()
  }
  
  
  # Export Results ----------------------------------------------------------
  
  # Make results data.frame
  hypFF <- data.frame(field, qo.oil, b.oil, Di.oil, qo.gas, b.gas, Di.gas)
  
  # Save fit results
  save(file=file.path(path$data,
                      paste("DCA_field_fits_", ver, ".rda", sep = "")),
       list=c("hypFF"))
}