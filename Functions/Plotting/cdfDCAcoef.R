# Function Info -----------------------------------------------------------
# Name:      cdfDCAcoef.R (DCA coefficient plots)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# hyp - logical indicating whether or not to plot CDFs for hyperbolic
# coefficients

# Qfit - logical indicating whether or not to plot CDFs for cumulative
# coefficients


# Outputs -----------------------------------------------------------------

# Set of CDF plots for coefficients in both the hyperbolic decline curve and 
# cumulative production equations as given by DCAupdateCDF and QfitDCAupdateCDF
# functions


# Function ----------------------------------------------------------------
cdfDCAcoef <- function(hyp, Qfit) {
  
  # Set line colors for quantiles used in quant
  flinecolor <- rainbow(length(field))
  
  # If hyp is TRUE, plot hyperbolic fits
  if (hyp) {
    
    # Hyp plots for oil -------------------------------------------------------
    
    # Main plot - qo for oil
    plot(DCA.cdf.coef.oil[[1]],
         type = "l",
         col = flinecolor[1],
         xlab = "qo value for oil (bbl/month)",
         ylab = "Cumulative Probability",
         main = "CDF for qo for Oil Production - by Field")
    
    # Lines for other fields
    for (i in 2:length(field)) {
      lines(DCA.cdf.coef.oil[[((i-1)*4+1)]], col = flinecolor[i])
    }
    
    # Legend
    legend("bottomright", as.character(field), ncol = 2, col = c(flinecolor), lty = 1)
    
    # Main plot - b for oil
    plot(DCA.cdf.coef.oil[[2]],
         type = "l",
         col = flinecolor[1],
         xlab = "b value for oil (dimensionless)",
         ylab = "Cumulative Probability",
         main = "CDF for b for Oil Production - by Field")
    
    # Lines for other fields
    for (i in 2:length(field)) {
      lines(DCA.cdf.coef.oil[[((i-1)*4+2)]], col = flinecolor[i])
    }
    
    # Legend
    legend("bottomright", as.character(field), ncol = 2, col = c(flinecolor), lty = 1)
    
    # Main plot - Di for oil
    plot(DCA.cdf.coef.oil[[3]],
         type = "l",
         col = flinecolor[1],
         xlab = "Di value for oil (dimensionless)",
         ylab = "Cumulative Probability",
         main = "CDF for Di for Oil Production - by Field")
    
    # Lines for other fields
    for (i in 2:length(field)) {
      lines(DCA.cdf.coef.oil[[((i-1)*4+3)]], col = flinecolor[i])
    }
    
    # Legend
    legend("bottomright", as.character(field), ncol = 2, col = c(flinecolor), lty = 1)
    
    # Hyp plots for gas -------------------------------------------------------
    
    # Main plot - qo for gas
    plot(DCA.cdf.coef.gas[[1]],
         type = "l",
         col = flinecolor[1],
         xlab = "qo value for gas (MCF/month)",
         ylab = "Cumulative Probability",
         main = "CDF for qo for gas Production - by Field")
    
    # Lines for other fields
    for (i in 2:length(field)) {
      lines(DCA.cdf.coef.gas[[((i-1)*4+1)]], col = flinecolor[i])
    }
    
    # Legend
    legend("bottomright", as.character(field), ncol = 2, col = c(flinecolor), lty = 1)
    
    # Main plot - b for gas
    plot(DCA.cdf.coef.gas[[2]],
         type = "l",
         col = flinecolor[1],
         xlab = "b value for gas (dimensionless)",
         ylab = "Cumulative Probability",
         main = "CDF for b for Gas Production - by Field")
    
    # Lines for other fields
    for (i in 2:length(field)) {
      lines(DCA.cdf.coef.gas[[((i-1)*4+2)]], col = flinecolor[i])
    }
    
    # Legend
    legend("bottomright", as.character(field), ncol = 2, col = c(flinecolor), lty = 1)
    
    # Main plot - Di for gas
    plot(DCA.cdf.coef.gas[[3]],
         type = "l",
         col = flinecolor[1],
         xlab = "Di value for gas (dimensionless)",
         ylab = "Cumulative Probability",
         main = "CDF for Di for Gas Production - by Field")
    
    # Lines for other fields
    for (i in 2:length(field)) {
      lines(DCA.cdf.coef.gas[[((i-1)*4+3)]], col = flinecolor[i])
    }
    
    # Legend
    legend("bottomright", as.character(field), ncol = 2, col = c(flinecolor), lty = 1)
  }
  
  # If Qfit is TRUE, then plot cumulative fits
  if (Qfit) {
    
    # Qfit plots for oil ------------------------------------------------------
    
    # Main plot - Cp for oil
    plot(Q.DCA.cdf.coef.oil[[1]],
         type = "l",
         col = flinecolor[1],
         xlim = c(opt$Q.cdf.oil.from[1], opt$Q.cdf.oil.to[1]),
         xlab = "Cp value for oil (bbl/month^0.5)",
         ylab = "Cumulative Probability",
         main = "CDF for Cp for Oil Production - by Field")
    
    # Lines for other fields
    for (i in 2:length(field)) {
      lines(Q.DCA.cdf.coef.oil[[(i*2-1)]], col = flinecolor[i])
    }
    
    # Legend
    legend("bottomright", as.character(field), ncol = 2, col = c(flinecolor), lty = 1)
    
    # Main plot - c1 for oil
    plot(Q.DCA.cdf.coef.oil[[2]],
         type = "l",
         col = flinecolor[1],
         xlim = c(opt$Q.cdf.oil.from[2], opt$Q.cdf.oil.to[2]),
         xlab = "c1 value for oil (bbl/month)",
         ylab = "Cumulative Probability",
         main = "CDF for c1 for Oil Production - by Field")
    
    # Lines for other fields
    for (i in 2:length(field)) {
      lines(Q.DCA.cdf.coef.oil[[(i*2)]], col = flinecolor[i])
    }
    
    # Legend
    legend("topleft", as.character(field), ncol = 2, col = c(flinecolor), lty = 1)
    
    
    # Qfit plots for gas ------------------------------------------------------
    
    # Main plot - Cp for gas
    plot(Q.DCA.cdf.coef.gas[[1]],
         type = "l",
         col = flinecolor[1],
         xlim = c(opt$Q.cdf.gas.from[1], opt$Q.cdf.gas.to[1]),
         xlab = "Cp value for gas (MCF/month^0.5)",
         ylab = "Cumulative Probability",
         main = "CDF for Cp for Gas Production - by Field")
    
    # Lines for other fields
    for (i in 2:length(field)) {
      lines(Q.DCA.cdf.coef.gas[[(i*2-1)]], col = flinecolor[i])
    }
    
    # Legend
    legend("bottomright", as.character(field), ncol = 2, col = c(flinecolor), lty = 1)
    
    # Main plot - c1 for gas
    plot(Q.DCA.cdf.coef.gas[[2]],
         type = "l",
         col = flinecolor[1],
         xlim = c(opt$Q.cdf.gas.from[2], opt$Q.cdf.gas.to[2]),
         xlab = "c1 value for gas (bbl/month)",
         ylab = "Cumulative Probability",
         main = "CDF for c1 for Gas Production - by Field")
    
    # Lines for other fields
    for (i in 2:length(field)) {
      lines(Q.DCA.cdf.coef.gas[[(i*2)]], col = flinecolor[i])
    }
    
    # Legend
    legend("topleft", as.character(field), ncol = 2, col = c(flinecolor), lty = 1)
  }
  
  # Main plot - td for oil
  plot(DCA.cdf.coef.oil[[4]],
       type = "l",
       col = flinecolor[1],
       xlab = "Time Delay between drilling and start of production for oil (months)",
       ylab = "Cumulative Probability",
       main = "CDF for Time Delay for Oil Production - by Field")
  
  # Lines for other fields
  for (i in 2:length(field)) {
    lines(DCA.cdf.coef.oil[[((i-1)*4+4)]], col = flinecolor[i])
  }
  
  # Legend
  legend("bottomright", as.character(field), ncol = 2, col = c(flinecolor), lty = 1)
  
  # Main plot - td for gas
  plot(DCA.cdf.coef.gas[[4]],
       type = "l",
       col = flinecolor[1],
       xlab = "Time Delay between drilling and start of production for gas (months)",
       ylab = "Cumulative Probability",
       main = "CDF for Time Delay for Gas Production - by Field")
  
  # Lines for other fields
  for (i in 2:length(field)) {
    lines(DCA.cdf.coef.gas[[((i-1)*4+4)]], col = flinecolor[i])
  }
  
  # Legend
  legend("bottomright", as.character(field), ncol = 2, col = c(flinecolor), lty = 1)
}