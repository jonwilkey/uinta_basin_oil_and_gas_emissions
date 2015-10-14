# Function Info -----------------------------------------------------------
# Name:      bplotHypDCAcoef.R (Boxplot for hyperbolic DCA coefficients)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# None - hardcoded


# Outputs -----------------------------------------------------------------

# Set of boxplots for coefficients in hyperbolic decline curve equation as given
# by DCAupdate results matrices mo and mg.


# Function ----------------------------------------------------------------
bplotHypDCAcoef <- function() {
  
  # Only fitted wells
  hmo <- mo[which(mo$fit.1 == 1),]
  hmg <- mg[which(mg$fit.1 == 1),]
  
  
  # Data for oil ------------------------------------------------------------
  
  # Predefine ind vector
  ind <- NULL
  
  # Get row indices of fields which we analyzed individually
  for (k in 1:(length(field)-1)) {
    
    ind <- c(ind, which(hmo$w_field_num == field[k]))
  }
  
  # Anything not in ind is field 999, overwrite their field numbers
  hmo$w_field_num[-ind] <- 999
  
  # Make data.frame of field and DCA coefficient values, then rename
  ocf <- data.frame(hmo$w_field_num,
                    hmo$qo.1,
                    hmo$b.1,
                    hmo$Di.1,
                    hmo$tdelay)
  names(ocf) <- c("field", "qo", "b", "Di", "td")
  
  
  # Data for gas ------------------------------------------------------------
  
  # Predefine ind vector
  ind <- NULL
  
  # Get row indices of fields which we analyzed individually
  for (k in 1:(length(field)-1)) {
    
    ind <- c(ind, which(hmg$w_field_num == field[k]))
  }
  
  # Anything not in ind is field 999, overwrite their field numbers
  hmg$w_field_num[-ind] <- 999
  
  # Make data.frame of field and DCA coefficient values, then rename
  gcf <- data.frame(hmg$w_field_num,
                    hmg$qo.1,
                    hmg$b.1,
                    hmg$Di.1,
                    hmg$tdelay)
  names(gcf) <- c("field", "qo", "b", "Di", "td")
  
  
  # Plots for oil -----------------------------------------------------------
  
  # qo for oil
  boxplot(qo ~ field,
          ocf,
          log = "y",
          range = 0,
          ylim = c(1, 1.1*max(ocf$qo)),
          xlab = "Field Number",
          ylab = "qo value for oil (bbl)",
          main = "Oil qo values")
  
  # b for oil
  boxplot(b ~ field,
          ocf,
          range = 0,
          ylim = c(0, 1.1*max(ocf$b)),
          xlab = "Field Number",
          ylab = "b value for oil (dimensionless)",
          main = "Oil b values")
  
  # Di for oil
  boxplot(Di ~ field,
          ocf,
          log = "y",
          range = 0,
          ylim = c(0.001, 1.1*max(ocf$Di)),
          xlab = "Field Number",
          ylab = "Di value for oil (dimensionless)",
          main = "Oil Di values")
  
  # td for oil
  boxplot(td ~ field,
          ocf,
          range = 0,
          #ylim = c(0, 30),
          xlab = "Field Number",
          ylab = "Time delay value for oil production (months)",
          main = "Oil Production Time Delay Values")
  
  
  # Plots for gas -----------------------------------------------------------
  
  # qo for gas
  boxplot(qo ~ field,
          gcf,
          log = "y",
          range = 0,
          ylim = c(1, 1.1*max(gcf$qo)),
          xlab = "Field Number",
          ylab = "qo value for gas (MCF)",
          main = "Gas qo values")
  
  # b for gas
  boxplot(b ~ field,
          gcf,
          range = 0,
          ylim = c(0, 1.1*max(gcf$b)),
          xlab = "Field Number",
          ylab = "b value for gas (dimensionless)",
          main = "Gas b values")
  
  # Di for gas
  boxplot(Di ~ field,
          gcf,
          log = "y",
          range = 0,
          ylim = c(0.001, 1.1*max(gcf$Di)),
          xlab = "Field Number",
          ylab = "Di value for gas (dimensionless)",
          main = "Gas Di values")
  
  # td for oil
  boxplot(td ~ field,
          gcf,
          range = 0,
          #ylim = c(0, 30),
          xlab = "Field Number",
          ylab = "Time delay value for gas production (months)",
          main = "Gas Production Time Delay Values")
  
  # Cleanup -----------------------------------------------------------
  
  # Remove all temporary variables in this script
  remove(hmo, hmg, k, ocf, gcf)
}