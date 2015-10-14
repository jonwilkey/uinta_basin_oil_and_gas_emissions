# Function Info -----------------------------------------------------------
# Name:      bplotQfitDCAcoef.R (Boxplot for cumulative DCA coefficients)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# None - hardcoded


# Outputs -----------------------------------------------------------------

# Set of boxplots for coefficients in cumulative production equation as given
# by DCAupdate results matrices mo and mg.


# Function ----------------------------------------------------------------
bplotQfitDCAcoef <- function() {
  
  # Only fitted wells
  qmo <- mo[which(mo$Qfit.1 == 1),]
  qmg <- mg[which(mg$Qfit.1 == 1),]
  
  
  # Data for oil ------------------------------------------------------------
  
  # Predefine ind vector
  ind <- NULL
  
  # Get row indices of fields which we analyzed individually
  for (k in 1:(length(field)-1)) {
    
    ind <- c(ind, which(qmo$w_field_num == field[k]))
  }
  
  # Anything not in ind is field 999, overwrite their field numbers
  qmo$w_field_num[-ind] <- 999
  
  # Make data.frame of field and DCA coefficient values, then rename
  ocf <- data.frame(qmo$w_field_num,
                    qmo$Cp.1,
                    qmo$c1.1)
  names(ocf) <- c("field", "Cp", "c1")
  
  
  # Data for gas ------------------------------------------------------------
  
  # Predefine ind vector
  ind <- NULL
  
  # Get row indices of fields which we analyzed individually
  for (k in 1:(length(field)-1)) {
    
    ind <- c(ind, which(qmg$w_field_num == field[k]))
  }
  
  # Anything not in ind is field 999, overwrite their field numbers
  qmg$w_field_num[-ind] <- 999
  
  # Make data.frame of field and DCA coefficient values, then rename
  gcf <- data.frame(qmg$w_field_num,
                    qmg$Cp.1,
                    qmg$c1.1)
  names(gcf) <- c("field", "Cp", "c1")
  
  
  # Plots for oil -----------------------------------------------------------
  
  # Cp for oil
  boxplot(Cp ~ field,
          ocf,
          #log = "y",
          range = 0,
          #ylim = c(1, 1.1*max(ocf$Cp)),
          xlab = "Field Number",
          ylab = "Cp value for oil (bbl/month^0.5)",
          main = "Oil Cp values")
  
  # c1 for oil
  boxplot(c1 ~ field,
          ocf,
          range = 0,
          #ylim = c(0, 1.1*max(ocf$c1)),
          xlab = "Field Number",
          ylab = "c1 value for oil (bbl)",
          main = "Oil c1 values")
  
  
  # Plots for gas -----------------------------------------------------------
  
  # qo for gas
  boxplot(Cp ~ field,
          gcf,
          #log = "y",
          range = 0,
          #ylim = c(1, 1.1*max(gcf$Cp)),
          xlab = "Field Number",
          ylab = "Cp value for gas (MCF/month^0.5)",
          main = "Gas Cp values")
  
  # c1 for gas
  boxplot(c1 ~ field,
          gcf,
          range = 0,
          #ylim = c(0, 1.1*max(gcf$c1)),
          xlab = "Field Number",
          ylab = "c1 value for gas (MCF)",
          main = "Gas c1 values")
  
  # Plots for gas -----------------------------------------------------------
  
  # Remove temporary variables in this script
  remove(qmo, qmg, k, ocf, gcf)
}