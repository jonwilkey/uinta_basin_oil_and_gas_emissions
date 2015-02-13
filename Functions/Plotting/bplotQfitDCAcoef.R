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
  
  # Get row index of fits for fields we're specifically fitting
  ind <- which(qmo$w_field_num == opt$field[1] |
                 qmo$w_field_num == opt$field[2] |
                 qmo$w_field_num == opt$field[3] |
                 qmo$w_field_num == opt$field[4] |
                 qmo$w_field_num == opt$field[5] |
                 qmo$w_field_num == opt$field[6] |
                 qmo$w_field_num == opt$field[7] |
                 qmo$w_field_num == opt$field[8] |
                 qmo$w_field_num == opt$field[9] |
                 qmo$w_field_num == opt$field[10])
  
  # Make data.frame of field and DCA coefficient values, then rename
  ocf <- data.frame(qmo$w_field_num[ind],
                    qmo$Cp.1[ind],
                    qmo$c1.1[ind])
  names(ocf) <- c("field", "Cp", "c1")
  
  
  # Data for gas ------------------------------------------------------------
  
  # Get row index of fits for fields we're specifically fitting
  ind <- which(mg$w_field_num == opt$field[1] |
                 mg$w_field_num == opt$field[2] |
                 mg$w_field_num == opt$field[3] |
                 mg$w_field_num == opt$field[4] |
                 mg$w_field_num == opt$field[5] |
                 mg$w_field_num == opt$field[6] |
                 mg$w_field_num == opt$field[7] |
                 mg$w_field_num == opt$field[8] |
                 mg$w_field_num == opt$field[9] |
                 mg$w_field_num == opt$field[10])
  
  # Make data.frame of field and DCA coefficient values, then rename
  gcf <- data.frame(mg$w_field_num[ind],
                    mg$Cp.1[ind],
                    mg$c1.1[ind])
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
}