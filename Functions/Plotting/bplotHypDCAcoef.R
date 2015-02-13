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
  
  # Get row index of fits for fields we're specifically fitting
  ind <- which(hmo$w_field_num == opt$field[1] |
                 hmo$w_field_num == opt$field[2] |
                 hmo$w_field_num == opt$field[3] |
                 hmo$w_field_num == opt$field[4] |
                 hmo$w_field_num == opt$field[5] |
                 hmo$w_field_num == opt$field[6] |
                 hmo$w_field_num == opt$field[7] |
                 hmo$w_field_num == opt$field[8] |
                 hmo$w_field_num == opt$field[9] |
                 hmo$w_field_num == opt$field[10])
  
  # Make data.frame of field and DCA coefficient values, then rename
  ocf <- data.frame(hmo$w_field_num[ind],
                    hmo$qo.1[ind],
                    hmo$b.1[ind],
                    hmo$Di.1[ind],
                    hmo$tdelay[ind])
  names(ocf) <- c("field", "qo", "b", "Di", "td")
  
  
  # Data for gas ------------------------------------------------------------
  
  # Get row index of fits for fields we're specifically fitting
  ind <- which(hmg$w_field_num == opt$field[1] |
                 hmg$w_field_num == opt$field[2] |
                 hmg$w_field_num == opt$field[3] |
                 hmg$w_field_num == opt$field[4] |
                 hmg$w_field_num == opt$field[5] |
                 hmg$w_field_num == opt$field[6] |
                 hmg$w_field_num == opt$field[7] |
                 hmg$w_field_num == opt$field[8] |
                 hmg$w_field_num == opt$field[9] |
                 hmg$w_field_num == opt$field[10])
  
  # Make data.frame of field and DCA coefficient values, then rename
  gcf <- data.frame(hmg$w_field_num[ind],
                    hmg$qo.1[ind],
                    hmg$b.1[ind],
                    hmg$Di.1[ind],
                    hmg$tdelay[ind])
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
}