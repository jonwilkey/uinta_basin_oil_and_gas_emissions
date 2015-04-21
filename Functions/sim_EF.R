# Function Info -----------------------------------------------------------
# Name:      sim_EF.R (Simulated Emission Factor Selector)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# times - number of wells for which to pick emission factors

# EF - data.frame of emission factor means and standard deviations defined in
# IO_options.R


# Outputs -----------------------------------------------------------------

# result - data frame of emission factors for each pollutant by activity for
# each well


# Description -------------------------------------------------------------

# This function randomly picks emission factors based on the mean and standard
# deviations in EF


# Function ----------------------------------------------------------------

sim_EF <- function(times, EF) {
  
  # Hardcoded - sorry!
  
  # Predefine results list object
  result <- NULL
  
  # For each species that has an emission factor
  for (i in 1:3) {
    
    # Predefine temporary EF matrix
    temp.EF <- matrix(0, nrow = times, ncol = nrow(EF))
    
    # For each EF category
    for (j in 1:nrow(EF)) {
      
      # If mean of EF for species i in category j is nonzero
      if(EF[j,i] != 0) {
        
        # Pick EFs using rnorm()
        temp.EF[,j] <- rnorm(n = times, mean = EF[j,i], sd = EF[j,(i+3)])
        
        # Rewrite negative EF values as zero
        temp.EF[,j] <- ifelse(temp.EF[,j] < 0, 0, temp.EF[,j])
      }
    }
    
    # If i = 1, working on CO2e
    if(i == 1) {
      
      # Save out EFs for CO2e
      result$EFdrill.co2  <- rowSums(temp.EF[,c(1:3,5)])
      result$EFrework.co2 <- rowSums(temp.EF[,c(3:4)])
      result$EFcompl.co2  <- temp.EF[,6]
      result$EFprod.co2   <- temp.EF[,7]
      result$EFproc.co2   <- temp.EF[,8]
      result$EFtrans.co2  <- temp.EF[,9]
      result$EFoprod.co2  <- temp.EF[,10]
      result$EFotrans.co2 <- temp.EF[,11]
    }
    
    # If i = 2, working on CH4
    if(i == 2) {
      
      # Save out EFs for CH4
      result$EFdrill.ch4  <- rowSums(temp.EF[,c(1:3,5)])
      result$EFrework.ch4 <- rowSums(temp.EF[,c(3:4)])
      result$EFcompl.ch4  <- temp.EF[,6]
      result$EFprod.ch4   <- temp.EF[,7]
      result$EFproc.ch4   <- temp.EF[,8]
      result$EFtrans.ch4  <- temp.EF[,9]
      result$EFoprod.ch4  <- temp.EF[,10]
      result$EFotrans.ch4 <- temp.EF[,11]
    }
    
    # If i = 3, working on VOCs
    if(i == 3) {
      
      # Save out EFs for VOCs
      result$EFdrill.voc  <- rowSums(temp.EF[,c(1:3,5)])
      result$EFrework.voc <- rowSums(temp.EF[,c(3:4)])
      result$EFcompl.voc  <- temp.EF[,6]
      result$EFprod.voc   <- temp.EF[,7]
      result$EFproc.voc   <- temp.EF[,8]
      result$EFtrans.voc  <- temp.EF[,9]
      result$EFoprod.voc  <- temp.EF[,10]
      result$EFotrans.voc <- temp.EF[,11]
    }
  }
  
  # Transform result into data.frame
  result <- as.data.frame(result)
  
  # Return the results
  return(result)
}