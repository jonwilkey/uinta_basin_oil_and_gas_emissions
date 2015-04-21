# Function Info -----------------------------------------------------------
# Name:      sim_DCC.R (Simulated Decline Curve Coefficients Selector)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# decline.type - character string switch for determining what decline/production
# curve equation to use (hyperbolic DC or cumulative production)

# times - number of wells for which to generate DCC

# field - vector of field numbers to be analyzed individually

# fieldnum - vector of field numbers indicating what field each well is in

# DCA.cdf.coef.oil - CDFs for each coefficient in hyperbolic decline curve eq.
# for each field for oil production

# DCA.cdf.coef.gas - CDFs for each coefficient in hyperbolic decline curve eq.
# for each field for gas production

# Q.DCA.cdf.coef.oil - CDFs for each coefficient in cumulative production eq.
# for each field for oil production

# Q.DCA.cdf.coef.gas - CDFs for each coefficient in cumulative production eq.
# for each field for gas production


# Outputs -----------------------------------------------------------------

# DCC - data frame with DCCs for each well for both oil and gas production


# Description -------------------------------------------------------------

# This function randomly picks the DCCs for each well based on the CDFs for each
# DCC contained in DCA.cdf.coef.oil/gas (for hyperbolic DCCs) and
# Q.DCA.cdf.coef.oil/gas (for cumulative DCCs). A switch (decline.type) controls
# which type of DCCs are generated.


# Function ----------------------------------------------------------------

sim_DCC <- function(decline.type, times, field, fieldnum, DCA.cdf.coef.oil,
                    DCA.cdf.coef.gas, Q.DCA.cdf.coef.oil, Q.DCA.cdf.coef.gas) {
  
  # Predefine DCC object
  DCC <- NULL
  
  # Check - which decline curve method is being used?
  switch(decline.type,
         
         # Hyperbolic DC
         a = {
           
           # Define DCA coefficient vectors and time delay
           DCC$qo.oil <- rep(0, times)
           DCC$b.oil  <- DCC$qo.oil
           DCC$Di.oil <- DCC$qo.oil
           DCC$qo.gas <- DCC$qo.oil
           DCC$b.gas  <- DCC$qo.oil
           DCC$Di.gas <- DCC$qo.oil
           
           # For each field
           for (i in 1:length(field)) {
             
             # Get indices of wells located in field i
             ind <- which(fieldnum == field[i])
             
             # Pull coefficient CDFs for field i
             cdf.qo.oil <- DCA.cdf.coef.oil[[(i-1)*4+1]]
             cdf.b.oil  <- DCA.cdf.coef.oil[[(i-1)*4+2]]
             cdf.Di.oil <- DCA.cdf.coef.oil[[(i-1)*4+3]]
             cdf.qo.gas <- DCA.cdf.coef.gas[[(i-1)*4+1]]
             cdf.b.gas  <- DCA.cdf.coef.gas[[(i-1)*4+2]]
             cdf.Di.gas <- DCA.cdf.coef.gas[[(i-1)*4+3]]
             
             # Pick values for each coefficient
             DCC$qo.oil[ind] <- cdf.qo.oil$PDF.x[findInterval(runif(length(ind)),c(0,cdf.qo.oil$CDF), all.inside = T)]
             DCC$b.oil[ind]  <- cdf.b.oil$PDF.x[ findInterval(runif(length(ind)),c(0,cdf.b.oil$CDF ), all.inside = T)]
             DCC$Di.oil[ind] <- cdf.Di.oil$PDF.x[findInterval(runif(length(ind)),c(0,cdf.Di.oil$CDF), all.inside = T)]
             DCC$qo.gas[ind] <- cdf.qo.gas$PDF.x[findInterval(runif(length(ind)),c(0,cdf.qo.gas$CDF), all.inside = T)]
             DCC$b.gas[ind]  <- cdf.b.gas$PDF.x[ findInterval(runif(length(ind)),c(0,cdf.b.gas$CDF ), all.inside = T)]
             DCC$Di.gas[ind] <- cdf.Di.gas$PDF.x[findInterval(runif(length(ind)),c(0,cdf.Di.gas$CDF), all.inside = T)]
           }
         },
         
         # Cumulative DC
         b = {
           
           # Define DCA coefficient vectors and time delay
           DCC$Cp.oil <- rep(0, times)
           DCC$c1.oil <- DCC$Cp.oil
           DCC$Cp.gas <- DCC$Cp.oil
           DCC$c1.gas <- DCC$Cp.oil
           
           # For each field
           for (i in 1:length(field)) {
             
             # Get indices of wells located in field i
             ind <- which(fieldnum == field[i])
             
             # Pull coefficient CDFs for field i
             cdf.Cp.oil <- Q.DCA.cdf.coef.oil[[(i-1)*2+1]]
             cdf.c1.oil <- Q.DCA.cdf.coef.oil[[(i-1)*2+2]]
             cdf.Cp.gas <- Q.DCA.cdf.coef.gas[[(i-1)*2+1]]
             cdf.c1.gas <- Q.DCA.cdf.coef.gas[[(i-1)*2+2]]
             
             # Pick values for each coefficient
             DCC$Cp.oil[ind] <- cdf.Cp.oil$PDF.x[findInterval(runif(length(ind)),c(0,cdf.Cp.oil$CDF), all.inside = T)]
             DCC$c1.oil[ind] <- cdf.c1.oil$PDF.x[findInterval(runif(length(ind)),c(0,cdf.c1.oil$CDF), all.inside = T)]
             DCC$Cp.gas[ind] <- cdf.Cp.gas$PDF.x[findInterval(runif(length(ind)),c(0,cdf.Cp.gas$CDF), all.inside = T)]
             DCC$c1.gas[ind] <- cdf.c1.gas$PDF.x[findInterval(runif(length(ind)),c(0,cdf.c1.gas$CDF), all.inside = T)]
           }
         })
  
  # Return the wellType vector
  return(as.data.frame(DCC))
}