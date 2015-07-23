# Function Info -----------------------------------------------------------
# Name:      sim_DCC.R (Simulated Decline Curve Coefficients Selector)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# decline.type.oil/gas - character string switch for determining what 
# decline/production curve equation to use (hyperbolic DC or cumulative 
# production)

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

# tsteps - vector of dates (in months) corresponding to each simulated time step

# tDrill - numeric/integer vector of time steps in which each well is drilled

# tend - age of well (in months) at start of simulation period (for prior wells
# only)

# DCAlnormFit - data.frame with trendline fit parameters for known distributions


# Outputs -----------------------------------------------------------------

# DCC - data frame with DCCs for each well for both oil and gas production


# Description -------------------------------------------------------------

# This function randomly picks the DCCs for each well based on the CDFs for each
# DCC contained in DCA.cdf.coef.oil/gas (for hyperbolic DCCs) and
# Q.DCA.cdf.coef.oil/gas (for cumulative DCCs). A switch (decline.type) controls
# which type of DCCs are generated.


# Function ----------------------------------------------------------------

sim_DCC <- function(decline.type.oil, decline.type.gas, times, field, fieldnum,
                    DCA.cdf.coef.oil, DCA.cdf.coef.gas, Q.DCA.cdf.coef.oil,
                    Q.DCA.cdf.coef.gas, tsteps, tDrill, tend, DCAlnormFit) {
  
  # Predefine DCC object
  DCC <- NULL
  
  
  # Oil DCCs --------------------------------------------------------------
  
  # Check - which decline curve method is being used?
  switch(decline.type.oil,
         
         # Hyperbolic DC
         a = {
           
           # Define DCA coefficient vectors
           DCC$qo.oil <- rep(0, times)
           DCC$b.oil  <- DCC$qo.oil
           DCC$Di.oil <- DCC$qo.oil
           
           # For each field
           for (i in 1:length(field)) {
             
             # Get indices of wells located in field i
             ind <- which(fieldnum == field[i])
             
             # Pull coefficient CDFs for field i
             cdf.qo.oil <- DCA.cdf.coef.oil[[(i-1)*4+1]]
             cdf.b.oil  <- DCA.cdf.coef.oil[[(i-1)*4+2]]
             cdf.Di.oil <- DCA.cdf.coef.oil[[(i-1)*4+3]]
             
             # Pick values for each coefficient
             DCC$qo.oil[ind] <- cdf.qo.oil$PDF.x[findInterval(runif(length(ind)),c(0,cdf.qo.oil$CDF), all.inside = T)]
             DCC$b.oil[ind]  <- cdf.b.oil$PDF.x[ findInterval(runif(length(ind)),c(0,cdf.b.oil$CDF ), all.inside = T)]
             DCC$Di.oil[ind] <- cdf.Di.oil$PDF.x[findInterval(runif(length(ind)),c(0,cdf.Di.oil$CDF), all.inside = T)]
           }
         },
         
         # Cumulative DC
         b = {
           
           # Define DCA coefficient vectors
           DCC$Cp.oil <- rep(0, times)
           DCC$c1.oil <- DCC$Cp.oil
           
           # For each field
           for (i in 1:length(field)) {
             
             # Get indices of wells located in field i
             ind <- which(fieldnum == field[i])
             
             # Pull coefficient CDFs for field i
             cdf.Cp.oil <- Q.DCA.cdf.coef.oil[[(i-1)*2+1]]
             cdf.c1.oil <- Q.DCA.cdf.coef.oil[[(i-1)*2+2]]
             
             # Pick values for each coefficient
             DCC$Cp.oil[ind] <- cdf.Cp.oil$PDF.x[findInterval(runif(length(ind)),c(0,cdf.Cp.oil$CDF), all.inside = T)]
             DCC$c1.oil[ind] <- cdf.c1.oil$PDF.x[findInterval(runif(length(ind)),c(0,cdf.c1.oil$CDF), all.inside = T)]
           }
         },
         
         # Basin Level Distribution Fit
         c = {
           
           # Calculate time difference vector (years since 1983). If new well
           # (i.e. tDrill >= 1) calculate time difference based on tsteps, else
           # for oil wells use the tend column
           td <- ifelse(tDrill >= 1,
                        floor(as.numeric(difftime(tsteps[tDrill], as.Date("1984-01-01"), units = "days"))*(1/365.25)),
                        floor(as.numeric(difftime(as.Date(as.yearmon("2010-01-01")-tend/12), as.Date("1984-01-01"), units = "days"))*(1/365.25)))
           
           # Pick coefficients from defined distributions using "q----" functions
           DCC$Cp.oil <- qlnorm(p =       runif(length(td)),
                                meanlog = with(DCAlnormFit, p1[1]*td+p2[1]),
                                sdlog =   with(DCAlnormFit, p1[2]*exp(-p2[2]*td)))
           
           DCC$c1.oil <- qnorm(p =        runif(length(td)),
                               mean =     with(DCAlnormFit, p1[5]*td+p2[5]),
                               sd =       with(DCAlnormFit, p1[6]*td+p2[6]))
         },
         
         # Field Level Distribution Fit
         d = {
           
           # Calculate time difference vector (years since 1983). If new well
           # (i.e. tDrill >= 1) calculate time difference based on tsteps, else
           # for oil wells use the tend column
           td <- ifelse(tDrill >= 1,
                        floor(as.numeric(difftime(tsteps[tDrill], as.Date("1984-01-01"), units = "days"))*(1/365.25)),
                        floor(as.numeric(difftime(as.Date(as.yearmon("2010-01-01")-tend/12), as.Date("1984-01-01"), units = "days"))*(1/365.25)))
           
           # Define DCA coefficient vectors
           DCC$Cp.oil <- rep(0, times)
           DCC$c1.oil <- DCC$Cp.oil
           
           # For each field
           for (i in 1:length(field)) {
             
             # Get indices of wells located in field i
             ind <- which(fieldnum == field[i])
             
             # Pick values for each coefficient in field i
             DCC$Cp.oil[ind] <- qlnorm(p =       runif(length(ind)),
                                       meanlog = with(DCAlnormFit, p1[i+8]*td+p2[i+8]),
                                       sdlog =   with(DCAlnormFit, p1[i+12]*exp(-p2[i+12]*td)))
             
             DCC$c1.oil[ind] <- qnorm(p =        runif(length(ind)),
                                      mean =     with(DCAlnormFit, p1[i+24]*td+p2[i+24]),
                                      sd =       with(DCAlnormFit, p1[i+28]*td+p2[i+28]))
           }
         })
  
  
  # Gas DCCs --------------------------------------------------------------
  
  # Check - which decline curve method is being used?
  switch(decline.type.gas,
         
         # Hyperbolic DC
         a = {
           
           # Define DCA coefficient vectors
           DCC$qo.gas <- rep(0, times)
           DCC$b.gas  <- DCC$qo.gas
           DCC$Di.gas <- DCC$qo.gas
           
           # For each field
           for (i in 1:length(field)) {
             
             # Get indices of wells located in field i
             ind <- which(fieldnum == field[i])
             
             # Pull coefficient CDFs for field i
             cdf.qo.gas <- DCA.cdf.coef.gas[[(i-1)*4+1]]
             cdf.b.gas  <- DCA.cdf.coef.gas[[(i-1)*4+2]]
             cdf.Di.gas <- DCA.cdf.coef.gas[[(i-1)*4+3]]
             
             # Pick values for each coefficient
             DCC$qo.gas[ind] <- cdf.qo.gas$PDF.x[findInterval(runif(length(ind)),c(0,cdf.qo.gas$CDF), all.inside = T)]
             DCC$b.gas[ind]  <- cdf.b.gas$PDF.x[ findInterval(runif(length(ind)),c(0,cdf.b.gas$CDF ), all.inside = T)]
             DCC$Di.gas[ind] <- cdf.Di.gas$PDF.x[findInterval(runif(length(ind)),c(0,cdf.Di.gas$CDF), all.inside = T)]
           }
         },
         
         # Cumulative DC
         b = {
           
           # Define DCA coefficient vectors
           DCC$Cp.gas <- rep(0, times)
           DCC$c1.gas <- DCC$Cp.gas
           
           # For each field
           for (i in 1:length(field)) {
             
             # Get indices of wells located in field i
             ind <- which(fieldnum == field[i])
             
             # Pull coefficient CDFs for field i
             cdf.Cp.gas <- Q.DCA.cdf.coef.gas[[(i-1)*2+1]]
             cdf.c1.gas <- Q.DCA.cdf.coef.gas[[(i-1)*2+2]]
             
             # Pick values for each coefficient
             DCC$Cp.gas[ind] <- cdf.Cp.gas$PDF.x[findInterval(runif(length(ind)),c(0,cdf.Cp.gas$CDF), all.inside = T)]
             DCC$c1.gas[ind] <- cdf.c1.gas$PDF.x[findInterval(runif(length(ind)),c(0,cdf.c1.gas$CDF), all.inside = T)]
           }
         },
         
         # Basin Level Distribution Fit
         c = {
           
           # Calculate time difference vector (years since 1983). If new well
           # (i.e. tDrill >= 1) calculate time difference based on tsteps, else
           # for oil wells use the tend column
           td <- ifelse(tDrill >= 1,
                        floor(as.numeric(difftime(tsteps[tDrill], as.Date("1984-01-01"), units = "days"))*(1/365.25)),
                        floor(as.numeric(difftime(as.Date(as.yearmon("2010-01-01")-tend/12), as.Date("1984-01-01"), units = "days"))*(1/365.25)))
           
           # Pick coefficients from defined distributions using "q----" functions
           DCC$Cp.gas <- qlnorm(p =       runif(length(td)),
                                meanlog = with(DCAlnormFit, p1[3]*td+p2[3]),
                                sdlog =   with(DCAlnormFit, p1[4]*exp(-p2[4]*td)))
           
           DCC$c1.gas <- qnorm(p =        runif(length(td)),
                               mean =     with(DCAlnormFit, p1[7]*td+p2[7]),
                               sd =       with(DCAlnormFit, p1[8]*td+p2[8]))
         },
         
         # Field Level Distribution Fit
         d = {
           
           # Calculate time difference vector (years since 1983). If new well
           # (i.e. tDrill >= 1) calculate time difference based on tsteps, else
           # for oil wells use the tend column
           td <- ifelse(tDrill >= 1,
                        floor(as.numeric(difftime(tsteps[tDrill], as.Date("1984-01-01"), units = "days"))*(1/365.25)),
                        floor(as.numeric(difftime(as.Date(as.yearmon("2010-01-01")-tend/12), as.Date("1984-01-01"), units = "days"))*(1/365.25)))
           
           # Define DCA coefficient vectors
           DCC$Cp.gas <- rep(0, times)
           DCC$c1.gas <- DCC$Cp.gas
           
           # For each field
           for (i in 1:length(field)) {
             
             # Get indices of wells located in field i
             ind <- which(fieldnum == field[i])
             
             # Pick values for each coefficient in field i
             DCC$Cp.gas[ind] <- qlnorm(p =       runif(length(ind)),
                                       meanlog = with(DCAlnormFit, p1[i+16]*td+p2[i+16]),
                                       sdlog =   with(DCAlnormFit, p1[i+20]*exp(-p2[i+20]*td)))
             
             DCC$c1.gas[ind] <- qnorm(p =        runif(length(ind)),
                                      mean =     with(DCAlnormFit, p1[i+32]*td+p2[i+32]),
                                      sd =       with(DCAlnormFit, p1[i+36]*td+p2[i+36]))
           }
         })
  
  # Return the wellType vector
  return(as.data.frame(DCC))
}
