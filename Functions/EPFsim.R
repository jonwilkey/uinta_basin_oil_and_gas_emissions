# Function Info -----------------------------------------------------------
# Name:      EPFsim.R (Energy Price Forecast Simulation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# epf.type - character switch indicating which price forecasting method to use

# product - character value, either "oil" or "gas" indicating which commodity to
# generate price forecast for


# Outputs -----------------------------------------------------------------

# epp - matrix of energy price paths with rows = MC simulation iterations and
# columns = time steps


# Description -------------------------------------------------------------

# This function simulates oil and gas price paths using one of the methods
# specified in the IO_options.R script. Note that this function primarily acts
# as a wrapper around other functions and options, and as such the inputs listed
# are only those unique to EPFsim() that control the execution of the function.


# Function ----------------------------------------------------------------
EPFsim <- function(epf.type, product) {

    # Switch for EPF simulation method
    switch(epf.type,

           # If Auto-ARIMA simulation, epf.type == "a"
           a = {

               # Allocate space for epp matrix
               epp <- matrix(0, nrow = opt$nrun, ncol = opt$MC.tsteps)

               # If oil
               if (product == "oil") {

                   # Auto-ARIMA oil price path simulation
                   for (i in 1:nrow(epp)) {

                       epp[i, ] <- simulate.Arima(object = op.ARIMA$afit,
                                                  nsim   = ncol(epp),
                                                  lambda = 0)
                   }

               } else {

                   # Auto-ARIMA gas price path simulation
                   for (i in 1:nrow(epp)) {

                       epp[i, ] <- simulate.Arima(object = gp.ARIMA$afit,
                                                  nsim   = ncol(epp),
                                                  lambda = 0)
                   }
               }
           },

           # If Manual ARIMA simulation, epf.type == "b"
           b = {

               # Allocate space for epp matrix
               epp <- matrix(0, nrow = opt$nrun, ncol = opt$MC.tsteps)

               # If oil
               if (product == "oil") {

                   # Manual ARIMA oil price path simulation
                   for (i in 1:nrow(epp)) {

                       epp[i, ] <- simulate.Arima(object = op.ARIMA$mfit,
                                                  nsim   = ncol(epp),
                                                  lambda = 0)
                   }

               } else {

                   # Manual ARIMA gas price path simulation
                   for (i in 1:nrow(epp)) {

                       epp[i, ] <- simulate.Arima(object = gp.ARIMA$mfit,
                                                  nsim   = ncol(epp),
                                                  lambda = 0)
                   }
               }

           },

           # If EIA forecast with error propagation, ep.type == "c"
           c = {

               # If running for oil
               if (product == "oil") {

                   # Run EIAsim for oil
                   epsim <- EIAsim(nrun =     opt$nrun,
                                   Eoil =     Eoil,
                                   Egas =     Egas,
                                   EoilFrac = EoilFrac,
                                   EgasFrac = EgasFrac,
                                   op.FC =    op.FC.ref,
                                   gp.FC =    gp.FC.ref,
                                   type =     opt$epf.EIA.type$oil,
                                   fracProb = opt$epf.EIA.fracProb$oil)

                   # Extract objects from list
                   epp <- epsim$op

               } else {

                   # Run EIAsim for gas
                   epsim <- EIAsim(nrun =     opt$nrun,
                                   Eoil =     Eoil,
                                   Egas =     Egas,
                                   EoilFrac = EoilFrac,
                                   EgasFrac = EgasFrac,
                                   op.FC =    op.FC.ref,
                                   gp.FC =    gp.FC.ref,
                                   type =     opt$epf.EIA.type$gas,
                                   fracProb = opt$epf.EIA.fracProb$gas)

                   # Extract objects from list
                   epp <- epsim$gp
               }
           },

           # If using actual energy price paths, ep.type == "d"
           d = {

               # Get prices from eia.hp data.frame

               # If oil
               if (product == "oil") {

                   # Get prices for oil
                   epp <- matrix(rep(eia.hp$OP[(nrow(eia.hp)-opt$MC.tsteps+1):
                                                   nrow(eia.hp)],
                                     times = opt$nrun),
                                 nrow  = opt$nrun,
                                 ncol  = opt$MC.tsteps,
                                 byrow = T)

               } else {

                   # Get prices for gas
                   epp <- matrix(rep(eia.hp$GP[(nrow(eia.hp)-opt$MC.tsteps+1):
                                                   nrow(eia.hp)],
                                     times = opt$nrun),
                                 nrow  = opt$nrun,
                                 ncol  = opt$MC.tsteps,
                                 byrow = T)
               }
           },

           # If using constant energy price paths, ep.type == "e"
           e = {

               # If oil
               if (product == "oil") {

                   # Get average price of last n periods for oil
                   mepp <- mean(eia.hp$OP[(nrow(eia.hp) - opt$epf.const$noil + 1):
                                              nrow(eia.hp)])

               } else {

                   # Get average price of last n periods for gas
                   mepp <- mean(eia.hp$GP[(nrow(eia.hp) - opt$epf.const$ngas + 1):
                                              nrow(eia.hp)])
               }

               # Set constant price
               epp <- matrix(mepp,
                             nrow  = opt$nrun,
                             ncol  = opt$MC.tsteps,
                             byrow = T)
           },

           # If using user-specified price path, ep.type = "f"
           f = {

               # Define epp using user-specified price matrices in opt

               # If oil
               if (product == "oil") {

                   epp <- opt$epf.uepf$oil

               } else {

                   epp <- opt$epf.uepf$gas
               }
           })

    # Return result
    return(epp)
}
