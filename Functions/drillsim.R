# Function Info -----------------------------------------------------------
# Name:      drillsim.R (Drilling schedule calculation)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - List object containing directory paths for file I/O

# simOP/GP - energy price paths for oil/gas from EPFsim.R function

# nrun - number of iterations in overall simulation

# drillModel - list of lm() fit objects for each drilling model

# type - character string for switch indicating "sim" to use simulated schedule
# or "actual" for actual drilling schedule

# p - DOGM database data frame

# tstart - start date of simulation period

# tstop - stop date of simulation period

# simtype - character switch indicating which model to choose for the simulated
# drilling schedule


# Outputs -----------------------------------------------------------------

# Drilled - matrix with rows = MC simulation runs in model and columns =
# timesteps


# Description -------------------------------------------------------------

# This function calculates the number of wells drilled in response to energy
# prices.


# Function ----------------------------------------------------------------
drillsim <- function(path, simOP, simGP, nrun, drillModel, type, p, tstart,
                     tstop, simtype) {

    # Check - simulate drilling schedule or use actual drilling schedule
    switch(type,

           # If simulating drilling schedule is used
           sim = {

               # Internal functions ---------------------------------------

               # Prior well model
               PWM <- function(OP, GP, par, init) {

                   # Initial wells drilled
                   w <- round(par[1] * OP[1] +
                                  par[2] * GP[1] +
                                  par[3] * init +
                                  par[4])

                   # If negative, set as zero
                   w <- ifelse(w < 0, 0, w)

                   # For all other timesteps
                   for(i in 2:length(OP)) {

                       # Calculate wells drilled
                       w <- c(w, round(par[1] * OP[i] +
                                           par[2] * GP[i] +
                                           par[3] * w[length(w)] +
                                           par[4]))

                       # Again, if negative set to zero
                       w <- ifelse(w < 0, 0, w)
                   }

                   # Return result
                   return(w)
               }

               # Energy price model
               EPM <- function(fit, OP, GP) {

                   temp <- round(fit[2] * OP + fit[3] * GP + fit[1])

                   # Remove negative well counts, if any exist
                   ifelse(temp > 0, temp, 0)
               }

               # Oil price model
               OPM <- function(fit, OP) {

                   temp <- round(fit[2]*OP+fit[1])

                   # Remove negative well counts, if any exist
                   ifelse(temp > 0, temp, 0)
               }

               # Gas price model
               GPM <- function(fit, GP) {

                   temp <- round(fit[2]*GP+fit[1])

                   # Remove negative well counts, if any exist
                   ifelse(temp > 0, temp, 0)
               }


               # Calculate drilling schedule ------------------------------

               # Predefine matrix for drilling schedule results. Rows =
               # simulation runs, columns = timesteps.
               Drilled <- matrix(0, nrow = nrun, ncol = ncol(simOP))

               # lm models use prior oil/gas price data, extract price data
               # prior to start of MC sim
               price <- eia.hp[as.Date(eia.hp$month) < tstart, ]

               # Switch through different model types
               switch(simtype,

                      # Prior well model
                      a = {

                          # Get initial number of wells drilled
                          di <- length(
                              unique(
                                  p$p_api[
                                      which(
                                          as.yearmon(p$h_first_prod) ==
                                              tail(price$month, 1)
                                      )
                                      ]
                              )
                          )

                          # For each run
                          for (i in 1:nrun) {

                              # Calculate number of wells drilled
                              Drilled[i,] <- PWM(OP   = simOP[i,],
                                                 GP   = simGP[i,],
                                                 par  = drillModel$pwm,
                                                 init = di)
                          }
                      },

                      # Energy price model
                      b = {

                          # Select indices for prior prices to use from energy
                          # price forecast
                          tdr <- 1:(ncol(simOP) - drillModel$N$epm)

                          # For each run
                          for (i in 1:nrun) {

                              # Calculate number of wells drilled
                              Drilled[i,] <- EPM(fit = coef(drillModel$epm),
                                                 OP =  c(tail(price$OP,
                                                              drillModel$N$epm),
                                                         simOP[i,tdr]),
                                                 GP =  c(tail(price$GP,
                                                              drillModel$N$epm),
                                                         simGP[i,tdr]))
                          }
                      },

                      # Oil price model
                      c = {

                          # Select indices for prior prices to use from energy
                          # price forecast
                          tdr <- 1:(ncol(simOP) - drillModel$N$opm)

                          # For each run
                          for (i in 1:nrun) {

                              # Calculate number of wells drilled
                              Drilled[i,] <- OPM(fit = coef(drillModel$opm),
                                                 OP =  c(tail(price$OP,
                                                              drillModel$N$opm),
                                                         simOP[i,tdr]))
                          }
                      },

                      # Gas price model
                      d = {

                          # Select indices for prior prices to use from energy
                          # price forecast
                          tdr <- 1:(ncol(simOP) - drillModel$N$gpm)

                          # For each run
                          for (i in 1:nrun) {

                              # Calculate number of wells drilled
                              Drilled[i,] <- GPM(fit = coef(drillModel$gpm),
                                                 GP =  c(tail(price$GP,
                                                              drillModel$N$epm),
                                                         simGP[i,tdr]))
                          }
                      })
           },

           # If actual schedule is used...
           actual = {

               # Actual number of wells drilled as timeseries. sqldf returns
               # result as a data.frame, so brackets extract numerical results
               # as a vector.
               Drilled.act <- subset(p,
                                     subset = ((h_well_type == "OW" |
                                                    h_well_type == "GW") &
                                                   h_first_prod >= tstart &
                                                   h_first_prod <= tstop),
                                     select = c("p_api", "h_first_prod"))

               Drilled.act$h_first_prod <- as.Date(
                   as.yearmon(Drilled.act$h_first_prod)
                   )

               Drilled.act <- sqldf("select count(distinct(p_api))
                     from 'Drilled.act'
                     group by h_first_prod")[[1]]

               # Turn into matrix
               Drilled <- matrix(rep(Drilled.act, nrun), nrow = nrun, byrow = T)
           })

    # Return result
    return(Drilled)
}
