# Function Info -----------------------------------------------------------
# Name:      drillingModelUpdate.R (Drilling Schedule Model Fitting)
# Author(s): Jon Wilkey, Michael Hogue
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - List object containing directory paths for file I/O

# p - DOGM databse data.frame

# min.depth - Minimum well depth, used as subsetting criteria

# tstart - Starting date for wells to be included in analysis

# tstop - Stopping date for wells to be included in analysis

# ver - Version number for file naming of exported data.frames

# eia.hp - EIA historical energy prices

# tlag - the max number of time steps in the past to search for the best value
# of N, see description below for details


# Outputs -----------------------------------------------------------------

# drillModel - lm() object containing fit of # of wells drilled (OW, GW, or D)
# in response to oil and gas prices.


# Description -------------------------------------------------------------

# This function fits the drilling history in the Basin within the specified time
# period to the EIA historical oil and gas prices according to the following
# functions over a specified time window (between tstart and tstop):

# (a) W_n = a * OP_n + b * GP_n + c * W_n-1 + d
# (b) W_n = a * OP_n-N + b * GP_n-N + c
# (c) W_n = a * OP_n-N + b
# (d) W_n = a * GP_n-N + b

# where W is the wells drilled (oil, gas, or dry), n is the timestep (in
# months), OP is first purchase price (FFP) of oil in Utah ($/bbl, inflation
# adjusted to the date associated with "cpi" option input), GP is FFP of gas in
# Utah ($/MCF, also inflation adjusted), N is the number of time steps in the
# past, and all other terms are fitted coefficients.

# Note that in model (a), all time steps are used (effectively) with
# exponentially decaying weighting (e.g. the current time step n's values have
# the highest weight, and the observation the furthest back in the past the
# least weight). For models (b) - (d), the best value of N is searched for by
# comparing the residual sum of the squares (RSS) the values of 1 through tlag
# (as defined in the function input).


# Function ----------------------------------------------------------------

drillingModelUpdate <- function(path, p, min.depth, tstart, tstop, ver, eia.hp,
                                tlag) {

  # Internal functions ------------------------------------------------------

  # Model "a" (prior model) RSS minimization function
  min.RSS <- function(d, init, par) {

    # Initial wells drilled
    w <- round(par[1]*d$OP[1]+par[2]*d$GP[1]+par[3]*init+par[4])

    # Set to zero if negative
    w <- ifelse(w < 0, 0, w)

    # Calculate initial RSS
    RSS <- (w-d$wells[1])^2

    # For each subsequent time step
    for(i in 2:nrow(d)) {

      # Wells drilled
      w <- round(par[1]*d$OP[i]+par[2]*d$GP[i]+par[3]*w+par[4])

      # Set to zero if negative
      w <- ifelse(w < 0, 0, w)

      # Calculate RSS
      RSS <- (w-d$wells[i])^2+RSS
    }

    # Return RSS value
    return(RSS)
  }


  # Determine number of wells drilled each month ------------------------------

  # Add column 'drill_month' as truncated to month version of h_first_prod
  p$drill_month <- as.yearmon(p[,"h_first_prod"])

  # Create dataframe containing dates of 1st production for each unique APD #
  # and the field it is located in
  well <- sqldf("select distinct(p_api), drill_month, h_well_type, h_td_md
                from p")

  # Drop NA observations
  well <- na.omit(well)

  # Only wells (1) inside price history timeframe (and prior month), (2) with
  # depths > 0, and (3) that were drilled with the intention of being producing
  # wells (i.e. oil wells, gas wells, or dry wells).
  well <- subset(well, subset = (drill_month >= (as.yearmon(min(eia.hp$month))-1/12) &
                                   drill_month <= as.yearmon(max(eia.hp$month)) &
                                   h_td_md >= min.depth &
                                 (h_well_type == "OW" |
                                  h_well_type == "GW" |
                                  h_well_type == "D")))

  # Determine total number of wells drilled each year
  drill <- sqldf("select drill_month, count(p_api)
                 from well
                 group by drill_month")

  m <- merge(x = eia.hp, y = drill,
             by.x = "month", by.y = "drill_month",
             all = TRUE)

  # Save two vectors for (1) wells drilled in prior year (prior) and (2) wells
  # drilled in current year (wells)
  prior <- m[(1:(nrow(m)-1)),4]
  wells <- m[(2:(nrow(m))),4]

  # Any values in prior or well that are "NA" are because the # of wells drilled
  # in that particular time step were == 0 and the SQL query omitted them.
  # Rewrite those values as being 0 to correct them.
  prior[is.na(prior)] <- 0
  wells[is.na(wells)] <- 0

  # Create data.frame with all the data need to run lm()
  d <- data.frame(as.Date(eia.hp[,1]), wells, prior, eia.hp[,c(2,3)])
  names(d) <- c("month", "wells", "prior", "OP", "GP")

  # Make copy for export
  drillModelData <- d


  # Fit model "a" -----------------------------------------------------------

  # Get LHS sample points
  spLHS <- optimumLHS(n = 128, k = 4)

  # use sample points to pick initial guesses for optim function
  parLHS <- matrix(c(qunif(spLHS[,1],-1,1),
                     qunif(spLHS[,2],-5,5),
                     qunif(spLHS[,3],-1,2),
                     qunif(spLHS[,4],-25,25)),
                   nrow = nrow(spLHS), ncol = ncol(spLHS))

  # Get index of dates within fitting time window
  dr <- which(d$month == tstart):which(d$month == tstop)

  # Get initial well drilling value
  init <- d$prior[dr[1]]

  # Define parR (optim coefficients results from each set of initial guesses)
  # and RSSr (the RSS value result with those parameters)
  parR <- matrix(0, nrow = nrow(parLHS), ncol = ncol(parLHS))
  RSSr <- rep(0,nrow(parR))

  # For each set of initial guesses
  for (j in 1:nrow(spLHS)) {

    # Use optim to minimize min.RSS function
    temp <-     optim(par = parLHS[j,], fn = min.RSS, d = d[dr,], init = init)

    # Pull parameter values and RSS results
    parR[j,] <- temp$par
    RSSr[j] <-  temp$value
  }

  # Only keep fit with the lowest RSSr value
  pwm <- parR[which.min(RSSr),]


  # Fit models "b", "c", and "d" --------------------------------------------

  # Create lists of fit objects
  epm <- list()
  opm <- list()
  gpm <- list()

  for (i in 1:tlag) {

      # Fit other price models
      epm[[i]] <- lm(d$wells[dr]~d$OP[dr-i]+d$GP[dr-i])
      opm[[i]] <- lm(d$wells[dr]~d$OP[dr-i])
      gpm[[i]] <- lm(d$wells[dr]~d$GP[dr-i])
  }

  # Function for getting residual sum of squares
  RSS <- function(x) {

      # Calculate and return RSS
      return(sum(residuals(x) ^ 2))
  }

  # Find which value of N gives min RSS for each model
  N <- list(epm = which.min(lapply(X = epm, FUN = RSS)),
            opm = which.min(lapply(X = opm, FUN = RSS)),
            gpm = which.min(lapply(X = gpm, FUN = RSS)))


  # Export fitted model -----------------------------------------------------

  # Create list for export of various models
  drillModel <- list(pwm = pwm,
                     epm = epm[[N$epm]],
                     opm = opm[[N$opm]],
                     gpm = gpm[[N$gpm]],
                     N   = N)

  # Save results
  save(file = file.path(path$data,
                        paste("drillModel_", ver, ".rda", sep = "")),
       list = c("drillModel",
                "drillModelData"))
}
