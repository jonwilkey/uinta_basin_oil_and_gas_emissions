# Function Info -----------------------------------------------------------
# Name:      hypfit.R (Hyperbolic Decline Curve Fitting)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# ws - production data, first column = time, second column = production data
# (oil or gas)

# bin - size of bin increment for summation (e.g. bin = 6 gives 6 month
# intervals over which months 1-6, 7 - 12, etc. will have their production
# values summed together)

# diff.bin.cutoff - minimum relative % difference between any two bins required
# for well to be considered restarted/reworked

# minProdRec - minimum number of non-zero production records required for
# fitting to be attempted, otherwise curve segment is skipped.

# api - well API #, used for merging (after DCA is finished) and inside this
# function for labeling plots.

# b.start - initial guess for b value in hyperbolic DC eq.

# Di.start - initial guess for Di value in hyperbolic DC eq.

# lower - lower limits for nlsLM to use for terms in hyperbolic DC eq., must be
# vector 3 units long containing lower limits for c(qo, b, Di)

# upper - upper limits for nlsLM to use for terms in hyperbolic eq., same format
# as lower limits.

# plotFlag - TRUE/FALSE, flag used for determining whether or not to plot
# results

# type - Character string stating either "Oil" or "Gas" for switching labels on
# plots.


# Outputs -----------------------------------------------------------------

# r - data.frame with various fit parameters. First row is for first decline
# curve segment, second row is for last decline curve segment.

# plot - (Optional) plots results of fits


# Description -------------------------------------------------------------

# This function applies the hyperbolic decline curve fit equation:

# [1] q = qo * (1 + b * Di * t) ^ (-1 / b)

# Using the nlsLM solver on the production data provided in data.frame ws, after
# applying the binning algorithm binStartStop.R to identify the start/stop
# points for the first decline curve and last decline curve for the given well.


# Function ----------------------------------------------------------------

hypfit <- function(ws, bin, diff.bin.cutoff, minProdRec, api, b.start, Di.start,
                   lower, upper, plotFlag, type, n.stopB.min, n.startT.search,
                   Cp.start, c1.start, Qlower, Qupper) {
  
  # Predefine data.frame "r" for returning results with following columns:
  qo        <- rep(0, times = 2) # Initial production rate coefficient
  b         <- qo                # Decline exponent
  Di        <- qo                # Initial decline rate
  tdelay    <- qo                # Time delay (in months) between start of first decline curve and first production of the well
  fitFirst  <- qo                # Binary indicating 1 if fit of first curve was successful, 0 otherwise
  fitLast   <- qo                # Binary for last decline curve
  skipped   <- qo                # Binary indicating at least one curve was skipped
  failed    <- qo                # Binary indicating at least one curve was not fit by nlsLM
  Cp        <- qo                # Cumulative production fit coefficient
  c1        <- qo                # Cumulative production fit constant
  QfitFirst <- qo                # Binary indicating 1 if fit first curve of cumulative production curve
  QfitLast  <- qo                # Binary for last cumulative production curve
  Qfailed   <- qo                # Binary indicating at least one cumulative curve was not fit by nlsLM
  rework    <- qo                # Binary - has well been reworked (i.e. more than one decline curve?), 1 if yes, 0 if no
  
  # Make data.frame for r
  r <- data.frame(api, qo, b, Di, tdelay, fitFirst, fitLast, skipped, failed,
                  Cp, c1, QfitFirst, QfitLast, Qfailed, rework)
  
  # Calculate cumulative production
  Q <- cumsum(ws$prod)
  
  # Make data.frame with correct names
  wq <- data.frame(ws$time, Q)
  names(wq) <- c("time", "Q")
  
  # Run binning algorithm to get first decline curve and last delcine curve
  stsp <- binStartStop(w = ws,
                       bin = bin,
                       diff.bin.cutoff = diff.bin.cutoff,
                       n.stopB.min = n.stopB.min,
                       n.startT.search = n.startT.search)
  
  # Get time delay
  r$tdelay <- ws[stsp[1,1],1]-ws[1,1]
  
  # Check - are first curve and last curve the same? If so only have to do DCA
  # once
  if (all(stsp[1,] == stsp[2,])) {
    
    # Check if minimum records requirement is met for decline curve segment
    if ((stsp[1,2]-stsp[1,1]+1) >= minProdRec) {
      
      # Set hyp object to initial value of NULL
      hyp = NULL
      
      # Fit using nlsLM solver with try() wrapper to keep errors from breaking
      # the loop
      try(hyp <- nlsLM(formula = prod ~ qo*(1+b*Di*time)^(-1/b),
                       data =    ws[stsp[1,1]:stsp[1,2],],
                       start =   list(qo = ws[stsp[1,1],2], b = b.start, Di = Di.start),
                       lower =   lower,
                       upper =   upper,
                       control = list(maxiter=1000)),
          silent = TRUE)
      
      # Set Qfit object to initial value of NULL
      Qfit = NULL
      
      # Fit using nlsLM solver with try() wrapper to keep errors from breaking
      # the loop
      try(Qfit <- nlsLM(formula = Q ~ Cp*time^0.5+c1,
                         data =    wq[stsp[1,1]:stsp[1,2],],
                         start =   list(Cp = Cp.start, c1 = c1.start),
                         lower =   Qlower,
                         upper =   c(Qupper[1], wq$Q[stsp[1,1]]),
                         control = list(maxiter=1000)),
          silent = TRUE)
      
      # If fit works, extract fit info and flag as fitting first and last
      if (!is.null(hyp)) {
        r$qo[1:2]       <- coef(hyp)[1]
        r$b[1:2]        <- coef(hyp)[2]
        r$Di[1:2]       <- coef(hyp)[3]
        r$fitFirst[1:2] <- 1
        r$fitLast[1:2]  <- 1
      } else {
        # The fit failed for both first/last curve (since they are the same).
        # However we can still note the initial production rate.
        r$qo[1:2]     <- ws[stsp[1,1],2]
        r$failed[1:2] <- 1
      }
      
      # If Qfit fit works, extract fit info
      if (!is.null(Qfit)) {
        r$Cp[1:2]        <- coef(Qfit)[1]
        r$c1[1:2]        <- coef(Qfit)[2]
        r$QfitFirst[1:2] <- 1
        r$QfitLast[1:2]  <- 1
      } else {
        # The fit failed for first curve
        r$Qfailed[1:2] <- 1
      }
      
    } else {
      # There weren't enough records to try fitting first/last (since they are
      # the same). Flag as being skipped, and note initial production rate.
      r$qo[1:2]      <- ws[stsp[1,1],2]
      r$skipped[1:2] <- 1
    }
  } else {
    # The two curves are different, and we have to fit each one seperately
    
    # Switch value of rework column to indicate that well has been reworked
    r$rework[1:2] <- 1
    
    # --- First Curve ---
    # Check if minimum records requirement is met for first decline curve
    if ((stsp[1,2]-stsp[1,1]+1) >= minProdRec) {
      
      # Set hyp object to initial value of NULL
      hyp1 = NULL
      
      # Fit using nlsLM solver with try() wrapper to keep errors from breaking
      # the loop
      try(hyp1 <- nlsLM(formula = prod ~ qo*(1+b*Di*time)^(-1/b),
                       data =    ws[stsp[1,1]:stsp[1,2],],
                       start =   list(qo = ws[stsp[1,1],2], b = b.start, Di = Di.start),
                       lower =   lower,
                       upper =   upper,
                       control = list(maxiter=1000)),
          silent = TRUE)
      
      # Set Qfit1 object to initial value of NULL
      Qfit1 = NULL
      
      # Fit using nlsLM solver with try() wrapper to keep errors from breaking
      # the loop
      try(Qfit1 <- nlsLM(formula = Q ~ Cp*time^0.5+c1,
                        data =    wq[stsp[1,1]:stsp[1,2],],
                        start =   list(Cp = Cp.start, c1 = c1.start),
                        lower =   Qlower,
                        upper =   c(Qupper[1], wq$Q[stsp[1,1]]),
                        control = list(maxiter=1000)),
          silent = TRUE)
      
      # If hyp1 fit works, extract fit info and flag as fitting first
      if (!is.null(hyp1)) {
        r$qo[1]       <- coef(hyp1)[1]
        r$b[1]        <- coef(hyp1)[2]
        r$Di[1]       <- coef(hyp1)[3]
        r$fitFirst[1] <- 1
      } else {
        # The fit failed for first curve However we can still note the initial
        # production rate.
        r$qo[1]     <- ws[stsp[1,1],2]
        r$failed[1] <- 1
      }
      
      # If Qfit1 fit works, extract fit info
      if (!is.null(Qfit1)) {
        r$Cp[1]        <- coef(Qfit1)[1]
        r$c1[1]        <- coef(Qfit1)[2]
        r$QfitFirst[1] <- 1
      } else {
        # The fit failed for first curve
        r$Qfailed[1] <- 1
      }
      
    } else {
      # There weren't enough records to try fitting first. Flag as being
      # skipped, and note initial production rate.
      r$qo[1]      <- ws[stsp[1,1],2]
      r$skipped[1] <- 1
    }
    
    # --- Last Curve ---
    # Check if minimum records requirement is met for last decline curve
    if ((stsp[2,2]-stsp[2,1]+1) >= minProdRec) {
      
      # Set hyp object to initial value of NULL
      hyp2 = NULL
      
      # Fit using nlsLM solver with try() wrapper to keep errors from breaking
      # the loop
      try(hyp2 <- nlsLM(formula = prod ~ qo*(1+b*Di*time)^(-1/b),
                        data =    ws[stsp[2,1]:stsp[2,2],],
                        start =   list(qo = ws[stsp[2,1],2], b = b.start, Di = Di.start),
                        lower =   lower,
                        upper =   upper,
                        control = list(maxiter=1000)),
          silent = TRUE)
      
      # Set Qfit2 object to initial value of NULL
      Qfit2 = NULL
      
      # Fit using nlsLM solver with try() wrapper to keep errors from breaking
      # the loop
      try(Qfit2 <- nlsLM(formula = Q ~ Cp*time^0.5+c1,
                         data =    wq[stsp[2,1]:stsp[2,2],],
                         start =   list(Cp = Cp.start, c1 = c1.start),
                         lower =   Qlower,
                         upper =   c(Qupper[1], wq$Q[stsp[2,1]]),
                         control = list(maxiter=1000)),
          silent = TRUE)
      
      # If hyp2 fit works, extract fit info and flag as fitting last
      if (!is.null(hyp2)) {
        r$qo[2]       <- coef(hyp2)[1]
        r$b[2]        <- coef(hyp2)[2]
        r$Di[2]       <- coef(hyp2)[3]
        r$fitLast[2]  <- 1
      } else {
        # The fit failed for last curve. However we can still note the initial 
        # production rate.
        r$qo[2]     <- ws[stsp[2,1],2]
        r$failed[2] <- 1
      }
      
      # If Qfit2 fit works, extract fit info
      if (!is.null(Qfit2)) {
        r$Cp[2]       <- coef(Qfit2)[1]
        r$c1[2]       <- coef(Qfit2)[2]
        r$QfitLast[2] <- 1
      } else {
        # The fit failed for last curve
        r$Qfailed[1] <- 1
      }
      
    } else {
      # There weren't enough records to try fitting last. Flag as being skipped,
      # and note initial production rate.
      r$qo[2]      <- ws[stsp[2,1],2]
      r$skipped[2] <- 1
    }
  }
  
  # If plotFlag = TRUE, plot results
  if (plotFlag == TRUE) {
    
    # Main Plot
    plot(ws$time, ws$prod,
         main = paste(type, "Production from API #", api),
         xlab = "Time Since First Production (months)",
         ylab = ifelse(type == "Oil",
                       paste(type, "Production (bbl)"),
                       paste(type, "Production (MCF)")),
         col = "grey")
    
    # If first/last the same then hyp exists and converged if not NULL
    if (exists("hyp")) {
      if (!is.null(hyp)) {
        lines(ws[stsp[1,1]:stsp[1,2],1], fitted(hyp), col = "blue")
        abline(v = ws$time[stsp[1,1]], col = "blue", lty = 2)
        abline(v = ws$time[stsp[1,2]], col = "blue", lty = 2)
      }
    }
    
    # Else, first curve fitted if hyp1 exists and converged if not NULL
    if (exists("hyp1")) {
      if (!is.null(hyp1)) {
        lines(ws[stsp[1,1]:stsp[1,2],1], fitted(hyp1), col = "red")
        abline(v = ws$time[stsp[1,1]], col = "red", lty = 2)
        abline(v = ws$time[stsp[1,2]], col = "red", lty = 2) 
      }
    }
    
    # Else, last curve fitted if hyp1 exists and converged if not NULL
    if (exists("hyp2")) {
      if (!is.null(hyp2)) {
        lines(ws[stsp[2,1]:stsp[2,2],1], fitted(hyp2), col = "green")
        abline(v = ws$time[stsp[2,1]], col = "green", lty = 2)
        abline(v = ws$time[stsp[2,2]], col = "green", lty = 2)
      }
    }
    
    # Add legend
    legend("topright",
           c("Actual", "Both", "First", "Last"),
           bg = "white",
           pch = c(1, NA, NA, NA),
           lty = c(NA, 1, 1, 1),
           col = c("grey", "blue", "red", "green"))
    
    # Main cumulative production plot
    plot(wq$time, wq$Q,
         main = paste("Cumulative", type, "Production from API #", api),
         xlab = "Time Since First Production (months)",
         ylab = ifelse(type == "Oil",
                       paste("Cumulative", type, "Production (bbl)"),
                       paste("Cumulative", type, "Production (MCF)")),
         col = "grey")
    
    # If first/last the same then Qfit exists and converged if not NULL
    if (exists("Qfit")) {
      if (!is.null(Qfit)) {
        lines(wq[stsp[1,1]:stsp[1,2],1], fitted(Qfit), col = "blue")
        abline(v = wq$time[stsp[1,1]], col = "blue", lty = 2)
        abline(v = wq$time[stsp[1,2]], col = "blue", lty = 2)
      }
    }
    
    # Else, first curve fitted if hyp1 exists and converged if not NULL
    if (exists("Qfit1")) {
      if (!is.null(Qfit1)) {
        lines(wq[stsp[1,1]:stsp[1,2],1], fitted(Qfit1), col = "red")
        abline(v = wq$time[stsp[1,1]], col = "red", lty = 2)
        abline(v = wq$time[stsp[1,2]], col = "red", lty = 2) 
      }
    }
    
    # Else, last curve fitted if hyp1 exists and converged if not NULL
    if (exists("Qfit2")) {
      if (!is.null(Qfit2)) {
        lines(wq[stsp[2,1]:stsp[2,2],1], fitted(Qfit2), col = "green")
        abline(v = wq$time[stsp[2,1]], col = "green", lty = 2)
        abline(v = wq$time[stsp[2,2]], col = "green", lty = 2)
      }
    }
    
    # Add legend
    legend("topleft",
           c("Actual", "Both", "First", "Last"),
           bg = "white",
           pch = c(1, NA, NA, NA),
           lty = c(NA, 1, 1, 1),
           col = c("grey", "blue", "red", "green"))
  }
  
  # Finally, return r
  return(r)
}