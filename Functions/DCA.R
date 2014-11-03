# declineCurve.R

# Inputs ------------------------------------------------------------------

# 


# Outputs -----------------------------------------------------------------

# 


# Description -------------------------------------------------------------

# blah




# Function ----------------------------------------------------------------
DCA <- function(path, p, tsteps, field, min.depth, max.depth,
                           well.depth.step, version) {}

llplot <- function(x, y, xlim, ylim, title, xlab, ylab) {
  
  # Plot the data, hiding the points for now to prevent the calls to abline()
  # from drawing over the points.  
  plot(x, y,
       type = "n",
       log  = "xy",
       main = title,
       xlab = xlab,
       ylab = ylab,
       xlim = c(1, xlim),
       ylim = c(1, ylim))
  
  # Put grid lines on the plot (first horizontal, then vertical). Switch differentiates between 
  
  abline(h = c(1:10 %o% 10^(0:ceiling(log10(ylim)))),
         col = "grey")
  
  abline(v = c(1:10 %o% 10^(0:ceiling(log10(xlim)))),
         col = "grey")
  
  # Draw the points over the grid lines  
  points(x, y)
  
  # Redraw the plot box over the grid lines
  box()
}



# Subset data -------------------------------------------------------------

ps <- subset(p,
             subset = (time != 0 &
                       (p$h_well_type == "OW" |
                        p$h_well_type == "GW")),
             select = c("p_api",
                        "p_oil_prod",
                        "p_gas_prod",
                        "p_water_prod",
                        "time",
                        "h_well_type",
                        "w_field_num",
                        "w_totcum_oil",
                        "w_totcum_gas"))

# Get list of unique wells and order by cumulative oil and gas production
well <- sqldf("select distinct p_api, w_field_num, w_totcum_oil, w_totcum_gas
              from ps
              order by w_totcum_oil DESC, w_totcum_gas DESC")

# Plot and fit each well
for (i in 1:nrow(well)) {
  # Get subset "w" of "ps" data.frame which contains only rows whose API#
  # matches API# of well in row "i" of "well" data.frame
  w <- subset(ps, subset = (p_api == well$p_api[i]))
  
  # Remove zeroes in production record
  w <- w[-which(w$p_oil_prod == 0),]
  
  # Also remove negative production (really database?)
  w <- w[-which(w$p_oil_prod < 0),]
  
  # If statement to skip wells with short histories (fewer than minProdRec rows)
  if (nrow(w) >= minProdRec) {
    # Smooth data with spline
    sm <- smooth.spline(w$time, w$p_oil_prod)
    
    # Save smoothed points to data.frame
    sm <- data.frame(sm$x, sm$y)
    
    # Perform regression for all DCA equations
    sepd <- nlsLM(sm.y ~ qo*exp(-(sm.x/tau)^n),
                          data = sm,
                          start = list(qo = , tau = 1.78, delta = 1.16),
                          control = list(maxiter=1000))
    
    sepd <- nls(formula = p_oil_prod ~ qo*exp(-(time/tau)^n),
                data = w,
                #start = list(qo = 1e3, tau = 1.78, n = 1.16))
                start = list(qo = 1e3, tau = 1, n = 1))
    
    w$lnp <- log(w$p_oil_prod)
    sepd <- nls(formula = lnp ~ qo-(time/tau)^n,
                data = w,
                #start = list(qo = 1e3, tau = 1.78, n = 1.16))
                start = list(qo = 6.907755, tau = 10, n = 0.8))
    
    sepd <- optim()
    
  }
}

test <- subset(ps, subset = (p_api == well$p_api[1]))

llplot(x = test$time,
       y = test$p_oil_prod,
       xlim = 500,
       ylim = 100e3,
       title = "Oil Production from Well BLAH",
       xlab = "Time (months)",
       ylab = "Oil Production (bbl)")


nlsLM(oil ~ qo*exp(-(time/tau)^n),
      data = test,
      start = list(qo = 1340, tau = 1, n = 1),
      control = list(maxiter = 1e3))