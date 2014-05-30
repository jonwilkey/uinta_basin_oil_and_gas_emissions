# Severance Tax Function
ST <- function (volume, price, royalty_rate, API = 39.6) {
  # This function determines severance tax payments.
  # Severance tax rates
  s_low  <- 0.030 # for values <= $13/bbl
  s_high <- 0.050 # for values > $13/bbl
  s_cf   <- 0.002 # Conservation fee
  
  # Determine wellhead value (WV). API gravity of WTI (39.6 degrees) is used as
  # the basis for this calculation. If producing an unconventional oil (from
  # shale or sand), replace the default API gravity argument in the function
  # call with the API gravity of the oil in question
  WV <- (API / 39.6) * price
  
  # Deductions on WV to determine taxable value (TV). Currently just deducting
  # royalty payments (given here on $/bbl basis from "R" function)
  TV <- WV - royalty_rate
  
  # Determine fraction of TV above split tax rate (f_st) of $13/bbl
  f_st <- rep(0, length(TV))
  f_st <- ifelse(test = (TV - 13)/TV < 0,
                 yes = 0,
                 no = (TV - 13)/TV)
  
  rST <- (s_low * (1 - f_st) + s_high * f_st) * TV + s_cf * TV # ST per unit vol
  ST <- rST * volume # ST total
  return(ST)
}