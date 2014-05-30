## put 'production.rda' in your working directory
load("production.rda")

## in 'production.rda' there is a data frame 'df' that holds the data. Extract from 'df' only those wells drilled in Duchesne or Uintah counties.
df <- subset(production, subset = (welldata_county %in% c('UINTAH','DUCHESNE')))

## because wells can start production at any day during the first month of production I drop the initial month of
## production (t = 0) for the purpose of estimating parameters. Once the model parameters are estimated use the model to
## predict the t = 0 production. Alternatively, scale first month production based on how many days during the
## month the well produced.
df <- subset(df, subset = (ptime != 0))

## split df into two data frames, one for oil wells the other for gas wells
dfo <- subset(df, subset = (histdata_well_type == "OW"))
dfg <- subset(df, subset = (histdata_well_type == "GW"))

## The hyperbolic decline curve is fit to data by nonlinear least-squares. The R function nls() finds the minimum of the
## sum of squares function S(x, parameters) using Gauss-Newton. derive() returns the evaluations of S(x,parameters) and
## its gradient across parameters, for use in the Gauss-Newton routine.

# hyperbolic function
hyperbol <- deriv(~ alpha * (1 + theta * delta * x)^(-1/theta),
                  namevec = c("alpha", "theta", "delta"),                  
                  function(x, alpha, theta, delta){}
                  )

## gas production from gas wells
hgg <- nls(proddata_gas_prod ~ hyperbol(ptime, alpha, delta, theta),
          start = c(alpha = 30000, delta = 1.3, theta = 0.2),
          trace = TRUE,
          data = dfg,
          control = list(maxiter = 500, tol = 1e-06, minFactor = 1/(1024),
            printEval = FALSE, warnOnly = TRUE)                    
          )

## oil production from gas wells 
hog <- nls(proddata_oil_prod ~ hyperbol(ptime, alpha, delta, theta),
          start = c(alpha = 3000, delta = 1.2, theta = 0.5),
          trace = TRUE,
          data = dfg,
          control = list(maxiter = 500, tol = 1e-06, minFactor = 1/(1024),
            printEval = FALSE, warnOnly = TRUE)                    
          )

## gas production from oil wells
hgo <- nls(proddata_gas_prod ~ hyperbol(ptime, alpha, delta, theta),
          start = c(alpha = 20000, delta = 1.2, theta = 0.1),
          trace = TRUE,
          data = dfo,
          control = list(maxiter = 500, tol = 1e-06, minFactor = 1/(1024),
            printEval = FALSE, warnOnly = TRUE)                    
          )

## oil production from oil wells 
hoo <- nls(proddata_oil_prod ~ hyperbol(ptime, alpha, delta, theta),
          start = c(alpha = 3000, delta = 1.2, theta = 0.8),
          trace = TRUE,
          data = dfo,
          control = list(maxiter = 500, tol = 1e-06, minFactor = 1/(1024),
            printEval = FALSE, warnOnly = TRUE)                    
          )
