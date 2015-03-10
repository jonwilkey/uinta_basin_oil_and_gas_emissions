# pick some random wells from p

test <- unique(p$p_api)
test <- test[round(runif(1e3, min = 1, max = length(test)))]

pdf(file.path(path$plot, "Water Test Fits.pdf"))

for (i in 1:length(test)) {
  
  # Get water records for well i
  w <- subset(p,
              subset = (p_api == test[i]),
              select = c("time", "p_water_prod"))
  names(w) <- c("time", "water")
  
  # Calc cumulative water for w
  Q <- cumsum(w$water)
  
  # Null hyp and Qfit
  hyp <- NULL
  Qfit <- NULL
  
  # Try fitting
  try(hyp <- nlsLM(formula = water ~ qo*(1+b*Di*time)^(-1/b),
                   data =    w,
                   start =   list(qo = 2317, b = 3, Di = 0.5),
                   lower =   c(0, 0, 0),
                   upper =   c(Inf, Inf, Inf),
                   control = list(maxiter=1000)),
      silent = T)
  
  try(Qfit <- nlsLM(formula = Q ~ Cp*sqrt(time)+c1,
                    data =    w,
                    start =   list(Cp = 2200, c1 = 0),
                    lower =   c(0, -Inf),
                    upper =   c(Inf, Inf),
                    control = list(maxiter=1000)),
      silent = T)
  
  plot(w$time, w$water,
       main = paste("Water Production from API #", test[i]),
       xlab = "Time Since First Production (months)",
       ylab = "Water Production (bbl)")  
  if(!is.null(hyp)) {
    lines(w$time, fitted(hyp), col = "blue")
    legend("topright", c("Data", "Fit"), pch = c(1, NA), lty = c(NA, 1), col = c("black", "blue"))
  }  
  
  plot(w$time, Q,
       main = paste("Cumulative Water Production from API #", test[i]),
       xlab = "Time Since First Production (months)",
       ylab = "Cumulative Water Production (bbl)")
  if(!is.null(Qfit)) {
    lines(w$time, fitted(Qfit), col = "blue")
    legend("topright", c("Data", "Fit"), pch = c(1, NA), lty = c(NA, 1), col = c("black", "blue"))
  }
}

dev.off()