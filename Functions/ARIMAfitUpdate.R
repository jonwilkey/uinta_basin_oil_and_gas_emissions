# Function Info -----------------------------------------------------------
# Name:      ARIMAfitUpdate.R (ARIMA Fitting)
# Author(s): Jon Wilkey
# Contact:   jon.wilkey@gmail.com


# Inputs ------------------------------------------------------------------

# path - List object containing directory paths for file I/O

# eia.hp - data.frame of EIA historical energy prices with observations "month"
# for time index, "OP" for first purchase price (FFP) of oil in Utah ($/bbl,
# inflation adjusted to the date associated with "cpi" option input), and "GP"
# for FFP of gas in Utah ($/MMBtu, also inflation adjusted)

# tstart - start date of prices to include in analysis

# tstop - stop date of prices to include in analysis

# ver - Version number for file naming of exported data.frames

# opdq/gpdq - User-specified ARIMA model for oil/gas prices, respectively

# plotFlag - TRUE/FALSE value indicating whether or not to plot cross-validation
# of forecast fits


# Outputs -----------------------------------------------------------------

# ARIMAfitOP & ARIMAfitGP - ARIMA fit objects for oil and gas price histories


# Description -------------------------------------------------------------

# This function uses ARIMA (autoregressive integrated moving average) time
# series analysis tools in the forecast package to fit the specified ARIMA model
# to oil and gas price histories.


# Function ----------------------------------------------------------------

ARIMAfitUpdate <- function(path, eia.hp, tstart, tstop, ver, opdq, gpdq,
                           plotFlag) {

    # Prepare data for fitting ------------------------------------------------

    # Get year and month of start of eia.hp data
    y.eia <- as.year(eia.hp$month[1])
    m.eia <- month(eia.hp$month[1])

    # Create time-series objects out of oil and gas price data
    op <- ts(data = eia.hp$OP, frequency = 12, start = c(y.eia, m.eia))
    gp <- ts(data = eia.hp$GP, frequency = 12, start = c(y.eia, m.eia))

    # Create c(year, month) vectors for training data from tstart/tstop
    win.start <- c(as.year(tstart), month(tstart))
    win.stop  <- c(as.year(tstop),  month(tstop))

    # Get training data
    op.train <- window(op, start = win.start, end = win.stop)
    gp.train <- window(gp, start = win.start, end = win.stop)


    # Fit ARIMA models --------------------------------------------------------

    # Fitting function
    fitfun <- function(x, pdq, ic) {

        # Auto ARIMA fit
        afit <- auto.arima(x, lambda = 0)

        # Manual ARIMA fit
        mfit <- Arima(x, order = pdq, lambda = 0, include.constant = ic)

        # Return fits
        return(list(afit = afit, mfit = mfit))
    }

    # Apply function to both op and gp
    op.ARIMA <- fitfun(x = op.train, pdq = opdq$order, ic = opdq$ic)
    gp.ARIMA <- fitfun(x = gp.train, pdq = gpdq$order, ic = gpdq$ic)

    # Plotting function
    pfun <- function(xfit, xts, ptype, mainlab) {

        # Make forecast
        fc <- forecast(xfit,
                       h      = nrow(eia.hp) - length(xfit$x),
                       level  = c(40, 80),
                       lambda = 0)

        # Specify forecast line colors
        lc <- rainbow(5)

        # Make main plot
        if(ptype == "OP") {

            # Use oil-related axis labels
            plot(xts,
                 ylim = c(0, 1.05 * max(c(xts, fc$upper[,2]))),
                 xlab = "Date (months)",
                 ylab = "Oil Price ($ / bbl)",
                 main = mainlab)

        } else {

            # Use gas-related axis labels
            plot(xts,
                 ylim = c(0, 1.05 * max(c(xts, fc$upper[,2]))),
                 xlab = "Date (months)",
                 ylab = "Gas Price ($ / MCF)",
                 main = mainlab)
        }

        # Add forecast lines
        lines(x = eia.hp$month[length(xfit$x):nrow(eia.hp)],
              y = c(xfit$x[length(xfit$x)], fc$upper[,2]), col = lc[1])
        lines(x = eia.hp$month[length(xfit$x):nrow(eia.hp)],
              y = c(xfit$x[length(xfit$x)], fc$upper[,1]), col = lc[2])
        lines(x = eia.hp$month[length(xfit$x):nrow(eia.hp)],
              y = c(xfit$x[length(xfit$x)], fc$mean), col = lc[3])
        lines(x = eia.hp$month[length(xfit$x):nrow(eia.hp)],
              y = c(xfit$x[length(xfit$x)], fc$lower[,1]), col = lc[4])
        lines(x = eia.hp$month[length(xfit$x):nrow(eia.hp)],
              y = c(xfit$x[length(xfit$x)], fc$lower[,2]), col = lc[5])

        # Add margin text
        mtext(paste("Training Data:", as.yearmon(tstart),
                    "to",             as.yearmon(tstop)))

        # Add legend
        legend("topleft",
               c("Actual", "90%", "70%", "50%", "30%", "10%"),
               col = c("black", lc),
               lty = 1)
    }


    # If plotting (only plot if running model in cross-validation mode)
    if(plotFlag == TRUE) {

        # Plot cross-validated forecast
        pdf(file.path(path$plot, paste("ARIMA fits ", ver, ".pdf", sep = "")))

        pfun(xfit = op.ARIMA$afit, xts = op, ptype = "OP",
             mainlab = paste("Auto-ARIMA (",
                             op.ARIMA$afit$arma[1], ",",
                             op.ARIMA$afit$arma[6], ",",
                             op.ARIMA$afit$arma[2], ") ",
                             "fit for Oil Prices", sep = ""))

        pfun(xfit = gp.ARIMA$afit, xts = gp, ptype = "GP",
             mainlab = paste("Auto-ARIMA (",
                             gp.ARIMA$afit$arma[1], ",",
                             gp.ARIMA$afit$arma[6], ",",
                             gp.ARIMA$afit$arma[2], ") ",
                             "fit for Gas Prices", sep = ""))

        pfun(xfit = op.ARIMA$mfit, xts = op, ptype = "OP",
             mainlab = paste("Manual ARIMA (",
                             opdq$order[1], ",",
                             opdq$order[2], ",",
                             opdq$order[3], ") ",
                             "fit for Oil Prices", sep = ""))

        pfun(xfit = gp.ARIMA$mfit, xts = gp, ptype = "GP",
             mainlab = paste("Manual ARIMA (",
                             gpdq$order[1], ",",
                             gpdq$order[2], ",",
                             gpdq$order[3], ") ",
                             "fit for Gas Prices", sep = ""))

        # Close PDF
        dev.off()
    }


    # Export results ----------------------------------------------------------

    save(file = file.path(path$data, paste("ARIMAfit_", ver, ".rda", sep = "")),
         list = c("op.ARIMA",
                  "gp.ARIMA"))
}
