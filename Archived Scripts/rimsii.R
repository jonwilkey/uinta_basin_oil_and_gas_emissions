#-------------------------------------------------------------------------------
# Options
#-------------------------------------------------------------------------------

options(stringsAsFactors=FALSE)
options(drop=TRUE)

#-------------------------------------------------------------------------------
# Libraries
#-------------------------------------------------------------------------------

library(plyr)
library(foreign)

#-------------------------------------------------------------------------------
# Paths
#-------------------------------------------------------------------------------

data_root <- "C:/Users/jonwi/Dropbox/CLEAR/DOGM Data/RIMS II State Oil and Gas/Uinta Basin"

#-------------------------------------------------------------------------------
# Data
#-------------------------------------------------------------------------------

# Read in lookup between 20-industry codes and labels.
ind20 <- read.fwf(file.path(data_root, "row20.txt"),
                  header=FALSE,
                  widths=c(4, 52-4),
                  strip.white=TRUE
                  )
names(ind20) <- c("code", "label")

# Read in lookup between 60-industry codes and labels.
ind60 <- read.table(file.path(data_root, "row60code.txt"),
                    header=FALSE,
                    sep="|",
                    strip.white=TRUE
                    )
names(ind60) <- c("code", "label") 

# Read in employment multipliers
employment <- read.fwf(file.path(data_root, "JBSQRG01.DAT"),
                       header=FALSE,
                       widths=c(4, 4, 7),
                       strip.white=TRUE
                       )
names(employment) <- c("ind20code", "ind60code", "employment_multiplier")

# Read in earnings multipliers
earnings <- read.fwf(file.path(data_root, "ERSQRG01.DAT"),
                     header=FALSE,
                     widths=c(4, 4, 7),
                     strip.white=TRUE
                     )
names(earnings) <- c("ind20code", "ind60code", "earnings_multiplier")

#-------------------------------------------------------------------------------
# Tabulations
#-------------------------------------------------------------------------------

ddply(employment,
      c("ind60code"),
      function(x) {
          data.frame(total_multiplier = sum(x$employment_multiplier))
      }
      )

ddply(earnings,
      c("ind60code"),
      function(x) {
          data.frame(total_multiplier = sum(x$earnings_multiplier))
      }
      )