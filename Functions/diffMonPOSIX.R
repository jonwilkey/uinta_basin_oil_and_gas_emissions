## function returns number of months between first and last
diffMonPOSIX <- function(first, last) {
  first = as.POSIXlt(first)
  last =  as.POSIXlt(last)
  (12 * last$year + last$mon) - (12 * first$year + first$mon)
}
