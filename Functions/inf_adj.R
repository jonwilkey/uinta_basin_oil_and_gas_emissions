# Inflation Adjustment
inf_adj <- function (price, index, basis) {
  price_adj <- price * (basis / index)
  return (price_adj)
}