# Royalty function
royalty <- function (volume, price, landowner = "state") {
  # This function determines the royalty payments.
  
  # Pick rate based on landownership
  rate <- switch(landowner,
                 federal = 0.1250,
                 indian  = 0.1667,
                 state   = 0.1250,
                 fee     = 0.1250)
  
  royalty <- rate * volume * price
  return(royalty)
}