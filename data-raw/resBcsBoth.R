## --------------------------------------------------------------------------- #
## resBcsBoth.R
##
## Project: UK HE EM
## Purpose: Runs progUkheEm() on BCS data and saves resBcs
## Author: Oliver Cassagneau-Francis
## Date: 24 Apr 2021 11:07
## --------------------------------------------------------------------------- #
## Notes:

# load packages
# library(ukheEm)
devtools::load_all()

# call progUkheEm() on BCS data using test score as y1
resBcsBoth <- list()
for (K in 2:20) {
  resBcsBoth[[K]] <- progUkheEm(
    dt = dtBcs4Em,
    K = K,
    varList = c(
      id = "bcsid",
      left = "left",
      right = "right",
      y2 = "combnScore",
      w = "logWkPay",
      z = "c5e7", # adult life benefits: live away from home
      d = "degree"
    ),
    maxiter = 400,
    y1cont = FALSE, y1b = TRUE
  )
  print(paste0("K = ", K, " completed."))
}

use_data(resBcsBoth, overwrite = TRUE)
