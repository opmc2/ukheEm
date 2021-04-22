## --------------------------------------------------------------------------- #
## resBcs.R
##
## Project: UK HE EM
## Purpose: Runs progUkheEm() on BCS data and saves resBcs
## Author: Oliver Cassagneau-Francis
## Date: Wed Apr 07 11:45:08 2021
## --------------------------------------------------------------------------- #
## Notes:

# load packages
# library(ukheEm)
devtools::load_all()

# call progUkheEm() on BCS data using test score as y1
resBcsParInc <- list()
for (K in 2:10) {
  resBcsParInc[[K]] <- progUkheEm(
    dt = dtBcs4Em,
    K = K,
    varList = c(
      id = "bcsid",
      left = "left",
      right = "right",
      y2 = "logWkPay",
      z = "c5e7", # adult life benefits: live away from home
      d = "degree"
    ),
    maxiter = 50,
    y1cont = FALSE
  )
}

use_data(resBcsParInc, overwrite = TRUE)
