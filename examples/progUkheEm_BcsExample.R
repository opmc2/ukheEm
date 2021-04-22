## --------------------------------------------------------------------------- #
## progUkheEm_BcsExample.R
##
## Project: UK HE EM
## Purpose: Runs example of progUkheEm() on BCS data
## Author: Oliver Cassagneau-Francis
## Date: Thu Mar 25 11:45:08 2021
## --------------------------------------------------------------------------- #
## Notes:

# load packages
# library(ukheEm)
load_all()

# call progUkheEm() on BCS data with maxiter = 20
resBcs <- list()
for (K in 2:20) {
  resBcs[[K]] <- progUkheEm(
    dt = dtBcs4Em,
    K = K,
    varList = c(
      id = "bcsid",
      y1 = "combnScore",
      y2 = "logWkPay",
      z = "c5e7", # adult life benefits: live away from home
      d = "degree"
    ),
    maxiter = 400,
    y1cont = TRUE
  )
}
