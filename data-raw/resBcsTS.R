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
load_all()

# call progUkheEm() on BCS data using test score as y1
resBcsTS <- list()
for (K in 2:20) {
  resBcsTS[[K]] <- progUkheEm(
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

use_data(resBcsTS, overwrite = TRUE)
