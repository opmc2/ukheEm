## --------------------------------------------------------------------------- #
## resBcsNoncog.R
##
## Project: UK HE EM
## Purpose: Runs progUkheEm() on Next Steps (lsype) data and saves
## Author: Oliver Cassagneau-Francis
## Date: Wed Apr 07 11:45:08 2021
## --------------------------------------------------------------------------- #
## Notes:

# load packages
# library(ukheEm)
load_all()

# call progUkheEm() on BCS data using parental income as y1
resBcsNoncog_male <- list()
for (K in 2:20) {
  resBcsNoncog_male[[K]] <- progUkheEm(
    dt = dtBcs4Em[sex == 1],
    K = K,
    varList = c(
      id = "bcsid",
      y1 = "noncogScore", # combined cognitive score
      w = "logWkPay",
      z = "c5e7", # adult life benefits: live away from home
      d = "degree"
    ),
    maxiter = 400,
    y1cont = TRUE,
    y1log = FALSE,
    J = 1
  )
}

use_data(resBcsNoncog_male, overwrite = TRUE)
