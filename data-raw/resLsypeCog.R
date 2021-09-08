## --------------------------------------------------------------------------- #
## resLsypeCog.R
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
resLsypeCog <- list()
for (K in 2:20) {
  resLsypeCog[[K]] <- progUkheEm(
    dt = dtLsype4Em,
    K = K,
    varList = c(
      id = "NSID",
      y1 = "cogScore", # combined cognitive score
      w = "logWkPay",
      z = "leaveHome", # adult life benefits: live away from home
      d = "degree25"
    ),
    maxiter = 400,
    y1cont = TRUE,
    y1log = FALSE,
    J = 1
  )
}

use_data(resLsypeCog, overwrite = TRUE)
