## --------------------------------------------------------------------------- #
## resLsypeParInc.R
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
resLsypeParIncNoncog_md <- list()
i <- 1
for (K1 in 2:4) {
  for (K2 in 2:4) {
    resLsypeParIncNoncog_md[[i]] <- progUkheEm_md(
      dt = dtLsype4Em,
      K = c(K1, K2),
      varList = c(
        id = "NSID",
        y1 = "logParInc",
        y2 = "noncogScore", # combined noncognitive test score
        w = "logWkPay",
        z = "leaveHome", # adult life benefits: live away from home
        d = "degree25"
      ),
      maxiter = 400,
      y1cont = TRUE,
      y1log = TRUE
    )
    i <- i+1
  }
}

use_data(resLsypeParIncNoncog_md, overwrite = TRUE)
