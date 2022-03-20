## --------------------------------------------------------------------------- #
## resBcsCogNoncog.R
##
## Project: UK HE EM
## Purpose: Runs progUkheEm() on (bcs) data and saves
## Author: Oliver Cassagneau-Francis
## Date: Thu 23 Sep 11:45:08 2021
## --------------------------------------------------------------------------- #
## Notes:

# load packages
# library(ukheEm)
devtools::load_all()

# call progUkheEm() on BCS data using parental income as y1
resBcsCogNoncog_female <- list()
for (K in 2:20) {
  resBcsCogNoncog_female[[K]] <- progUkheEm_v4r(
    dt = dtBcs4Em[sex == 2],
    K = K,
    varList = c(
      id = "bcsid",
      y1 = "cogScore",
      y2 = "noncogScore", # combined noncognitive test score
      w = "logWkPay",
      z = "c5e7", # adult life benefits: live away from home
      d = "degree"
    ),
    maxiter = 400,
    y1cont = TRUE,
    y1log = FALSE,
    J = 2
  )
}

use_data(resBcsCogNoncog_femaleV2, overwrite = TRUE)
