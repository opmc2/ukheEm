## --------------------------------------------------------------------------- #
## resBcsBoth.R
##
## Project: UK HE EM
## Purpose: Runs progUkheEm() on BCS data and saves resBcs
## Author: Oliver Cassagneau-Francis
## Date: 6 August 2021 11:07
## --------------------------------------------------------------------------- #
## Notes:

# load packages
# library(ukheEm)
devtools::load_all()

# call progUkheEm() on BCS data using test score as y1
resBcsBoth_md <- list()
i <- 1
for (K1 in 2:4) {
  for (K2 in 2:6) {
    resBcsBoth_md[[i]] <- progUkheEm_md(
      dt = dtBcs4Em,
      K = c(K1, K2),
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
      y1cont = FALSE
    )
    print(paste0("K1 = ", K1, ", K2 = ", K2, " completed."))
    i <- i+1
  }
}

use_data(resBcsBoth_md, overwrite = TRUE)
