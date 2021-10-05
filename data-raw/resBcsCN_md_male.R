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
resBcsCN_md_male <- list()
i <- 1
for (K1 in 2:6) {
  for (K2 in 2:6) {
    resBcsCN_md_male[[i]] <- progUkheEm_md(
      dt = dtBcs4Em[sex == 1],
      K = c(K1, K2),
      varList = c(
        id = "bcsid",
        y1 = "cogScore",
        y2 = "noncogScore",
        w = "logWkPay",
        z = "c5e7", # adult life benefits: live away from home
        d = "degree"
      ),
      maxiter = 400,
      y1cont = TRUE, y1log = FALSE
    )
    print(paste0("K1 = ", K1, ", K2 = ", K2, " completed."))
    i <- i+1
  }
}

use_data(resBcsCN_md_male, overwrite = TRUE)
