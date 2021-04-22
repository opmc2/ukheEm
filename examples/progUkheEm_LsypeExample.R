## --------------------------------------------------------------------------- #
## progUkheEm_LsypeExample.R
##
## Project: UK HE EM
## Purpose: Runs example of progUkheEm() on BCS data
## Author: Oliver Cassagneau-Francis
## Date: Thu Mar 25 11:45:08 2021
## --------------------------------------------------------------------------- #
## Notes:

# call progUkheEm() on Next Steps (LSYPE1) data with maxiter = 20
progUkheEm(
  dt = "data/dtLsype4Em.rds",
  K = 4,
  varList = c(
    y1 = "contAnnIncHH_w1",
    y2 = "grssWkPay25",
    z = "attSchl",
    d = "degree"
  ),
  maxiter = 20,
  y1cont = FALSE
)
