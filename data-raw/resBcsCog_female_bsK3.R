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
devtools::load_all()


dtBcs4Bs <- resBcsCog_female[[3]][[3]]
for (iter in 1:5) {
  resBcsBs <- list()
  bsW <- igraph::sample_dirichlet(100, rep(1, 1131))*1131
  for (bs in 1:100) {
    print(paste0("Bootstrap number: ", bs))
    dtBcs4Bs[, bsWeight := rep(bsW[, bs], 3)]

    resBcsBs[[bs]] <- progUkheEm(
      dt = dtBcs4Bs,
      K = 3,
      varList = c(
        id = "id",
        y1 = "y1", # combined cognitive score,
        y2 = "y2",
        w = "w",
        z = "z", # adult life benefits: live away from home
        d = "d"
      ),
      maxiter = 400,
      y1cont = TRUE,
      y1log = FALSE,
      J = 1
    )
  }

  saveRDS(resBcsBs, file = paste0("data/resBcsBsCog_female_FSsvBsW_", iter, ".rds"))
  print("100 bootstrap samples saved.")
}
