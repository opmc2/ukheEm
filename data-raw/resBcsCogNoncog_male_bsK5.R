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

load("~/Dropbox/work/myRpkgs/ukheEm/data/resultsVersionPaper/resBcsCogNoncog_male.rda")
dtBcs4Bs <- resBcsCogNoncog_male[[5]][[3]]
# reorder types by mu
dtBcs4Bs[, oldType := type]
dtBcs4Bs <- merge(
  dtBcs4Bs,
  dtBcs4Bs[d == FALSE, .(muDF = Hmisc::wtd.mean(w, pk)), by = .(type)],
  by = c("type")
)
dtBcs4Bs[, type := frank(muDF, ties.method = "dense")]
dtBcs4Bs[, type := factor(type, levels = 1:5)]
for (iter in 1:5) {
  resBcsBs <- list()
  bsW <- igraph::sample_dirichlet(100, rep(1, 745))*745
  for (bs in 1:100) {
    print(paste0("Bootstrap number: ", bs))
    dtBcs4Bs$bsWeight <- rep(bsW[, bs], 5)

    resBcsBs[[bs]] <- try(progUkheEm_v4r(
      dt = dtBcs4Bs,
      K = 5,
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
      J = 2, bsWeights = TRUE, startVals = "dt"
    ))
  }

  saveRDS(resBcsBs, file = paste0("data/resBcsBsCN_maleK5_FSsvBsW_", iter, ".rds"))
  print("100 bootstrap samples saved.")
}
