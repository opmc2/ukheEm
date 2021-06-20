## -----------------------------------------------------------------------------
## progUkHeEm.R
##
## Project: UK HE Experience
## Purpose: Contains the main function to implement the EM algorithm.
## Author: Oliver Cassagneau-Francis
## Date: Mon Aug 10 14:25:01 2020
## -----------------------------------------------------------------------------
## Notes:

#' EM algorithm on wage and education data
#'
#' This is the main function which takes data and other parameters and estimates
#' a mixture model including a treatment effect of schooling, on wage and
#' education data. The data needs to be in a specific format (see \code{dt})
#' and the model estimated is described in the accompanying vignettes.
#'
#' @param dt Either
#'   \itemize{
#'     \item the path to a data.table containing the wage and education
#'       data. The data.table should have been saved as a
#'       \code{.rda} file using \code{save()} or \code{use_data()}, or
#'     \item a data.table containing the wage and education
#'       data.
#'   }
#'   The columns containing the outcomes should be supplied as a named
#'   vector in \code{varList}.
#' @param K An integer corresponding to the number of types.
#' @param varList A named vector of the key variables in the model. The names
#'   should be:
#'   \itemize{
#'     \item \code{id}: a unique id for each individual
#'     \item \code{y1-yJ}: the J pre-treatment outcomes
#'     \item \code{w}: the post-treatment outcome (log-wages)
#'     \item \code{z}: the instrument
#'     \item \code{d}: the treatment (university here)
#'   }
#' @param startVals Either a character vector to use \code{kmeans()} or a vector
#'   of starting values for the algorithm.
#' @param maxiter An integer specifying the maximum number of iterations before
#'   stopping.
#' @param y1cont A logical value indicating whether the first pre-t outcome is
#'   continuous. If \code{y1cont} is set to \code{FALSE}, a special ML routine
#'   is used, and the data.table \code{dt} \strong{must} contain two extra
#'   columns, \code{left} and \code{right}:
#'   \itemize{
#'     \item the lower bound of the interval containing \code{y1} (\code{left})
#'     \item the upper bound of the interval containing \code{y1} (\code{right})
#'   }
#' @param J An integer equal to the number of pre-treatment outcomes.
#' @return Returns a named list containing:
#' \describe{
#'   \item{\code{listLoglike}}{A list containing the log-likelihood after each
#'     iteration for analysing convergence.}
#'   \item{\code{listParams}}{A list containing the parameters after each
#'     iteration for analysing convergence.}
#'   \item{\code{dtLong}}{The final data.table where calculations took place
#'     including the final parameters and intermediate values.}
#' }
# #' @example examples/progUkheEm_BcsExample.R
# #' @example examples/progUkheEm_LsypeExample.R
progUkheEm <- function(
  dt, K, varList,
  startVals = "kmeans",
  maxiter = 400,
  y1cont = TRUE,
  J = 1
) {

  if (is.character(dt)) dt <- load(here::here(dt))

  cols2keep <- varList

  dt <- dt[, ..cols2keep]
  names(dt) <- names(varList)
  dt <- dt[complete.cases(dt)]

  NN <- dt[, .N]

  if (isFALSE(y1cont)) {
    outcomes <- c("left", "right", paste0("y", 2:J), "w")
  } else {
    outcomes <- c(paste0("y", 1:J), "w")
  }

  # set start values
  if (startVals == "kmeans") {
      startVals <- kmeansSVs(dt[, ..outcomes], K, y1cont = y1cont, J = J)
  }

  dt[, svType := startVals$svTypes]

  alpha <- startVals$alpha
  sigmaY <- as.matrix(startVals$sigmaY[order(svType)][, .(y1, y2)])

  # make dt long by types
  dtLong <- list()
  for (k in 1:K) {
    dtLong[[k]] <- data.table(dt, type = as.character(k))
  }
  dtLong <- rbindlist(dtLong)

  # lists to track loop
  listLike <- list()
  listLoglike <- list()
  listDelta <- list()
  listParams <- list()
  iter <- 1L
  delta <- 100
  tol <- 1e-3

  # ---- pre-loop setup ----
  dtLong[, pk := fcase(type == as.character(svType), .9,
                       default = .1 / (K-1))]


  # -------------- #
  # ---- LOOP ----
  # -------------- #

  while(iter < maxiter & abs(delta) > tol) {

    # NOTE: E-step is at end as first loop is technically zero-th iteration

    # ---- M-step ----

    # update mu and sigmaNu (parameters of the test score distribution)

    if (isTRUE(y1cont)) {
      dtLong[, paste0("alpha", 1:J) := lapply(
        .SD, Hmisc::wtd.mean, weights = pk
      ), by = .(type), .SDcols = paste0("y", 1:J)]

      dtLong[, paste0("sigmaY", 1:J) := lapply(
        .SD, wtd.sd, weights = pk
      ), by = .(type), .SDcols = paste0("y", 1:J)]

    } else if (J > 1) {
      alphaSigmaRes <- list()
      for (k in 1:K) {
        alphaSigmaRes[[k]] <- optim(
          par = c(alpha[k, 1], sigmaY[k, 1]),
          fn = function(theta) -ell(theta, x = dtLong[type == as.character(k)])
        )
        alpha[k, 1] <- alphaSigmaRes[[k]]$par[[1]]
        sigmaY[k, 1] <- alphaSigmaRes[[k]]$par[[2]]
        dtLong[type == k, c("alpha1", "sigmaY1") := .(alpha[k, 1], sigmaY[k, 1])]

        dtLong[, paste0("alpha", 2:J) := lapply(
          .SD, Hmisc::wtd.mean, weights = pk
        ), by = .(type), .SDcols = paste0("y", 2:J)]

        dtLong[, paste0("sigmaY", 2:J) := lapply(
          .SD, wtd.sd, weights = pk
        ), by = .(type), .SDcols = paste0("y", 2:J)]

      }
    } else {

      alphaSigmaRes <- list()
      for (k in 1:K) {
        alphaSigmaRes[[k]] <- optim(
          par = c(alpha[k, 1], sigmaY[k, 1]),
          fn = function(theta) -ell(theta, x = dtLong[type == as.character(k)])
        )
        alpha[k, 1] <- alphaSigmaRes[[k]]$par[[1]]
        sigmaY[k, 1] <- alphaSigmaRes[[k]]$par[[2]]
        dtLong[type == k, c("alpha1", "sigmaY1") := .(alpha[k, 1], sigmaY[k, 1])]

      }
    }


    # update alpha and sigmaEps (parameters of the wage dist. @25)

    dtLong[, c("mu", "sigmaW") := .(
      ifelse(is.na(Hmisc::wtd.mean(w, pk)),
             mu,
             Hmisc::wtd.mean(w, pk)),
      ifelse(is.na(wtd.sd(w, pk)),
             sigmaW,
             sqrt(Hmisc::wtd.mean(w, pk)))),
      by = .(type, d)]



    # pi(k,z,d)

    dtLong[, pi_kzd := sum(pk) / NN, by = .(type, z, d)]


    # update likelihood|K
    # currently only w is log normal. other outcomes are assumed normal.
    if (isTRUE(y1cont)) {

      if (J == 1) {
        dtLong[, likelihoodK := pi_kzd *
                 dnorm(y1, mean = alpha1, sd = sigmaY1) *
                 1 / exp(w) * dnorm(w, mean = mu, sd = sigmaW)]
      } else if (J == 2) {
        dtLong[, likelihoodK := pi_kzd *
                 dnorm(y1, mean = alpha1, sd = sigmaY1) *
                 dnorm(y2, mean = alpha2, sd = sigmaY2) *
                 1 / exp(w) * dnorm(w, mean = mu, sd = sigmaW)]
      } else if (J > 2) {
        print("There are too many pre-t outcomes.")
        stop()
      }
    } else if (J == 1) {
      dtLong[, likelihoodK := pi_kzd *
               (pnorm(right, mean = alpha1, sd = sigmaY1) -
                  pnorm(left, mean = alpha1, sd = sigmaY1)) *
               (1 / exp(w)) * dnorm(w, mean = mu, sd = sigmaW)]
    } else if (J == 2) {
      dtLong[, likelihoodK := pi_kzd *
               (pnorm(right, mean = alpha1, sd = sigmaY1) -
                  pnorm(left, mean = alpha1, sd = sigmaY1)) *
               dnorm(y2, mean = alpha2, sd = sigmaY2) *
               (1 / exp(w)) * dnorm(w, mean = mu, sd = sigmaW)]
    } else if (J > 2) {
      print("There are too many pre-t outcomes.")
      stop()
    }


    # sum likelihood|K to give likelihood
    dtLong[, likelihood := sum(likelihoodK), by = id]

    # save parameters and likelihoods
    listParams[[iter]] <- dtLong[, .(
      mu = mean(mu),
      alpha = mean(alpha),
      sigmaNu = mean(sigmaNu),
      sigmaEps = mean(sigmaEps),
      pi_kzd = mean(pi_kzd)
    ), by = .(type, d, z)]

    listLike[[iter]] <- dtLong[type == "1", prod(likelihood)]
    listLoglike[[iter]] <- dtLong[type == "1", sum(log(likelihood))]

    if (iter > 1) {
      delta <- listLoglike[[iter]] - listLoglike[[iter-1]]
      listDelta[[iter-1]] <- delta
    }

    # ---- E-step ----

    # update pk
    dtLong[, pk := likelihoodK / likelihood]
    if (anyNA(dtLong$pk)) {
      dtLong[is.na(pk), pk := 0]
      print("Some pk were NaN. Replaced by zero.")
    }

    # increase iter by 1
    iter <- iter + 1

    # ---- end of loop ----
  }

  return(
    list(listLoglike = listLoglike,
         listParams = listParams,
         dtLong = dtLong)
  )
}
