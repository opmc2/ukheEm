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
#' @param dt A data.table containing the wage and education data. The outcomes
#'   columns containing the outcomes should be named \code{yp} for the before
#'   treatment outcome, and \code{yw} for the post-treatment.
#' @param K An integer corresponding to the number of types.
#' @param startVals Either a character vector to use \code{kmeans()} or a vector
#'   of starting values for the algorithm.
#' @param maxiter An integer specifying the maximum number of iterations before
#'   stopping.
#' @param y1cont A logical value indicating whether the first outcome is
#'   continuous. If not a special ML routine is used.
progUkheEm <- function(dt, K, startVals = "kmeans", maxiter = 400,
                       y1cont = T) {

  # Notes:
  # -------------------------------------------------------------------------- #
  # yp and yw are the outcomes
  # currently yp is the before treatment outcome
  # -------------------------------------------------------------------------- #

  NN <- dt[, .N]

  # set start values
  if (is.character(startVals)) {
    if(startVals == "kmeans") startVals <- kmeansSVs(dt, K, y1cont = y1cont)
  }

  dt[, svType := startVals$svTypes]

  # initial mu and sigmaNu for starting values
  if (isFALSE(y1cont)) {
    sigmaNuVec <- startVals$sigmaNu
    muVec <- startVals$mu
  }

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

    # NOTE: E-step is at end as first loop in technically zero-th iteration

    # ---- M-step ----

    # update mu and sigmaNu

    if (isTRUE(y1cont)) {
      dtLong[, c("mu", "sigmaNu") := .(Hmisc::wtd.mean(yp, pk),
                                       sqrt(Hmisc::wtd.var(yp, pk))),
             by = .(type, degree)]
    } else {
      muSigmaRes <- list()
      for (k in 1:K) {
        muSigmaRes[[k]] <- optim(
          par = c(muVec[[k]], sigmaNuVec[[k]]),
          fn = function(theta) -ell(theta, x = dtLong[type == k])
        )
        muVec[[k]] <- muSigmaRes[[k]]$par[[1]]
        sigmaNuVec[[k]] <- muSigmaRes[[k]]$par[[2]]
        dtLong[type == k, c("mu", "sigmaNu") := .(muVec[[k]], sigmaNuVec[[k]])]
      }
    }


    # update alpha and sigmaEps

    dtLong[, c("alpha", "sigmaEps") := .(
      ifelse(is.na(Hmisc::wtd.mean(yw, pk)), alpha, Hmisc::wtd.mean(yw, pk)),
      ifelse(is.na(sqrt(Hmisc::wtd.var(yw, pk))),
             sigmaEps,
             sqrt(Hmisc::wtd.mean(yw, pk)))),
      by = .(type, degree)]



    # update bk and gamma

    modelS <- glm(
      formula = degree ~ type + attSchl - 1,
      family = binomial(link = "logit"),
      data = dtLong,
      weights = pk
    )

    coeffS <- coefficients(modelS)

    for (k in 1:K) {
      dtLong[type == paste0(k), bk := coeffS[paste0("type", k)]]
    }

    for (val in levels(dtLong[, attSchl])) {
      dtLong[attSchl == val, gamma := coeffS[paste0("attSchl", val)]]
    }
    dtLong[is.na(gamma), gamma := 0]

    dtLong[, Ps1_kz := fitted.values(modelS)]

    # update pi(k,z)

    dtLong[, pi_kz := sum(pk) / NN, by = .(type, attSchl)]


    # update likelihood|K
    dtLong[, likelihoodK := pi_kz * Ps1_kz *
             (pnorm(right, mean = mu, sd = sigmaNu) - pnorm(left, mean = mu, sd = sigmaNu)) *
             (1 / exp(yw)) * dnorm(yw, mean = alpha, sd = sigmaEps)]

    # sum likelihoodK to give likelihood
    dtLong[, likelihood := sum(likelihoodK), by = bcsid]

    # save parameters and likelihoods
    listParams[[iter]] <- dtLong[, .(
      mu = mean(mu),
      alpha = mean(alpha),
      sigmaNu = mean(sigmaNu),
      sigmaEps = mean(sigmaEps),
      pi_kz = mean(pi_kz),
      bk = mean(bk),
      gamma = mean(gamma)
    ), by = .(type, degree, attSchl)]

    listLike[[iter]] <- dtLong[type == "1", prod(likelihood)]
    listLoglike[[iter]] <- dtLong[type == "1", sum(log(likelihood))]

    if (iter > 1) {
      delta <- listLoglike[[iter]] - listLoglike[[iter-1]]
      listDelta[[iter-1]] <- delta
    }

    # ---- E-step ----

    # update pk
    dtLong[, pk := likelihoodK / likelihood]

    # increase iter by 1
    iter <- iter + 1

    # ---- end of loop ----
  }

  return(
    list(listLoglike = listLoglike, listParams = listParams, dtLong = dtLong)
  )
}
