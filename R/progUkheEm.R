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
#'       \code{.rds} file using \code{saveRDS}, or
#'     \item a data.table containing the wage and education
#'       data.
#'   }
#'   The columns containing the outcomes should be supplied as a named
#'   vector in \code{varList}.
#' @param K An integer corresponding to the number of types.
#' @param varList A named vector of the key variables in the model. The names
#'   should be:
#'   \itemize{
#'     \item \code{y1}: the pre-treatment outcome
#'     \item \code{y2}: the post-treatment outcome (log-wages)
#'     \item \code{z}: the instrument
#'     \item \code{d}: the treatment (university here)
#'   }
#' @param startVals Either a character vector to use \code{kmeans()} or a vector
#'   of starting values for the algorithm.
#' @param maxiter An integer specifying the maximum number of iterations before
#'   stopping.
#' @param y1cont A logical value indicating whether the first outcome is
#'   continuous. If \code{y1cont} is set to \code{FALSE}, a special ML routine
#'   is used, and the data.table \code{dt} \strong{must} contain two extra
#'   columns, \code{left} and \code{right}:
#'   \itemize{
#'     \item the lower bound of the interval containing \code{y1} (\code{left})
#'     \item the upper bound of the interval containing \code{y1} (\code{right})
#'   }
#' @return Returns a named list containing:
#' \describe{
#'   \item{\code{listLoglike}}{A list containing the log-likelihood after each
#'     iteration for analysing convergence.}
#'   \item{\code{listParams}}{A list containing the parameters after each
#'     iteration for analysing convergence.}
#'   \item{\code{dtLong}}{The final data.table where calculations took place
#'     including the final parameters and intermediate values.}
#' }
# #' @example examples/progUkheEm_ex1.R
progUkheEm <- function(
  dt, K, varList,
  startVals = "kmeans",
  maxiter = 400,
  y1cont = T
) {

  if (is.character(dt)) dt <- readRDS(here::here(dt))

  setnames(dt, old = varList, new = names(varList))

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
      dtLong[, c("mu", "sigmaNu") := .(Hmisc::wtd.mean(y1, pk),
                                       sqrt(Hmisc::wtd.var(y1, pk))),
             by = .(type, d)]
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
      ifelse(is.na(Hmisc::wtd.mean(y2, pk)), alpha, Hmisc::wtd.mean(y2, pk)),
      ifelse(is.na(sqrt(Hmisc::wtd.var(y2, pk))),
             sigmaEps,
             sqrt(Hmisc::wtd.mean(y2, pk)))),
      by = .(type, d)]



    # update bk and gamma

    modelS <- glm(
      formula = d ~ type + z - 1,
      family = binomial(link = "logit"),
      data = dtLong,
      weights = pk
    )

    coeffS <- coefficients(modelS)

    for (k in 1:K) {
      dtLong[type == paste0(k), bk := coeffS[paste0("type", k)]]
    }

    for (val in levels(dtLong[, z])) {
      dtLong[z == val, gamma := coeffS[paste0("z", val)]]
    }
    dtLong[is.na(gamma), gamma := 0]

    dtLong[, Ps1_kz := fitted.values(modelS)]

    # update pi(k,z)

    dtLong[, pi_kz := sum(pk) / NN, by = .(type, z)]


    # update likelihood|K
    dtLong[, likelihoodK := pi_kz * Ps1_kz *
             (pnorm(right, mean = mu, sd = sigmaNu) - pnorm(left, mean = mu, sd = sigmaNu)) *
             (1 / exp(y2)) * dnorm(y2, mean = alpha, sd = sigmaEps)]

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

    # increase iter by 1
    iter <- iter + 1

    # ---- end of loop ----
  }

  return(
    list(listLoglike = listLoglike, listParams = listParams, dtLong = dtLong)
  )
}
