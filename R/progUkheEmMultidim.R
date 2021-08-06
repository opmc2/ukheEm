## -----------------------------------------------------------------------------
## progUkHeEmMultidim.R
##
## Project: UK HE returns + determinants
## Purpose: Contains the main function to implement the EM algorithm with
##          multidimensional types.
## Author: Oliver Cassagneau-Francis
## Date: Sat Jun 26 14:07:01 2021
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
#' @param K A J-dimensional vector corresponding to the number of types.
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
#' @param y1log A logical value indicating if the first pre-t measurement is in
#'   logs.
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
progUkheEm_md <- function(
  dt, K, varList,
  startVals = "kmeans",
  maxiter = 400,
  y1cont = TRUE, y1log = FALSE,
  J = length(K)
) {

  if (is.character(dt)) dt <- load(here::here(dt))

  cols2keep <- varList

  dt <- dt[, ..cols2keep]
  names(dt) <- names(varList)
  dt <- dt[complete.cases(dt)]

  NN <- dt[, .N]
  KK <- prod(K) # the number of types (total)

  if (isFALSE(y1cont)) {
    outcomes <- c("id", "left", "right", paste0("y", 2:J), "w")
  } else {
    outcomes <- c("id", paste0("y", 1:J), "w")
  }

  # set start values
  if (startVals == "kmeans") {
      startVals <- kmeansSVs_md(dt[, ..outcomes], K, y1cont = y1cont, J = J)
  }

  dt <- merge(dt, startVals$y[, .(id, svType1, svType2, svType)], by = "id")

  alpha <- startVals$alpha
  sigmaY <- startVals$sigmaY

  # make dt long by types
  dtLong <- list()
  for (k1 in 1:K[[1]]) {
    dtLong[[k1]] <- list()
    for (k2 in 1:K[[2]]) {
      dtLong[[k1]][[k2]] <- data.table(dt)
      dtLong[[k1]][[k2]][, c(paste0("type", 1:2), "type") := .(paste0(k1), paste(k2), paste0(k1, k2))]
    }
    dtLong[[k1]] <- rbindlist(dtLong[[k1]])
  }
  dtLong <- rbindlist(dtLong)
  setorder(dtLong, type1, type2, id)
  dtLong[, `:=`(
    type1 = factor(type1),
    type2 = factor(type2),
    type = factor(type)
  )]

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
                       default = .1 / (KK-1))]

  dtLong[, `:=`(
    mu = Hmisc::wtd.mean(w, pk),
    sigmaW = wtd.sd(w, pk)
  ), by = .(type, d)]


  # -------------- #
  # ---- LOOP ----
  # -------------- #

  while(iter < maxiter & abs(delta) > tol) {

    # NOTE: E-step is at end as first loop is technically zero-th iteration

    # ---- M-step ----

    # update mu and sigmaNu (parameters of the test score distribution)

    if (isTRUE(y1cont)) {
      dtLong[, `:=` (
        alpha1 = Hmisc::wtd.mean(y1, pk),
        sigma1 = wtd.sd(y1, pk)
      ), by = type1]

      if (J > 1) dtLong[, `:=` (
        alpha2 = Hmisc::wtd.mean(y2, pk),
        sigma2 = wtd.sd(y2, pk)
      ), by = type2]

      if (J > 2) dtLong[, `:=` (
        alpha3 = Hmisc::wtd.mean(y3, pk),
        sigma3 = wtd.sd(y3, pk)
      ), by = type3]

    } else {
      alphaSigmaRes <- list()
      for (k in 1:K[[1]]) {
        alphaSigmaRes[[k]] <- optim(
          par = c(alpha[[1]][[k]], sigmaY[[1]][[k]]),
          fn = function(theta) -ell(theta, x = dtLong[type == as.character(k)])
        )
        alpha[[1]][[k]] <- alphaSigmaRes[[k]]$par[[1]]
        sigmaY[[1]][[k]] <- alphaSigmaRes[[k]]$par[[2]]
        dtLong[type1 == k, c("alpha1", "sigmaY1") := .(alpha[[1]][[k]], sigmaY[[1]][[k]])]
      }

      if (J > 1) dtLong[, `:=` (
        alpha2 = Hmisc::wtd.mean(y2, pk),
        sigmaY2 = wtd.sd(y2, pk)
      ), by = type2]

      if (J > 2) dtLong[, `:=` (
        alpha3 = Hmisc::wtd.mean(y3, pk),
        sigmaY3 = wtd.sd(y3, pk)
      ), by = type3]
    }


    # update mu and sigmaW (parameters of the wage dist. @25)
    # can only update as "fully interacted model" i.e. mu for each type
    modelW <- lm(
      w ~ type:d - 1,
      data = dtLong,
      weights = pk / sigmaW
    )

    dtLong[, mu := ifelse(
      is.na(modelW$coefficients[paste0("type", type, ":", "d", d)]),
      mu, modelW$coefficients[paste0("type", type, ":", "d", d)])]

    dtLong[, sigmaW := ifelse(is.na(wtd.sd(w - mu, pk)),
                              sigmaW,
                              wtd.sd(w - mu, pk)), by = .(type, d)]

    # pi(k,z,d)

    dtLong[, pi_kzd := sum(pk) / NN, by = .(type, z, d)]


    # update likelihood|K
    # currently only w is log normal. other outcomes are assumed normal.
    if (isTRUE(y1cont)) {
      if (isTRUE(y1log)) {
        if (J == 1) {
          dtLong[, likelihoodK := pi_kzd *
                   1 / exp(y1) * dnorm(y1, mean = alpha1, sd = sigmaY1) *
                   1 / exp(w) * dnorm(w, mean = mu, sd = sigmaW)]
        } else if (J == 2) {
          dtLong[, likelihoodK := pi_kzd *
                   1 / exp(y1) * dnorm(y1, mean = alpha1, sd = sigmaY1) *
                   dnorm(y2, mean = alpha2, sd = sigmaY2) *
                   1 / exp(w) * dnorm(w, mean = mu, sd = sigmaW)]
        } else if (J == 3) {
          dtLong[, likelihoodK := pi_kzd *
                   1 / exp(y1) * dnorm(y1, mean = alpha1, sd = sigmaY1) *
                   dnorm(y2, mean = alpha2, sd = sigmaY2) *
                   dnorm(y3, mean = alpha3, sd = sigmaY3) *
                   1 / exp(w) * dnorm(w, mean = mu, sd = sigmaW)]
        } else if (J > 3) {
          print("There are too many pre-t outcomes.")
          stop()
        }
      } else {
        if (J == 1) {
          dtLong[, likelihoodK := pi_kzd *
                   dnorm(y1, mean = alpha1, sd = sigmaY1) *
                   1 / exp(w) * dnorm(w, mean = mu, sd = sigmaW)]
        } else if (J == 2) {
          dtLong[, likelihoodK := pi_kzd *
                   dnorm(y1, mean = alpha1, sd = sigmaY1) *
                   dnorm(y2, mean = alpha2, sd = sigmaY2) *
                   1 / exp(w) * dnorm(w, mean = mu, sd = sigmaW)]
        } else if (J == 3) {
          dtLong[, likelihoodK := pi_kzd *
                   dnorm(y1, mean = alpha1, sd = sigmaY1) *
                   dnorm(y2, mean = alpha2, sd = sigmaY2) *
                   dnorm(y3, mean = alpha3, sd = sigmaY3) *
                   1 / exp(w) * dnorm(w, mean = mu, sd = sigmaW)]
        } else if (J > 3) {
          print("There are too many pre-t outcomes.")
          stop()
        }
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
    } else if (J == 3) {
      dtLong[, likelihoodK := pi_kzd *
               (pnorm(right, mean = alpha1, sd = sigmaY1) -
                  pnorm(left, mean = alpha1, sd = sigmaY1)) *
               dnorm(y2, mean = alpha2, sd = sigmaY2) *
               dnorm(y3, mean = alpha3, sd = sigmaY3) *
               (1 / exp(w)) * dnorm(w, mean = mu, sd = sigmaW)]
    } else if (J > 3) {
      print("There are too many pre-t outcomes.")
      stop()
    }


    # sum likelihood|K to give likelihood
    dtLong[, likelihood := sum(likelihoodK), by = id]

    # save parameters and likelihoods
    if (J == 1) {
      listParams[[iter]] <- dtLong[, .(
        alpha1 = mean(alpha1),
        sigmaY1 = mean(sigmaY1),
        mu = mean(mu),
        sigmaW = mean(sigmaW),
        pi_kzd = mean(pi_kzd)
      ), by = .(type, d, z)]
    } else if (J == 2) {
      listParams[[iter]] <- dtLong[, .(
        alpha1 = mean(alpha1), alpha2 = mean(alpha2),
        sigmaY1 = mean(sigmaY1), sigmaY2 = mean(sigmaY2),
        mu = mean(mu),
        sigmaW = mean(sigmaW),
        pi_kzd = mean(pi_kzd)
      ), by = .(type, d, z)]
    } else if (J == 3) {
      listParams[[iter]] <- dtLong[, .(
        alpha1 = mean(alpha1), alpha2 = mean(alpha2), alpha3 = mean(alpha3),
        sigmaY1 = mean(sigmaY1), sigmaY2 = mean(sigmaY2),
        sigmaY3 = mean(sigmaY3),
        mu = mean(mu),
        sigmaW = mean(sigmaW),
        pi_kzd = mean(pi_kzd)
      ), by = .(type, d, z)]
    }

    listLike[[iter]] <- dtLong[type == "11", prod(likelihood)]
    listLoglike[[iter]] <- dtLong[type == "11", sum(log(likelihood))]

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
