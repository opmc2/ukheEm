## -----------------------------------------------------------------------------
## helperFuns.R
##
## Project: UK HE Exp
## Purpose: Helper functions for the main EM function
## Author: Oliver Cassagneau-Francis
## Date: Mon Aug 10 14:23:29 2020
## -----------------------------------------------------------------------------
## Notes:


#' K-means Starting Values
#'
#' Use \code{\link[stats]{kmeans}} to select starting values for the EM algorithm.
#'
#' @param y A vector or matrix.
#' @param K A number (of types / groups).
#' @param y1cont A logical indicating the first component of y is continuous.
#' @return A list of the starting values and other information.
#'   \itemize{
#'     \item \code{P_k} The proportions assigned to each group by
#'     \code{\link[stats]{kmeans}}
#'     \item \code{mu} The mean pre-treatment outcomes for each group
#'     \item \code{alpha} The mean post-treatment outcomes for each group
#'     \item \code{sigmaNu, sigmaEps} The standardard deviation of the pre- and
#'       post-treatment outcomes for each group
#'     \item \code{svTypes} The groups assigned to each observation by
#'       \code{\link[stats]{kmeans}}, i.e. \code{cluster}
#'     \item \code{muSigmaRes} An intermediate list containing results of ML to
#'       make y1 continuous if it is not. i.e. if \code{y1cont == FALSE}.
#'   }
kmeansSVs <- function(y, K, y1cont = TRUE, y1b = FALSE) {
  if (isTRUE(y1cont)) {
    # y should be a vector or matrix containing the continuous outcomes, and
    # K is the number of types
    res <- kmeans(y, centers = K, nstart = 5)
    mu <- res$centers[, 1]
    alpha <- cbind(res$centers[, 2], res$centers[, 2])
    y[, svType := res$cluster]
    sigmas <- y[, .(sigmaNu = sd(y1), sigmaEps = sd(y2)),
                by = svType][order(svType)]
    sigmaNu <- sigmas$sigmaNu
    sigmaEps <- cbind(sigmas$sigmaEps, sigmas$sigmaEps)
    P_k <- y[, .N, by = svType][order(svType)][, N] / y[, .N]
    muSigmaRes <- list()
  } else if (isTRUE(y1b)) {
    # y contains a continuous outcome (currently y2), "left" and "right" for
    # binned data, and continuous y1b.
    # K is the number of types
    res <- kmeans(y[, .(y1b, y2)], centers = K, nstart = 5)
    muB <- res$centers[, 1]
    alpha <- cbind(res$centers[, 2], res$centers[, 2])
    y[, svType := res$cluster]
    muSigmaRes <- list()
    mu <- numeric(length = K)
    sigmaNu <- numeric(length = K)
    y[, pk := 1]
    for (k in 1:K) {
      muSigmaRes[[k]] <- optim(
        par = ellInit(y[svType == k]),
        fn = function(theta) -ell(theta, x = y[svType == k])
      )
      mu[[k]] <- muSigmaRes[[k]]$par[[1]]
      sigmaNu[[k]] <- muSigmaRes[[k]]$par[[2]]
    }
    sigmaEps <- y[, sd(y2),
                  by = svType][order(svType)][, V1]
    P_k <- y[, .N, by = svType][order(svType)][, N] / y[, .N]
  } else {
    # y contains a continuous outcome (currently y2) and "left", "right" for
    # binned data.
    # K is the number of types
    res <- kmeans(y[, y2], centers = K, nstart = 5)
    alpha <- cbind(res$centers[, 1], res$centers[, 1])
    y[, svType := res$cluster]
    muSigmaRes <- list()
    mu <- numeric(length = K)
    sigmaNu <- numeric(length = K)
    y[, pk := 1]
    for (k in 1:K) {
      muSigmaRes[[k]] <- optim(
        par = ellInit(y[svType == k]),
        fn = function(theta) -ell(theta, x = y[svType == k])
      )
      mu[[k]] <- muSigmaRes[[k]]$par[[1]]
      sigmaNu[[k]] <- muSigmaRes[[k]]$par[[2]]
    }
    sigmaEps <- y[, sd(y2),
                  by = svType][order(svType)][, V1]
    P_k <- y[, .N, by = svType][order(svType)][, N] / y[, .N]
  }
  if (isTRUE(y1b)) {
    return(list(
      P_k = P_k, mu = mu, muB = muB, alpha = alpha, sigmaNu = sigmaNu,
      sigmaEps = sigmaEps, svTypes = res$cluster,
      muSigmaRes = muSigmaRes
    ))
  } else {
    return(list(
      P_k = P_k, mu = mu, alpha = alpha, sigmaNu = sigmaNu,
      sigmaEps = sigmaEps, svTypes = res$cluster,
      muSigmaRes = muSigmaRes
    ))
  }

}

# likelihood to fit Gaussian to binned data
#
#' Likelihood for binned data
#'
#' A function to calculate the likelihood of binned data from a Gaussian
#' distribution.
#'
#' @param theta A vector containing the parameters of the distribution to fit,
#'   the mean (\code{theta[[1]]}) and the sd (\code{theta[[2]]}).
#' @param x A data.table containing each binned observation as two values: a
#'   column labelled \code{left} corresponding to the lower bound and
#'   \code{right} corresponding to the upper.
#' @return The value for the likelihood of the data in \code{x} being drawn
#'   from the Gaussian distribution with parameters \code{theta}.
ell <- function(theta, x) {
  sum(
    x[, pk] * log(pnorm(x[, right], mean = theta[[1]],
                        sd = theta[[2]]) -
                    pnorm(x[, left], mean = theta[[1]],
                          sd = theta[[2]])))
}

#' Initial values for parameters of Gaussian distribution
#'
#' Calculates initial values for the Gaussian distribution from binnned data to
#' use with \code{\link[stats]{optim}} when performing ML.
#'
#' @inheritParams ell
ellInit <- function(x) {
  mid <- (x[, left] + x[, right]) / 2
  c(mean(mid, na.rm = T), sd(mid, na.rm = TRUE))
}
