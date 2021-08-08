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
#' @param J An integer with the number of pre-treatment outcomes.
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
kmeansSVs <- function(y, K, y1cont = TRUE, J = 1) {
  if (isTRUE(y1cont)) {

    # y should be a vector or matrix containing the continuous outcomes, and
    # K is the number of types

    res <- kmeans(y, centers = K, nstart = 5)
    alpha <- res$centers[, 1:J]
    mu <- cbind(res$centers[,J+1], res$centers[, J+1])
    y[, svType := res$cluster]
    sigmaY <- y[, lapply(.SD, sd), .SDcols = paste0("y", 1:J),
                by = svType][order(svType)]
    sigmaW <- y[, sd(w),
                by = svType][order(svType)][, V1]
    P_k <- y[, .N, by = svType][order(svType)][, N] / y[, .N]
    alphaSigmaRes <- list()

  } else if (isFALSE(y1cont)) {

    # y contains one pre-t outcome as binned data
    # K is the number of types

    res <- kmeans(y[, -c("left", "right")], centers = K, nstart = 5)
    alpha <- matrix(nrow = K, ncol = J)
    if (J > 1) alpha[, -1] <- res$centers[, 1:J-1]
    y[, svType := res$cluster]
    mu <- cbind(res$centers[,J], res$centers[, J])
    alphaSigmaRes <- list()
    if (J > 1) sigmaY <- y[, lapply(.SD, sd), .SDcols = paste0("y", 2:J),
                by = svType][order(svType)]
    y[, pk := 1]
    for (k in 1:K) {
      alphaSigmaRes[[k]] <- optim(
        par = ellInit(y[svType == k]),
        fn = function(theta) -ell(theta, x = y[svType == k])
      )
      alpha[k, 1] <- alphaSigmaRes[[k]]$par[[1]]
      sigmaY[svType == k, y1 := alphaSigmaRes[[k]]$par[[2]]]
    }
    sigmaW <- y[, sd(w),
                  by = svType][order(svType)][, V1]
    P_k <- y[, .N, by = svType][order(svType)][, N] / y[, .N]
  }

  return(list(
    P_k = P_k, mu = mu, alpha = alpha, sigmaY = sigmaY,
    sigmaW = sigmaW, svTypes = res$cluster,
    alphaSigmaRes = alphaSigmaRes
  ))

}

#' K-means Starting Values (multidimensional types)
#'
#' Uses \code{\link[stats]{kmeans}} to select starting values for the EM
#' algorithm.
#'
#' @param y A vector or matrix containing the outcomes.
#' @param K A vector containing the number of types / groups.
#' @param y1cont A logical indicating the first component of y is continuous.
#' @param J An integer with the number of pre-treatment outcomes.
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
kmeansSVs_md <- function(y, K, y1cont = TRUE, J = length(K)) {

  # y should be a vector or matrix containing the continuous outcomes, and
  # K is the number of types

  if (isFALSE(y1cont)) y[, y1 := left + right / 2]

  res <- list()
  alpha <- list()
  for (j in 1:J) {
    res[[j]] <- kmeans(y[[paste0("y", j)]], centers = K[[j]], nstart = 5)
    alpha[[j]] <- res[[j]]$centers
    y[, paste0("svType", j) := res[[j]]$cluster]
  }

  if (J == 1) {
    y[, svType := paste0(svType1)]
  } else if (J == 2) {
    y[, svType := paste0(svType1, svType2)]
  } else if (J == 3) {
    y[, svType := paste0(svType1, svType2, svType3)]
  } else {
    print("J > 3")
  }

  sigmaY <- list()
  y[, c("alpha1", "sigmaY1") := .(mean(y1), sd(y1)), by = svType1]
  sigmaY[[1]] <- y[, unique(sigmaY1), by = svType1][order(svType1)][["V1"]]
  if (J > 1) {
    y[, c("alpha2", "sigmaY2") := .(mean(y2), sd(y2)), by = svType2]
    sigmaY[[2]] <- y[, unique(sigmaY2), by = svType2][order(svType2)][["V1"]]
  }
  if (J > 2) {
    y[, c("alpha3", "sigmaY3") := .(mean(y3), sd(y3)), by = svType3]
    sigmaY[[3]] <- y[, unique(sigmaY3), by = svType3][order(svType3)][["V1"]]
  }

  y[, c("mu", "sigmaW") := .(mean(w), sd(w)), by = svType]
  NN <- y[, .N]
  y[, P_k := .N / NN, by = svType]

  return(list(
    y = y, res = res, alpha = alpha, sigmaY = sigmaY
  ))

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

#' Weighted standard deviation
#'
#' Calculates weighted standard deviation as the square root of wtd.var.
#'
#' @inheritParams Hmisc::wtd.var
wtd.sd <- function(...) sqrt(Hmisc::wtd.var(...))
