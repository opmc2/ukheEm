% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/helperFuns.R
\name{ell}
\alias{ell}
\title{Likelihood for binned data}
\usage{
ell(theta, x)
}
\arguments{
\item{theta}{A vector containing the parameters of the distribution to fit,
the mean (\code{theta[[1]]}) and the sd (\code{theta[[2]]}).}

\item{x}{A data.table containing each binned observation as two values: a
column labelled \code{left} corresponding to the lower bound and
\code{right} corresponding to the upper.}
}
\value{
The value for the likelihood of the data in \code{x} being drawn
  from the Gaussian distribution with parameters \code{theta}.
}
\description{
A function to calculate the likelihood of binned data from a Gaussian
distribution.
}
