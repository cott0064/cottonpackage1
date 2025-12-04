#' logbin2
#'
#' Log-likelihood for a binomial combination.
#'
#' @param theta Probability parameter
#'
#' @return Log-likelihood value
#' @export
logbin2 <- function(theta) {
  log(dbinom(2, prob = theta, size = 6)) +
    log(dbinom(4, prob = theta, size = 10))
}

#' mymaxlikg
#'
#' Computes and plots likelihood and finds the MLE.
#'
#' @param lfun Log-likelihood function (default = "logbin2")
#' @param theta Numeric vector of theta values
#'
#' @return Theta value at the maximum likelihood
#' @export
mymaxlikg <- function(lfun = "logbin2", theta) {
  nth <- length(theta)
  thmat <- matrix(theta, nrow = nth, ncol = 1, byrow = TRUE)
  z <- apply(thmat, 1, lfun)
  zmax <- which(z == max(z))[1]
  plot(theta, exp(z), type = "l")
  abline(v = theta[zmax], col = "blue")
  axis(3, at = theta[zmax], labels = round(theta[zmax], 4))
  theta[zmax]
}
