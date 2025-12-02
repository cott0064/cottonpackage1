#' myncurve
#'
#' @param mu mean
#' @param sigma standard deviation
#' @param a value to compute P(X <= a)
#'
#' @importFrom graphics curve polygon
#' @autoglobal
#'
#' @importFrom stats dnorm pbinom pnorm
#' @importFrom grDevices rainbow
#' @importFrom graphics abline barplot curve hist layout lines mtext par points polygon
#'
#' @returns A named list with components 'mu', 'sigma', 'a', and 'area'
#' @export
#'
#' @examples
#' \dontrun{myncurve(mu=10, sigma=5, a=6)}
myncurve = function(mu, sigma,a ){
  curve(dnorm(x,mean=mu,sd=sigma),
        xlim = c(mu-3*sigma, mu + 3*sigma),
        main = paste("P(X <=", a, ")"),
        ylab = "Density",
        xlab = "x")

  x_vals = seq(mu - 3 * sigma, a, length = 1000)
  y_vals = dnorm(x_vals, mean = mu, sd = sigma)
  polygon(c(x_vals, a), c(y_vals,0), col="red")

  area = round(pnorm(a, mean = mu, sd = sigma), 4)

  return(list(mu = mu, sigma = sigma, a =a, area = area))
}

