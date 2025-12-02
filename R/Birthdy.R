#' Birthday Function
#'
#' @description Computes the probability that at least two people in a group share the same birthday.
#'
#' @param x Integer or numeric vector. The number of people in the group (sample size)
#'
#' @returnsA numeric value (or vector) of probabilities.
#' @export
#'
#'
#' @examples
#' birthday(20:24)
#'
birthday <- function(x){
  1 - exp(lchoose(365, x) + lfactorial(x) - x*log(365))
}


