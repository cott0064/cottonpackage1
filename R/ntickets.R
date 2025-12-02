#' ntickets
#'
#' @param N number of seats on plane
#' @param gamma probability of overbooking
#' @param p probability passenger will show
#'
#' @importFrom graphics abline barplot points
#'
#' @importFrom grDevices rainbow
#' @importFrom graphics abline barplot curve hist layout lines mtext par points polygon
#' @importFrom stats dnorm pbinom pnorm
#'
#' @return list of nc, nd, N, p, gamma, and a plot
#' @export
#'
#' @examples
#' \dontrun{ntickets(N = 400, gamma = 0.02, p = 0.95)}
ntickets = function(N, gamma, p){

  # APPROPRIATE DISCRETE DISTRIBUTION

  # range of n
  range = seq(N, N + 50, by = 1)
  # discrete objective
  obj_discrete = 1 - gamma - pbinom(N, size = range, prob = p)
  # index of min objective
  idx_discrete = which.min(abs(obj_discrete))
  # min n value
  nd = range[idx_discrete]
  # plot
  plot(range, obj_discrete,
       type = "n",
       main = "Objective vs n for optimal tickets sold (Discrete)", # title
       xlab = "n values", # x label
       ylab = "Objective") # y label
  # line of discrete objective
  lines(range, obj_discrete)
  # points along line of discrete objective
  points(range, obj_discrete, pch = 21, col = "black", bg = "blue")
  # point for optimal n
  points(range[idx_discrete], obj_discrete[idx_discrete], pch = 21, col = "black", bg = "#FDF9D8") # OU creme hex code :)
  # horizontal and vertical lines of intersection for optimal n
  abline( h = 0, v = range[idx_discrete], col = "#841617", lty = 1) # I crimson hex code :)


  # NORMAL APPROXIMATION

  # range of n
  range = seq(N, N + 50, by = 1)
  # approximation
  z_value = (N + (1/2) - range * p) / sqrt(range * p * (1 - p))
  p_approx = pnorm(z_value)
  # continuous objective
  obj_continuous = 1 - gamma - p_approx
  # index of min objective
  idx_cont = which.min(abs(obj_continuous))
  # min n
  nc = range[idx_cont]
  # plot
  plot(range, obj_continuous,
       type = "n",
       main = "Objective vs n for optimal tickets sold (Continuous)", # title
       xlab = "n values", # x label
       ylab = "Objective") # y label
  # line of continuous objective
  lines(range, obj_continuous)
  # point for optimal n
  points(range[idx_cont], obj_continuous[idx_cont], pch = 21, bg = "#FDF9D8", cex = 1)
  # horizontal and vertical reference lines
  abline(h = 0, v = range[idx_discrete], col = "darkblue", lty = 1)
  # return the list of variables
  return(list(nd = nd, nc = nc, N = N, p = p, gamma = gamma))
}

