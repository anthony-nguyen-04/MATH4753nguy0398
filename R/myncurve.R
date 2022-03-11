#' Displays a normal curve (given mu and sigma), shades in the area between
#' the curve and the x-axis from -Inf (approximated to 3 SD's away from the mean)
#' to x=a (given a), and returns the area as a form of probability.
#'
#' @param mu the mean of the normal distribution
#' @param sigma the standard deviation of the normal distribution
#' @param a the right-most value, such that the probability is calculated from
#' negative infinity to x=a
#'
#' @return the probability of P(Y <= a)
#'
#' @export

myncurve = function(mu, sigma,a){
  curve(dnorm(x, mean=mu, sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))

  xcurve=seq(mu-3*sigma,a,length=1000)

  ycurve=dnorm(xcurve, mu, sigma)

  polygon(c(mu-3*sigma,xcurve,a),c(0,ycurve,0),col="Red")

  prob=pnorm(a, mu, sigma)
  prob=round(prob,4)

  return(prob)
}
