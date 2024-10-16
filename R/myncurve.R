#' @title myncurve
#' @importFrom graphics curve polygon text
#' @importFrom stats dnorm pnorm
#'
#' @param mu The mean of the normal distribution
#' @param sigma The standard deviation of the normal distribution
#' @param a The upper bound of the lower tail
#'
#' @return A plot of the normal distribution with polygon that shades the lower tail and a output of the probability rounded to 4 decimal place.
#'
#' @export
#'
#' @examples
#' myncurve(10, 5, 6)
myncurve = function(mu, sigma, a){
  x <- NULL
  curve(dnorm(x,mean=mu,sd=sigma), xlim = c(mu-3*sigma, mu + 3*sigma))
  xcurve=seq(mu-3*sigma,a,length=1000)
  ycurve=dnorm(xcurve,mu,sigma)
  polygon(c(mu-3*sigma, xcurve, a), c(0, ycurve, 0), col = "Pink")
  area <- round(pnorm(a, mu, sigma), 4)
  text(a, dnorm(a, mu, sigma) / 2, paste0("Area/Probability = ", area))
  return(list(mu = mu, sigma = sigma, area = area))
}
