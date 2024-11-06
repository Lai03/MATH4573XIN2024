#' Title
#'
#' @importFrom graphics axis
#' This function calculates the maximum likelihood estimate (MLE) for a given range of parameter values (`theta`) and a specified likelihood function (`lfun`). It plots the likelihood curve and highlights the parameter value that maximizes the likelihood.
#'
#' @param lfun A likelihood function that takes a single parameter and returns the log-likelihood value. This function is applied to each value of `theta`. It should be provided as a function name without quotes.
#' @param theta A numeric vector of possible parameter values to evaluate, representing the range of values for the parameter of interest.
#'
#' @return The function returns the value of `theta` that maximizes the likelihood.
#' @export
#'
#' @examples
#' # Example usage with a custom likelihood function
#' logbin2 <- function(theta) { sum(dbinom(c(3,3,4,3,4,5,5,4), size=20, prob=theta, log=TRUE)) }
#' mymaxlikg(lfun=logbin2, theta=seq(0, 1, length=10000))
mymaxlikg=function(lfun="logbin2",theta) { # default log lik is a combination bin
  nth=length(theta)  # nu. of valuse used in theta
  thmat=matrix(theta,nrow=nth,ncol=1,byrow=TRUE) # Matrix of theta
  z=apply(thmat,1,lfun) # z holds the log lik values
  zmax=max(which(z==max(z)))  # finding the INDEX of the max lik
  plot(theta,exp(z),type="l") # plot of lik
  abline(v=theta[zmax],col="Blue")   #  verical line through max
  axis(3,theta[zmax],round(theta[zmax],4))  # one tick on the third axis
  theta[zmax]   # theta corresponding to max lik
}
