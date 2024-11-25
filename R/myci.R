#' Title
#'
#' @importFrom stats qt sd
#' @param x input sample
#'
#' @return 95 percent confidence interval
#' @export
#'
#' @examples
#' d=c(5.0581, 4.9707, 5.0893, 4.9334, 4.9777, 5.0285, 4.8555, 4.9565)
#' myci(d)
myci=function(x) { #
  n=length(x)
  df=n-1
  s=sd(x)
  sampleMean=mean(x)
  mp = c(-1,1)

  # for 95% ci
  alpha=0.05
  t=qt(1-alpha/2,df)
  sampleMean + mp*(t*s)/(sqrt(n))
}
