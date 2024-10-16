#' Title
#' @importFrom graphics abline
#' @importFrom stats pbinom pnorm
#'
#' @param N The total number of seats in the plane
#' @param gamma  The probability a plane will be truly overbooked
#' @param p The probability of a show
#'
#' @return Return a named list with number of tickets calculated with discrete distribution, number of tickets calculated with normal approximation, N, p, and gamma
#' @export
#'
#' @examples
#' nticket(N=400,gamma = 0.02, p = 0.95)
nticket <- function(N = 200, gamma = 0.02, p = 0.95){
  # Discrete case (binomial distribution)
  n = seq(N, floor(N + N/10), by = 1)
  tmp = 1 - gamma - pbinom(q = N, size = n, prob = p)
  ind = which.min(abs(tmp))


  # Continuous case
  f <- function(x){
    1 - gamma - pnorm(q = N + 0.5,  # Adding continuity correction
                      mean = x * p,
                      sd = sqrt(x * p * (1 - p)))
  }

  indc = stats::uniroot(f, c(N, floor(N + N/10)))

  # Top plot: Discrete case
  plot(x = n, y = tmp,
       xlab = "n", ylab = "Objective",
       main = paste("Objective Vs n to find optimal tickets sold (", n[ind], ") gamma=", gamma, " N=", N, " discrete"),
       col = "Blue", pch = 19)
  abline(h = tmp[ind], v = n[ind], col = "Red")

  # Bottom plot: Continuous case (normal approximation with continuity correction)
  curve(f,
        xlim = c(N,N + floor(N/10)),
        xlab = "n", ylab = "Objective",
        main = paste("Objective Vs n to find optimal tickets sold (", indc$root, ") gamma=", gamma, " N=", N, " Continuous"))

  abline(h = f(indc$root),
         v = indc$root,
         lwd = 1.5,
         col = "Blue")


  # Return the results as a named list
  list(nd = n[ind],  # Discrete optimal n
       nc = round(indc$root,4),  # Continuous optimal n with continuity correction
       N = N, gamma = gamma, p = p)
}
