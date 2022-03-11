#' Calculate the value of y = beta_0 + beta_1 \* x + beta_2 \* I (x > x_k)
#' I (x > x_k) is 1 when x > x_k and 0 otherwise.
#' x_k = 18
#'
#' @param x a number
#' @param coef array that holds the coefficients for our function (beta_0, beta_1, beta_2)
#' @return y = beta_0 + beta_1 \* x + beta_2 \* I (x > x_k)
#'
#' @export

myf <- function(x,coef){
  return(coef[1]+coef[2]*(x) + coef[3]*(x-18)*(x-18>0))
}

