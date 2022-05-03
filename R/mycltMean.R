#' Generates an n-sized random sample from the uniform distribution for a given
#' amount of iterations. Then, calculates the mean value for each simulated
#' iterations to form a histogram of this data.
#'
#' @param n the size of the random sample
#' @param iter the number of iterations within the simulation
#'
#' @return the vector containing the vector of mean values for each iteration's
#' random sample
#'
#' @export
#'
mycltMean=function(n,iter){

  # generates random sample from uniform of (n*iter) size
  # lower limit of 0, upper limit of 5
  y=runif(n*iter,0,5)

  # transforms Y into a (n x iter) matrix
  # each column represents one simulation
  data=matrix(y,nr=n,nc=iter,byrow=TRUE)

  # calculates the mean of each column on a column-by-column basis
  mn=apply(data,2,mean)

  # forms histogram of the mean vector from above
  hist(mn)

  # vector holding the mean of each iteration's random sample
  mn
}
