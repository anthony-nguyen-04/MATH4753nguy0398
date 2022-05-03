#' Given a probability-based function and a range of potential values,
#' determines the value of theta that has the highest likelihood of occurring.
#'
#' @param lfun Function applied to the range of potential values (theta)
#' @param theta Range of values, with the value with the highest likelihood
#' being chosen
#'
#' @return theta value, with the maximum likelihood
#'
#' @export
#'
mymaxlikg=function(lfun="logbin2",theta) {

  # numbers of values in theta
  nth=length(theta)

  # converts theta into a (nth x 1) matrix
  thmat=matrix(theta,nrow=nth,ncol=1,byrow=TRUE)

  # applies lfun to all values within theta
  z=apply(thmat,1,lfun)

  # finds index for theta with max likelihood value
  zmax=max(which(z==max(z)))

  # plots theta with its corresponding likelihood value
  plot(theta,exp(z),type="l")

  # places vertical line through theta with max likelihood value
  abline(v=theta[zmax],col="Blue")

  # one tick on the third axis
  axis(3,theta[zmax],round(theta[zmax],4))

  # prints theta with maximum likelihood value
  theta[zmax]

  # returns theta with maximum likelihood value
  # (ADDED)
  return(theta[zmax])
}
