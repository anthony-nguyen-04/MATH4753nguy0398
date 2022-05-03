#' Given data, a probability-based function, and a range of potential values,
#' this function determines the potential value that has the highest likelihood
#' of generating this data.
#'
#' @param x Data, used to determine the maximum likelihood outcome
#' @param lfun Function applied to the range of potential values
#' @param param Range of values, with the value with the highest likelihood
#' being chosen
#' @param ... more parameters for plotting maximum likelihood
#'
#' @return List, containing the index of the maximum likelihood outcome, the
#' maximum likelihood outcome, the maximum likelihood, and the slope of
#' the maximum likelihood outcome
#'
#' @export

mymaxlik=function(lfun,x,param,...){

  # length of potential values
  np=length(param)

  # applies the function (lfun) to all combinations of the elements
  # of x and the elements of param, with these elements serving
  # as the function's parameters
  z=outer(x,param,lfun)

  # creates vector, made up of the column sums
  y=apply(z,2,sum)

  # plots each potential value with its corresponding likelihood
  plot(param,y,col="Blue",type="l",lwd=2,...)

  # gets index of what potential value has the maximum likelihood
  i=max(which(y==max(y)))

  # plots a vertical line of where the maximum likelihood is located
  abline(v=param[i],lwd=2,col="Red")

  # places a point where the maximum likelihood is located
  points(param[i],y[i],pch=19,cex=1.5,col="Black")
  axis(3,param[i],round(param[i],2))

  # check slopes. If it is a max the slope should change sign from + to -
  # We should get three + and two -vs
  ifelse(i-3>=1 & i+2<=np, slope<-(y[(i-2):(i+2)]-y[(i-3):(i+1)])/(param[(i-2):(i+2)]-param[(i-3):(i+1)]),slope<-"NA")
  return(list(i=i,parami=param[i],yi=y[i],slope=slope))
}
