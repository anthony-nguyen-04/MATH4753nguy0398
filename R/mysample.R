#' Generates a given amount of iterations for a given amount of random samples
#' between 1 and 10 (inclusive) with replacement. Then, graphs the data
#' to a barplot, before waiting to start the next iteration.
#'
#' @param n the amount of times a number from [1:10] will be selected by random
#'  sampling in a given iteration
#' @param iter the amount of experiments that this random sampling will occur
#' @param time the time between any two iterations
#'
#' @export

mysample <- function(n, iter=10,time=0.5){
  for( i in 1:iter){
    # make a sample
    s=sample(1:10,n,replace=TRUE)

    # turn the sample into a factor
    sf=factor(s,levels=1:10)

    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Example sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )

    #release the table
    Sys.sleep(time)
  }
}
