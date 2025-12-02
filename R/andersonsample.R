#' andersonsample
#'
#' @param n Integer. Sample size.
#' @param iter Integer. Number of iterations. Default is 10.
#' @param time Numeric. Time parameter. Default is 0.5.
#'
#' @return Output of the sampling function.
#' @export
#'
#' @examples
mysample=function(n, iter=10,time=0.5){
  for( i in 1:iter){
    #make a sample
    s=sample(1:10,n,replace=TRUE)
    # turn the sample into a factor
    sf=factor(s,levels=1:10)
    #make a barplot
    barplot(table(sf)/n,beside=TRUE,col=rainbow(10),
            main=paste("Anderson sample()", " iteration ", i, " n= ", n,sep="") ,
            ylim=c(0,0.2)
    )

    #release the table
    Sys.sleep(time)
  }
}
