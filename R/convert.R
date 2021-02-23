#' @title Converts x values having an index into n values defined by the same
#' y index
#'
#' @description Converts x values having an index (of y values for instance)
#' into n values defined by the same index (but having possibly more values)
#'
#' @param x a vector
#' @param xindex the index for each x value (vector of same length
#' than x)
#' @param n a vector of the values into which to convert the x values
#' @param nindex the index for each n value (vector of same length
#' than n)
#' @examples
#' x      <- c(10,20)
#' xindex <- c(1,2)
#'
#' n      <- seq(0.1,1,by = 0.1)
#' nindex <- 1:length(n)
#'
#' convert(x,xindex,n,nindex)
#'
#' @importFrom dplyr left_join
#' @export

convert <- function(x,xindex,n,nindex)
{
  d1 <- data.frame(xindex,x)
  d2 <- data.frame(nindex,n)

  colnames(d1) <- c("index","x")
  colnames(d2) <- c("index","n")

  df       <- left_join(d1,d2,"index")

  output <- df[,c(2,3,1)]

  return(output)
}
