#' Encases two numbers between multiples of a given number
#'
#' @param x1 the first value of the interval
#' @param x2 the second value of the interval (can be higher or lower,
#' but never equal to x1)
#' @param n the number to find the multiples from
#' @return the multiples of n directly encompassing x1 and x2
#'
#' @seealso Similar function : \code{\link{casing}}
#'
#' @examples
#' encase(5,1,5)
#'
#' @export


encase <- function(x1,x2,n)
{
  l1  <- round(x1/n,digits=0)
  l2  <- round(x2/n,digits=0)

  if(x1 == x2){stop("x1 and x2 should be different")} else {}

  if((l1 - x1/n) == 0){r1 <- x1} else {
    if(x2 >= x1){
      if((l1*n) < x1){r1 <- l1*n} else {r1 <- (l1*n) - n}
    } else {
      if((l1*n) > x1){r1 <- l1*n} else {r1 <- (l1*n) + n}
    }
  }

  if((l2 - x2/n) == 0){r2 <- x2} else {
    if(x2 >= x1){
      if((l2*n) > x2){r2 <- l2*n} else {r2 <- (l2*n) + n}
    } else {
      if((l2*n) < x2){r2 <- l2*n} else {r2 <- (l2*n) - n}
    }

  }
  res <- c(r1,r2)
  return(res)
}

