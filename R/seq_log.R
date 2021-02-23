#' Gives the repartition of values for a log 10 scale between a given interval
#'
#' @param x1 the first value of the interval
#' @param x2 the second value of the interval (can be higher or lower,
#' but never equal to x1)
#' @param divide whether to divide the result for major values (1,10,100)
#' and minor values (2,3,...,20,30,...)
#' @return the repartition of values for a log 10 scale between x1 and x2
#' @seealso \code{\link{convertAxis}}
#' @examples
#' x1 <- 101
#' x2 <- 0.29
#'
#' seq_log(x1, x2)
#' seq_log(x1, x2, divide = TRUE)
#'
#' @export

seq_log <- function(x1,x2,divide = FALSE)
{
  d  <- c(min(x1,x2),max(x1,x2))
  dc <- log10(d)

  dlim <- encase(dc[1],dc[2],1)
  du   <- seq(dlim[1],dlim[2],by = 1)
  s10  <- 10^du

  n <- length(s10) - 1

  accu <- c()

  for(i in 1:n)
  {
    u <- s10[i]
    t <- seq(u,s10[i + 1] - u,by = u)

    accu <- c(accu,t)

    if(i == n) accu <- c(accu,s10[i+1])
  }

  if(!divide) {

    res <- accu[accu >= d[1] & accu <= d[2]]

  } else if (divide) {

    inter <- accu[accu >= d[1] & accu <= d[2]]
    maj <- intersect(inter,s10)
    min <- setdiff(inter,s10)
    res <- list(maj,min)

  } else {warning("The \"divide\" parameter should be logical")}

  return(res)
}
