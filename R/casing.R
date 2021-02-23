#' Finds values in a vector directly above and below a number
#'
#' @param x a number
#' @param into a vector where to find the values directly above and
#' below x
#' @return a vector of the values of "into" vector directly above and below x
#' respectively
#'
#' @seealso Similar function : \code{\link{encase}}
#'
#' @examples
#' casing(0.21,c(0.3,0.4,0.1,0.2))
#'
#' @export

casing <- function(x,into)
{
  v  <- sort(unique(into))

  if(x > max(v)) {
    stop("x higher than the maximum of v ")
  } else if (x < min(v)) {
    stop("x lower than the minimum of v ")
  }

  pos.pro <- which.min(abs(v-x))
  val.pro <- x - v[pos.pro]

  if(val.pro == 0) {
    pos.sec <- pos.pro
  } else if (val.pro > 0) {
    pos.sec <- pos.pro + 1
  } else if (val.pro < 0) {
    pos.sec <- pos.pro - 1
  }

  return(sort(c(v[pos.pro],v[pos.sec])))
}
