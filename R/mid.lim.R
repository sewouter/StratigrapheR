#' @title Provides mid-points intervals in an ordered vector
#' @description Provides mid-points intervals in an ordered vector
#' @param x an ordered vector
#' @param id a vector of n interval IDs (default is 1 for each interval)
#' @param b a character vector for the interval boundaries rules, see
#' \code{as.lim} help page for details
#' @return a lim object of intervals with boundaries at midway between the x
#' values
#' @seealso \code{\link{as.lim}}
#' @examples
#' mid.lim(c(1,3,7,20,45,63))
#'
#' @export

mid.lim <- function(x, id = 1L, b = "[]")
{

  if(is.unsorted(x)){
    stop("The x values should be ordered")
  }

  xp <- c(NA, x[-length(x)])
  xm <- c(x[-1],NA)

  d <- data.frame(x = x, xp = xp, xm = xm)

  d$l <- d$x + (d$xp - d$x)/2
  d$r <- d$x - (d$x - d$xm)/2

  d$l[1] <- d$x[1]
  d$r[nrow(d)] <- d$x[nrow(d)]

  res <- as.lim(l = d$l, r = d$r, id = id, b = b)

  return(res)

}
