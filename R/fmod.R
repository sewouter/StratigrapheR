#' Universal remainder function
#'
#' @description Given a [xmin,xmax[ or ]xmin,xmax] interval, this function
#' determines the remainder of each numeric relative to this interval. In other
#' words if the interval was repeated over the whole numeric domain, this
#' function determines where each value would be positioned in a given
#' repetition.
#'
#' @param x vector of floating point numbers
#' @param xmax,xmin the limits of the interval
#' @param bounds how to deal with boundaries (right- or left-open; '[['
#' or ']]')
#' @seealso \code{\link{incfix}}, \code{\link{dipfix}} and \code{\link{transphere}}
#' @examples
#' fmod(c(1260.23,360),360)
#'
#' fmod(c(1260.23,360),360,bounds = "]]")
#'
#' fmod(c(1260.23,360),360 + 180, 180)
#'
#' @export

fmod <- function (x, xmax, xmin = 0, bounds = "[[")
{
  b <- rebound(bounds)

  if(b == "][" | b == "[]"){
    stop(paste("The 'bounds' parameter should be '[[' or ']]' or equivalent",
               ", i.e. boundaries should be open only on one side", sep = ""))
  }

  # res <- rep(NA,length(x))

  m <- xmax - xmin

  if(m <= 0) stop("The 'xmin' parameter should be less than the 'xmax' one.")

  f <- floor((x - xmin)/m)
  res <- ((x - xmin) - m * f) + xmin

  if(bounds == "]]"){
    res[x == xmax | x == xmin] <- xmax
  }

  return(res)
}
