#' Suppresses every n th element of a vector
#'
#' @param x a vector (numbers, integers, characters, you name it)
#' @param nth the multiple of position where the elements will be
#' suppressed (nth + 1 actually) or kept (if inverse = T)
#' @param empty whether the suppressed element should be replaced by ""
#' @param inverse opposite reaction: n th elements only will be kept
#'
#' @return a vector with the remaining values
#'
#' @author Adam D. Smith
#'
#' @seealso practical usage of this function for axes: \code{\link{minorAxis}}
#'
#' @examples
#' numvec <- 0:20
#'
#' every_nth(numvec, 3)
#'
#' every_nth(numvec, 3, empty = FALSE)
#'
#' every_nth(numvec, 3, inverse = TRUE)
#'
#' every_nth(numvec, 3, empty = FALSE, inverse = TRUE)
#'
#' @export

every_nth <- function(x, nth, empty = TRUE, inverse = FALSE)
{
  if(nth < 1) stop("The 'nth' parameter should be superior or equal to 1")

  if (!inverse) {
    if(empty) {
      x[1:nth == 1] <- ""
      x
    } else {
      x[1:nth != 1]
    }
  } else {
    if(empty) {
      x[1:nth != 1] <- ""
      x
    } else {
      x[1:nth == 1]
    }
  }
}


