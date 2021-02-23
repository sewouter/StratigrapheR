#' @title Clips a standardised pointsvg object into a given frame
#'
#' @description Clips a svg object imported as data frame using
#' \code{\link{pointsvg}} if outside of a given frame. In other words it removes
#' the elements of the svg that are entirely outside a given area.
#'
#' @param object a pointsvg object (svg object imported as data frame
#' using \code{\link{pointsvg}}).
#' @param xmin,xmax,ymin,ymax clipping coordinates, default to +-Inf (no
#' clipping)
#' @param by.entity whether to remove all entities having points out of
#' the clipping zone (TRUE; default) or to only remove the points out it (FALSE,
#' and to use on lines for better result)
#' @seealso \code{\link{centresvg}}, \code{\link{changesvg}},
#' \code{\link{framesvg}} and \code{\link{pointsvg}}
#'
#' If you want to also keep the elements that are only partly inside the
#' clipping region: \code{\link{ignore}}
#'
#' @examples
#' # Simple use
#'
#' object <- example.ammonite
#'
#' plot(c(-1,1), c(-1,1), type = "n", ylab = "y", xlab = "x")
#'
#' res.object <- clipsvg(object, xmax = 0.5)
#'
#' abline(v = 0.5)
#'
#' centresvg(object, 0, 0, lty = 2)
#' centresvg(res.object, 0, 0, col = "red", lwd = 2)
#'
#' # Advanced used
#'
#' object2 <- example.breccia
#'
#' plot(c(-1,3), c(-1,11), type = "n", ylab = "y", xlab = "x")
#'
#' object2replicated <- framesvg(object2, 0,2,c(0,4,8), c(2,6,10),
#'                               output = TRUE)
#'
#' object2clipped    <- clipsvg(object2replicated, 0, 1.7, 1, 9)
#'
#' rect(0, 1, 1.7, 9, border = "red")
#'
#' placesvg(object2clipped, border = "red", lwd = 2)
#'
#' @export

clipsvg <- function(object, xmin =  -Inf, xmax = +Inf, ymin = -Inf, ymax = +Inf,
                    by.entity = TRUE)
{
  if(!(by.entity == TRUE | by.entity == FALSE)){
    stop("The by.entity parameter should be TRUE or FALSE")
  }

  if(by.entity){

    out <- subset(object, object$x > xmax | object$x < xmin |
                    object$y > ymax | object$y < ymin)

    l <- unique(out$id)

    res <- subset(object,!(object$id %in% l))

  } else {

    res <- subset(object, object$x <= xmax & object$x >= xmin &
                    object$y <= ymax & object$y >= ymin)

  }

  return(res)
}

