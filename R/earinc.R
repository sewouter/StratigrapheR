#' @title Recalculates inclination in equal area projection
#'
#' @description Recalculates the inclination in equal area projection
#'
#' @param inc inclination of the data; it is the angle from the
#' horizontal, is positive downward, and ranges from +90° for straight down to
#' -90° for straight up (Tauxe, 2010).
#' @examples
#' earinc(20)
#'
#' @export

earinc <- function(inc)
{
  i <- abs(inc)

  if(any(i > 90)) stop("inc should range from -90 to 90")

  return(abs(sqrt(2) * sin(pi/180 * (90 - i)/2)))
}
