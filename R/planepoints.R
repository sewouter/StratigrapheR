#' Describes planes by points
#'
#' @description Gives the coordinates (dec and inc) of three perpendicular
#' directions to describe planes.
#'
#' @param strike strike of the data; it is the angle from the north of
#' the horizontal line of the plane. It is corrected by dipfix().
#' @param dip dip of the data; it is the angle from the horizontal taken
#' on the line of the plane perpendicular to the one of the strike. It is
#' corrected by dipfix().
#' @param quadrant the quadrant where the plane dips downward. Accepted
#' values are NA, 'N', 'S', 'W' or 'E' (lower- or uppercase alike) for
#' correction by dipfix().
#' @param inverted whether the plane is inverted or not. The default is
#' NA, it assumes that no bed is inverted.
#'
#' @details The directions are x for dip-direction line (direction of maximum
#' downward dip), y for the horizontal line, z for the upper pole; additionally
#' a magnitude is given to use y as a rotation axis to get the plane back at the
#' horizontal. If the plane is inverted, y, z and mag will be changed,
#' accordingly, with a rotation of 180° around x for y and z.
#'
#' @return a list of x, y and z declinations and inclinations (dec and inc), and
#' a rotation magnitude
#'
#' @examples
#' strike   <- c(-60, 180,20)
#' dip      <- c(-60,20,-45)
#' quadrant <- c("N","W",NA)
#' inverted <- c(FALSE,FALSE,TRUE)
#'
#' res <- planepoints(strike,dip,quadrant,inverted)
#'
#' deci <- c(res$x$dec, res$y$dec, res$z$dec)
#' inci <- c(res$x$inc, res$y$inc, res$z$inc)
#'
#' earnet()
#'
#' earplanes(strike,dip,quadrant,hsphere = "l")
#' earpoints(deci,inci)
#'
#' @export


planepoints <- function(strike, dip, quadrant = NA, inverted = NA)
{

  l <- length(strike)

  if(!((all(inverted == TRUE | inverted == FALSE) & length(inverted) == l) |
       (is.na(inverted[[1]]) & length(inverted) == 1))){
    stop(paste("inverted should be NA and of length 1, or of length n and ",
               "made of either T or F"), sep = "")
  }

  if(is.na(inverted[[1]])) inverted <- rep(FALSE, l)

  i <- dipfix(strike = strike, dip = dip, quadrant = quadrant)

  s <- i$strike
  d <- i$dip

  x <- list(dec = fmod(s + 90, 360), inc = d)

  y <- list()

  y$dec <- rep(NA,l)

  y$dec[!inverted] <- s[!inverted]
  y$dec[inverted]  <- fmod(s[inverted] + 180, 360)

  y$inc <- rep(0,l)

  incz <- rep(NA,l)

  incz[!inverted] <- d[!inverted] - 90
  incz[inverted]  <- d[inverted] + 90

  z <- incfix(s + 90, incz)

  mag <- rep(NA,l)

  mag[!inverted] <- -d[!inverted]
  mag[inverted]  <- d[inverted ]- 180

  res <- list(x = x, y = y, z = z, mag = mag)

  return(res)

}

