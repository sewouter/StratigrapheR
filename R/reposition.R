#' Core correction
#'
#' @description Core correction : declination and inclination are corrected for
#' cores of given declination, inclination and rotation
#'
#' @param dec declination of the data; it is the angle from the north
#' taken on an horizontal plane. It is measured clockwise from North and ranges
#' from 0 to 360° (Tauxe 2010).
#' @param inc inclination of the data; it is the angle from the
#' horizontal, is positive downward, and ranges from +90° for straight down to
#' -90° for straight up (Tauxe, 2010).
#'
#' @param cdec declination of the core.
#' @param cinc inclination of the core.
#' @param crot rotation of the core; it is the angle of rotation around
#' the core direction clockwise between the measurement and the actual core
#' orientation. In others words it is the magnitude of the rotation to apply
#' clockwise to the measured data using the core direction as an axis.
#' @seealso \code{\link{rotate}} and \code{\link{restore}}
#' @examples
#' # ----
#'
#' d <- zeq_example
#'
#' dec <- d$Dec
#' inc <- d$Inc
#'
#' cdec <- 75
#' cinc <- 45
#' crot <- 90
#'
#' par(mfrow = c(2,2))
#'
#' earnet()
#' earpoints(dec,inc)
#' earpoints(0, 90, l = list(cex = 2))
#' earpoints(0, 90, l = list(col = "red", bg = "red"))
#' title("1. Laboratory projection,
#'       axis for rotating the specimen")
#'
#' # Roll ----
#'
#' roll <- reposition(dec, inc, crot = 90)
#'
#' earnet()
#' earpoints(roll$dec,roll$inc)
#' earpoints(0, 90, l = list(cex = 2))
#' earpoints(90, 0, h = list(col = "red", bg = "pink"), double = TRUE)
#' title("2. Correction of the specimen rotation,
#'       in red the axis for tilting the specimen")
#'
#' # Tilt ---
#'
#' tilt <- reposition(dec, inc, cinc = cinc ,crot = crot)
#'
#' earnet()
#' earpoints(0, cinc, l = list(cex = 2))
#' earpoints(tilt$dec, tilt$inc)
#' earpoints(0,90, l = list(col = "red", bg = "red"))
#' title("3. Correction of the specimen inclination,
#'       in red the axis for rotating the tilted specimen")
#'
#' # Orient ---
#'
#' orient <- reposition(dec, inc, cdec = cdec, cinc = cinc ,crot = crot)
#'
#' earnet()
#' earpoints(cdec, cinc, l = list(cex = 2))
#' earpoints(orient$dec, orient$inc)
#' title("4. Full geographical repositioning,
#'       the big dot is the core orientation")
#'
#' par(mfrow = c(1,1))
#'
#' # ----
#'
#' @export

reposition <- function(dec, inc, cdec = 0, cinc = 90, crot = 0)
{
  l <- length(dec)

  ld <- length(cdec)
  li <- length(cinc)
  lr <- length(crot)

  if(length(inc) != l) {
    stop(paste("The 'dec' and 'inc' parameters should have ",
               "the same length", sep = ""))
  }

  if(!(ld == l | ld == 1)) {
    stop(paste("The 'cdec' parameter should have ",
               "the same length than 'dec' and 'inc', or a length of 1",
               sep = ""))
  }

  if(!(li == l | li == 1)) {
    stop(paste("The 'cinc' parameter should have ",
               "the same length than 'dec' and 'inc', or a length of 1",
               sep = ""))
  }

  if(!(lr == l | lr == 1)) {
    stop(paste("The 'crot' parameter should have ",
               "the same length than 'dec' and 'inc', or a length of 1",
               sep = ""))
  }

  if(all(crot == 0)){

    roll <- list(dec = dec, inc = inc)

  } else {

    if(lr == 1) crot <- rep(crot, l)

    roll <- rotate(dec, inc, rep(0,l), rep(90,l), crot)

  }

  if(all(cinc == 90)){

    tilt <- list(dec = roll$dec, inc = roll$inc)

  } else {

    if(li == 1) cinc <- rep(cinc, l)

    tilt <- rotate(roll$dec, roll$inc, rep(90,l), rep(0,l), 90-cinc)

  }

  if(all(cdec[[1]] == 0)){

    orient <- list(dec = tilt$dec, inc = tilt$inc)

  } else {

    if(ld == 1) cdec <- rep(cdec, l)

    orient <- rotate(tilt$dec, tilt$inc, rep(0,l), rep(90,l), cdec)

  }

  return(orient)

}

