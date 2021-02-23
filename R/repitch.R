#' Converts pitch into declination and inclination
#'
#' @description Finds the declination and inclination of a line defined by a
#' pitch on a plane
#'
#' @param pitch pitch (or rake) of the data; it is the angle between the
#' strike of the plane and a line. It is taken from the left side going downward
#' along the dip, and is positive downward.
#' @param strike strike of the data; it is the angle from the north of
#' the horizontal line of the plane. It is corrected by the \code{\link{dipfix}}
#' function.
#' @param dip dip of the data; it is the angle from the horizontal taken
#' on the line of the plane perpendicular to the one of the strike. It is
#' corrected by the \code{\link{dipfix}} function.
#' @param quadrant the quadrant were the plane dips downward. Accepted
#' values are NA, 'N', 'S', 'W' or 'E' (lower- or uppercase alike) for
#' correction by the \code{\link{dipfix}} function.
#' @return a list of declination and inclination of the defined lines
#' @references Eric Carlson of the Colorado School of Mines is acknowledged for
#'  his rake to plunge calculator on which this function is based.
#' @seealso  \code{\link{dipfix}}, \code{\link{incfix}} and
#' \code{\link{transphere}}
#' @examples
#' strike <- c(90, 135, 135, 135)
#' dip    <- c(0,  65,  65,  65)
#' pitch  <- c(40, 40,  140, -40)
#'
#' earnet()
#' earplanes(strike,dip,hsphere = "b", a = list(col = "red", lwd = 2))
#'
#' res <- repitch(pitch = pitch, strike = strike, dip = dip)
#'
#' earpoints(dec = res$dec, inc = res$inc)
#'
#' @export

repitch <- function(pitch, strike, dip, quadrant = NA)
{
  corr <- dipfix(strike, dip)

  fstrike <- corr$strike
  fdip    <- corr$dip

  ls <- length(fstrike)
  lr <- length(pitch)

  if(lr == 1){
    pitch <- rep(pitch, ls)
  } else if (!(lr == 1 | lr == ls)){
    stop("The 'pitch' parameter should be of length 1 or n")
  }

  modpitch <- fmod(pitch, 180, -180, bounds = "]]")

  fpitch <- modpitch
  fpitch[modpitch < 0] <- -fpitch[modpitch < 0]

  beta <- abs((atan(tan(((fpitch) * pi)/180) * cos((fdip * pi)/180)) * 180)/pi)

  beta[fpitch > 90] <- 180 - beta[fpitch > 90]

  dec <- fstrike + beta
  dec[modpitch < 0] <- fstrike[modpitch < 0] - beta[modpitch < 0]

  inc <- (asin(sin((modpitch * pi)/180) * sin((fdip* pi)/180)) * 180)/pi

  res <- list(dec = dec, inc = inc)

  return(res)

}
