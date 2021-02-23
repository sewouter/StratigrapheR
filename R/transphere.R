#' Convertion between declinaison/inclination/intensity and cartesian
#' coordinates
#'
#' @description Convertion between declinaison/inclination/intensity and cartesian
#' coordinates (modified from RFOC package)
#'
#' @param dec declination of the data; it is the angle from the north
#' taken on an horizontal plane. It is measured clockwise from North and ranges
#' from 0 to 360° (Tauxe 2010). Values outside this range are corrected by
#' incfix().
#' @param inc inclination of the data; it is the angle from the
#' horizontal, is positive downward, and ranges from +90° for straight down to
#' -90° for straight up (Tauxe, 2010). Values outside this range are corrected
#' by incfix().
#' @param int intensity of the data. Defaults to one (unit sphere).
#' @param x,y,z cartesian coordinates. x is the North, y the East, and z
#' straight down. If dec and inc are not provided they are used to be converted
#' back in dec, inc and int data. Output is corrected by incfix().
#' @param into overriding parameter for generalisation: if "dii" dec, inc and
#' int will remain as they are, and if "xyz" cartesian coordinates will remain
#' as they are
#' @return a list of coordinates, in cartesian form or dec, inc, int form
#' following the input
#' @seealso \code{\link{fmod}}, \code{\link{dipfix}} and \code{\link{incfix}}
#' @examples
#' transphere(dec = c(65,135), inc = c(32,74))
#'
#' l <- transphere(dec = c(65,135), inc = c(32,74))
#' transphere(x = l$x, y = l$y, z = l$z)
#'
#' @export

transphere <- function(dec = NA, inc = NA, int = 1, x = NA, y  = NA, z = NA,
                       into = "other")
{

  if(!is.na(dec[[1]]) & !is.na(inc[[1]])){

    l <- incfix(dec,inc)

    if(into == "dii"){
      if(length(int) == 1 & length(dec) != 1) int <- rep(int, length(dec))
      res <- list(dec = l$dec, inc = l$inc, int = int)
      return(res)
    }

    deci <- l$dec * pi/180
    inci <- l$inc * pi/180
    zi   <- sin(inci) * int
    m    <- cos(inci) * int
    xi   <- cos(deci) * m
    yi   <- sin(deci) * m

    res <- list(x = xi, y = yi, z = zi)
    return(res)

  } else if(!is.na(x[[1]]) & !is.na(y[[1]]) & !is.na(z[[1]])){

    if(into == "xyz"){
      res <- list(x = x, y = y, z = z)
      return(res)
    }

    inti <- sqrt(x^2 + y^2 + z^2)
    deci <- 180/pi * atan2(y,x)
    inci <- 180/pi * asin(z/inti)

    l <- incfix(deci,inci)

    res <- list(dec = l$dec, inc = l$inc, int = inti)
    return(res)
  }

}
