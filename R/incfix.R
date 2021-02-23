#' Fix Inclination
#'
#' @description Fix inclination and declination so that they fall in the correct
#' quadrant and hemisphere (modified from RFOC package)
#'
#' @param dec declination of the data; it is the angle from the north
#' taken on an horizontal plane. It is measured clockwise from North and ranges
#' from 0 to 360° (Tauxe 2010). Values outside this range are corrected by this
#' function.
#' @param inc inclination of the data; it is the angle from the
#' horizontal, is positive downward, and ranges from +90° for straight down to
#' -90° for straight up (Tauxe, 2010). Values outside this range are corrected
#' by this function.
#' @param hsphere the hemisphere onto which to project the data. Either
#' "b" for both. This is the default and useful for paleomagnetism. In this
#' case positive and negative values of inc are permitted. Or "l" for lower, and
#' "u" for upper, allowing only negative or positive inc values respectively.
#' @details Quadrants are determined by the sine and cosine of the dip angle:
#' co = cos(dip), si = sin(dip),
#' quad[co>=0 & si>=0] = 1, quad[co<0 & si>=0] = 2,
#' quad[co<0 & si<0] = 3 and quad[co>=0 & si<0] = 4.
#' Samples at inc == 0° and inc == 90° are taken as exceptions (cf. code).
#' Be cautions with the floating point error however, round if needed.
#' @seealso \code{\link{fmod}}, \code{\link{dipfix}} and
#' \code{\link{transphere}}
#' @examples
#' incfix(591,-425,"b")
#' incfix(591,-425,"u")
#' incfix(591,-425,"l")
#'
#' @export

incfix <- function (dec,inc,hsphere = "b")
{
  inci <- inc
  deci <- dec

  res <- list()

  tinc <- fmod(inc, 360)
  co   <- cos((pi/180) * tinc)
  si   <- sin((pi/180) * tinc)
  ang  <- (180/pi) * atan2(si, co)

  quad <- rep(NA, length(inc))

  quad[co > 0 & si > 0] <- 1
  quad[co < 0 & si > 0] <- 2
  quad[co < 0 & si < 0] <- 3
  quad[co > 0 & si < 0] <- 4

  quad[tinc ==  90] <- "ps"
  quad[tinc == 270] <- "pn"
  quad[tinc ==   0] <- "ep"
  quad[tinc == 180] <- "en"

  if(hsphere == "b"){

    inci[quad == 1] <- ang[quad == 1]
    inci[quad == 2] <- 180 - ang[quad == 2]
    inci[quad == 3] <- -ang[quad == 3] - 180
    inci[quad == 4] <- ang[quad == 4]

    deci[quad == 1] <- dec[quad == 1]
    deci[quad == 2] <- 180 + dec[quad == 2]
    deci[quad == 3] <- 180 + dec[quad == 3]
    deci[quad == 4] <- dec[quad == 4]

    inci[quad == "ps"] <- 90
    inci[quad == "pn"] <- -90
    inci[quad == "ep"] <- 0
    inci[quad == "en"] <- 0

    deci[quad == "ps"] <- dec[quad == "ps"]
    deci[quad == "pn"] <- dec[quad == "pn"]
    deci[quad == "ep"] <- dec[quad == "ep"]
    deci[quad == "en"] <- 180 + dec[quad == "en"]

    res$dec <- fmod(deci, 360)
    res$inc <- inci
    return(res)

  } else if(hsphere == "l"){

    inci[quad == 1] <- ang[quad == 1]
    inci[quad == 2] <- 180 - ang[quad == 2]
    inci[quad == 3] <- 180 + ang[quad == 3]
    inci[quad == 4] <- -ang[quad == 4]

    deci[quad == 1] <- dec[quad == 1]
    deci[quad == 2] <- 180 + dec[quad == 2]
    deci[quad == 3] <- dec[quad == 3]
    deci[quad == 4] <- 180 + dec[quad == 4]

    inci[quad == "ps"] <- 90
    inci[quad == "pn"] <- 90
    inci[quad == "ep"] <- 0
    inci[quad == "en"] <- 0

    deci[quad == "ps"] <- dec[quad == "ps"]
    deci[quad == "pn"] <- dec[quad == "pn"]
    deci[quad == "ep"] <- dec[quad == "ep"]
    deci[quad == "en"] <- 180 + dec[quad == "en"]

    res$dec <- fmod(deci, 360)
    res$inc <- inci
    return(res)

  } else if(hsphere == "u"){

    inci[quad == 1] <- -ang[quad == 1]
    inci[quad == 2] <- -180 + ang[quad == 2]
    inci[quad == 3] <- -180 - ang[quad == 3]
    inci[quad == 4] <- ang[quad == 4]

    deci[quad == 1] <- 180 + dec[quad == 1]
    deci[quad == 2] <- dec[quad == 2]
    deci[quad == 3] <- 180 + dec[quad == 3]
    deci[quad == 4] <- dec[quad == 4]

    inci[quad == "ps"] <- -90
    inci[quad == "pn"] <- -90
    inci[quad == "ep"] <- 0
    inci[quad == "en"] <- 0

    deci[quad == "ps"] <- dec[quad == "ps"]
    deci[quad == "pn"] <- dec[quad == "pn"]
    deci[quad == "ep"] <- dec[quad == "ep"]
    deci[quad == "en"] <- 180 + dec[quad == "en"]

    res$dec <- fmod(deci, 360)
    res$inc <- inci
    return(res)
  }
}

