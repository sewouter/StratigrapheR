#' Fix Dip
#'
#' @description Fix dip and strike of planes so that they fall in the correct
#' quadrant. The provided quadrant is the determining factor. If unavailable or
#' not helpful, the sign of the dip is used as determining factor.
#'
#' @param strike strike of the data; it is the angle from the north of
#' the horizontal line of the plane. Corrected, its range goes from 0° to 360°.
#' @param dip dip of the data; it is the angle from the horizontal taken
#' on the line of the plane perpendicular to the one of the strike. In other
#' words it is the plane's maximum angular deviation from the horizontal.
#' It is positive downward, and ranges from +90° for straight down to -90° for
#' straight up. Dip values in [-180,-90] or/and ]90,180] indicate inversion of
#' the plane.
#' @param quadrant the quadrant where the plane dips downward. Accepted
#' values are NA, 'N', 'S', 'W' or 'E' (lower- or uppercase alike). Is
#' independant of inversion
#' @param inverted whether the plane is upside down.
#' @details the strike will be corrected as the orientation of the dip (i.e.
#' downward) minus 90°; it ranges from 0 to 360°. It is determined firstly from
#' the quadrant. If the quadrant is missing or not helpful (e.g. 'N' or 'S' for
#' a strike of 0° or 180°, 'E' or 'W' for a strike of 90° or 270°), it is
#' determined using the sign of the dip. Inversion will be indicated if the dip
#' values are in [-180,-90] or/and ]90,180], or simply if inverted = T. The
#' inversion does not influence the calculation of the strike, dip and quadrant:
#' whether the plane is upside down does not change these parameters output.
#' @return a list of the corrected strike, dip and quadrant
#' @seealso \code{\link{fmod}}, \code{\link{incfix}} and
#' \code{\link{transphere}}
#' @examples
#' strike   <- c(-60, 180,20,0,20)
#' dip      <- c(-60,20,-45,110,-90)
#' quadrant <- c("N",NA,NA,NA,"E")
#' inverted <- c(FALSE,TRUE,FALSE,TRUE,FALSE)
#'
#' dipfix(strike,dip,quadrant,inverted)
#'
#' dipfix(strike,dip,quadrant)
#'
#' @export

dipfix <- function(strike, dip, quadrant = NA, inverted = NA)
{
  l <- length(strike)

  if(l != length(dip)) stop("strike and dip should be of length n")

  if(is.na(quadrant[[1]]) & length(quadrant) == 1){
    q <- rep(NA,l)
  } else {
    q <- quadrant
  }

  if(l != length(q)) stop("quadrant should be of length n or should just be NA")

  li <- length(inverted)

  if(!((li == 1 & is.na(inverted[[1]])) |
       (li == l & class(inverted) == "logical"))){
    stop(paste("The 'inverted' parameter should be NA or a logical of ",
               "same length than the other parameters", sep = ""))
  }

  if(isTRUE(any(q == "n"))) q[q == "n"] <- "N"
  if(isTRUE(any(q == "s"))) q[q == "s"] <- "S"
  if(isTRUE(any(q == "w"))) q[q == "w"] <- "W"
  if(isTRUE(any(q == "e"))) q[q == "e"] <- "E"

  if(any(!(q == "N" | q == "S" | q == "W" | q == "E" | is.na(q)))){

    stop(paste("Invalid quadrant values (should be NA, ",
               "'N', 'S', 'W' or 'E' (lower- or uppercase alike)",
               sep = ""))

  }

  s  <- fmod(strike, 180)
  s2 <- fmod(strike, 360)
  d  <- fmod(dip,90,-90,bounds = "]]")

  lin <- rep(NA, length(s))

  lin[s == 0 | s == 180] <- "N/S"
  lin[s == 90]           <- "W/E"
  lin[s > 0  & s <  90]  <- "NE/SW"
  lin[s > 90 & s < 180]  <- "SE/NW"

  non.help <- (lin == "N/S" & !is.na(q) & (q == "N" | q == "S")) |
    (lin == "W/E" & !is.na(q) & (q == "W" | q == "E"))

  if(any(non.help)){
    warning(paste("Non helping quadrant values (N or S for a strike of 0 or ",
                  "180, E or W for a strike of 90 or 270)", sep = ""))
    q[non.help] <- NA
  }

  dat <- data.frame(s,d,q,lin)

  cor <- rep(NA, l)

  cor[lin == "NE/SW" & (q == "S" | q == "E")] <- 0
  cor[lin == "NE/SW" & (q == "N" | q == "W")] <- 180

  cor[lin == "SE/NW" & (q == "N" | q == "E")] <- 180
  cor[lin == "SE/NW" & (q == "S" | q == "W")] <- 0

  cor[lin == "N/S" & q == "E"] <- 0
  cor[lin == "N/S" & q == "W"] <- 180

  cor[lin == "W/E" & q == "N"] <- 0
  cor[lin == "W/E" & q == "S"] <- 180

  c1 <- s + cor

  qexist  <- !is.na(q)
  qabsent <- is.na(q) & (d < 0 | dip == -90)

  s2[qexist]  <- c1[qexist]
  s2[qabsent] <- fmod(s2[qabsent] + 180, 360)

  res <- list()

  res$strike <- s2
  res$dip    <- abs(d)

  nq <- rep(NA, length(s))

  nq[res$strike >=  45 & res$strike <= 135] <- "S"
  nq[res$strike >  135 & res$strike <  225] <- "W"
  nq[res$strike >= 225 & res$strike <= 315] <- "N"
  nq[res$strike >  315 | res$strike <   45] <- "E"

  res$quadrant <- nq

  tdip <- fmod(dip,180,-180)
  out  <-  tdip > 90 | tdip <= -90

  if (li == l & class(inverted) == "logical") {

    inv <- inverted

    if(any(!inv[out])){
      warning("Samples having dip values in [-180,-90] or/and ]90,180]",
              " or equivalent are filled out as not inverted (inverted = F)",
              ", which is contradictory. By default they will be considered ",
              "as inverted (inverted = T).", sep = "")
    }

    inv[out] <- T

  } else {

    inv <- rep(F, length(s))
    inv[out] <- T

  }

  res$inverted <- inv

  return(res)

}

