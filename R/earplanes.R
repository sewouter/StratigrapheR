#' Draws planes on an equal area stereonet
#'
#' @description Draws planes on an equal area stereonet (modified from RFOC
#' package)
#'
#' @param strike strike of the data; it is the angle from the north of
#' the horizontal line of the plane. It is corrected by the \code{\link{dipfix}}
#' function.
#' @param dip dip of the data; it is the angle from the horizontal taken
#' on the line of the plane perpendicular to the one of the strike. It is
#' corrected by the \code{\link{dipfix}} function.
#' @param quadrant the quadrant were the plane dips downward. Accepted
#' values are NA, 'N', 'S', 'W' or 'E' (lower- or uppercase alike) for
#' correction by the \code{\link{dipfix}} function.
#' @param hsphere the hemisphere onto which to project the data. Either
#' "b" for both, "l" for lower, and u" for upper.
#' @param ndiv the number of intervals between each 10° (in declination)
#' @param a,l,u list of graphical parameters to feed lines() for the all
#' lines, or for the lines of the upper (u) and lower (l) hemisphere (the two
#' latter override a). See ?lines help page for the possible arguments. See
#' ?merge_list for further information.
#' @param output whether to return an output (position of the points
#' making the lines in the stereographic projection)
#' @param plot whether to plot
#' @param xh orientation of the x axis: can be 'WE' or 'SN'.
#' @param unique whether to only plot each similar plan once.
#' @return the x,y coordinates of each projected plane
#' @references RFOC package
#' @seealso  \code{\link{earnet}}, \code{\link{earpoints}} and
#' \code{\link{dipfix}}
#' @examples
#' strike   <- c(45,  0)
#' dip      <- c(20, 65)
#'
#' earnet()
#' earplanes(strike,dip,hsphere = "b")
#'
#' encircle(earinc(dip))
#'
#' @export

earplanes <- function(strike, dip, quadrant = NA,
                      hsphere = "l", ndiv = 10,
                      a = list(col = "black", lwd = 1),
                      l = list(lty = 1), u = list(lty = 3),
                      output = FALSE, plot = TRUE, xh = "WE",
                      unique = TRUE)
{

  if(!(xh == "SN" | xh == "WE")) {
    stop("The 'xh' parameter should be 'WE' or 'SN'.")
  }

  i <- dipfix(strike = strike, dip = dip, quadrant = quadrant)

  if(unique){

    dtf  <- data.frame(strike = i$strike, dip = i$dip)
    dtfu <- unique(dtf)

    s <- dtfu$strike
    d <- dtfu$dip

  } else {

    s <- i$strike
    d <- i$dip

  }

  len <- length(s)

  beta <- s * pi/180

  co  <- cos(beta)
  si  <- sin(beta)

  phi <- matrix(rep(pi/180 * seq(-90, 90, by = 10/ndiv),len),
                nrow = len, byrow = TRUE)

  lambda <- (90 - d) * pi/180

  alpha  <- acos(cos(phi) * cos(lambda))

  tq     <- sqrt(2) * sin(alpha/2)

  sint               <- sin(phi)/sin(alpha)
  sint[is.nan(sint)] <- 1

  temps                <- rep(1, length(sint)) - (sint * sint)
  temps[is.nan(temps)] <- 0
  temps[temps < 0]     <- 0

  x <- tq * sqrt(temps)
  y <- tq * sint

  res <- list()

  if(xh == "WE") {
    res$x <- co * x + si * y
    res$y <- -si * x + co * y
  } else if(xh == "SN"){
    res$x <- -si * x + co * y
    res$y <- -(co * x + si * y)
  }

  if(plot) {

    lj <- merge_list(l, list(lty = 1))
    uj <- merge_list(u, list(lty = 3))

    aj <- merge_list(a, list(col = "black", lwd = 1))

    li <- merge_list(l, list(lty = 1), a, list(col = "black", lwd = 1))
    ui <- merge_list(u, list(lty = 3), a, list(col = "black", lwd = 1))

    if(hsphere == "b" | hsphere == "l"){
      for(i in seq_len(len))
      {
        li$x <- res$x[i,]
        li$y <- res$y[i,]
        do.call(lines,li)
      }
    }

    if(hsphere == "b" | hsphere == "u"){
      for(i in seq_len(len))
      {
        ui$x <- -res$x[i,]
        ui$y <- -res$y[i,]
        do.call(lines,ui)
      }
    }

  }

  if(output) return(res)

}

