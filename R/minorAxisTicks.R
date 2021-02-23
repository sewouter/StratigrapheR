#' @title Compute Pretty Minor Axis Tick Scales
#'
#' @description Compute pretty mark locations for minor ticks, based on the way
#' that traditional R graphics do it.
#'
#' @param usr the user coordinates of the minimum and maximum limits of the axis
#' @param n the number of intervals defined by the minor ticks
#' @param at.maj the positions at which major tick-marks are to be drawn.
#' By default (when NULL) tickmark locations are computed buy the axisTicks
#' function
#' @param extend whether to add minor ticks even outside the major ticks
#' (T) or not (F)
#'
#' @seealso \code{\link{minorAxis}}, \code{\link{seq_log}}
#'
#' This function is based on \code{\link{every_nth}}, which suppresses values
#' every multiple of a given number.
#'
#' @examples
#' minorAxisTicks(usr = c(-20, 620), n = 10)
#'
#' @importFrom grDevices axisTicks
#' @export
#'
minorAxisTicks <- function(usr, n = NULL, at.maj = NULL, extend = T)
{

  if(is.null(at.maj)) {

    at.maj <- axisTicks(usr, log = F)

  } else {

    r1 <- at.maj - min(usr)
    r2 <- at.maj - max(usr)

    p1 <- which.min(abs(r1))
    p2 <- which.min(abs(r2))

    if(!(abs(r1[p1]/min(usr)) <  1.5e-8) & r1[p1] < 0) p1 <- p1 + 1
    if(!(abs(r2[p2]/max(usr)) <  1.5e-8) & r2[p2] > 0) p2 <- p2 - 1

    at.maj <- at.maj[p1:p2]

  }

  # Give at.min when n is NULL or 1----

  if(is.null(n)){

    res <- list(at.maj)

  } else {

    # Work the minor ticks: check regularity ----

    mina <- min(at.maj)
    maxa <- max(at.maj)
    difa <- maxa - mina
    na   <- difa / (length(at.maj) - 1)

    # Checks----

    sia <- seq(mina,maxa,by = na)

    if(!isTRUE(all.equal(sort(sia),sort(at.maj)))) {
      stop("at.maj should be regularly spaced and sorted")
    }

    if(!(is.numeric(n) & length(n) == 1)){
      stop("n should be a numeric of length one")
    }

    # Work it ----

    tick.pos <- c(mina,maxa,difa/na)
    nat.int  <- (tick.pos[2] - tick.pos[1])/tick.pos[3]

    # Define the position of minor ticks ----

    distance.between.minor <- nat.int/n

    p <- seq(min(at.maj), max(at.maj), by = distance.between.minor)
    q <- sort(every_nth(p,n,empty=FALSE))

    # Extend outside of major ticks range if necessary ----

    if(!extend) {

      tick.seq <- q

    } else {

      possible.low.minors <- min(at.maj) - ((n-1):1) * distance.between.minor
      possible.hi.minors  <- max(at.maj) + (1:(n-1)) * distance.between.minor

      r3 <- possible.low.minors - min(usr)
      r4 <- possible.hi.minors  - max(usr)

      p3 <- which.min(abs(r3))
      p4 <- which.min(abs(r4))

      if(!(abs(r3[p3]/min(usr)) <  1.5e-8) & r3[p3] < 0) p3 <- p3 + 1
      if(!(abs(r4[p4]/max(usr)) <  1.5e-8) & r4[p4] > 0) p4 <- p4 - 1

      if(p3 < length(possible.low.minors) + 1){
        low.candidates <- seq(p3, length(possible.low.minors), 1)
        low.laureates  <- possible.low.minors[low.candidates]
      } else {
        low.laureates  <- NULL
      }

      if(p4 > 0){
        hi.candidates  <- seq(1, p4, 1)
        hi.laureates   <- possible.hi.minors[ hi.candidates]
      } else {
        hi.laureates  <- NULL
      }

      tick.seq <- c(low.laureates,q,hi.laureates)

    }

    res <- list(at.maj, tick.seq)

  }

  return(res)

}


