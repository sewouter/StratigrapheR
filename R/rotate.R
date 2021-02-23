#' Spherical rotation around fixed axes
#'
#' @description Spherical rotation around given rotation axes
#'
#' @param dec declination of the data; it is the angle from the north
#' taken on an horizontal plane. It is measured clockwise from North and ranges
#' from 0 to 360° (Tauxe 2010).
#' @param inc inclination of the data; it is the angle from the
#' horizontal, is positive downward, and ranges from +90° for straight down to
#' -90° for straight up (Tauxe, 2010).
#' @param rdec declination of the rotation axes (of length 1 or n).
#' @param rinc inclination of the rotation axes (of length 1 or n).
#' @param rmag magnitude of rotation (following the notation of the
#' Stereonet software): a positive rotation is clockwise looking in the
#' direction of the given declination and inclination; of length 1 or n).
#' @references
#' \itemize{
#'   \item Tauxe, L., 2010. Essentials of Paleomagnetism. University of
#'   California Press.
#'   \item Allmendinger, R. W., Cardozo, N. C., and Fisher, D., 2013, Structural
#'   Geology Algorithms: Vectors & Tensors: Cambridge, England, Cambridge
#'   University Press, 289 pp.
#'   \item Cardozo, N., and Allmendinger, R. W., 2013, Spherical projections
#'   with OSXStereonet: Computers & Geosciences, v. 51, no. 0, p. 193 - 205,
#'   doi: 10.1016/j.cageo.2012.07.021
#' }
#' @seealso \code{\link{rmatrix}}, \code{\link{restore}} and
#' \code{\link{reposition}}
#' @examples
#' earnet()
#'
#' inc <- seq(0,85,5)
#' dec <- rep(0,length(inc))
#'
#' earpoints(dec,inc)
#'
#' rdec <- rep(0, length(inc))
#' rinc <- rep(90, length(inc))
#'
#' mag <- 90
#' rmag  <- seq(mag, 0, by = -mag/(length(inc)-1))
#'
#' rot <- rotate(dec,inc,rdec,rinc,rmag)
#'
#' earpoints(dec = round(rot$dec,digits = 2), inc = round(rot$inc,digits = 2),
#'           l = list(bg = "green"),
#'           u = list(bg = "blue"),
#'           h = list(bg = "yellow"))
#'
#' earpoints(dec = 0, inc = 90, l = list(bg = "red"))
#'
#' @export


rotate <- function(dec,inc,rdec,rinc,rmag)
{

  nd  <- length(dec)
  ni  <- length(inc)

  n <- max(nd,ni)

  if(!(nd == n & ni == n)){
    stop("The 'dec' and 'inc' parameters should be of same length")
  }

  nrd <- length(rdec)
  nri <- length(rinc)
  nrm <- length(rmag)

  nr <- max(nrd,nri,nrm)

  if(!(nrd == nr & nri == nr & nrm == nr)){
    stop("The 'rdec', 'rinc' and 'rmag' parameters should be of same length")
  }

  l  <- transphere(dec,inc)

  if(nr == 1){

    dm <- matrix(c(l$x,l$y,l$z),nrow = 3,byrow = TRUE)
    rm <- rmatrix(rdec,rinc,rmag)

    rot <- rm %*% dm

    res <- transphere(x = rot[1,],y = rot[2,],z = rot[3,])
    out <- list(dec = res$dec, inc = res$inc)

  } else if(nr == n){

    df <- data.frame(x = l$x, y = l$y, z = l$z)
    rf <- rmatrix(rdec,rinc,rmag,as.data.frame = TRUE)

    df$xp <- df$x*rf$rxx + df$y*rf$ryx + df$z*rf$rzx
    df$yp <- df$x*rf$rxy + df$y*rf$ryy + df$z*rf$rzy
    df$zp <- df$x*rf$rxz + df$y*rf$ryz + df$z*rf$rzz

    resf <- transphere(x = df$xp, y = df$yp, z = df$zp)
    out  <- list(dec = resf$dec, inc = resf$inc)

  } else {

    stop(paste("The 'rdec', 'rinc' and 'rmag' parameters should be of length 1",
               " or have the same length than the other parameters.", sep = ""))

  }

  return(out)

}
