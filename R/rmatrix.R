#' Rotation matrix
#'
#' @description Computes a rotation matrix for a given rotation axis and angle
#' based on Tauxe et al. (2010).
#'
#' @param dec declination of the rotation axis; it is the angle from the
#' north taken on an horizontal plane. It is measured clockwise from North and
#' ranges from 0 to 360° (Tauxe 2010).
#' @param inc inclination of the rotation axis; it is the angle from the
#' horizontal, is positive downward, and ranges from +90° for straight down to
#' -90° for straight up (Tauxe, 2010).
#' @param mag magnitude of rotation (following the notation of the
#' Stereonet software) a positive rotation is clockwise looking in the direction
#' of the given declination and inclination)
#' @param as.data.frame logical, whether to output the matrix as a data
#' frame. This is used when multiple arguments are provided to simplify and
#' boost calculations.
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
#' @examples
#' rmatrix(135,20,60)
#'
#' rmatrix(c(135,0),c(20,90),c(60,90), as.data.frame = TRUE)
#'
#' @export

rmatrix <- function(dec, inc, mag, as.data.frame = FALSE)
{

  nd <- length(dec)
  ni <- length(inc)
  nm <- length(mag)

  n <- max(nd,ni,nm)

  if(!(nd == n & ni == n & nm == n)){
    stop("The 'dec', 'inc' and 'mag' parameters should be of same length")
  }

  l <- transphere(dec = dec, inc = inc)

  co <- cos(pi/180 * -mag)
  si <- sin(pi/180 * -mag)

  rxx <- l$x * l$x * (1 - co) + co
  rxy <- l$x * l$y * (1 - co)  - l$z *si
  rxz <- l$x * l$z * (1 - co)  + l$y *si
  ryx <- l$y * l$x * (1 - co)  + l$z *si
  ryy <- l$y * l$y * (1 - co) + co
  ryz <- l$y * l$z * (1 - co)  - l$x *si
  rzx <- l$z * l$x * (1 - co)  - l$y *si
  rzy <- l$z * l$y * (1 - co)  + l$x *si
  rzz <- l$z * l$z * (1 - co) + co

  if(!as.data.frame){

    if(n != 1) {
      stop(paste("If the length of the arguments is > 1 then 'as.data.frame'",
                 " should be TRUE"))
    }

    res <- matrix(c(rxx,rxy,rxz,ryx,ryy,ryz,rzx,rzy,rzz),
                  ncol = 3,byrow = FALSE)

  } else if(as.data.frame){

    res <- data.frame(rxx,rxy,rxz,ryx,ryy,ryz,rzx,rzy,rzz)

  } else{

    stop("The 'as.data.frame' parameter should be TRUE or FALSE")

  }

  return(res)

}
