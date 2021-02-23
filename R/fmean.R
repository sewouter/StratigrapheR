#' Fischer mean
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
#' @param id a name for each point, identifying each group of points you would
#' like to treat separately
#' @param cart whether to output as cartesian coordinates, defaults to F
#' @return a list of coordinates for the fischer mean, in cartesian form or dec,
#' inc, int form
#'
#' @seealso \code{\link{fmod}}, \code{\link{dipfix}} and \code{\link{incfix}}
#' @examples
#' dec <- c(rnorm(10, mean = 45, sd = 5), rnorm(10, mean = 20, sd = 5))
#' inc <- c(rnorm(10, mean = 45, sd = 5), rnorm(10, mean = 20, sd = 5))
#' id  <- c(rep(1, 10), rep(2, 10))
#'
#' earnet()
#' earpoints(dec, inc)
#'
#' fm <- fmean(dec, inc, id = id)
#'
#' earpoints(fm $dec, fm$inc, l = list(bg = "red"))
#'
#' @export

fmean <- function(dec = NA, inc = NA, int = 1, x = NA, y = NA, z = NA,
                  id = NULL, cart = F)
{
  if(is.null(id)) id <- rep(1, length(dec))

  xyz <- transphere(dec = dec, inc = inc, int = int, x = x, y = y, z = z,
                    into = "xyz")

  sx <- cumsum(xyz$x)
  sy <- cumsum(xyz$y)
  sz <- cumsum(xyz$z)

  keep <- !duplicated(id, fromLast = T)

  sx <- sx[keep]
  sy <- sy[keep]
  sz <- sz[keep]

  l <- length(sx)

  sx <- sx - c(0, sx[-l])
  sy <- sy - c(0, sy[-l])
  sz <- sz - c(0, sz[-l])

  r2 <- sx^2 + sy^2 + sz^2
  r  <- sqrt(r2)

  out <- list(id = unique(id), x = sx/r, y = sy/r, z = sz/r)

  if(!cart) {
    outi <- transphere(x = out$x, y = out$y, z = out$z)
    out  <- list(id = out$id, dec = outi$dec, inc = outi$inc, int = outi$int)
  }

  return(out)

}
