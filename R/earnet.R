#' Draws an equal area stereonet
#'
#' @description Draws Equal Area Stereo-Net. Lambert azimuthal Equal-Area
#' (Schmidt) from Snyder p. 185-186 (modified from RFOC package)
#'
#' @param xlim,ylim the x and y minimal limits. The actual limits can
#' change to keep a x/y ratio of 1
#' @param ndiv the number of intervals between each line crossing
#' @param col the colour of the net
#' @param border the colour of the border and crosshair
#' @param lwd the line width
#' @param orientation logical, whether to add captions indicating the
#' orientation of the plot.
#' @param xh orientation of the x axis: can be 'WE' or 'SN'. Has to be
#' provided to earplanes and earpoints
#' @param add logical, whether to add the circle to an existing plot
#' @references Snyder, John P., 1987, Map Projections-a working manual,
#' USGS-Professional Paper, 383p. pages 185-186, RFOC package
#' @seealso  \code{\link{earinc}}, \code{\link{earplanes}},
#' \code{\link{earpoints}} and \code{\link{zijderveld}}
#' @examples
#' par(mfrow = c(1,2))
#' earnet()
#' earnet(xh  = "SN")
#' par(mfrow = c(1,1))
#' @importFrom grDevices gray
#' @export

earnet <- function (xlim = c(-1.1,1.1), ylim = c(-1.1,1.1), ndiv = 10,
                    col = gray(0.7), border = "black", lwd = 1,
                    orientation = TRUE, xh = "WE", add = FALSE)
{
  if(ndiv != trunc(ndiv)) stop("ndiv should be a natural number")

  if(xh == "WE"){
    cap <- c("S","W","N","E")
    las <- 1
  } else if (xh == "SN") {
    cap <- c("E","S","W","N")
    las <- 3
  } else {
    stop("The 'xh' parameter should be 'WE' or 'SN'.")
  }

  if (add == FALSE) {
    plot(xlim, ylim, type = "n",
         asp = 1, xaxt = "n", yaxt = "n", ann = FALSE)
  }

  lam = pi * seq(from = 0, to = 180, by = 10/ndiv)/180
  lam0 = pi/2
  for (j in seq(from = -80, to = 80, by = 10)) {
    phi = j * pi/180
    R = sqrt(2)/2
    kp = sqrt(2/(1 + cos(phi) * cos(lam - lam0)))
    x = R * kp * cos(phi) * sin(lam - lam0)
    y = R * kp * sin(phi)

    if(xh == "WE"){
      lines(x, y, col = col, lwd = lwd)
    } else {
      lines(y, -x, col = col, lwd = lwd)
    }

  }

  earplanes(rep(0,17),seq(10, 170, by = 10), ndiv = ndiv,
            a = list(col = col, lwd = lwd), xh = xh)

  encircle(ndiv = 36 * ndiv, type = "l", lwd = lwd)

  segments(c(-0.02, 0), c(0, -0.02), c(0.02, 0), c(0, 0.02),
           col = border, lwd = lwd)

  if(orientation){
    axis(1,pos = -1, at = 0, lwd = 0, lwd.ticks = 1,labels = cap[1],
         tcl = -0.2, las = 1, padj = -1)
    axis(2,pos = -1, at = 0, lwd = 0, lwd.ticks = 1,labels = cap[2],
         tcl = -0.2, las = 1, hadj = 0.2)
    axis(3,pos =  1, at = 0, lwd = 0, lwd.ticks = 1,labels = cap[3],
         tcl = -0.2, las = 1, padj = 1)
    axis(4,pos =  1, at = 0, lwd = 0, lwd.ticks = 1,labels = cap[4],
         tcl = -0.2, las = 1, hadj = 1)
  }
}


