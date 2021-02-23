#' Draws points on an equal area stereonet
#'
#' @description Draws points on an equal area stereonet (modified from RFOC
#' package)
#'
#' @param dec declination of the data; it is the angle from the north
#' taken on an horizontal plane. It is measured clockwise from North and ranges
#' from 0 to 360° (Tauxe 2010). Values outside this range are corrected by the
#' \code{\link{incfix}} function.
#' @param inc inclination of the data; it is the angle from the
#' horizontal, is positive downward, and ranges from +90° for straight down to
#' -90° for straight up (Tauxe, 2010). Values outside this range are corrected
#' by the \code{\link{incfix}} function.
#' @param hsphere the hemisphere onto which to project the data. The
#' default is "b" for both: this useful in the case of oriented vectors rather
#' than lines like for paleomagnetism. Other choices are "l" and "u" for lower
#' and upper hemisphere.
#' @param double whether to plot the equivalent point to one having an
#' inclination of 0°(with dec = dec +180)
#' @param a,l,h,u list of graphical parameters to feed points() for all
#' points, or for the points of the upper (u) and lower (l) hemisphere, and of
#' the samples having an inclination of 0° (h) (the three latter override a).
#' See ?points help page for the possible arguments. See the example for
#' illustration, and ?merge_list for further information.
#' @param labels labels to each point
#' @param pos position of each label (see text() help page)
#' @param output whether to return an output (position of the points in
#' the stereographic projection)
#' @param plot whether to plot
#' @param xh orientation of the x axis: can be 'WE' or 'SN'.
#' @return the x,y coordinates of each point in the projection
#' @references Snyder, John P., 1987, Map Projections-a working manual,
#' USGS-Professional Paper, 383p. pages 185-186, RFOC package
#' @seealso  \code{\link{earnet}}, \code{\link{earplanes}} and
#' \code{\link{incfix}}
#' @examples
#' earnet()
#'
#' h <- 17
#' m <- 11
#'
#' if(m < 10) a <- "0" else a <- ""
#'
#' title(paste("Il est ", h, "h",a,m, sep = ""))
#'
#' i1 <- seq(40, 100, by = 10)
#' i2 <- seq(0, -100, by = -10)
#' d1 <- rep(h * 30 + m * 0.5, length(i1))
#' d2 <- rep(m*6, length(i2))
#'
#' inc <- c(i1,i2)
#' dec <- c(d1,d2)
#'
#' earpoints(dec,inc)
#'
#' @export

earpoints <- function(dec, inc, hsphere = "b", double = FALSE,
                       a = list(pch = 21, col = "black"),
                       l = list(bg = "black"),
                       h = list(bg = "grey"),
                       u = list(bg = "white"),
                       labels = NA, pos = 4,
                       output = FALSE, plot = TRUE,
                       xh = "WE")
{

  if(!(xh == "SN" | xh == "WE")) {
    stop("The 'xh' parameter should be 'WE' or 'SN'.")
  }

  fix <- incfix(dec = dec, inc = inc, hsphere = hsphere)

  m <- earinc(fix$inc)
  x <- m * sin(pi/180 * fix$dec)
  y <- m * cos(pi/180 * fix$dec)

  if (xh == "SN") {
    x <-  y
    y <- -x
  }

  if (plot) {

    li <- merge_list(l, list(bg = "black"), a, list(pch = 21, col = "black"))
    hi <- merge_list(h, list(bg = "grey"),  a, list(pch = 21, col = "black"))
    ui <- merge_list(u, list(bg = "white"), a, list(pch = 21, col = "black"))

    li$x <- x[fix$inc > 0]
    li$y <- y[fix$inc > 0]

    if(double){
      hi$x <- c(x[fix$inc == 0],-x[fix$inc == 0])
      hi$y <- c(y[fix$inc == 0],-y[fix$inc == 0])
    } else {
      hi$x <- x[fix$inc == 0]
      hi$y <- y[fix$inc == 0]
    }

    ui$x <- x[fix$inc < 0]
    ui$y <- y[fix$inc < 0]

    do.call(points,li)
    do.call(points,hi)
    do.call(points,ui)

    if (!is.na(labels)) text(x, y, labels = labels, pos = pos)

  }

  if(output) return(list(x = x, y = y))

}
