#' Draws a Zijderveld plot
#'
#' @description Draws a Zijderveld plot: it projects 3D points (having
#' declination, inclination and intensity) in 2D, horizontally and vertically.
#'
#' @param dec declination of the data; it is the angle from the north
#' taken on an horizontal plane. It is measured clockwise from North and ranges
#' from 0 to 360° (Tauxe 2010). Values outside this range are corrected
#' by incfix().
#' @param inc inclination of the data; it is the angle from the
#' horizontal, is positive downward, and ranges from +90° for straight down to
#' -90° for straight up (Tauxe, 2010). Values outside this range are corrected
#' by incfix().
#' @param int intensity of the data.
#' @param xh orientation of the x axis for the horizontal points: can be
#' 'SN' or 'WE'.
#' @param xv orientation of the x axis for the horizontal points: can be
#' 'SN', 'WE' or 'modified' (for the latter the horizontal projection of the
#' vector given by the square root of the addition of the squared horizontal
#' components).
#' @param centre logical, whether the [0,0] point should be in the centre
#' of the plot. Is ignored if xlim and/or ylim are defined.
#' @param xlim,ylim the x and y minimal limits. The actual limits can
#' change to keep a x/y ratio of 1.
#' @param unit the tick interval.
#' @param xlab,ylab the titles for the axes.
#' @param labels a character vector of labels to add to each point.
#' @param nlabels the number of labels to skip (for clarity).
#' @param h,v,f,t,l list of graphical parameters to feed the graphical
#' functions: h, v and f are fed to points() for the horizontal, vertical and
#' first points respectively; t is fed to the text() for the labels and l is
#' fed to lines() for the lines joining each horizontal and vertical points. See
#' ?points, ?text and ?lines help page for the possible arguments. See
#' ?merge_list for further information.
#' @param anchored logical, whether the lines should be anchored to the
#' [0,0] point.
#' @param style the style of the plot: 'branches', 'box0', 'box1', or
#' 'box2'. The boxes are advised when zooming using xlim and/or ylim.
#' @param tcl The length of tick marks (see par() help page).
#' @param orientation logical, whether to add captions indicating the
#' orientation of the plot.
#' @param scientific logical or NA, whether have scientific notation
#' (e.g. -1.0E-06) or not (e.g. 0.00015). If NA, R will be left only judge.
#' @param decimals the number of decimals if scientific is T or F. Having
#' not enough decimals can lead to override the unit parameter, but the tick
#' labels will be correctly aligned.
#' @param add logical, whether to add the plot to an existing plot.
#'
#' @details By default horizontal projection is made of black points, vertical
#' of white points.
#'
#' @references
#' \itemize{
#'   \item Tauxe, L., 2010. Essentials of Paleomagnetism. University of
#'   California Press.
#'   }
#'
#' @seealso \code{\link{earnet}}
#' @examples
#' zd <- zeq_example
#'
#' ori <- par()$mfrow
#'
#' par(mfrow = c(1,2))
#'
#' zijderveld(dec = zd$Dec, inc = zd$Inc, int = zd$Int,
#'            xh = "WE", unit = 10^-5)
#'
#' zijderveld(dec = zd$Dec, inc = zd$Inc, int = zd$Int,
#'            style = "box1", scientific = FALSE, decimals = 5,
#'            labels = zd$Treat, nlabels = 2)
#'
#' par(mfrow = ori)
#'
#' @export

zijderveld <- function(dec, inc, int, xh = "WE", xv = xh, centre = F,
                       xlim = NA, ylim = NA, unit = NA,
                       xlab = "", ylab = "",
                       labels = NA, nlabels = 1,
                       h = list(pch = 19),
                       v = list(pch = 21, bg = "white"),
                       f = list(pch = 21, bg = "white", cex = 1.5),
                       t = list(pos = 3, offset = 0.5),
                       l = list(),
                       anchored = T,
                       style = "branches",
                       tcl = 0.2,
                       orientation = TRUE,
                       scientific = NA,
                       decimals = 10,
                       add = FALSE)
{
  # Determine the x and y values for horizontal and vertical points ----

  cart <- transphere(dec = dec, inc = inc, int = int)

  if(xh == "SN"){

    xhi <- cart$x
    yhi <- -cart$y

  } else if(xh == "WE"){

    xhi <- cart$y
    yhi <- cart$x

  } else {

    stop("The xh parameter should be 'SN' or 'WE'.")

  }

  yvi <- -cart$z

  if(xv == "SN"){

    xvi <- cart$x

  } else if (xv == "WE"){

    xvi <- cart$y

  } else if(xv == "modified"){

    xvi <- sqrt(cart$x^2 + cart$y^2)

  } else {

    stop("The xh parameter should be 'SN', 'WE' or 'modified'.")

  }

  # Determine and plot the environment ----

  if(!add){

    # Determine the axes titles ----

    if(orientation){

      if(xh == xv){

        if(xh == "WE") {

          xcap <- c("W","E")
          ycap <- c("S, Down", "N, Up")

        } else if(xh == "SN"){

          xcap <- c("S","N")
          ycap <- c("E, Down", "W, Up")

        }

      } else {

        if(xv == "WE") {

          xcap <- c("S, W","N, E")
          ycap <- c("E, Down", "W, Up")

        } else if(xv == "SN"){

          xcap <- c("W, S","E, N")
          ycap <- c("S, Down", "N, Up")

        } else if(xv == "modified"){

          if(xh == "WE") {

            xcap <- c("W, mod","E, mod")
            ycap <- c("S, Down", "N, Up")

          } else if(xh == "SN"){

            xcap <- c("S, mod","N, mod")
            ycap <- c("E, Down", "W, Up")

          }

        }

      }

    }

    # Determine the default axes limits ----

    if(centre){

      xmax <- max(abs(c(xhi,xvi)))

      xtlim <- c(-xmax,xmax)

    } else {

      xmax <- max(c(xhi,xvi,0))
      xmin <- min(c(xhi,xvi,0))

      xtlim <- c(xmin,xmax)

    }

    if(centre){

      ymax <- max(abs(c(yhi,yvi)))

      ytlim <- c(-ymax,ymax)

    } else {

      ymax <- max(c(yhi,yvi,0))
      ymin <- min(c(yhi,yvi,0))

      ytlim <- c(ymin,ymax)

    }

    # Determine the intervals between ticks ----

    if(is.na(unit)[[1]]){

      px <- pretty(xtlim)
      py <- pretty(ytlim)

      mult <- min(c((px[2] - px[1]), (py[2] - py[1])))

      if(!is.na(xlim[[1]]) & length(xlim) == 2 & class(xlim) == "numeric"){
        pxn  <- pretty(xlim)
        mult <- min(mult, (pxn[2] - pxn[1]))
      }

      if(!is.na(ylim[[1]]) & length(ylim) == 2 & class(ylim) == "numeric"){
        pyn  <- pretty(ylim)
        mult <- min(mult, (pyn[2] - pyn[1]))
      }

    } else if((class(unit) == "numeric" | class(unit) == "integer") &
              length(unit) == 1 & !is.na(unit) & unit > 0){

      mult <- unit

    } else {

      stop(paste("The 'unit' parameter should be NA or a positive",
                 " non zero numeric of length one.", sep = ""))

    }

    # Determine the axes extremes ----

    xlcas <- encase(xtlim[1] - mult, xtlim[2] + mult, mult)
    ylcas <- encase(ytlim[1] - mult, ytlim[2] + mult, mult)

    # Determine plot limits ----

    if(is.na(xlim[[1]])){
      xlim <- xlcas
    }

    if(is.na(ylim[[1]])){
      ylim <- ylcas
    }

    # Plot the environment ----

    plot(0, 0, asp = 1, xlim = xlim, ylim = ylim, type = "n",
         xlab = "", ylab = "", axes = F)

    pusr <- par("usr")

    if(style == "box0" | style == "box1" | style == "box2"){

      box()

      xcas <- encase(pusr[1], pusr[2], mult)
      ycas <- encase(pusr[3], pusr[4], mult)

      xtic <- seq(xcas[1], xcas[2], mult)
      ytic <- seq(ycas[1], ycas[2], mult)

      if(!is.na(scientific[[1]])) {

        if(scientific){
          xtic <- formatC(xtic, format = "e", digits = decimals)
          ytic <- formatC(ytic, format = "e", digits = decimals)
        } else if (!scientific){
          xtic <- formatC(unique(round(xtic, decimals)),decimals, format = "f")
          ytic <- formatC(unique(round(ytic, decimals)),decimals, format = "f")
        }

        axis(1, labels = xtic, lwd = 0, lwd.ticks = 1, tcl = -tcl, at = xtic)
        axis(2, labels = ytic, lwd = 0, lwd.ticks = 1, tcl = -tcl, las = 1,
             at = ytic)

      } else {

        axis(1, lwd = 0, lwd.ticks = 1, tcl = -tcl, at = xtic)
        axis(2, lwd = 0, lwd.ticks = 1, tcl = -tcl, las = 1,
             at = ytic)

      }

      if((style == "box0" | style == "box1") & orientation){
        text(xlcas, 0, labels = xcap)
        text(0, ylcas, labels = ycap)
      }

      if(style == "box2" & orientation){
        axis(3, pos = 0, labels = xcap, lwd = 0, las = 1,
             at = xlcas, padj = 0.5)
        axis(4, pos = 0, labels = ycap, lwd = 0, las = 1,
             at = ylcas, hadj = 0.2)
      }

      if(style == "box1") points(0,0,pch = 3,cex = 1.5)

      if(style == "box2" ) abline(v = 0,h = 0)

    } else if(style == "branches"){

      xtic <- seq(xlcas[1], xlcas[2], mult)
      ytic <- seq(ylcas[1], ylcas[2], mult)

      if(!is.na(scientific[[1]])) {

        if(scientific){
          xtic <- formatC(xtic, format = "e", digits = decimals)
          ytic <- formatC(ytic, format = "e", digits = decimals)
        } else if (!scientific){
          xtic <- formatC(unique(round(xtic, decimals)),decimals, format = "f")
          ytic <- formatC(unique(round(ytic, decimals)),decimals, format = "f")
        }

        xmar <- c(xtic[1], xtic[length(xtic)])
        ymar <- c(ytic[1], ytic[length(ytic)])

        axis(1, pos = 0, labels = xmar, lwd = 0, at = xmar)
        axis(2, pos = 0, labels = ymar, lwd = 0, las = 1, at = ymar)

      } else {

        xmar <- c(xtic[1], xtic[length(xtic)])
        ymar <- c(ytic[1], ytic[length(ytic)])

        axis(1, pos = 0, lwd = 0, at = xmar)
        axis(2, pos = 0, lwd = 0, las = 1, at = ymar)

      }

      axis(1, pos = 0, labels = F, lwd = 1, lwd.ticks = 0,
           at = xlcas)
      axis(1, pos = 0, labels = F, lwd = 0, lwd.ticks = 1, tcl = -tcl,
           at = xtic)
      axis(1, pos = 0, labels = F, lwd = 0, lwd.ticks = 1, tcl = tcl, at = xtic)

      axis(3, pos = 0, labels = xcap, lwd = 0, las = 1,
           at = c(xtic[1], xtic[length(xtic)]))

      axis(2, pos = 0, labels = F, lwd = 1, lwd.ticks = 0,
           at = ylcas)
      axis(2, pos = 0, labels = F, lwd = 0, lwd.ticks = 1, tcl = -tcl,
           at = ytic)
      axis(2, pos = 0, labels = F, lwd = 0, lwd.ticks = 1, tcl = tcl, at = ytic)

      axis(4, pos = 0, labels = ycap, lwd = 0, las = 1,
           at = c(ytic[1], ytic[length(ytic)]))

    } else {

      stop(paste("The 'style' parameter should be 'box0',",
                 " 'box1', 'box2' or 'branches'.", sep = ""))

    }

  }

  # Plot the data ----

  hi <- merge_list(h, list(pch = 19))
  vi <- merge_list(v, list(pch = 21, bg = "white"))
  fi <- merge_list(f, list(pch = 21, bg = "white", cex = 1.5))

  hi <- c(list(x = xhi, y = yhi), hi)
  vi <- c(list(x = xvi, y = yvi), vi)
  fi <- c(list(x = c(xhi[1],xvi[1]), y = c(yhi[1],yvi[1])), fi)

  if(anchored){
    xlhi <- c(xhi,0)
    ylhi <- c(yhi,0)
    xlvi <- c(xvi,0)
    ylvi <- c(yvi,0)
  } else {
    xlhi <- xhi
    ylhi <- yhi
    xlvi <- xvi
    ylvi <- yvi
  }

  lhi <- c(list(x = xlhi, y = ylhi), l)
  lvi <- c(list(x = xlvi, y = ylvi), l)

  do.call(lines, lhi)
  do.call(lines, lvi)

  do.call(points, fi)
  do.call(points, hi)
  do.call(points, vi)

  if(!(length(labels) == 1 & is.na(labels[[1]]))){

    labels <- every_nth(labels, nlabels, inverse = TRUE)

    ti <- merge_list(t, list(pos = 3, offset = 0.5))

    thi <- c(list(x = xhi, y = yhi, labels = labels), ti)
    tvi <- c(list(x = xvi, y = yvi, labels = labels), ti)

    do.call(text,thi)
    do.call(text,tvi)

  }

}
