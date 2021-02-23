#' @title Sets the plot environment to draw a long data set
#'
#' @description Sets the plot environment to draw a long dataset. It is without
#' background, and with only axes with major and minor ticks.
#'
#' @param xlim,ylim the x and y limits (e.g. xlim = c(-1,1))
#' @param xtick,ytick the interval between each major ticks for x and y
#' @param nx,ny the number of intervals between major ticks to be
#' divided by minor ticks in the x and y axes
#' @param xaxs,yaxs The style of axis interval calculation to be used
#' for the x and y axes. By default it is "i" (internal): it just finds an axis
#' with pretty labels that fits within the original data range. You can also set
#' it to "r" (regular): it first extends the data range by 4 percent at each end
#' and then finds an axis with pretty labels that fits within the extended
#' range. See ?par for further explanation
#' @param xarg,yarg a list of arguments to feed to minorAxis() for the
#' x and y axes. See the ?minorAxis help page for the possible arguments. See
#' ?merge_list for further information.
#' @param add whether to add to an existing plot
#'
#' @seealso Similar functions: \code{\link{greySet}} and \code{\link{blackSet}}
#'
#' To create axes with major and minor ticks: \code{\link{minorAxis}}
#'
#' To print a plot in pdf: \code{\link{pdfDisplay}}
#'
#' To automatically determine pretty interval limits: \code{\link{encase}}
#'
#' @examples
#' y <- c(0,11,19,33)
#' x <- c(1,2,2.5,4)
#'
#' a <- min(y)
#' b <- max(y)
#'
#' f <- encase(a-1,b,5)
#'
#' whiteSet(c(0,4), f, ytick = 5, ny = 5, xaxs = "r")
#'
#' points(x, y, pch=19)
#' @export

whiteSet <- function (xlim, ylim, xtick = NA, ytick = NA, nx = 1, ny = 1,
                      xaxs = "i", yaxs = "i", xarg = list(tick.ratio = 0.5),
                      yarg = list(tick.ratio = 0.5, las = 1), add = FALSE)
{

  if (!(round(nx) == nx & nx >= 1) | !(round(ny) == ny & ny >= 1)) {
    stop("The nx and ny parameters should be integers and higher or equal to 1")
  }

  opar <- par("xaxs", "yaxs")
  on.exit(do.call(par, opar))

  par(xaxs = xaxs, yaxs = yaxs)

  plot.new()
  plot.window(xlim, ylim)

  usr <- par("usr")

  if (!is.null(xarg)) {

    if (is.na(xtick)) {
      xpar <- par("xaxp")
      xtick <- abs(xpar[2] - xpar[1])/xpar[3]
    }

    xra <- usr[c(1, 2)]
    xra <- encase(xra[1], xra[2], xtick)

    if (xlim[1] < xlim[2]) {

      xt <- seq(from = xra[1], to = xra[2], by = xtick)

    } else if (xlim[1] > xlim[2]) {

      xt <- seq(from = xra[1], to = xra[2], by = -xtick)

    } else {

      stop("The two first elements of xlim must be different numbers")

    }

    lx <- merge_list(xarg, list(side = 1, n = nx, at.maj = xt),
                     list(tick.ratio = 0.5))

    do.call(minorAxis, lx)
  }

  if (!is.null(yarg)) {

    if (is.na(ytick)) {

      ypar <- par("yaxp")
      ytick <- abs(ypar[2] - ypar[1])/ypar[3]

    }

    yra <- usr[c(3, 4)]
    yra <- encase(yra[1], yra[2], ytick)

    if (ylim[1] < ylim[2]) {

      yt <- seq(from = yra[1], to = yra[2], by = ytick)

    } else if (ylim[1] > ylim[2]) {

      yt <- seq(from = yra[1], to = yra[2], by = -ytick)

    } else {

      stop("The two first elements of ylim must be different numbers")

    }

    ly <- merge_list(yarg, list(side = 2, n = ny, at.maj = yt),
                     list(tick.ratio = 0.5, las = 1))

    do.call(minorAxis, ly)
  }
}

