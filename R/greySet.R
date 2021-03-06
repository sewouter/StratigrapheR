#' @title Sets the plot environment to draw a long vertical data set
#'
#' @description Sets the plot environment to draw a long dataset. It provides
#' grey bands as supplementary scale, and axes with major and minor ticks.
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
#' @param v whether the grey bands are vertical
#' @param inverse inverse the bands position
#' @param abbr text to be repeated in the grey bands each major tick
#' @param skip number of text redundancies to be skipped
#' @param targ,rarg a list of arguments to feed to text() and rect()
#' respectively. If set to NULL, does not add the corresponding element.
#'
#' @return A plotting environment to draw a long data set
#'
#' @seealso Similar functions: \code{\link{whiteSet}} and \code{\link{greySet}}
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
#' f<- encase(a-1,b,5)
#'
#' greySet(c(0,4),f,abbr="abbr", ytick = 10, ny = 10)
#'
#' points(x, y, pch=19)
#' @export

greySet <- function(xlim, ylim, xtick = NA, ytick = NA, nx = 1, ny = 1,
                    xaxs = "i", yaxs = "i", xarg = list(tick.ratio = 0.5),
                    yarg = list(tick.ratio = 0.5, las = 1), v = T, inverse = F,
                    abbr = "", skip = 0, targ = list(col = "white", lwd = 2),
                    rarg = list(border = NA, col = "grey85"))
{

  opar <- par("xaxs","yaxs")

  on.exit(do.call(par,opar))

  par(xaxs = xaxs, yaxs = yaxs)

  plot.new()
  plot.window(xlim,ylim)

  usr <- par("usr")

  # ----

  if(is.na(xtick)){
    xpar  <- par("xaxp")
    xtick <- abs(xpar[2] - xpar[1])/xpar[3]
  }

  xra <- usr[c(1,2)]
  xra <- encase(xra[1], xra[2], xtick)

  if(xlim[1] < xlim[2]){

    xt <- seq(from = xra[1], to = xra[2], by = xtick)

  } else if (xlim[1] > xlim[2]) {

    xt <- seq(from = xra[1], to = xra[2], by = -xtick)

  } else {stop("The two first elements of xlim must be different numbers")}

  # Y axis ----

  if(is.na(ytick)){
    ypar  <- par("yaxp")
    ytick <- abs(ypar[2] - ypar[1])/ypar[3]
  }

  yra <- usr[c(3,4)]
  yra <- encase(yra[1], yra[2], ytick)

  if(ylim[1] < ylim[2]){

    yt <- seq(from = yra[1], to = yra[2], by = ytick)

  } else if (ylim[1] > ylim[2]) {

    yt <- seq(from = yra[1] ,to = yra[2],by = -ytick)

  } else {stop("The two first elements of ylim must be different numbers")}

  # Background ----

  if(v){
    xy  <- xt
    cor <- xtick
    dt  <- yt
  } else {
    xy  <- yt
    cor <- ytick
    dt  <- xt
  }

  xy <- c((min(xy) - cor), sort(xy), (max(xy) + cor))

  if(inverse){
    xy <- xy + cor
  }

  xy1 <- every_nth(xy, 2, empty = F)
  xy2 <- every_nth(xy, 2, empty = F, inverse = T)

  cx <- max(length(xy1),length(xy2))

  usr <- par("usr")

  tdt  <- every_nth(dt, skip + 1, empty = FALSE, inverse = TRUE)

  ltdt <- length(tdt)

  ldt  <- (c(tdt[-1], 2*tdt[ltdt] - tdt[ltdt-1]) + tdt)/2

  if(v){

    for(i in seq_len(cx))
    {
      pos <- c(xy1[i], xy2[i])

      if(!is.null(rarg)){

        lr  <- merge_list(list(xleft = pos[1], ybottom = usr[3],
                               xright = pos[2], ytop= usr[4]),
                          rarg, list(border = NA, col = "grey85"))

        do.call(rect, lr)

      }

      if(!is.null(targ)){

        lt  <- merge_list(list(x = mean(pos), y = tdt, labels = abbr),
                          targ, list(col = "white", lwd = 2))

        ll  <- merge_list(list(x = pos[1], y = ldt, labels = pos[1]),
                          targ, list(col = "white", lwd = 2, srt = 90,
                                     adj = c(NA,-0.5)))

        do.call(text, lt)

        do.call(text, ll)

      }

    }

  } else {

    for(i in seq_len(cx))
    {
      pos <- c(xy1[i], xy2[i])

      if(!is.null(rarg)){

        lr  <- merge_list(list(xleft = usr[1], ybottom = pos[1],
                               xright = usr[2], ytop= pos[2]),
                          rarg, list(border = NA, col = "grey85"))

        do.call(rect, lr)

      }

      if(!is.null(targ)){

        lt  <- merge_list(list(x = tdt, y = mean(pos), labels = abbr),
                          targ, list(col = "white", lwd = 2))

        ll  <- merge_list(list(x = ldt, y = pos[1], labels = pos[1]),
                          targ, list(col = "white", lwd = 2, adj = c(NA,1)))

        do.call(text, lt)

        do.call(text, ll)

      }

    }

  }

  # ----

  if(!is.null(xarg)){

    lx <- merge_list(xarg, list(side = 1, n = nx, at.maj = xt),
                     list(tick.ratio = 0.5))

    do.call(minorAxis, lx)

  }

  if(!is.null(yarg)){

    ly <- merge_list(yarg, list(side = 2, n = ny, at.maj = yt),
                     list(tick.ratio = 0.5, las = 1))

    do.call(minorAxis, ly)

  }

}


