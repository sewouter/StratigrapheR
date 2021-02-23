#' @title Adds an axis with minor ticks to a plot
#'
#' @description Adds an axis with minor ticks to a plot, but with the
#' possibility to have no superposition of minor ticks on major ticks, allowing
#' to export a clean plot in vector format. It is based on the
#' minor.tick function in the Hmisc package.
#'
#' @param side an integer (here 1,2,3 or 4) specifying which side of the
#' plot the axis is to be drawn on. The axis is placed as follows: 1=below,
#' 2=left, 3=above and, 4=right.
#' @param n the number of intervals defined by the minor ticks
#' @param at.maj the positions at which major tick-marks are to be drawn.
#' By default (when NULL) tickmark locations are computed, see the "Details"
#' part in the ?axis help page.
#' @param at.min the positions at which minor tick-marks are to be drawn.
#' This parameter overrides n.
#' @param range the range of the axis
#' @param tick.ratio ratio of lengths of minor tick marks to major tick
#' marks. The length of major tick marks is retrieved from par("tcl") unless
#' specified otherwise.
#' @param labels.maj this can either be a logical value specifying
#' whether (numerical) annotations are to be made at the major tickmarks, or a
#' character or expression vector of labels to be placed at the major
#' tickpoints.
#' @param extend whether to add minor ticks even outside the major ticks
#' (T) or not (F)
#' @param line,pos,outer,font,lty,lwd,lwd.ticks,col,col.ticks,hadj,padj,tcl,...
#' see the ?axis function help page for the other parameters
#'
#' @seealso Set a plot environment with minorAxis: \code{\link{whiteSet}},
#' \code{\link{blackSet}} and \code{\link{greySet}}
#'
#' The ticks repartition is computed using \code{\link{minorAxisTicks}}
#'
#' @examples
#' plot.new()
#' plot.window(xlim = c(0,1), ylim = c(0,1))
#'
#' minorAxis(1, n = 10, range = c(0.12,0.61))
#'
#' minorAxis(3, n = 10, extend=FALSE)
#' @export

minorAxis <- function(side, n = NULL, at.maj = NULL, at.min = NULL, range = NULL,
                      tick.ratio = 0.5, labels.maj = TRUE, line = NA, pos = NA,
                      outer = FALSE, font = NA, lty = "solid", lwd = 1,
                      lwd.ticks = lwd, col = NULL, col.ticks = NULL, hadj = NA,
                      padj = NA, extend = FALSE, tcl = NA, ...)
{

  if(side == 1 | side == 3){
    usr <- par("usr")[c(1,2)]
    if(par("xlog")) usr <- 10^usr
  } else if (side == 2 | side == 4) {
    usr <- par("usr")[c(3,4)]
    if(par("ylog")) usr <- 10^usr
  }

  if(!is.null(range)) {
    limits <- c(max(c(min(range), min(usr))), min(c(max(range), max(usr))))
  } else {
    limits <- usr
  }

  if(is.null(at.min)){
    ticks <- minorAxisTicks(usr = limits, n = n,
                            at.maj = at.maj, extend = extend)

    at.maj <- unlist(unlist(ticks[1]))

  }

  if(is.null(labels.maj)) labels.maj <- unlist(ticks[1])

  if((is.null(n)) & is.null(at.min)){

    if(is.na(tcl)) tcl <- par("tcl")

    if(!extend) limits <- c(min(at.maj), max(at.maj))

    axis(side, at = limits, labels = FALSE, tick = TRUE, line = line,
         pos = pos, outer = outer, lty = lty, lwd = lwd, lwd.ticks = 0,
         col = col,...)

    axis(side, at = at.maj, labels = labels.maj, tick = TRUE,
         line = line, pos = pos, outer = outer, font = font, lty = lty,
         lwd = 0, lwd.ticks = lwd.ticks, col = col, col.ticks = col.ticks,
         hadj = hadj, padj = padj, tcl = tcl,...)

  } else {

    if(is.na(tcl)) maj.tcl <- par()$tcl else if (!is.na(tcl)) maj.tcl <- tcl

    min.tcl <- maj.tcl*tick.ratio

    if(is.null(at.min)) at.min <- unlist(ticks[2])

    if(!extend) limits <- c(min(c(at.maj, at.min)), max(c(at.maj, at.min)))

    axis(side, at = limits, labels = FALSE, tick = TRUE, line = line,
         pos = pos, outer = outer, lty = lty, lwd = lwd, lwd.ticks = 0,
         col = col,...)

    axis(side, at = at.maj, labels = labels.maj, tick = TRUE,
         line = line, pos = pos, outer = outer, font = font, lty = lty,
         lwd = 0, lwd.ticks = lwd.ticks, col = col, col.ticks = col.ticks,
         hadj = hadj, padj = padj, tcl = maj.tcl,...)

    axis(side, at = at.min, labels = FALSE, tick = TRUE, line = line,
         pos = pos, outer = outer, lwd = 0, lwd.ticks = lwd.ticks, col = col,
         col.ticks = col.ticks, tcl = min.tcl,...)

  }
}
