#' @title Converts the axis following a given formula
#'
#' @description Converts the axis following a given formula, and places ticks in
#' the new axis value
#'
#' @param side an integer specifying which side of the plot the axis is
#' to be drawn on. The axis is placed as follows: 1=below, 2=left, 3=above and
#' 4=right.
#' @param formula the formula to be converted. Should be of the form y ~ f(x)
#' @param at.maj a vector of the position and labels of the major ticks
#' @param at.min a vector of the position of minor ticks
#' @param labels his can either be a logical value specifying whether
#' (numerical) annotations are to be made at the major tickmarks, or a character
#' or expression vector of labels to be placed at the major tickpoints.
#' @param tick.ratio the ratio of minor to major tick size
#' @param line,pos,font,lty,lwd,lwd.ticks,col,col.ticks,hadj,padj,tcl,... see
#' ?axis function help page for these parameters
#' @seealso \code{\link{minorAxis}}
#' @examples
#' plot(1,1,type = "n", xlim = c(0,12), axes = FALSE ,xlab = "", ylab = "")
#'
#' axis(3)
#'
#' l <- seq_log(10^0,10^12,divide = TRUE)
#'
#' convertAxis(1,y ~ log10(x),l[[1]],l[[2]])
#'
#' @export

convertAxis <- function(side, formula, at.maj, at.min = NULL, labels = at.maj,
                        tick.ratio = 0.75, line = NA, pos = NA, font = NA,
                        lty = "solid", lwd = 1, lwd.ticks = lwd, col = NULL,
                        col.ticks = NULL, hadj = NA, padj = NA, tcl = NA,...)
{
  f <- formFunction(formula)

  # Define the length of ticks ----

  if(is.na(tcl)) maj.tcl <- par()$tcl else if (!is.na(tcl)) maj.tcl <- tcl

  min.tcl <- maj.tcl*tick.ratio

  # Define the length of the axis bar

  if(is.null(at.min)){
    limits <- f(x = c(min(at.maj),max(at.maj)))
  } else {
    limits <- f(x = c(min(c(at.maj,at.min)),max(c(at.maj,at.min))))
  }

  # Plot the axes ----

  axis(side, at = limits, labels = FALSE, tick = TRUE, line = line,
       pos = pos, outer = outer, lty = lty, lwd = lwd, lwd.ticks = 0,
       col = col,...)

  maj.tick<- f(x = at.maj)

  axis(side = side, maj.tick,labels = labels, lwd = lwd, line = line, pos = pos,
       font = font, lwd.ticks = lwd.ticks, col = col, col.ticks = col.ticks, hadj = hadj,
       padj = padj, tcl = maj.tcl,...)

  if(!is.null(at.min)) {
    min.tick <- f(x = at.min)
    axis(side = side, min.tick, labels = FALSE, lwd = 0, lwd.ticks = lwd.ticks,
         line = line, pos = pos, font = font, col = col, col.ticks = col.ticks,
         tcl = min.tcl,...)
  }

}
