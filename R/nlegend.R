#' @title New legend element
#'
#' @description Prepares a plotting environment for a new element of a
#' multifigure legend
#'
#' @param t text to provide the legend
#' @param xt the x position of the text
#' @param xmin,xmax,ymin,ymax the x and y limits for the plotting area
#' @param temp whether to plot a template for visualisation
#' @param ... parameters to be fed to the \code{text} function, such as
#' \code{cex} for the size of the text
#'
#' @seealso \code{\link{multigons}}, \code{\link{bedtext}},
#' \code{\link{infobar}} and \code{\link{ylink}}
#'
#' @examples
#' opar <- par("mar")
#'
#' par(mar = c(0,0,0,0))
#'
#' layout(matrix(1:6, 6, 1, byrow = TRUE))
#'
#' nlegend(t = paste("Shaded stuff. By the way you can\nwrite",
#'                   "text in several lines if needed"), cex = 1.2)
#'
#' rect(-1,-1,1,1, density = 10)
#'
#' nlegend(t = paste0("Text: left side at x = 1.3 (default xt value)",
#'                    ";\nsize adapted with cex argument"),
#'                     temp = TRUE, cex = 1.4)
#'
#' par(mar = opar)
#'
#' @export

nlegend <- function(t = "Text", xt = 1.3,
                    xmax = 5,   xmin = -1.2,
                    ymax = 1.5, ymin = -ymax,
                    temp = FALSE, ...)
{
  plot.new()
  plot.window(xlim = c(xmin, xmax), ylim = c(ymin, ymax))

  text(xt, 0, t, adj = c(0,0.5), ...)

  if(temp){

    rect(1,1,-1,-1, col = "grey", border = NA)

    text(c(0,1,-1, 1,-1),c(0,1,1,-1,-1),
         c("[0,0]", "[1,1]", "[-1,1]", "[1,-1]", "[-1,-1]"))

  }
}
