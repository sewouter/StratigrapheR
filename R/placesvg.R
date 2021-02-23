#' @title Draws a pointsvg object
#'
#' @description Draws a svg object imported as data frame using
#' \code{\link{pointsvg}}, with its importation coordinates (or with
#' standardisation).
#'
#' @param object a pointsvg object (svg object imported as data frame
#' using \code{\link{pointsvg}}).
#' @param forget the elements that should be discarded, by their id
#' or index (i.e. name or number of appearance).
#' @param front,back the elements to be put in front and back position,
#' by their id or index (i.e. name or number of appearance). By default the
#' order is the one of the original .svg file.
#' @param standard whether to standardise (centre to (0,0), rescale so
#' that extreme points are at -1 and 1) or not (T or F)
#' @param keep.ratio if the object is to be  standardised, whether to
#' keep the x/y ratio (T or F)
#' @param border the lines color.
#' @param col the polygones background color. If density is specified with
#' a positive value this gives the color of the shading lines.
#' @param density the density of shading lines, in lines per inch. The
#' default value of NULL means that no shading lines are drawn.
#' @param angle the slope of shading lines, given as an angle in degrees
#' (counter-clockwise)
#' @param lty,lwd the border line type and width, see ?par for details.
#' @param scol,slty,slwd the colour, type and width of the shading lines.
#'
#' @seealso \code{\link{centresvg}} and \code{\link{framesvg}}
#' @examples
#' object <- example.ammonite
#'
#' plot(c(-2,2), c(-2,2), type = "n")
#'
#' placesvg(object, lty = 1,density = 20, angle = 45)
#'
#' @export

placesvg <- function(object, forget = NULL, front = NULL, back = NULL,
                     standard = FALSE, keep.ratio = FALSE,
                     col = NA, border = "black",
                     density = NULL, angle = 45,
                     lwd = par("lwd"), lty = par("lty"),
                     scol = border, slty = lty, slwd = lwd)
{
  object <- changesvg(object, front = front, back = back, forget = forget,
                      standard = standard, keep.ratio = keep.ratio)

  on <- ignore(object$i, object$x, object$y, list(type = object$type))

  if(length(on$x) != 0){

    id <- unique(data.frame(i = on$i, type = on$type, stringsAsFactors = F))

    for(j in seq_len(nrow(id)))
    {
      itype <- id$type[j]

      oix <- on$x[on$i == id$i[j] & on$type == itype]
      oiy <- on$y[on$i == id$i[j] & on$type == itype]

      if(itype == "P"){

        if(length(density) != 0){

          polygon(oix, oiy, col = col, border = NA)
          polygon(oix, oiy, col = scol, border = NA, density = density,
                  angle = angle, lwd = slwd, lty = slty)
          polygon(oix, oiy, col = NA, border = border, lwd = lwd, lty = lty)

        } else {

          polygon(oix, oiy, col = col, border = border,lwd = lwd, lty = lty)

        }

      } else if (itype == "L") {
        lines(oix, oiy,col = border, lwd = lwd, lty = lty)
      }
    }
  }
}


