#' @title Draws a standardised pointsvg object into a given frame
#'
#' @description Draws a svg object imported as data frame using
#' \code{\link{pointsvg}} into a given frame.
#'
#' @param object a pointsvg object (svg object imported as data frame
#' using \code{\link{pointsvg}}).
#' @param xmin,xmax the x value for the left and right side of the symbol
#' @param ymin,ymax the y value for the low and high side of the symbol7
#' @param forget the elements that should be discarded, by their id
#' or index (i.e. name or number of appearance).
#' @param front,back the elements to be put in front and back position,
#' by their id or index (i.e. name or number of appearance). By default the
#' order is the one of the original .svg file.
#' @param standard whether to standardise (centre to (0,0), rescale so
#' that extreme points are at -1 and 1) or not (T or F)
#' @param keep.ratio if the object is to be  standardised, whether to
#' keep the x/y ratio (T or F)
#' @param col the polygones background color. If density is specified with
#' a positive value this gives the color of the shading lines.
#' @param border the lines color.
#' @param density the density of shading lines, in lines per inch. The
#' default value of NULL means that no shading lines are drawn.
#' @param angle the slope of shading lines, given as an angle in degrees
#' (counter-clockwise)
#' @param lty,lwd the border line type and width, see ?par for details.
#' @param scol,slty,slwd the colour, type and width of the shading lines.
#' @param plot whether to add to a plot
#' @param output whether to output the new object coordinates
#'
#' @details The \code{\link{centresvg}} and
#' \code{\link{framesvg}} have a lot of similarities with the
#' \code{\link{multigons}} function: the graphical parameters are mostly
#' identical. However there is a strong distinction between the -svg functions
#' and multigons: when providing several graphical arguments, multigons will
#' attribute them to each polygon, whereas the .svg functions will use them for
#' each repetition of the .svg object. Using the latter, the graphical
#' parameters will be applied to all the elements of a drawing. If you want
#' a finer personalisation you have to use multigons and multilines (or an
#' hybrid of the two, yet to be coded).
#'
#' @seealso Similar functions: \code{\link{centresvg}} and
#' \code{\link{placesvg}}
#'
#' Change the drawing: \code{\link{changesvg}} and \code{\link{clipsvg}}
#'
#' Uses \code{\link{ignore}} to avoid drawing unnecessary objects
#'
#' @examples
#' # Simple use
#'
#' object <- example.ammonite
#'
#' xmin <- c(8,7)
#' xmax <- c(10,9)
#' ymin <- c(7,6)
#' ymax <- c(9,8)
#'
#' plot(c(-10,10), c(-10,10), type = "n")
#'
#' abline(v = unique(c(xmax, xmin)))
#' abline(h = unique(c(ymax, ymin)))
#'
#' framesvg(object, xmin, xmax, ymin, ymax, col = c("white", "grey80"))
#'
#' # Precision positioning
#'
#' l <- c(1,2,3)
#' r <- c(0,1,2)
#' h  <- c(4,3,4)
#' i   <- c("B1","B2","B3")
#'
#' basic.litholog <- litholog(l,r,h,i)
#'
#' whiteSet(xlim = c(0,4), ylim = c(0,3), ytick = 1, ny = 10)
#'
#' framesvg(example.lense, 0,3,1,2, forget = "P1", border = "red", lwd = 3)
#'
#' multigons(basic.litholog$i, basic.litholog$xy, basic.litholog$dt)
#'
#' @export

framesvg <- function(object, xmin, xmax, ymin, ymax,
                     forget = NULL, front = NULL, back = NULL,
                     standard = FALSE, keep.ratio = FALSE,
                     col = NA, border = "black", density = NA, angle = 45,
                     lwd = par("lwd"), lty = par("lty"),
                     scol = border, slty = lty, slwd = lwd,
                     plot = TRUE, output = FALSE)
{

  object <- changesvg(object, front = front, back = back, forget = forget,
                      standard = standard, keep.ratio = keep.ratio)

  argi <- list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax,
               border = border, col = col, density = density, angle = angle,
               lty = lty, lwd = lwd, scol = scol, slty = slty , slwd = slwd)

  larg <- unlist(lapply(argi,length))
  lj   <- max(larg)

  if(any(!(larg == 1 | larg == lj))){
    stop("The arguments beside 'object' should be of length 1 or n")
  }

  if(all(larg[1:4] == 1) & lj != 1){
    stop(paste("Multiple graphical parameters should apply for multiple",
               " drawings\n (they do not apply for each element of ",
               "the drawing, see multilines\n and multigons for that). ",
               "For multiple drawings provide at least one\n of the ",
               "following arguments with multiple information: xmin, "
               ,"xmax,\n ymin, ymax", sep = ""))
  }

  or1 <- which(larg == lj)
  or2 <- which(larg == 1)

  am <- data.frame(argi[or1], stringsAsFactors = F)

  au <- data.frame(argi[or2], stringsAsFactors = F)
  au <- au[rep(1,lj),]
  row.names(au) <- NULL

  if(lj == 1) {
    a1 <- am
  } else if(ncol(am) != 0 & ncol(au) != 0){

    a1 <- cbind(am,au)
    a1[,c(or1,or2)] <- a1
    colnames(a1)  <- names(argi)

  } else if (ncol(am) == 0 & ncol(au) != 0){
    a1 <- au
  } else if (ncol(am) != 0 & ncol(au) == 0){
    a1 <- am
  }

  n <- nrow(a1)
  l <- nrow(object)
  r <- length(unique(object$i))

  nid <- rep(seq_len(n),rep(l,n))

  a2 <- a1[nid,1:4]
  row.names(a2) <- NULL

  refine <- rep(seq_len(n), rep(r,n))
  a3 <- a1[refine,5:13]

  object <- object[rep(seq_len(l),n),]

  object$id <- paste(object$id, nid, sep = "_")

  ox <- (object$x * ((a2$xmax - a2$xmin)/2)) + ((a2$xmax + a2$xmin)/2)
  oy <- (object$y * ((a2$ymax - a2$ymin)/2)) + ((a2$ymax + a2$ymin)/2)

  o   <- object
  o$x <- ox
  o$y <- oy

  if(isTRUE(plot)){

    on <- ignore(o$i, o$x, o$y, list(type = o$type), arg = as.list(a3))

    if(length(on$x) != 0){

      id <- unique(data.frame(i = on$i, type = on$type, stringsAsFactors = F))

      for(j in seq_len(nrow(id)))
      {
        itype <- id$type[j]

        oix <- on$x[on$i == id$i[j] & on$type == itype]
        oiy <- on$y[on$i == id$i[j] & on$type == itype]

        if(itype == "P"){

          if(!(is.na(on$density[j])) | isFALSE(on$density[j] < 0)){

            polygon(oix, oiy, col = on$col[j], border = NA)
            polygon(oix, oiy, col = on$scol[j], border = NA,
                    density = on$density[j], angle = on$angle[j],
                    lwd = on$slwd[j], lty = on$slty[j])
            polygon(oix, oiy, col = NA, border = on$border[j],
                    lwd = on$lwd[j], lty = on$lty[j])

          } else {

            polygon(oix, oiy, col = on$col[j], border = on$border[j],
                    lwd = on$lwd[j], lty = on$lty[j])

          }

        } else if (itype == "L") {
          lines(oix, oiy,col = on$border[j], lwd = on$lwd[j], lty = on$lty[j])
        }

      }

    }
  }

  if(isTRUE(output)) {
    return(data.frame(o[,1:4]))
  }

}
