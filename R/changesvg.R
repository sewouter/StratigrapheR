#' @title Changes a pointsvg object
#'
#' @description Changes a svg object imported as data frame using
#' \code{\link{pointsvg}}.
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
#' @param round whether to round the coordinates or not (T or F)
#' @param xdigits the number of digits after the decimal to round to for
#' x values
#' @param ydigits the number of digits after the decimal to round to for
#' y values
#' @param xinverse whether to inverse the plotting for x values (T or F)
#' @param yinverse whether to inverse the plotting for y values (T or F)
#' @return A data.frame with x and y coordinates, ids for each object, and a
#' type, either line (L) or polygon (P)
#' @seealso Importing .svg objects: \code{\link{pointsvg}}
#'
#' Plot the drawing and change the coordinates :\code{\link{placesvg}},
#' \code{\link{centresvg}} and \code{\link{framesvg}}
#'
#' Clip the drawing: \code{\link{clipsvg}}
#' @examples
#' object1 <- example.lense
#'
#' opar <- par("mfrow")
#' par(mfrow = c(1,3))
#'
#' plot(c(-1,1), c(-1,1), type = "n")
#' placesvg(object1)
#'
#' plot(c(-1,1), c(-1,1), type = "n")
#' object2 <- changesvg(object1, forget = 1)
#' placesvg(object2)
#'
#' plot(c(-1,1), c(-1,1), type = "n")
#' object3 <- changesvg(object1, forget = "P1", standard = TRUE)
#' placesvg(object3)
#'
#' par(mfrow = opar)
#'
#' @export

changesvg <- function(object, forget = NULL, front = NULL, back = NULL,
                      standard = FALSE, keep.ratio = F, round = FALSE,
                      xdigits = 4, ydigits = 4,
                      xinverse = FALSE, yinverse = FALSE)
{
  if(!(standard == TRUE | standard == FALSE)){
    stop("The 'standard' parameter should be TRUE or FALSE")
  }

  if(!(keep.ratio == TRUE | keep.ratio == FALSE)){
    stop("The 'keep.ratio' parameter should be TRUE or FALSE")
  }

  if(!(round == TRUE | round == FALSE)){
    stop("The 'round' parameter should be TRUE or FALSE")
  }

  if(!(xinverse == TRUE | xinverse == FALSE)){
    stop("The 'xinverse' parameter should be TRUE or FALSE")
  }

  if(!(yinverse == TRUE | yinverse == FALSE)){
    stop("The 'yinverse' parameter should be TRUE or FALSE")
  }

  if(!is.null(forget) | !is.null(front) | !is.null(back)){

    ids <- unique(object$id)

    if(class(ids) != "character") stop("The object ids should be characters")

    ns  <- length(ids)

    mfi <- match(front, ids)
    mla <- match(back, ids)
    mfo <- match(forget, ids)

    nafi <- is.na(mfi)
    nala <- is.na(mla)
    nafo <- is.na(mfo)

    mfi[nafi]  <- suppressWarnings(as.numeric(front))[nafi]
    mla[nala]  <- suppressWarnings(as.numeric(back))[nala]
    mfo[nafo]  <- suppressWarnings(as.numeric(forget))[nafo]

    test <- c(mfi, mla, mfo)

    if(any(is.na(test)) | !(ns >= max(test))) {
      stop("The 'front', 'back' and forget' parameters shoud refer to existing
           object elements, either by index or name in the object id column.")
    }

    if(any(duplicated(test))) {
      stop("The 'front', 'back' and forget' parameters should refer to different
           elements each")
    }

    newOrder <- c(mla, seq_len(ns)[-test], rev(mfi))

    object$gloVar.neworder <- match(match(object$id, ids), newOrder)
    object$gloVar.ni       <- seq_len(nrow(object))

    object <- object[!is.na(object$gloVar.neworder),]
    object <- arrange(object, gloVar.neworder, gloVar.ni)

  }

  if(standard) {

    xmax <- max(object$x)
    xmin <- min(object$x)

    ymax <- max(object$y)
    ymin <- min(object$y)

    if(!keep.ratio){

      object$x <- 2*(object$x - (xmax + xmin)/2)/(xmax-xmin)
      object$y <- 2*(object$y - (ymax + ymin)/2)/(ymax-ymin)

    } else {

      xymax <- max(c(xmax- xmin, ymax - ymin))

      object$x <- 2*(object$x - (xmax + xmin)/2)/(xymax)
      object$y <- 2*(object$y - (ymax + ymin)/2)/(xymax)

    }

  }

  if(round)
  {
    object$x <- round(object$x,digits = xdigits)
    object$y <- round(object$y,digits = ydigits)
  }

  if(xinverse) object$x <- -object$x

  if(yinverse) object$y <- -object$y

  return(object)

}
