#' @title Draws several lines
#'
#' @description Draws several polylines or group of points. This function
#' expands on the lines() and points functions from base R graphics. The
#' difference is that several lines and group of points can
#' be drawn in one line by providing an id: i. To each line and group of point
#' you can provide different graphical parameters (i.e. colour, type, etc).
#'
#' @param i a  line id for each x and y coordinate, i.e. the name of
#' each polyline. If you want to give each line a different aspect you should
#' provide a vector of n elements (if you have three lines "A1", "A2" and
#' "A3" with "A2" that should be blue you should provide the colours of all
#' three: e.g. \code{col = c("white", "blue", "white")})
#' @param x,y numeric vectors of x and y coordinates
#' @param j a list of the ids (names) in the order used for the
#' graphical parameters (e.g. colour, shading, etc...). By default they are in
#' their order of appearance in \code{i}
#' @param forget the lines that should not be drawn, by their id or
#' index (i.e. name or number of appearance).
#' @param front,back the lines to be put in front and back position,
#' by their id or index (i.e. name or number of appearance). By default the
#' order is the one defined by \code{j}, and if \code{j} is absent by the order
#' in \code{i}.
#' @param type character indicating the type of plotting. For this
#' function it is limited to "l" (lines, is the default), "p" (points) and "o"
#' (points overplotting lines).
#' @param col the color to draw the line. The default is black.
#' @param bg background (fill) color for the open plot symbols given by
#' pch = 21:25.
#' @param pch plotting 'character', i.e., symbol to use. See ?points for
#' further details
#' @param lty,lwd the line type and width, see ?par for details.
#' @param cex haracter (or symbol) expansion: a numerical vector. This
#' works as a multiple of par("cex")
#' @param lend,ljoin,lmitre additional graphical parameters, see ?par
#' for details.
#' @seealso \code{\link{multigons}}
#'
#' Complementary function: \code{\link{shift}}
#'
#' Uses \code{\link{ignore}} to avoid drawing unnecessary objects
#'
#' @examples
#' i <- c(rep("A1",6), rep("A2",6), rep("A3",6))
#' x <- c(1,2,3,3,2,1,4,5,6,6,5,4,7,8,9,9,8,7)
#' y <- c(1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6)
#'
#' plot(c(0,10),c(0,7),type = "n")
#'
#' multilines(i, x, y, j = c("A3", "A1", "A2"), lty =  c(1,2,3), lwd = 2,
#'            type = c("l", "o", "o"), pch = c(NA,21,24), cex = 2)
#'
#' @export

multilines <- function(i, x, y, j = unique(i),
                       forget = NULL, front = NULL, back = NULL,
                       type = "l", col = "black", bg = NA, pch = 19,
                       lty = par("lty"), lwd = par("lwd"), cex= par("cex"),
                       lend = 0, ljoin = 0, lmitre = 10)
{

  i <- as.character(i)

  j <- as.character(j)

  argi <- list(type = type, col = col, bg = bg, pch = pch, lty = lty, lwd = lwd,
               cex = cex, lend = lend, ljoin = ljoin, lmitre = lmitre)

  draw <- ignore(i = i, x = x, y = y, j = j, arg = argi)

  j  <- draw$j
  lj <- length(j)

  if(lj != 0){

    d <- data.frame(i = draw$i, x = draw$x, y = draw$y, stringsAsFactors = F)

    arg  <- draw[-c(1:4)]
    larg <- unlist(lapply(arg,length))

    am <- data.frame(arg[which(larg == lj)], stringsAsFactors = F)

    au <- data.frame(arg[which(larg == 1)], stringsAsFactors = F)
    au <- au[rep(1,lj),]

    if(lj == 1) {
      a1 <- am
    } else if(ncol(am) != 0 & ncol(au) != 0){
      a1 <- cbind(am,au)
    } else if (ncol(am) == 0 & ncol(au) != 0){
      a1 <- au
    } else if (ncol(am) != 0 & ncol(au) == 0){
      a1 <- am
    }

    # Order of drawing ----

    transformers <- c(front, back, forget)

    if(any(!is.na(suppressWarnings(as.numeric(transformers))))){

      num.front  <- suppressWarnings(as.numeric(front))
      num.back   <- suppressWarnings(as.numeric(back))
      num.forget <- suppressWarnings(as.numeric(forget))

      front[!is.na(num.front)]   <- j[num.front[!is.na(num.front)]]
      back[!is.na(num.back)]     <- j[num.back[!is.na(num.back)]]
      forget[!is.na(num.forget)] <- j[num.forget[!is.na(num.forget)]]

    }

    transformers <- c(front, back, forget)

    if(any(duplicated(transformers))) {
      stop(paste("There should be no shared elements between ",
                 "the 'front', 'back' and 'forget' parameters", sep = ""))
    }

    if(!any(transformers %in% unique(draw$i))){

      o <- seq_len(lj)

    } else {

      lose <- match(forget,unique(j), integer(0))
      fo   <- match(front, unique(j), integer(0))
      lo   <- match(back, unique(j), integer(0))

      out <- c(fo,lo,lose)

      if(length(out) != 0) ro <- seq_len(length(j))[-out]

      o <- c(lo,ro,rev(fo))

    }

    # Drawing ----

    gpl <- split(a1, seq(nrow(a1)))[o]

    divi <- split(d, d$i)

    divi <- divi[match(j[o], names(divi))]

    poly_fun <- function(coor, leg)
    {
      lines(x = coor$x, y = coor$y,
            type = leg$type, pch = leg$pch,
            cex = leg$cex, bg = leg$bg,
            col = leg$col,
            lty = leg$lty, lwd = leg$lwd,
            lend = leg$lend, ljoin = leg$ljoin,
            lmitre = leg$lmitre)
    }

    mapply(poly_fun, divi, gpl)

    return(invisible())

  }

}
