#' @title Ignores useless objects
#'
#' @description Ignores useless objects: this function will discard the polygons
#' or polylines outside a certain range. This allows to avoid unnecessary work
#' for multigons(), multilines(), centresvg() and framesvg().
#'
#' @param i a polygon id for each x and y coordinate. If n objects are
#' provided there should be n unique ids describing them, and the
#' graphical parameters should be of length 1 or n.
#' @param x,y numeric vectors of coordinates.
#' @param d a list of named vectors going with i, x and y
#' @param j a list of the ids in the order used for the \code{arg}
#' arguments. By default they are in their order of appearance in \code{i}
#' @param arg a list of arguments f length 1 or n.
#' @param xlim,ylim the limits in x and y; if any object has all his
#' points past one of these limits, it will be removed.
#' @param xlog,ylog whether the axes have logarithmic scale
#'
#' @return
#' a list of i, x, y, d, j and arguments.
#'
#' @seealso Tributary functions: \code{\link{multigons}},
#' \code{\link{multilines}}, \code{\link{centresvg}} and \code{\link{framesvg}}
#'
#'
#' @examples
#' i <- c(rep("A1",6), rep("A2",6), rep("A3",6))
#' x <- c(1,2,3,3,2,1,4,5,6,6,5,4,7,8,9,9,8,7)
#' y <- c(1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6)
#'
#' xlim <- c(2,5)
#' ylim <- c(0,1.5)
#'
#' plot(c(0,10),c(0,10),type = "n")
#' rect(xlim[1], ylim[1], xlim[2], ylim[2])
#'
#' multilines(i, x, y, lty = 3, col = "grey80")
#'
#' res <- ignore(i, x, y, arg = list(lty =  1, lwd = 3,
#'               col = c("orange", "green", "red")),
#'               xlim = xlim, ylim = ylim)
#'
#' do.call(multilines, res)
#'
#' @export

ignore <- function(i, x, y = NA, d = list(), j = unique(i), arg = list(),
                   xlim = par("usr")[c(1,2)], ylim = par("usr")[c(3,4)],
                   xlog = par("xlog"), ylog = par("ylog"))
{

  i <- as.character(i)

  if(xlog) xlim <- 10^xlim

  if(ylog) ylim <- 10^ylim

  # Arguments ----

  error <- duplicated(c("i", "x", "y", names(d), names(arg)))

  if(any(error)) {
    stop("The names of the parameters in the lists 'd' and 'arg' should be ",
         "different from each other and from 'i', x' and 'y'.", sep = "")
  }

  lj <- length(j)

  larg <- unlist(lapply(arg,length))

  if(any(!(larg == 1 | larg == lj))){
    stop("The arguments beside 'i', 'x' and 'y' should be of length 1 or of same
         length than j")
  }

  am <- data.frame(arg[which(larg == lj)], stringsAsFactors = F)

  # X ----

  dx <- data.frame(i, x, stringsAsFactors = F)

  dx$xp <- !(dx$x > max(xlim))
  dx$xm <- !(dx$x < min(xlim))

  xp <- unique(dx$i[dx$xp])
  xm <- unique(dx$i[dx$xm])

  xt <- c(xp,xm)

  keepx <- xt[duplicated(xt)]

  # Y ----

  if(!(is.na(y[[1]] & length(y) == 1)))
  {

    dy <- data.frame(i, y, stringsAsFactors = F)

    dy$yp <- !(dy$y > max(ylim))
    dy$ym <- !(dy$y < min(ylim))

    yp <- unique(dy$i[dy$yp])
    ym <- unique(dy$i[dy$ym])

    yt <- c(yp,ym)

    keepy <- yt[duplicated(yt)]

    xyt <- c(keepx,keepy)

    keep  <- xyt[duplicated(xyt)]

  } else {

    keep <- keepx

  }

  keep <- intersect(j, keep)
  pos  <- i %in% keep

  res <- merge_list(list(i = i[pos], x = x[pos], y = y[pos]),
                    lapply(d,"[",pos),
                    list(j = j[j %in% keep]), am[j %in% keep,,drop = F],
                    arg)

  clean <- match(names(res),c("i", "x", "y", names(d), "j", names(arg)))

  res <- res[clean]
}
