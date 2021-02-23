#' @title  Circular shift
#'
#' @description Circular shift
#'
#' @param x a vector (characters, numerics, integers,...), data.frame or list
#' @param n a positive integer of length 1, giving the number of
#' positions to shift by (positive values generate lag)
#' @param names whether the names of the elements or rows should also shift
#' @return the same object than the input, but with a shifted order
#' @examples
#' # Simple use
#'
#' shift(x = c(6,8,10,12,2,4), n = 2)
#'
#' # Applications to litholog generation
#'
#' l <- c(1,2,3)
#' r <- c(0,1,2)
#' h  <- c(4,3,4)
#' i   <- c("B1","B2","B3")
#'
#' basic.litholog <- litholog(l,r,h,i)
#'
#' whiteSet(xlim = c(0,4), ylim = c(0,3),
#'          xaxs = "r", yaxs = "r",   # This gives a little room to the graph
#'          ytick = 1, ny = 10)
#'
#' multigons(basic.litholog$i, basic.litholog$xy, basic.litholog$dt,
#'           forget = "B1", lwd = 2)
#'
#' openbed <- subset(basic.litholog, basic.litholog == "B1")
#'
#' openbed <- shift(openbed, -1)
#'
#' lines(openbed$xy, openbed$dt, lwd = 2)
#'
#' @importFrom dplyr lead lag
#' @export

shift <- function(x, n = 1L, names = T)
{

  if(!(isTRUE(names) | isFALSE(names))) {
    stop("The parameter 'names' should be TRUE or FALSE")
  }

  if(inherits(x, "data.frame")) {

    len.s <- nrow(x)
    namex <- rownames(x)

  } else {

    len.s <- length(x)
    namex <- names(x)

  }

  s <- seq_len(len.s)

  n.shift <- fmod(n,len.s)

  shifted   <- lag(s, n.shift)
  shift.inv <- lead(s, len.s - n.shift)

  shifted[is.na(shifted)] <- shift.inv[!is.na(shift.inv)]

  if(inherits(x, "data.frame")) {
    res <- x[shifted,,drop = FALSE]
  } else {
    res <- x[shifted]
  }

  if(!is.null(namex) & isTRUE(names)){

    if(inherits(x, "data.frame")) {
      rownames(res) <- namex[shifted]
    } else {
      names(res) <- namex[shifted]
    }

  } else {

    if(inherits(x, "data.frame")) {
      rownames(res) <- namex
    } else {
      names(res) <- namex
    }

  }

  return(res)

}
