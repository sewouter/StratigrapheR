#' @title Inverts the intervals
#' @description Gives a negative of the intervals of a lim object
#' @param lim an object convertible into a lim object: either a vector
#' of length 2 or a list of n left (1st element) and n right (2ndt element)
#' interval limits
#' @param l a vector of n left interval limits
#' @param r a vector of n right interval limits
#' @param b a character vector for the interval boundaries rules: "[]"
#' (or "closed") to include both boundaries points, "][" (or "()" and "open") to
#' exclude both boundary points, "[[" (or "[)","right-open" and"left-closed") to
#' include only the left boundary point, and "]]" (or "(]", "left-open",
#' "right-closed") to include only the right boundary point. The notation is
#' simplified to "[]", "[[", "]]" and "][" only.
#' @param xlim the minimum and maximum of the new lim object (minimum and
#'maximum of the old one if NA; is the default)
#' @return a lim object of intervals in between the provided intervals
#' @seealso \code{\link{as.lim}}
#' @examples
#' l   <- c(1,3,5,7,9,10)
#' r   <- c(3,4,7,8,9,11)
#' b   <- "]["
#'
#' xlim <- c(-1,15)
#'
#' res <- flip.lim(l = l, r = r, b = b, xlim = xlim)
#'
#' plot(1,1,type = "n", xlim = c(-4, 20), ylim = c(0.3, 1.8))
#' rect(l, 1.1, r, 1.4, col = "green", border = "darkgreen", lwd = 3)
#' rect(res$l, 1, res$r, 0.7, col = "red", border = "darkred", lwd = 3)
#' abline(v = xlim)
#'
#' @export

flip.lim <- function(lim = NULL, l = NULL, r = NULL, b = "[]", xlim = NA)
{
  b <- rebound(b)

  lim.function <- as.lim(lim = lim, l = l, r = r, b = b)

  dis <- are.lim.distinct(lim = lim.function)
  adj <- are.lim.nonadjacent(lim = lim.function)
  ord <- are.lim.ordered(lim = lim.function, dependently = T)

  if(!(dis & adj & ord)) lim.function <- simp.lim(lim.function)

  nl   <- lim.function$r
  nr   <- lim.function$l
  invb <- lim.function$b

  bulk <- c(nl, nr)

  if(length(xlim) == 1 & is.na(xlim[[1]])) {

    min <- min(bulk)
    max <- max(bulk)

  } else if(length(xlim) == 2 &
            (class(xlim) == "numeric" | class(xlim) == "integer")){

    min <- min(xlim)
    max <- max(xlim)

    if(min == max) stop("xlim should be made of two different numerics")

    if(xlim[1] > xlim[2] & b == "[[") b <- "]]"
    if(xlim[1] > xlim[2] & b == "]]") b <- "[["

  }

  if(b == "[]"){
    minb <- T
    maxb <- T
  } else if(b == "[["){
    minb <- T
    maxb <- F
  } else if(b == "[["){
    minb <- F
    maxb <- T
  } else if(b == "]["){
    minb <- F
    maxb <- F
  }

  c1 <- !(nl < min | nr > max)

  nl   <- nl[c1]
  nr   <- nr[c1]
  invb <- invb[c1]

  d <- data.frame(l = c(min, nl), r = c(nr, max),
                  lb = c(minb, !(invb == "[]" | invb == "]]")),
                  rb = c(!(invb  == "[]" | invb == "[["), maxb))

  d <- left_join(d, data.frame(lb = c(T,T,F,F),
                               rb = c(T,F,T,F),
                               b = c("[]", "[[", "]]","]["),
                               stringsAsFactors = F),
                 by = c("lb", "rb"))

  res <- as.lim(l = d$l, r = d$r, b = d$b)

  return(res)
}
