#' @title Create / Check / Manipulate lim objects
#'
#' @description Functions to create and check limits of intervals (what
#' we define here as a 'lim' object), with control of specified properties.
#' Basically we define an interval by its left and right boundaries, by an id
#' and by a rule of boundary inclusion.
#'
#' @param lim a list of n left (1st element) and n right (2ndt element)
#' interval limits, of n interval IDs, and of n interval boundary rules (e.g.
#' "[]")
#' @param l the left interval limits (numerical vector of length n)
#' @param r the right interval limits (numerical vector of length n)
#' @param id the interval IDs (numerical or character vector of length n,
#' the default is 1 for each interval). They can be similar for different
#' intervals.
#' @param b the interval boundaries rules: "[]"
#' (or "closed") to include both boundaries points, "][" (or "()" and "open") to
#' exclude both boundary points, "[[" (or "[)","right-open" and"left-closed") to
#' include only the left boundary point, and "]]" (or "(]", "left-open",
#' "right-closed") to include only the right boundary point. The notation is
#' simplified to "[]", "[[", "]]" and "][" only.
#' @param decreasingly whether the order to check for or set for is
#' decreasing
#' @param dependently whether the intervals themselves should be ordered
#' relatively to the other
#' @param check.lim whether to check if the object is a lim object
#'
#' @details \code{as.lim}: creates a lim object
#'
#' \code{is.lim}: checks if arguments qualify as a lim object
#'
#' \code{are.lim.nonunique}: checks if there are no intervals of identical l and
#' r
#'
#' \code{are.lim.nonadjacent}: checks if there are no pairs of intervals having
#' at least one similar boundary
#'
#' \code{are.lim.distinct}: checks if the intervals are not overlapping
#'
#' \code{are.lim.ordered}: checks if the intervals are ordered (in l and r, and
#' if dependently is TRUE, relative to the other intervals of same id)
#'
#' \code{order.lim}: orders l and r parts of the intervals (use \code{simp.lim}
#' for more advanced ordering)
#'
#' @seealso To find which values are in which interval: \code{\link{in.lim}}
#'
#' To simplify intervals by merging overlapping parts: \code{\link{simp.lim}}
#'
#' To extract the part outside of intervals: \code{\link{flip.lim}}
#'
#' To make intervals with boundaries in between given values:
#' \code{\link{mid.lim}}
#'
#' To discretise intervals: \code{\link{tie.lim}}
#'
#' To simplify boundary rules into "[]", "[[", "]]" and "][":
#'  \code{\link{rebound}}
#'
#' To plot interval data as rectangles: \code{\link{infobar}}
#'
#' @examples
#' example <- as.lim(l = c(0,1,2), r = c(0.5,2.1,2.5), id = "I")
#'
#' is.lim(lim = example)
#'
#' are.lim.nonunique(l = c(0,1,2),r = c(0.5,1.5,2.5))
#'
#' are.lim.nonunique(l = c(0,1,2),r = c(0.5,1.5,2))
#'
#' are.lim.nonadjacent(l = c(0,1,2),r = c(0.5,1.5,2.5))
#'
#' are.lim.nonadjacent(l = c(0,1,1.5),r = c(0.5,1.5,2))
#'
#' are.lim.ordered(l = c(0,1,2),r = c(0.5,1.5,2.5))
#'
#' are.lim.ordered(l = c(0,1,2.5),r = c(0.5,1.5,2))
#'
#' are.lim.ordered(l = c(0,1,2),r = c(0.5,1.5,2.5),dependently = TRUE)
#'
#' are.lim.ordered(l = c(0,2,1),r = c(0.5,2.5,1.5),dependently = TRUE)
#'
#' are.lim.distinct(l = c(0,1,2),r = c(0.5,1.5,2.5))
#'
#' are.lim.distinct(l = c(0,1,2),r = c(0.5,3.5,2.5))
#'
#' order.lim(l = c(0,6,4,6,50), r = c(1,5,6,9,8),
#'           b = c("[[", "]]", "[[", "]]", "[["))
#'
#' @export

as.lim <- function(lim = NULL, l = NULL, r = NULL, id = 1L, b = "[]")
{
  if((length(lim) >= 4 & length(l) == 0 & length(r) == 0) |
     (length(lim) == 0 & length(l) != 0 & length(r) != 0)){

    if(length(lim) >= 4 & isTRUE(all(names(lim)[1:4] == c("l","r","id","b")))){

      l  <- lim[[1]]
      r  <- lim[[2]]
      id <- lim[[3]]
      b  <- lim[[4]]

    } else if(length(lim) >= 4) {

      stop(paste("The 4 first elements of the lim object should respectively",
                 " be 'l', 'r', 'id' and 'b'.", sep = ""))

    }

    if(inherits(l, "matrix") & inherits(r, "matrix")){

      test.l   <- as.vector(l)
      test.r   <- as.vector(r)
      isMATRIX <- T

    } else {

      test.l   <- l
      test.r   <- r
      isMATRIX <- F

    }

    if(!(inherits(test.l, "numeric") | inherits(test.l, "integer") |
         inherits(test.r, "numeric") | inherits(test.r, "integer"))){
      stop(paste("The inputs (elements of lim or l and r) ",
                 "should be integers or numericals", sep = ""))
    }

    if(length(l) != length(r)){
      stop(paste("The inputs (elements of lim or l and r) ",
                 "should be of same length", sep = ""))
    }

    if(length(id) == 1) id <- rep(id, length(l))
    if(length(b) == 1)  b  <- rep(b, length(l))

    if(isMATRIX){

      lcol <- ncol(l)

      if(lcol != ncol(r)) {
        stop("If 'l' and 'r' are matrices, they should be of same format")
      }

      id <- matrix(id, ncol = lcol)
      b  <- matrix(b, ncol = lcol)
    }

    if(length(id) != length(l)){
      stop("The parameter 'id' should be of length 1 or n")
    }

    if(length(b) != length(l)){
      stop("The parameter 'b' should be of length 1 or n")
    }

    res <- list(l = l, r = r, id = id, b = rebound(b))

    return(res)

  } else if(length(lim) != 0 & length(l) != 0 |
            length(lim) != 0 & length(r) != 0){

    stop(paste("If the parameter 'lim' is provided 'l' and 'r' should not be, ",
               "and if 'l' and 'r' are then 'lim' should not be", sep = ""))

  } else if(length(lim) < 4 & length(l) == 0 & length(l) == 0){

    stop("The 'lim' parameter should be made of 4 or more elements")

  } else {

    stop("The parameters 'l' and 'r', or 'lim' are not provided")

  }
}

#' @rdname as.lim
#' @export

is.lim <- function(lim = NULL, l = NULL, r = NULL, id = 1L, b = "[]")
{
  if((length(lim) != 0 & length(l) == 0 & length(r) == 0) |
     (length(lim) == 0 & length(l) != 0 & length(r) != 0)){

    if(length(lim) != 0){

      l  <- lim[[1]]
      r  <- lim[[2]]
      id <- lim[[3]]
      b  <- lim[[4]]

      if(length(id) != length(l)) return(FALSE)
      if(length(b) != length(l)) return(FALSE)

    } else {

      b <- rebound(b, na.errors = T)

    }

    if(inherits(l, "matrix") & inherits(r, "matrix")){

      test.l   <- as.vector(l)
      test.r   <- as.vector(r)

    } else {

      test.l   <- l
      test.r   <- r

    }

    if(!(inherits(test.l, "numeric") | inherits(test.l, "integer") |
         inherits(test.r, "numeric") | inherits(test.r, "integer"))){
      return(FALSE)
    }

    if(length(l) != length(r)) return(FALSE)

    if(!(length(id) == length(l) | length(id) == 1)) return(FALSE)

    if(!(length(b) == length(l) | length(b) == 1)) return(FALSE)

    if(any(is.na(b))) return(FALSE)

    return(TRUE)

  } else if(length(lim) != 0 & length(l) != 0 |
            length(lim) != 0 & length(r) != 0){
    stop("If the parameter 'lim' is provided 'l' and 'r' should not be,
         and if 'l' and 'r' are then 'lim' should not be")
  } else {
    stop("The parameters 'l' and 'r', or 'lim' are not provided")
  }

}

#' @rdname as.lim
#' @export

are.lim.nonunique <- function(lim = NULL, l = NULL, r = NULL,
                              check.lim = TRUE)
{
  if(check.lim){
    if(!is.lim(lim = lim, l = l, r = r)) {
      stop("The objects provided are not lim")
    }
  }

  if(length(lim) != 0){

    l <- lim[[1]]
    r <- lim[[2]]
  }

  if(any(l == r)) return(FALSE) else return(TRUE)
}

#' @rdname as.lim
#' @export

are.lim.nonadjacent <- function(lim = NULL, l = NULL, r = NULL, b = "[]",
                                check.lim = TRUE)
{
  if(check.lim){
    if(!is.lim(lim = lim, l = l, r = r, b = b)) {
      stop("The objects provided are not lim")
    }
  }

  if(length(lim) != 0){

    l <- lim[[1]]
    r <- lim[[2]]
    b <- lim[[4]]

  } else {

    b <- rebound(b)

    if(length(b) == 1) b <- rep(b, length(l))

  }

  keepl <- b == "[]" | b == "[["
  keepr <- (b == "[]" | b == "]]") & l != r

  t <- c(l[keepl],r[keepr])

  if(length(t) != length(unique(t))) return(FALSE) else return(TRUE)

}

#' @rdname as.lim
#' @export

are.lim.distinct <- function(lim = NULL, l = NULL, r = NULL,
                             check.lim = TRUE)
{
  if(check.lim){
    if(!is.lim(lim = lim, l = l, r = r)) {
      stop("The objects provided are not lim")
    }
  }

  lim1 <- order.lim(lim = lim, l = l, r = r)

  l <- lim1[[1]]
  r <- lim1[[2]]

  do <- cbind(l,r)[order(l),,drop = F]

  o  <- c(rbind(do[,1],do[,2]))

  if(is.unsorted(o)) return(FALSE) else return(TRUE)

}

#' @rdname as.lim
#' @export

are.lim.ordered <- function(lim = NULL, l = NULL, r = NULL, id = 1L,
                            decreasingly = FALSE, dependently = FALSE,
                            check.lim = TRUE)
{
  if(decreasingly != TRUE &
     decreasingly != FALSE){
    stop("The parameter 'decreasingly' should be T or F")
  }

  if(dependently != TRUE &
     dependently != FALSE) {
    stop("The parameter 'dependently' should be T or F")
  }

  if(check.lim){
    if(!is.lim(lim = lim, l = l, r = r, id = id)) {
      stop("The objects provided are not lim")
    }
  }

  if(length(lim) == 0){
    lim <- as.lim(l = l, r = r, id = id)
  }

  l  <- lim[[1]]
  r  <- lim[[2]]
  id <- lim[[3]]

  if(!dependently){

    if(!decreasingly){

      di <- r - l

      if(any(di < 0, na.rm = T)) return(FALSE)

    } else if(decreasingly){

      di <- l-r

      if(any(di < 0, na.rm = T)) return(FALSE)

    }

  } else {

    o   <- c(rbind(as.vector(l),as.vector(r)))
    idv <- as.vector(id)
    ido <- c(rbind(idv,idv))
    os  <- split(o,ido)


    if(!decreasingly){

      test <- lapply(os, function(v) !is.unsorted(v, na.rm = T))

      if(!all(unlist(test))) return(FALSE)

    } else if(decreasingly){

      test <- lapply(os, function(v) !is.unsorted(-v, na.rm = T))

      if(!all(unlist(test))) return(FALSE)

    }

  }

  return(TRUE)

}

#' @rdname as.lim
#' @export

order.lim <- function(lim = NULL, l = NULL, r = NULL, id = 1L, b = "[]",
                      decreasingly = FALSE)
{

  dl <- as.lim(lim = lim, l = l, r = r, id = id, b = b)

  if(!decreasingly) cond <- (dl$r - dl$l) < 0 else cond <- (dl$r - dl$l) > 0

  res <- dl

  res$l[cond] <- dl$r[cond]
  res$r[cond] <- dl$l[cond]

  left_open  <- cond & res$b == "]]"
  right_open <- cond & res$b == "[["

  res$b[left_open]  <- "[["
  res$b[right_open] <- "]]"

  return(res)

}
