#' @title Find indexes for n identical elements
#'
#' @description For a given vector, this function gives the indexes of identical
#' sets for a given number of repetitions
#'
#' @param x a vector, normally with repeated values
#' @param n the amount of repetitions that needs to be identified
#' @param first whether to take the first repetitions (T; is the default), or
#' the last ones (F)
#' @param warn whether to warn if NA values are generated due to the lack of
#' right amount of repetitions
#' @examples
#' ids <- c(rep("A", 4), rep("B", 6), rep("C", 2))
#'
#' val <- paste(ids, c(1:4, 1:6, 1:2), sep = "")
#'
#' nset(ids, 3, warn = FALSE)
#'
#' matrix(val[nset(ids, 3, warn = FALSE)], ncol = 3)
#'
#' matrix(val[nset(ids, 3, first = FALSE, warn = FALSE)], ncol = 3)
#'
#' @export

nset <- function(x, n, first = T, warn = T){

  if(isTRUE(first)){
    nx <- x
    sx <- seq_len(length(x))
  } else {
    nx <- rev(x)
    sx <- rev(seq_len(length(x)))
  }

  lr <- split(sx, nx)

  lengths <- sapply(lr, length)

  if(any(lengths < n & isTRUE(warn))) warning("NA values generated")

  lf <- lapply(lr, function(x) x[seq_len(n)])

  res <- do.call(rbind, lf)

  if(!isTRUE(first)) res <- res[,rev(seq_len(n))]

  res <- res[order(order(unique(x))),]

  return(res)

}
