#' @title Homogenise a list
#'
#' @description Takes each element of a list and repeats each one so they
#' have the same length. This function is designed to be
#' integrated in another function and clean its arguments.
#' \strong{IF YOU RECEIVED A WARNING FROM THIS FUNCTION IN ANOTHER FUNCTION:}
#' Check that the length of the arguments indicated by the warning are correct.
#'
#' @param i reference object of length n
#' @param n length to reach (is overriden by i)
#' @param l list for each element to be repeated to have a length n. These
#' elements have to be integers, numerics or characters.
#' @param cycle whether to recycle the elements or to only allow elements of
#' length 1 or n
#'
#' @return A list identical to the one initially provided, with elements length
#' homogenized to i
#'
#' @seealso \code{\link{merge_list}}
#'
#' @examples
#' i     <- rep(1:4, 2)
#'
#' l <- list(a = c(1,2,3),
#'           b = "R",
#'           d = 1:100,
#'           e = c("a", "b"),
#'           f = FALSE
#'           )
#'
#' homogenise(i = i, l = l)
#'
#' homogenise(n = 10, l = l)
#'
#' @export

homogenise <- function(i = NULL, n = NULL, l = list(), cycle = TRUE)
{

  if(!(isTRUE(cycle) | isFALSE(cycle))) {
    stop("The parameter 'cycle' should be TRUE or FALSE")
  }

  larg <- unlist(lapply(l,length))

  if(is.null(i) & is.null(n)) {
    marg <- max(larg)
  } else if(!is.null(i)){
    marg <- length(i)
  } else if(length(n) == 1 & (is.numeric(n) | is.integer(n))){
    marg <- n
  } else {
    stop("'n' should be a numeric or integer of length 1")
  }


  if(!cycle & any(!(larg == 1 | larg == marg))){

    prob_id <- names(l)[!(larg == 1 | larg == marg)]

    stop(paste0("The following element(s)/argument(s) ",
               "should be of length 1 or n (", marg, "):\n   - ",
                paste(prob_id, collapse = "\n   - "),
               "\n[see ?homogenise in the StratigrapheR",
               " package for more info]"),
         call. = F)
  }

  carg <- sapply(l, inherits,
                 c("integer", "numeric", "character", "logical", "factor"))

  if(any(!carg)){
    stop("The arguments should be of class",
         " 'integer', 'numeric', 'character', 'logical' or 'factor'",
         call. = F)
  }

  res  <- lapply(l,rep_len,marg)

  return(res)

}

#' @rdname homogenise
#' @export

homogenize <- function(i = NULL, n = NULL, l = list(), cycle = TRUE)
{
  homogenise(i = i, n = n, l = l, cycle = cycle)
}


