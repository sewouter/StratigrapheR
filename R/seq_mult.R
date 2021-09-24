#' Sequence ordered by multiple
#'
#' @param l the length of the sequence, or an object convertile into a vector
#' from which to determine the length from
#' @param mult the multiple to order by
#' @param inv whether to change mult into l/mult
#'
#' @examples
#' seq_mult(10, 2)
#'
#' seq_mult(15,3)
#'
#' seq_mult(24,8)
#'
#' seq_mult(seq(0.5,12,0.5),8)
#'
#' seq_mult(10,2)[seq_mult(10,5)]
#'
#' seq_mult(10,2)[seq_mult(10,2, inv = TRUE)]
#'
#' @importFrom stats complete.cases
#' @export

seq_mult <- function(l, mult, inv = FALSE)
{

  if(!(isTRUE(inv) | isFALSE(inv))) {
    stop("The 'bound' parameter should be T or F")
  }

  ll <- length(as.vector(l))

  if(ll != 1 | !(inherits(l, "integer") | inherits(l, "numeric"))) l <- ll

  ratio <- l/mult

  if(!complete.cases(ratio)) stop("Incorrect 'l' or 'mult' parameters")

  if(ratio != round(ratio)) {
    stop("'mult' (", mult, ") should be a multiple of 'l' or of the",
         " length of 'l' (", l, ")")
  }

  if(inv) mult <- l/mult

  res <- kronecker(1:mult, seq(0, l - mult, mult), "+")

  return(res)
}
