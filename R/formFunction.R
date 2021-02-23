#' @title Converts a formula into a function
#'
#' @description Converts a formula into a function
#'
#' @param formula the formula to be converted. Should be of the form y ~ f(x)
#' @examples
#' f <- formFunction(y ~ log10(x))
#'
#' f(x=1:10)
#' @importFrom utils tail
#' @export

formFunction <- function(formula) {
  cmd <- tail(as.character(formula),1)
  exp <- parse(text=cmd)
  function(...) eval(exp, list(...))
}
