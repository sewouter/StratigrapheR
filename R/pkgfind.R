#' @title Find a specific pattern in the code of functions in a package
#'
#' @description This function names all the functions in a package that contain
#' a specific character pattern, typically the name of a function.
#'
#' @param pkg a character string of the package to search in
#' @param pattern the pattern to search in the codes of the functions in the
#' package
#'
#' @return a vector of the names of the functions in which the
#' pattern is identified
#'
#'
#' @examples
#' pkgfind("StratigrapheR", "every_nth")
#'
#' @export

pkgfind <- function(pkg, pattern){

  library(pkg, character.only = TRUE)

  liszt <- ls(paste0("package:", pkg))

  l2 <- lapply(liszt, function(x) deparse1(get(x)))

  id <- sapply(l2, function(x) grepl(pattern, x))

  return(liszt[id])

}
