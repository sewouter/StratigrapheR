#' @title Simplifies boundary indicators for lim objects
#'
#' @description Simplifies boundary indicators for lim objects: from the wide
#' range supported by R ("[]", "[)", "(]", "()", "[[", "]]", "][", "open",
#' "closed", "left-open", "right-open", "left-closed", "right-closed") to "[]",
#' "[[", "]]" and "][" only
#'
#' @param b a vector of boundary indicators
#' @param na.errors whether to replace all other values by NA (rather
#' than simply stopping the function)
#' @return a simplified vector of boundary indicators (\code{"[]"}, \code{"[["},
#' \code{"]]"} and \code{"]["} only)
#'
#' @seealso \code{\link{as.lim}}
#'
#' @examples
#' bounds <- c("[]", "[)", "(]", "()",
#'             "[[", "]]", "][",
#'             "open", "closed",
#'             "left-open", "right-open",
#'             "left-closed", "right-closed")
#'
#' rebound(bounds)
#'
#' @export

rebound <- function(b, na.errors = F)
{

  cond <- (isTRUE(na.errors) | isFALSE(na.errors))

  if(!cond) stop("The 'na.errors' parameter should be T or F")

  if(all(b == "[]" | b == "[[" | b == "]]" | b == "][" | is.na(b))) return(b)

  b[b == "closed"] <- "[]"
  b[b == "()" | b == "open"] <- "]["
  b[b == "[)" | b == "right-open" | b == "left-closed"] <- "[["
  b[b == "(]" | b == "left-open" | b == "right-closed"] <- "]]"

  if(na.errors){

    b[!(b == "[]" | b == "][" | b == "[[" | b == "]]")] <- NA

    return(b)

  } else {

    if(!all(b == "[]" | b == "][" | b == "[[" | b == "]]")) {

      stop("Incorrect 'bounds' argument")

    } else {

      return(b)

    }

  }



}
