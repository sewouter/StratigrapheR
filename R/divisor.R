#' @title Greatest Common Rational Divisor
#'
#' @description Compute the Greatest Common Rational Divisor or test whether
#' a value is a common rational divisor of a suite of number
#'
#' @param x a numeric or integer vector
#' @param y a numeric or integer vector of vales to be tested as divisors of x
#' @param tolerance the order of tolerance for errors, i.e. the number of
#' decimals considered as being significative
#' @param relative whether to apply the tolerance to the x values divided by
#' the smallest x value (TRUE, is the default), or to the x values themselves
#' @param tries the amount of iterations: each iteration tests 10^n+1 more
#' possibilities than the previous one. This is to optimise computation while
#' allowing all possibilities to be explored. Each try takes exponentialy more
#' time than the previous one
#' @param speak whether to print a sentence at each try
#' @param use.names whether to use y values as names for the output
#'
#' @examples
#' divisor(x = c(0.03,0.75,0.3,2,100, 0.03, 100, 0), speak = FALSE)
#'
#' divisor(x = c(0.02,0.75,0.3,2,100.000002, 0.03, 100, 0), speak = FALSE)
#'
#' divisor(x = c(0.02,0.75,0.3,2,100.000002, 0.03, 100, 0) * 10^-10,
#'         speak = FALSE)
#'
#'
#' a <- c(0.02,0.75,0.3,2,100.000000002, 0.03, 100, 0)
#'
#' divisor(x = a)
#'
#' is.divisor(x = a, y = c(1, 0.01, 2*10^-9))
#'
#' divisor(x = a, tolerance = 7, speak = FALSE)
#'
#' divisor(x = a, relative = FALSE, speak = FALSE)
#'
#' @export

divisor <- function(x, tolerance = 8, relative = T, tries = 4, speak = T)
{

  if(!(isFALSE(relative) | isTRUE(relative))) {
    stop("The 'relative' parameter should be TRUE or FALSE'")
  }

  if(!(isFALSE(speak) | isTRUE(speak))) {
    stop("The 'speak' parameter should be TRUE or FALSE'")
  }

  x <- unique(abs(x))

  x <- x - x[which.min(abs(x))]

  x <- x[x != 0]

  # Divide by smallest

  mx <- x[which.min(abs(x))]
  d  <- x/mx

  if(!relative){

    # Test if tolerance is of lower order than the smallest

    if(-log10(mx) > tolerance) {
      stop(paste("If 'relative' is FALSE, the smallest value (zero excepted)",
                 "should of higher order than the order",
                 "defined by the 'tolerance' parameter"))
    }

  }

  # Test the dispersion of values

  general_tolerance <- 15 # Order of digits affected by floating-point

  if(log10(max(x)) >= (general_tolerance - tolerance)){
    stop(paste("The range of 'x' values is too large to find a meaningful",
               "greatest common rational divisor.",
               "To solve this problem you can change the values in x or",
               " lower the 'tolerance' parameter (i.e. the",
               "tolerance for floating-point aritmetics):in the later case be",
               "critical of the result."))
  }

  # Test and remove values that are multiples of the smallest value

  remain1 <- (d - floor(d + 10^-(tolerance - 1)))
  if(!relative) remain1 <- remain1 * mx

  rzero1 <-  abs(remain1) < 10^-tolerance

  d <- d[!rzero1]

  if(length(d) == 0) {

    if(!relative){

      res <- round(mx, tolerance)

    } else {

      res <- signif(mx, tolerance)

    }

  } else {

    # Multiply d [x/min(x)] by integers, and test if
    # this returns only integers within tolerance

    ld <- length(d)

    try_order_OLD <- 0
    try_order_i   <- 6 - ceiling(log10(ld))

    for(i in seq_len(as.integer(tries))){

      if(speak) {
        print(paste("Try ",i,": 10^",try_order_i,
                    " possibilities tested", sep = ""))
      }

      t <- 1:(10^try_order_i)
      t <- t[-(1:(10^try_order_OLD))]

      lt <- length(t)

      tmat <- matrix(rep(t,ld), ncol = ld)
      dmat <- matrix(rep(d, lt), ncol = ld, byrow = T)

      test <- dmat * tmat

      remain2 <- (test - floor(test + 10^-(tolerance - 1)))
      remain2 <- remain2/tmat
      if(!relative) remain2 <- remain2 * mx

      rzero2 <-  abs(remain2) < 10^-tolerance

      rzero2 <- matrix(as.integer(rzero2), ncol = ld)

      test[which(rowSums(rzero2) == ld),]

      res <- mx/t[which(rowSums(rzero2) == ld)[1]]

      if(!is.na(res)) break

      try_order_OLD <- try_order_i
      try_order_i   <- try_order_i + 1

    }

  }

  return(res)

}

#' @rdname divisor
#' @export

is.divisor <- function(x, y, tolerance = 8, relative = T, use.names = T)
{

  if(!(isFALSE(relative) | isTRUE(relative))) {
    stop("The 'relative' parameter should be TRUE or FALSE'")
  }

  x <- unique(abs(x))
  x <- x[x != 0]

  mx <- min(x)

  y <- abs(y)

  if(!relative){

    # Test if tolerance is of lower order than the smallest

    if(-log10(mx) > tolerance) {
      stop(paste("If 'relative' is FALSE, the smallest value (zero excepted)",
                 "should of higher order than the order",
                 "defined by the 'tolerance' parameter"))
    }

  }

  # Test the dispersion of values

  general_tolerance <- 15 # Order of digits affected by floating-point

  if(log10(max(x)) >= (general_tolerance - tolerance)){
    stop(paste("The range of 'x' values is too large to find a meaningful",
               "greatest common rational divisor.",
               "To solve this problem you can change the values in x or",
               " lower the 'tolerance' parameter (i.e. the",
               "tolerance for floating-point aritmetics):in the later case be",
               "critical of the result."))
  }


  # Test

  ly <- length(y)
  lx <- length(x)

  ymat <- matrix(rep(mx/y,lx), ncol = lx)
  xmat <- matrix(rep(x/mx, ly), ncol = lx, byrow = T)

  test <- xmat*ymat

  remain2 <- (test - floor(test + 10^-(tolerance - 1)))
  remain2 <- remain2 / ymat

  if(!relative) remain2 <- remain2 * mx

  rzero2 <-  abs(remain2) < 10^-tolerance

  rzero2 <- matrix(as.integer(rzero2), ncol = lx)

  res <- rowSums(rzero2) == lx

  if(isTRUE(use.names)) names(res) <- y

  return(res)

}








