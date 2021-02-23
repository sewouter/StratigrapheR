#' Expands the TRUE values of a T/F vector to their nth neighbours
#'
#' @param x a TRUE/FALSE vector (e.g. c(T,T,F,F,T,T))
#' @param n the proximity order of the FALSE values neighbouring the
#' TRUE values to be converted into TRUE (can be negative, should be convertible into an
#' integer). For instance 1 means that the F values dirctly next to a T will be
#' converted into T. 2 will apply that to the neighbours neigbhours, etc...
#' @return a vector of T/F values, with  the TRUE values expanded to their nth
#' neighbours
#' @examples
#' # Creating a test dataset ----
#'
#' y <- c(rep(c(0,1,0,-1),8),rep(-1,3),-1.5,
#'            rep(-1,2),rep(c(0,1,0,-1),8))
#' x <- 1:length(y)
#'
#' df <- data.frame(x,y)
#'
#' xclip <- c(20,48.5)
#' yclip <- c(-0.5,1.5)
#'
#' conditions <- df$y > yclip[1] & df$y < yclip[2] &
#'               df$x > xclip[1] & df$x < xclip[2]
#'
#' normt <- df[conditions,]
#'
#' # Plotting supporting data ----
#'
#' plot(df$x, df$y, type = "l", lty = 2, ylim = c(-2,2))
#'
#' rect(xclip[1], yclip[1], xclip[2], yclip[2])
#'
#' # See how the function reacts ----
#'
#' embiggened <- enlarge(conditions,1)
#'
#' test  <- df[embiggened,]
#'
#' lines(test$x,test$y, lwd = 2, col = "blue")
#'
#' points(normt$x,normt$y, type = "o", pch = 19,
#'        lty = 2, lwd= 2, col = "red")
#'
#'
#' legend(10, -1.6,
#'        legend = c(paste("Points initally isolated: they were chosen",
#'                         "to be the ones inside the rectangle"),
#'                   paste("Extension of the points: the first neighbours",
#'                         "of the points were added")),
#'        col = c("red", "blue"), pch = 19, lty = c(2,1), lwd = 2)
#'
#' @export

enlarge <- function(x,n)
{
  if(!all(x == FALSE | x == TRUE)) {
    stop("All x elements should be TRUE or FALSE")
  }

  ni <- as.integer(n)

  if(!(class(n) == "integer" | ni == n)) {
    stop("n should be an integer or convertible into an integer")
  }

  if(n == 0 | all(x == TRUE) | all(x == FALSE)){
    return(x)
  } else if (n < 0){
    ni <- -ni
    x <- !x
  }

  lx <- length(x)
  xi <- which(x)

  xd1 <- c(NA,diff(xi))
  xd2 <- c(xd1[-1],NA)

  xc1 <- xd1 > 2*n + 1
  xc2 <- c(xc1[-1],NA)

  xc <- is.na(xc1) | is.na(xc2) | xc1 | xc2

  r <- xi[which(xc)]

  xs <- (xd1 != 1 | is.na(xd1)) & (xd2 != 1 | is.na(xd2))

  r <- sort(c(r,xi[which(xs)]))

  lr <- length(r)

  o1 <- r[seq(1, lr - 1, by = 2)] - ni
  o2 <- r[seq(2, lr, by = 2)] + ni

  o  <- unlist(Map(`:`, o1, o2))
  ro <- o[o >= 1 & o <= lx]

  if(n > 0) {
    res <- 1:lx %in% ro
  } else if(n < 0 ){
    res <- !(1:lx %in% ro)
  }

  return(res)
}
