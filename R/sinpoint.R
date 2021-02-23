#' Gives a table of equally sampled points following a sinusoidal function
#'
#' @param x the x value of the end of the interval
#' @param y the y offset (see next parameter)
#' @param delta the difference between the min- and maxima in y
#' @param x0 the x value of the beginning of the interval (0 as default)
#' @param pos an integer specifying the kind of vertical offset; should
#' the sinusoidal function be shifted so that y is the first value (pos = 1, is
#' the default), the last value (2),the minimum (3) or the maximum (4) of the
#' function
#' @param phase the phase of the function at x0 in multiples of pi (1.5
#' as default; begins at its lowest)
#' @param nwave number of complete sinuses waves (1 as default)
#' @param nint number of intervals for the sampling (50 as default)
#' @return a table of points following a sinusoidal function
#' @examples
#' res <- sinpoint(c(4,5), 5, 1, x0 = c(0,1), pos = 3)
#'
#' plot(res$x, res$y)
#'
#' multilines(res$i, res$x, res$y, col = c("black" ,"red"), type = "o")
#'
#' @export

sinpoint <- function(x, y, delta, x0=0, pos= 1, phase=1.5, nwave=1, nint=50)
{
  argi <- list(x = x, y = y, delta = delta, x0 = x0, pos = pos, phase = phase,
               nwave = nwave, nint = nint)

  larg <- unlist(lapply(argi,length))
  lj   <- max(larg)

  if(any(!(larg == 1 | larg == lj))){
    stop("The arguments should be of length 1 or n")
  }

  or1 <- which(larg == lj)
  or2 <- which(larg == 1)

  am <- data.frame(argi[or1], stringsAsFactors = F)

  au <- data.frame(argi[or2], stringsAsFactors = F)
  au <- au[rep(1,lj),]
  row.names(au) <- NULL

  if(lj == 1) {
    rarg <- am
  } else if(ncol(am) != 0 & ncol(au) != 0){

    rarg <- cbind(am,au)
    rarg[,c(or1,or2)] <- rarg
    colnames(rarg)  <- names(argi)

  } else if (ncol(am) == 0 & ncol(au) != 0){
    rarg <- au
  } else if (ncol(am) != 0 & ncol(au) == 0){
    rarg <- am
  }

  rarg$by <- (rarg$x - rarg$x0)/rarg$nint
  rarg$a  <- 2*pi*rarg$nwave/(rarg$x-rarg$x0)

  rarg$q  <- NA

  p1 <- rarg$pos == 1
  p2 <- rarg$pos == 2
  p3 <- rarg$pos == 3
  p4 <- rarg$pos == 4

  rarg$q[p1] <-  sin(rarg$phase[p1]*pi) / 2*rarg$delta[p1]
  rarg$q[p2] <-  sin(rarg$a[p2]*(rarg$x[p2] - rarg$x0[p2])  + rarg$phase[p2]*pi)
  rarg$q[p2] <-  rarg$q[p2] / 2*rarg$delta[p2]
  rarg$q[p3] <-  rarg$delta[p3]/2
  rarg$q[p4] <- -rarg$delta[p4]/2

  dat <- data.frame(i = rep(seq_len(nrow(rarg)), rarg$nint+1),
                    x = unlist(mapply(seq,rarg$x0, rarg$x, rarg$by,
                                      SIMPLIFY = F)))

  dat$y <- sin(rarg$a[dat$i]*(dat$x - rarg$x0[dat$i]) + rarg$phase[dat$i]*pi)
  dat$y <- (dat$y/ 2*rarg$delta[dat$i]) + rarg$y[dat$i] - rarg$q[dat$i]

  dat <- dat[,c(2,3,1)]

  return(dat)
}
