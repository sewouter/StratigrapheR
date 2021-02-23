#' @title Irregular windowing
#'
#' @description Find the index of points in time-series that fall into a
#' specific window, even with irregular sampling rate. The iterations needed
#' in this function are equal to the maximum amount of points found in the
#' windows, therefore it should be reasonably efficient for short windows at
#' least.
#'
#' @param x the position values to be regrouped in windows
#' @param w the window length (top to bottom)
#' @param xout the center of each window, defaults to x
#' @param b the boundary rule at the top and bottom of the window: "][" means
#' that neither the top nor bottom are taken in, "[]" means that top and bottom
#' are taken in, "]]" and "[[" mean that only the top or the bottom are taken
#' in, respectively. Also accepts: "[)", "(]", "()", "open", "closed",
#' "left-open", "right-open", "left-closed" and "right-closed": see
#' \code{\link{rebound}} for more information
#' @param warn an integer of the amount of iterations after which a warning is
#' issued: this could mean that there are too many data points in a window, and
#' that the computation will become very inefficient. This is up to the user to
#' see. If you want to remove the warning, set this parameter to Inf
#' @param ... intensity values corresponding to each x position, making
#' time-series. They will be provided window by window in the output.
#'
#' @return a list made of the center values of the windows ($xout), a matrix of
#' the index of the original x values in each corresponding window ($x.index;
#' the rows correspond to each $xout value), a matrix of the x values in
#' each corresponding window ($x; the rows correspond to each $xout value),
#' the amount of points in each window ($n.size), and additional matrices of
#' additional intensity values provided in \code{...} (names correspond to the
#' names provided in \code{...}; the rows correspond to each $xout value)
#'
#' @examples
#' # Visual example ----
#'
#' set.seed(42)
#'
#' n <- 600
#' t <- seq_len(n)
#'
#' p1 <- 30
#' p2 <- 240
#'
#' xy <- (1 + 0.6 * sin(t*2*pi/p2)) * sin(t*2*pi/p1)  + 2 * sin(t*2*pi/p2) +
#'   rnorm(n, sd = 0.5) + t * 0.01
#'
#' inter_dt <- round(runif(length(xy), min = 0.5, max = 1.5),1)
#'
#' dt <- cumsum(inter_dt)
#'
#' keep <- runif(length(dt)) < 0.5
#'
#' xy <- xy[keep]
#' dt <- dt[keep]
#'
#' window <- in.window(dt, w = 30, 1:590, xy = xy)
#'
#' par(mfrow = c(1,2))
#'
#' plot(xy, dt, type = "o", pch = 19,
#'      ylim = c(0,600), main = "Moving Average")
#'
#' lines(rowMeans(window$xy, na.rm = TRUE), window$xout,
#'       col = "red", lwd = 2)
#'
#' plot(window$n.size, window$xout,  pch = 19,
#'      ylim = c(0,600), xlim = c(0,20), ylab = "dt",
#'      main = "Amount of Points in Average")
#'
#' # Test the boundary rule ----
#'
#' x <- c(1,1,2,3,4,6,8,10,15,16)
#' xout <- -6:22
#'
#' output <- in.window(x = x, w = 10, xout = xout, b = "]]")
#'
#' test <- output$x - output$xout
#' see  <- cbind(output$xout, output$x)
#'
#' colnames(see) <- c("xout", paste0("x", seq_len(ncol(see)-1)))
#'
#' test # difference between x and xout: it is contained in ]-5,5]
#' see
#'
#' @export

in.window <- function(x, w, xout = unique(x), b = "[]", warn = 100,...)
{

  b <- rebound(b)

  if(!(inherits(x, "numeric") | inherits(x, "integer"))){
    stop("The 'x' parameter should be numeric or integer")
  }

  if(!(inherits(w, "numeric") | inherits(w, "integer"))){
    stop("The 'w' parameter should be numeric or integer")
  }

  if(!(inherits(xout, "inumeric") | inherits(xout, "integer"))){
    stop("The 'xout' parameter should be numeric or integer")
  }

  if(is.unsorted(x)) stop("The parameter 'x' should be sorted")
  if(is.unsorted(xout)) stop("The parameter 'xout' should be sorted")

  if(!(inherits(warn, "numeric") | inherits(warn, "integer"))){
    stop("The 'warn' parameter should be numeric or integer")
  }

  warn <- as.integer(warn + 0.1)

  liszt <- list(...)

  h.list <- homogenise(i = x, l = liszt, cycle = F)

  # Find the x values closest to their xout equivalents ----

  d1 <- data.frame(pos = c(x, xout),
                   or = c(rep(T, length(x)), rep(F, length(xout))))

  d1 <- d1[order(d1$pos),]

  d1$nor <- as.integer(d1$or)

  d1$trick1 <- cumsum(d1$nor)

  d1$trick2 <- d1$trick1 + 1

  d2 <- d1[!d1$or,-(2:3),drop = F]

  row.names(d2) <- NULL

  d2$trick1[d2$trick1 ==0] <- NA

  d2$do <- x[d2$trick1]
  d2$up <- x[d2$trick2]

  d2$d.do <- d2$pos - d2$do
  d2$d.up <- d2$up  - d2$pos

  d2$decide <- d2$d.up - d2$d.do

  # Begin exception coding : define exactly what happens at boundary ----

  d2$high <- d2$decide > 0
  d2$equi <- d2$decide == 0

  d2$high[is.na(d2$high)] <- F
  d2$equi[is.na(d2$equi)] <- F

  d2$end <- d2$trick2
  d2$end[d2$high] <- d2$trick1[d2$high]

  if(b == "]]") {
    d2$end[d2$equi] <- d2$trick2[d2$equi]
  } else {
    d2$end[d2$equi] <- d2$trick1[d2$equi]
  }

  # End exception coding ----

  lx <- length(x)

  d2$end[d2$end > lx] <- lx

  d3 <- d2[,c(1,11),drop = F]

  d3$pi <- x[d3$end]

  # Work the windowing ----

  if(b == "[[" | b == "[]") b.lower <- T else b.lower <- F
  if(b == "]]" | b == "[]") b.upper <- T else b.upper <- F

  hw <- w/2
  xoutc <- d3$pos

  # a <- a
  # test.index <- d3$end
  # limit <- c("lower", "upper")
  # include <- F

  minifun <- function(xmf, hw, xoutc, test.index, limit, include){

    dmini <- data.frame(pos = xoutc, ti = test.index, tpos = xmf[test.index])

    dmini$dpos <- dmini$tpos - dmini$pos

    dmini$out <- dmini$ti

    if(limit[1] == "lower") {

      if(isTRUE(include)){
        dmini$out[dmini$dpos < -hw] <- NA
      } else {
        dmini$out[dmini$dpos <= -hw] <- NA
      }

    } else if (limit[1] == "upper"){

      if(isTRUE(include)){
        dmini$out[dmini$dpos > hw] <- NA
      } else {
        dmini$out[dmini$dpos >= hw] <- NA
      }

    } else if (limit[1] == "at"){

      dmini$out[abs(dmini$dpos) != hw] <- NA

    }

    return(dmini$out)

  }

  # ----

  d4 <- data.frame(oi = seq_len(length(xoutc)), xoutc = xoutc)

  f.lower <- minifun(x, hw, xoutc,
                     test.index = d3$end,
                     limit = "lower", include = b.lower)

  f.higher <- minifun(x, hw, xoutc,
                      test.index = d3$end,
                      limit = "upper", include = b.upper)

  d4$i0 <- f.higher
  d4$i0[is.na(f.lower)] <- NA

  # d4$see <- a[d3$end]

  # up and down data frames, recursion ----

  li <- nrow(d4)

  # down (with the sickness)

  accu.low <- d4

  recu.low <- accu.low[!is.na(d4$i0),,drop = F]

  accu.low$p0 <- x[d4$i0]

  if(nrow(recu.low) != 0){

    i <- 1

    repeat {

      # print(i)

      if(i == warn) {
        warning(paste("More than", warn,
                      "repetitions, see 'warn' parameter for details."))
      }

      ncl <- ncol(recu.low)

      test.ind.l <- recu.low[,ncl] - 1

      test.ind.l[test.ind.l == 0] <- NA

      outl <- minifun(x, hw, xoutc = recu.low$xoutc,
                      test.index = test.ind.l,
                      limit = "lower", include = b.lower)

      recu.low <- cbind(recu.low, outl)
      recu.low <- recu.low[!is.na(recu.low[,ncl+1]),,drop = F]

      l.low <- nrow(recu.low)

      if(l.low == 0) break

      add <- rep(NA, li)
      add[recu.low$oi] <- recu.low[,ncl+1]

      # dadd <- data.frame(add, a[add])
      # colnames(dadd) <- paste0(c("i-", "p-"), i)

      dadd <- data.frame(add)
      colnames(dadd) <- paste0(c("i-"), i)

      accu.low <- cbind(accu.low, dadd)

      i <- i+1

    }

  }

  # up(town girl)

  accu.high <- d4

  recu.high <- accu.high[!is.na(d4$i0),,drop = F]

  accu.high$p0 <- x[d4$i0]

  if(nrow(recu.high) != 0){

    j <- 1

    repeat {

      # print(j)

      if(j == warn) {
        warning(paste("More than", warn,
                      "repetitions, see 'warn' parameter for details."))
      }

      nch <- ncol(recu.high)

      test.ind.h <- recu.high[,nch] + 1

      test.ind.h[test.ind.h > lx] <- NA

      outh <- minifun(x, hw, xoutc = recu.high$xoutc,
                      test.index = test.ind.h,
                      limit = "upper", include = b.upper)

      recu.high <- cbind(recu.high, outh)
      recu.high <- recu.high[!is.na(recu.high[,nch+1]),,drop = F]

      l.high <- nrow(recu.high)

      if(l.high == 0) break

      add <- rep(NA, li)
      add[recu.high$oi] <- recu.high[,nch+1]

      # dadd <- data.frame(add, a[add])
      # colnames(dadd) <- paste0(c("i+", "p+"), i)

      dadd <- data.frame(add)
      colnames(dadd) <- paste0(c("i+"), j)

      accu.high <- cbind(accu.high, dadd)

      j <- j+1

    }

  }

  # Merge two data sets, add details ----

  end.i <- cbind(accu.low[,rev(seq_len(ncol(accu.high))[-(1:4)])],
                 accu.high[,-c(1,2,4)])

  end.i <- as.matrix(end.i)

  end.p <- matrix(x[end.i], ncol = ncol(end.i))

  end.presence <- matrix(as.integer(!is.na(end.i)),
                         ncol = ncol(end.i))

  end.n <- rowSums(end.presence)

  out <- list(xout = xout, x.index = end.i, x = end.p, n.size = end.n)

  # Add additional parameters in the windows ----

  if(length(h.list) != 0){

    tinyfun <- function(xy) matrix(xy[out$x.index], ncol = ncol(out$x.index))

    add.list <- lapply(h.list, tinyfun)

    out <- merge_list(out, add.list)

  }

  return(out)

}


