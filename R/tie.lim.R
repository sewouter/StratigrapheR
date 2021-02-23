#' @title Discretises lim objects
#'
#' @description Discretises continuous lim objects by constant interpolation
#'
#' @param lim an object convertible into a lim object: either a vector
#' of length 2 or a list of n left (1st element) and n right (2ndt element)
#' interval limits, and of n interval IDs. In this case the lim objects have to
#' be ordered, by ids, dependently to each other, and from left to right. For
#' each id the lim objects have to cover the entire interval from the lowest
#' to the highest value, without overlap.
#' @param l a vector of n left interval limits
#' @param r a vector of n right interval limits
#' @param y a vector of n values to discretise
#' @param xout a vector of numeric values specifying where interpolation
#' is to take place. It will be identical for each id. If NULL the result will
#' be continuous (points of a continous line).
#' @param id a vector of n interval IDs (default is 1 for each interval)
#' @param to.lower whether to take the left (lower) or right point for
#' interpolation at adjacent points
#' @param warn whether to warn if the sampling interval is prone to
#' miss the smallest intervals.
#'
#' @seealso \code{\link{as.lim}}
#'
#' @examples
#' l  <- matrix(1:30, ncol = 3, byrow = FALSE)
#' r  <- matrix(2:31, ncol = 3, byrow = FALSE)
#' id <- matrix(rep(c("C1", "C2", "C3"),10), ncol = 3, byrow = TRUE)
#' y  <- matrix(rep(1:10,3), ncol = 3, byrow = FALSE)
#' xout <- seq(-2,32,0.5)
#'
#' res  <- tie.lim(l = l, r = r,  y = y, xout = xout, id = id)
#'
#' cont <- tie.lim(l = l, r = r,  y = y, id = id)
#'
#' plot(res$x, res$y, pch = 19, col = "red")
#'
#' lines(cont$x[,1], cont$y[,1])
#' lines(cont$x[,2], cont$y[,2])
#' lines(cont$x[,3], cont$y[,3])
#'
#' @importFrom dplyr left_join lag
#' @export


tie.lim <- function(lim = NULL, l = NULL, r = NULL, y = NULL, xout = NULL,
                    id = 1L, to.lower = T, warn = T)
{

  if(to.lower != TRUE &
     to.lower != FALSE){
    stop("The parameter 'to.lower' should be T or F")
  }

  if(warn != TRUE &
     warn != FALSE){
    stop("The parameter 'quiet' should be T or F")
  }

  lima <- as.lim(lim = lim, l = l, r = r, id = id)

  la  <- lima$l
  ra  <- lima$r
  ida <- lima$id

  if(!are.lim.ordered(l = l, r = r, id = id, dependently = T)){
    stop(paste("The lim objects should be ordered dependently ",
               "(see are.lim.ordered help page for details)", sep = ""))
  }

  if(!all(l == lag(r), na.rm = T)){
    stop(paste("The elements of the lim objects should be adjacent to",
               " each other, covering the whole interval from the ",
               "lowest to highest value", sep = ""))
  }

  int <- min(r-l, na.rm = T)

  if(!is.null(xout)){
    if(warn & any(int <= abs(xout - lag(xout)), na.rm = T)){
      warning(paste("The intervals are undersampled, some may be lost,",
                    " try a sampling rate lower than ", int, sep = ""))
    }
  }

  lcol <- ncol(la)

  matcond1 <- all(apply(ida, 2, function(i) length(unique(i))) == 1)
  matcond2 <- length(unique(ida[1,])) == lcol

  if(!matcond1 | !matcond2) {
    stop(paste("If under matrix form, the 'id' parameter should be different",
               " for each column and similar for each row", sep = ""))
  }

  la  <- as.vector(la)
  ra  <- as.vector(ra)
  ida <- as.vector(ida)
  y   <- as.vector(y)

  isMATRIX <- T

  nl <- length(l)

  dxy <- data.frame(y, stringsAsFactors = F)

  dl <- data.frame(x = la, y = y, id = ida, stringsAsFactors = F)
  dr <- data.frame(x = ra, y = y, id = ida, stringsAsFactors = F)

  d <- rbind(dl, dr)
  d <- d[kronecker(1:(nl), c(0, nl), "+"), ]

  cont <- as.list(d)

  if(is.null(xout)){

    res <- lapply(cont, matrix, ncol = lcol)

  } else {

    xb  <- cont$x
    yb  <- cont$y
    idb <- cont$id

    ui <- unique(idb)
    ni <- length(ui)

    loc <- left_join(data.frame(id = idb), data.frame(id = ui, p = seq_len(ni)),
                     by = "id")

    lout <- length(xout)

    xoutb <- rep(xout, ni)

    idb <- factor(idb, levels = unique(idb))
    ls  <- split(xb, idb)

    nax  <- unlist(lapply(ls, function(x) all(is.na(x))), use.names = F)
    minx <- unlist(mapply(min, ls, na.rm = !nax), use.names = F)
    maxx <- unlist(mapply(max, ls, na.rm = !nax), use.names = F)

    minmat <- xoutb - as.vector(matrix(rep(minx, lout), ncol = ni, byrow = T))
    maxmat <- xoutb - as.vector(matrix(rep(maxx, lout), ncol = ni, byrow = T))

    keep              <- !(minmat < 0 | maxmat > 0)
    keep[is.na(keep)] <- F

    addx <- (loc$p - 1) * (max(maxx, na.rm = T) + max(abs(minx), na.rm = T) + 1)

    add <- as.vector(matrix(rep(seq_len(ni) - 1, lout), ncol = ni, byrow = T)) *
      (max(maxx, na.rm = T) + max(abs(minx), na.rm = T) + 1)

    xb    <- xb    + addx
    xoutb <- xoutb + add

    df <- data.frame(x = xb, y = yb)

    if(to.lower){

      dis <- approx(df$x, df$y, xoutb, method = "constant", ties = "ordered")

    } else {

      l1   <- nrow(df)
      df1   <- df[l1:1,]
      df1$x <- -df1$x

      xout1 <- -xout1

      dis <- approx(df1$x, df1$y, xoutb, method = "constant", ties = "ordered")

      dis$x <- -dis$x

    }

    res          <- dis
    res$x        <- res$x - add
    res$y[!keep] <- NA

    res$id <-  as.vector(matrix(rep(ui, lout), ncol = ni, byrow = T))

    res <- list(x = matrix(res$x, ncol = ni),
                y = matrix(res$y, ncol = ni),
                id = matrix(res$id, ncol = ni))

  }

  return(res)

}

