#' @title Finds bed intervals in a "litholog()"-like data frame
#'
#' @description Determines the interval of bed boundaries at the far left of a
#' litholog. This is used when the welding of varying bed boundaries changes
#' these intervals, and that you want to use bedtext() to print the name of the
#' beds on the log.
#'
#' @param i the id of the polygons in the "litholog()"-like data frame
#' @param dt the depth of the polygons in the "litholog()"-like data frame
#' @param xy the x values (i.e. hardness) of the polygons in the
#' "litholog()"-like data frame
#' @param warn whether you want to be annoyed
#' @return a list of minima (l) and maxima (r) of boundaries corresponding to
#' each bed (id)
#' @seealso \code{\link{litholog}}, \code{\link{weldlog}} and
#' \code{\link{bedtext}}
#' @examples
#' l <- c(0,1,2,3,4)
#' r <- c(1,2,3,4,5)
#' h   <- c(4,3,4,3,4)
#' i <- c("B1","B2","B3","B4","B5")
#' log  <- litholog(l, r, h, i)
#'
#' whiteSet(xlim = c(-1,5), ylim = c(-1,6))
#'
#' title("leftlog() gets the bed names in the right position")
#'
#' multigons(log$i, log$xy, log$dt, lty = 3)
#'
#' seg1 <- sinpoint(4, 0, 0.25, pos = 1, phase=0)
#' seg2 <- sinpoint(4, 0, 0.25, pos = 1, phase=1)
#'
#' welded <- weldlog(log, dt = c(2,3), seg = list(seg1, seg2), add.dt = 0.5)
#'
#' multigons(welded$i, welded$xy, welded$dt, lwd = 3, lty = 2, border = "red")
#'
#' old.log.interval <- leftlog(log$i, log$dt, log$xy)
#' new.log.interval <- leftlog(welded$i, welded$dt, welded$xy)
#'
#' bedtext(labels = new.log.interval$id,
#'         l= new.log.interval$l,
#'         r= new.log.interval$r,
#'         arg = list(col = "red"))
#'
#' @export

leftlog <- function(i, dt, xy, warn = TRUE)
{
  i <- as.character(i)

  log <- data.frame(i = i, dt = dt, xy = xy, stringsAsFactors = F)

  if(warn != TRUE &
     warn != FALSE){
    stop("The parameter 'warn' should be T or F")
  }

  minxy <- min(log$xy)

  if(minxy != 0 & warn) {
    stop(paste("The minimum of xy values is not zero. The bed xy values should",
               " all be of 0 on the extreme left side of the plot",
                  sep = ""))
  }

  lzero <- log[log$xy == min(log$xy),]

  id <- unique(lzero$i)

  listzero <- split(lzero$dt, f = factor(lzero$i, levels = id))

  r <- unlist(lapply(listzero,max), use.names = F)
  l <- unlist(lapply(listzero,min), use.names = F)

  res <- list(l = l, r = r, id = id)

  return(res)

}
