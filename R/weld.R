#' @title Combines segments with "litholog()"-like data frame
#'
#' @description Adds segments to the polygon forming the bed of a log in a
#' "litholog()"-like data frame.
#'
#' @param log a "litholog()"-like data frame on which the new segment
#' needs to be welded.
#' @param dt the dt value for each point of the added segment.
#' @param xy the xy value for each point of the added segment.
#' @param begin the row of log after which the segment will be added.
#' @param end the row of log before which the segment will be added
#' (end should be superior to begin).
#' @param erase erase the begin point ('begin'), end point ('end'), both
#' ('both') or only the points in between ('none').
#' @param order the order of the added points : can be the current order
#' ('current'), the current order inversed ('inverse'), or ordered by xy ('xy'
#' or '-xy') or dt ('dt' or '-dt').
#'
#' @return a "litholog()"-like data frame with the bed
#' that comprises the begin
#' and end row having the segment welded to it.
#' @seealso \code{\link{litholog}} and \code{\link{weldlog}}
#'
#' @examples
#' l <- c(1)
#' r <- c(2)
#' h <- c(4)
#' i <- c("B1")
#' log <- litholog(l, r, h, i)
#'
#' seg <- sinpoint(4, 1, 0.25, pos = 2, phase = 0.5)
#' welded <- weld(log, seg$y, seg$x, 3, 4, order = "inverse", erase = "both")
#'
#' plot(c(-1,5),c(0,3),type = "n")
#'
#' multigons(log$i,log$xy,log$dt)
#' points(seg$x,seg$y)
#'
#' multigons(welded$i, welded$xy, welded$dt, lty = 2, lwd = 3, border = "red")
#'
#' @importFrom dplyr arrange desc
#' @export

weld <- function(log, dt, xy, begin, end,
                 erase = "none", order = "current")
{
  nxy <- length(dt)

  if(nxy != length(xy)) stop(paste("The parameters dt and xy should",
                                   " have the same length", sep = ""))

  if(!(begin < end)) stop(paste("The parameter end should be inferior",
                                " to the parameter begin", sep = ""))

  n1 <- as.character(log[begin,1])
  n2 <- as.character(log[end,1])

  if(!identical(n1,n2)){stop("begin and end should be in the same bed")}

  if(erase == "none"){
    dlim <- begin
    ulim   <- end
  } else if (erase == "begin") {
    dlim <- begin - 1
    ulim   <- end
  } else if (erase == "end") {
    dlim <- begin
    ulim   <- end + 1
  } else if (erase == "both") {
    dlim <- begin - 1
    ulim   <- end + 1
  } else {stop("The parameter erase should be 'none','begin','end' or 'both'")}

  if(dlim >= 1 & dlim <= nrow(log)) {
    d <- log[1:dlim,]
  } else {
    d <- log[0,]
  }

  if(ulim >= 1 & ulim <= nrow(log)) {
    u <- log[ulim:nrow(log),]
  } else {
    u <- log[0,]
  }

  i <- as.character(rep(n1,nxy))

  ad <- data.frame(i, dt, xy, stringsAsFactors = F)
  colnames(ad) <- c("i", "dt", "xy")

  if (order == "xy"){
    ad <- arrange(ad,xy)
  } else if (order == "-xy") {
    ad <- arrange(ad,desc(xy))
  } else if (order == "dt") {
    ad <- arrange(ad,dt)
  } else if (order == "-dt") {
    ad <- arrange(ad,desc(dt))
  } else if (order == "inverse") {
    ad <- ad[nrow(ad):1,]
  } else if (order != "current") {
    stop(paste("The parameter order should be 'current', 'inverse',",
               " 'xy', '-xy', 'dt' or '-dt'", sep = ""))
  }

  res <- rbind(d,ad,u)
  rownames(res) <- NULL
  return(res)

}
