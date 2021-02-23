#' @title Compute the realtive thickness variations of sections
#'
#' @description Based on tie-points, this function computes the relative
#' thickness variations of different sections compared to a reference section or
#' composite sections
#'
#' @param dt a matrix of depth (or time) of the different tie points. Columns
#' are for the sections, rows for each tie point. No NA values are accepted,
#' if necessary, tie-points have to be estimated, using for instance the
#' \code{\link{strat.mean}} function
#' @param initial which tie-points are originally present in the sections (if
#' NULL, by default all the values are considered as originally present)
#' @param ref the column index for the section which acts as a reference (by
#' default, it is set to 1, for the first columns)
#' @param events the name of the tie points
#' @param sections the name of the sections
#'
#' @examples
#' dt     <- tie.points.example[,2:6]
#' events <- tie.points.example[,1]
#'
#' extended <- strat.mean(dt = dt, events = events)
#'
#' strat.var(extended$dt, extended$initial)
#'
#' @importFrom dplyr lead
#' @export

strat.var <- function(dt,  initial = NULL, ref = 1,
                      events = NULL, sections = NULL)
{

  if(is.null(initial)){
    initial <- dt
    initial[seq_len(nrow(dt)), seq_len(ncol(dt))] <- T
  }

  if(is.null(events)) events   <- row.names(dt)
  if(is.null(sections)) sections <- colnames(dt)

  if(length(events) != nrow(dt)) stop("Incorrect amount of events")
  if(length(sections) != ncol(dt)) stop("Incorrect amount of sections")


  if(any(is.na(dt))) {
    stop("There should not be any NA value in the 'dt' parameter")
  }

  ref <- as.integer(ref)

  if(!(length(ref) == 1 & ref %in% seq_len(ncol(dt)))){
    stop("The 'ref' parameter should stand for the index of a",
         " column of the 'dt' matrix")
  }

  l <- nrow(dt)
  s <- ncol(dt) - 1

  if(s < 1) stop("There should be two columns at least in the 'dt' matrix")

  pose           <- dt
  pose[!initial] <- NA
  pose[1,]       <- dt[1,]
  pose[l,]       <- dt[l,]

  pose           <- pose[,-ref]

  lpose <- unlist(pose)
  names(lpose) <- NULL

  minus <- matrix(unlist(rep(pose[1,], l)), nrow = l, byrow = T)
  maxus <- matrix(unlist(rep(pose[l,], l)), nrow = l, byrow = T)

  npose <- (pose - minus)/(maxus - minus)

  lnpose <- unlist(npose)
  names(lnpose) <- NULL

  eve <- rep(seq_len(l), s)
  sec <- rep(seq_len(s), each = l)

  out <- which(is.na(lpose))

  df <- data.frame(section = sections[-ref][sec],
                   se.id = sec, section.id = seq_len(s + 1)[-ref][sec],
                   le = events[eve], l.id = eve, l.dt = lpose, lndt = lnpose)

  ndf <- df[-out,]

  mdf <- lead(ndf)
  colnames(mdf)[4:7] <- c("re", "r.id", "r.dt", "rndt")

  iddf <- cbind(ndf, mdf)
  ddf <- iddf[-nrow(iddf),]

  intdf <- ddf[ddf[,1] == ddf[,8],]

  wdf <- intdf[c(1,2,3,4, 11,5,12,6,13,7,14)]

  wdf$int <- wdf$rndt - wdf$lndt

  composite <- dt[,ref]

  wdf$lref <- composite[wdf$l.id]
  wdf$rref <- composite[wdf$r.id]

  wdf$intref <- wdf$rref - wdf$lref

  wdf$dev.pct <- 100 * wdf$int/wdf$intref

  wdf$interval <- paste(wdf$le, wdf$re, sep = "-")

  result <- wdf[,c(1,3,17,6,7,8,9,16)]

  return(result)

}
