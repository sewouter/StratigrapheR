#' @title Changes boundaries segments in basic lithologs
#'
#' @description Adds personalised segements to bed boundaries of lithologs from
#' "litholog()"-like data frames
#'
#' @param log a "litholog()"-like data frame on which the new segments
#' need to be welded.
#' @param dt the position of the n boundaries to change.
#' @param seg a list of n dataframes having xy and dt coordinates for
#' the segments that are going to be welded to the log.
#' @param j the indexes of the segments attributed to each boundary or
#' the names of these segments. Should be of same length than dt.
#' @param col.xy the number of the column for the xy coordinates in the
#' seg dataframes.
#' @param col.dt the number of the column for the dt coordinates in the
#' seg dataframes.
#' @param auto.dt whether to automatically add the dt value to the dt of
#' the segments (with the add.dt value when it is not zero)
#' @param add.dt a value to add to the dt of the segments for each
#' boundary (in addition of the value of the \code{dt} parameter). Should be of
#' length 1 or of same length than dt.
#' @param omit1,omit2 the dt of the boundary for which either the upper
#' or lower bed should not be welded to (1 and 2 depending on the order of the
#' beds in the original log)
#' @param warn whether you want to be annoyed (beginners should find it
#' useful to be annoyed)
#' @return a "litholog()"-like data frame, with new bed boundaries
#' @seealso Complementary function\code{\link{litholog}}
#'
#' Underlying function: \code{\link{weld}}
#'
#' To generate sinuoisidal segments: \code{\link{sinpoint}}
#' To generate a lot of different sinuoisidal segments: see the example in
#' \code{\link{neatPick}}
#'
#' To import and adapt .svg files as segments: \code{\link{pointsvg}},
#' \code{\link{framesvg}}, \code{\link{centresvg}} and \code{\link{changesvg}}
#'
#' @examples
#' l <- c(0,1,2,3,4)
#' r <- c(1,2,3,4,5)
#' h   <- c(4,3,4,3,4)
#' i <- c("B1","B2","B3","B4","B5")
#' log  <- litholog(l, r, h, i)
#'
#' whiteSet(xlim = c(-1,5), ylim = c(-1,6))
#'
#' multigons(log$i, log$xy, log$dt, lty = 3)
#'
#' seg1 <- sinpoint(4, 0, 0.25, phase=0.5)
#' seg2 <- sinpoint(4, 0, 0.25, phase=1.5)
#'
#' welded <- weldlog(log, dt = c(2,3,4), seg = list(seg1 = seg1, seg2 = seg2),
#'                   j = c("seg1", "seg2", "seg2"))
#'
#' multigons(welded$i, welded$xy, welded$dt, lwd = 3, lty = 2, border = "red")
#'
#' @importFrom stats approx
#' @export

weldlog <- function(log, dt, seg, j = 1:length(dt),
                    col.xy = 1, col.dt = 2,
                    auto.dt = T, add.dt = 0,
                    omit1 = NULL, omit2 = NULL, warn = T)
{

  # Security and arguments preprocessing ----

  if(auto.dt != TRUE &
     auto.dt != FALSE){
    stop("The parameter 'auto.dt' should be T or F")
  }

  if(warn != TRUE &
     warn != FALSE){
    stop("The parameter 'warn' should be T or F")
  }

  segxy    <- sapply(seg,"[", col.xy)
  segdt    <- sapply(seg,"[", col.dt)

  segsimp <- data.frame(gloVar.xy = unlist(segxy, use.names = F),
                        dt = unlist(segdt, use.names = F))

  cl        <- sapply(segxy, length, USE.NAMES = F)
  names(cl) <- NULL

  n <- length(dt)

  if(n != length(j)) {
    stop("The parameters 'j' and 'dt' should have the same length")
  }

  ns <- length(seg)

  test.match<- match(j, names(seg))

  cond.num   <- !(is.numeric(j) & ns >= max(j))
  cond.match <- any(is.na(test.match))

  if(cond.num & cond.match) {
    stop("The parameter j shoud refer to existing seg elements, either by
         index or name in the 'seg' list.")
  }

  if(!cond.match) jind <- test.match else jind <- j

  nadd <- length(add.dt)

  if(!(nadd == 1 | nadd == n)){
    stop(paste("The parameter 'add.dt' should be of length 1 or of same length",
               " than the 'dt' parameter", sep = ""))
  }

  # Define order of segments ----

  segsimp$i <- rep(seq_len(ns), times= cl)

  retrieve <- !duplicated(segsimp$i) | !duplicated(segsimp$i, fromLast = T)

  sb <- segsimp[retrieve,]

  sb$sign               <- sign(lead(sb$gloVar.xy) - sb$gloVar.xy)
  sb$sign[sb$sign == 0] <- 1

  sbn        <- sb[seq(1,nrow(sb),2),]
  segsimp$gloVar.o  <- seq_len(nrow(segsimp)) * rep(sbn$sign, times= cl)

  # Add points to segments not covering all the bed lengths ----

  limits <- arrange(segsimp[retrieve,], i, gloVar.xy)

  inftest <- limits$dt != 0

  if(any(inftest) & all(add.dt == 0) & warn){

    warning(paste("The dt values of the beginning of following segment(s) ",
                  "are not zero : ",
                  paste(unique(limits$i[inftest]), collapse = " "),
                  ". This could bring erroneous ",
                  "results (check it yourself at the boundaries in question,",
                  " because in the end you're the boss boss). If you want to ",
                  "get rid of this pesky warning set the 'warn' parameter",
                  " to FALSE.",sep = ""))

  }

  nlim   <- nrow(limits)

  liminf <- limits[seq(1,nlim,2),]
  limsup <- limits[seq(2,nlim,2),]

  maxxy <- max(log$xy)

  liminf <- liminf[liminf$gloVar.xy > 0,]
  limsup <- limsup[limsup$gloVar.xy < maxxy,]

  suptest <- limsup$dt != 0

  if(any(suptest) & warn){

    warning(paste("The dt values of the end of these incomplete ",
                  "segments (not reaching the maximum xy of the beds in the",
                  " log) are not zero: ",
                  paste(limsup$i[suptest], collapse = " "), ". Extrapolation",
                  " will be applied, but it's going to be ugly. If you want to",
                  " get rid of this pesky warning set the 'warn' parameter",
                  " to FALSE.",  sep = ""))

  }

  liminf1 <- liminf
  limsup1 <- limsup

  ninf <- nrow(liminf)

  liminf1$gloVar.xy <- rep(0, ninf)
  liminf1$dt        <- rep(0, ninf)
  liminf1$gloVar.o  <- liminf$gloVar.o - 1

  nsup <- nrow(limsup)

  limsup1$gloVar.xy <- rep(maxxy, nsup)
  limsup1$dt        <- rep(0, nsup)
  limsup1$gloVar.o  <- limsup$gloVar.o + 1

  # Insert newly defined points and reorder ----

  segsimp <- rbind(segsimp,liminf1,limsup1)
  segsimp <- arrange(segsimp, i, gloVar.o)


  # For loop: weld each boundary to the log,
  # boundary by boundary, bed by bed ----

  ni <- seq_len(n)

  accu <- log

  # Work each boundary ----

  for(i in ni)
  {

    if(nadd == 1) offset <- add.dt else offset <- add.dt[i]

    boundary <- dt[[i]]
    choseg   <- jind[i]

    sxy <- segsimp[segsimp$i == choseg,1]
    sdt <- segsimp[segsimp$i == choseg,2] + offset

    if(auto.dt) sdt <- sdt + boundary

    sxylim <- c(min(sxy), max(sxy))

    b  <- as.character(unique(log[which(log$dt == boundary),]$i))

    # Work each bed related to the boundary

    if(any(boundary == omit1)){
      nk <- 2
    } else if(any(boundary == omit2)){
      nk <- 1
    } else {
      nk <- seq_len(length(b))
    }

    for(k in nk)
    {

      nabk <- b[k]

      bk <- subset(log, i == nabk)

      xylim <- c(min(bk$xy), max(bk$xy))

      s <- data.frame(sxy, sdt)
      colnames(s) <- c("xy", "dt")

      # Checking if the segment does not intersect with the bed in
      # undesirable parts, interpolation and extrapolation of
      # between segment and bed if necessary

      fd2 <- which(s$xy > xylim[2])

      if(any((fd2 - lag(fd2))[-1] != 1)){

        warning(paste("The segment number ",choseg,  " at the boundary dt = ",
                      boundary , " would create line crossings in bed ",
                      nabk, ". It was therefore not welded to that bed.",
                      sep = ""))

      } else {


        if(xylim[2] != sxylim[2]){

          if(!(any(sxy == xylim[2]))){

            new <- approx(s$xy, s$dt, xylim[2], method = "linear")

            dn <- data.frame(new$x, new$y)
            colnames(dn) <- c("xy", "dt")

            if(length(fd2) != 0) s <- s[-fd2,]
            s <- rbind(s, dn)

          } else {

            if(length(fd2) != 0) s <- s[-fd2,]

          }

        }

        fd1 <- which(s$xy < xylim[1])

        if(any((fd1 - lag(fd1))[-1] != 1)){

          warning(paste("The segment number ",choseg,  " at the boundary dt = ",
                        boundary , " would create line crossings in bed ",
                        nabk, ". It was therefore not welded to that bed.",
                        sep = ""))

        } else {

          if(xylim[1] != sxylim[1]){

            if(!(any(sxy == xylim[1]))){

              new <- approx(s$xy, s$dt, xylim[1], method = "linear")

              dn <- data.frame(new$x, new$y)
              colnames(dn) <- c("xy", "dt")

              if(length(fd1) != 0) s <- s[-fd1,]
              s   <- rbind(dn, s)

            }  else {

              if(length(fd1) != 0) s <- s[-fd1,]

            }

          }

          # Checking and welding ----

          pos <- which(accu$i == nabk & accu$dt == boundary)

          if(accu[pos[1],]$xy == s[1,]$xy){

            accu <- weld(accu, s$dt, s$xy, pos[1], pos[length(pos)],
                         erase = "both", order = "current")

          } else if (accu[pos[1],]$xy == s[nrow(s),]$xy) {

            accu <- weld(accu, s$dt, s$xy, pos[1], pos[length(pos)],
                         erase = "both",order = "inverse")

          } else {
            warning(paste("The boundary at dt = ", boundary,
                          " does not correspond to a",
                          " minima or maxima in bed ", nabk,
                          ", yet a point of that bed",
                          " is at that position", sep = ""))
          }

        }

      }

    }

  }

  return(accu)

}


