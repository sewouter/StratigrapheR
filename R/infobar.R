#' @title Draws rectangles with text in them
#'
#' @description Draws rectangles with text in them, typically to delimit
#' (stratigraphical) intervals (e.g. magnotochrons, but also
#' lithostratigraphy,...)
#'
#' @param xmin,xmax,ymin,ymax x and y limits for the rectangles. You can
#' either provide 1 or n of each (if you want to have always the same x limits
#' but multiple and different y ones it is possible)
#' @param labels a 1 or n character vector (i.e. text) specifying the
#' text to be written in the rectangle. You can write "" for no text.
#' @param m,t a list graphical parameters (of length 1 or n) to feed
#' multigons() for m and to text() for t. See respective help pages ?multigons
#' and ?text for the possible arguments. See the example for illustration, and
#' ?merge_list for further information.
#' @param srt,family,xpd further graphical parameters, see ?par for information
#'
#' @seealso Similar functions: \code{\link{multigons}}, \code{\link{bedtext}},
#' \code{\link{nlegend}} and \code{\link{ylink}}
#'
#' To deal with intervals: \code{\link{as.lim}} and related functions
#'
#' @examples
#' labels <- c("High 5", "Low 5", "5")
#' ymin <- c(10,-10,2.5)
#' ymax <- c(20,0, 7.5)
#'
#' plot(c(0,6),c(-20,20), type = "n")
#'
#' infobar(xmin = 0, xmax = 1, ymin = ymin, ymax = ymax, labels,
#'         m = list(col = c("grey","grey", "red"),
#'                  border = "black", density = 10),
#'         t = list(cex = 1.5, col = "white"))
#'
#' @export

infobar <- function(xmin, xmax, ymax, ymin, labels = NA,
                    m = list(), t = list(),
                    srt = 90, family = par("family"), xpd = par("xpd"))
{
  cond <- !(is.na(labels[[1]]) & length(labels) == 1)

  xy <- list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)

  m <- merge_list(m, list(col = "white"))

  # Security: verify lengths ----

  s0 <- unlist(lapply(xy,length))
  s2 <- unlist(lapply(m,length))

  if (cond){

    l <- list(labels = as.character(labels))

    s1 <- unlist(lapply(l,length))
    s3 <- unlist(lapply(t,length))

    sa <- c(s0,s1,s2,s3)

  } else {

    sa <- c(s0,s2)

  }

  maxl <- max(sa)

  if(!all(sa == maxl | sa == 1)){
    stop(paste("The parameters or elements in the list of parameters",
               " should be of length 1 or n", sep = ""))
  }

  # Get elements of length one to lenght n----

  if(maxl > 1){
    xy[which(s0 == 1)] <- lapply(xy[which(s0 == 1)],rep,maxl)
    m[which(s2 == 1)] <- lapply(m[which(s2 == 1)],rep,maxl)
  }

  if (cond){

    if(maxl > 1){
      l[which(s1 == 1)] <- lapply(l[which(s1 == 1)],rep,maxl)
      t[which(s3 == 1)] <- lapply(t[which(s3 == 1)],rep,maxl)
    }

    add <- list(x = (xy$xmin + xy$xmax)/2, y = (xy$ymin + xy$ymax)/2)
    l   <- c(l,add)

  }

  # Order ----

  ori    <- data.frame(xy)
  ori$id <- seq_len(nrow(ori))

  sec1 <- ori[,c(1,3,5)]
  sec2 <- ori[,c(1,4,5)]
  sec3 <- ori[,c(2,3,5)]
  sec4 <- ori[,c(2,4,5)]

  colnames(sec1) <- colnames(sec2) <- c("x","y","i")
  colnames(sec3) <- colnames(sec4) <- c("x","y","i")

  reso <- rbind(sec1,sec2,sec4,sec3)

  reso <- reso[as.vector(t(matrix(seq_len(maxl*4), nrow = maxl))),]

  # Plot ----

  do.call(multigons, merge_list(list(i = reso$i, x = reso$x, y = reso$y), m))

  if(cond){
    do.call(text, merge_list(l, t, list(srt = srt, family = family, xpd = xpd)))
  }

}


