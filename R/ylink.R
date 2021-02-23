#' Draws connection lines to connect two points in y
#'
#' @param y1,y2 y positions (you can provide several ones at once)
#' @param x1,x2 x positions (you can provide several ones at once)
#' @param ratio the ratio of the breaking points of the lines (from the
#' start or end to the centre)
#' @param xi1,xi2 x positions of the
#' breaking points of the lines.
#' @param l  a list of arguments to feed lines(). Go see ?lines to know
#' which arguments can be provided. See ?merge.list for further information.
#'
#' @seealso \code{\link{multilines}}, \code{\link{bedtext}},
#' \code{\link{infobar}} and \code{\link{nlegend}}
#'
#' @examples
#' plot(c(0,6),c(-20,20), type = "n")
#'
#' infobar(ymin = c(-20,0), ymax = c(0,20), xmin = 1, xmax = 0,
#'         m = list(col = c("black", "white")))
#'
#' infobar(ymin = c(-20,10), ymax = c(10,20), xmin = 5, xmax = 6,
#'         m = list(col = c("black", "white")))
#'
#' ylink(c(0,12),c(10,20), x1 = 1, x2 = 5, ratio = 0.2,
#'       l = list(lty = c(1,3), lwd = 2))
#'
#' @export
#'
ylink <- function(y1,y2,x1,x2, ratio = 0.1, xi1 = NA, xi2 = NA,
                  l = list(lty = 3))
{
  d <- list(y1 = y1, y2 = y2, x1 = x1, x2 = x2,
            ratio = ratio, xi1 = xi1, xi2 = xi2)

  s1 <- unlist(lapply(d,length))
  s2 <- unlist(lapply(l,length))

  sa <- c(s1,s2)

  m <- max(sa)

  if(!all(sa == m | sa == 1)){
    stop(paste("The parameters or elements in the lists of parameters",
               " should be of length 1 or n", sep = ""))
  }

  if(m > 1){
    d[which(s1 == 1)] <- lapply(d[which(s1 == 1)],rep,m)
    l[which(s2 == 1)] <- lapply(l[which(s2 == 1)],rep,m)
  }

  xin <- abs((d$x2 - d$x1))

  if(length(xi1) == 1 & is.na(xi1[[1]])){

    xs <- pmin(d$x1,d$x2)
    d$xi1 <- xs + ratio*xin*0.5

  }

  if(length(xi2) == 1 & is.na(xi2[[1]])){

    xe <- pmax(d$x1,d$x2)
    d$xi2 <- xe - ratio*xin*0.5

  }

  x <- data.frame(x1 = d$x1, xi1 = d$xi1, xi2 = d$xi2, x2 = d$x2)
  y <- data.frame(y1 = d$y1, yi1 = d$y1, yi2 = d$y2, y2 = d$y2)

  for(i in seq_len(m))
  {

    xi <- list(x[i,])
    yi <- list(y[i,])
    li <- lapply(l, `[`, i)

    do.call(lines,c(xi,yi,li))

  }

}
