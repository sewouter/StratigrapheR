#' @title Writes the names of the beds in a litholog
#'
#' @description Writes the names of the beds in a litholog. You can either place
#' them at the centre of the beds or in their upper and lower part. You can
#' also define a thickness below which the name won't be written, to avoid
#' excessive text crowding the plot.
#'
#' @param labels the name of each bed
#' @param l a vector of n left y (or dt, i.e. depth or time) interval
#' limits for each bed
#' @param r a vector of n right y (or dt, i.e. depth or time) interval
#' limits for each bed
#' @param x the position where to write the text (0.2 by default)
#' @param arg a list of arguments to feed text(). Go see ?text to know
#' which arguments can be provided. See ?merge.list for further information.
#' @param adj one or two values in [0, 1] which specify the x (and
#' optionally y) adjustment of the labels. c(0.5,0.5) is the default.
#' @param ymin minimum thickness of the bed to write its name (if NA,
#' a default value is calculated, but user input is best)
#' @param edge whether to put the bed name at the edge of the beds (T)
#' or in the center of the beds (F, is the default)
#'
#' @seealso \code{\link{litholog}} obvisously
#'
#' if your boundaries have to be recalculated: \code{\link{leftlog}}
#'
#' other functions complementing litholog: \code{\link{infobar}} and
#' \code{\link{ylink}}
#'
#' @examples
#' l  <- c(0,4,5,8)
#' r  <- c(4,5,8,16)
#'
#' x   <- c(4,5,3,4)
#' i <- c("B1","B2","B3","B4")
#'
#' test <- litholog(l, r, x, i)
#'
#' whiteSet(xlim = c(0,6), ylim = c(-10,30))
#'
#' multigons(test$i, test$xy, test$dt, col = c(NA, "black", "grey","NA"))
#'
#' bedtext(labels = i, r = r, l = l, edge = TRUE, x = 0.5,
#'         arg = list(col = c("black", "white", "white", "red")))
#'
#' @export

bedtext <- function(labels, l, r, x = 0.2, arg = list(cex = 1),
                    adj = c(0.5,0.5), ymin = NA, edge = FALSE)
{

  labels <- as.character(labels)

  if(is.na(ymin)){

    ymin <- mean(abs(l - r), na.rm = T)/3

  }

  # CHeck arguments ----

  li <- length(labels)

  larg <- unlist(lapply(arg,length))

  if(any(!(larg == 1 | larg == li))){
    stop(paste("The arguments beside 'ymin', 'edge' and 'adj' should be of",
               " length 1 or n", sep = ""))
  }

  am <- data.frame(arg[which(larg == li)], stringsAsFactors = F)

  au <- data.frame(arg[which(larg == 1)], stringsAsFactors = F)
  au <- au[rep(1,li),,drop = F]

  if(ncol(am) != 0 & ncol(au) != 0){
    a <- cbind(am,au)
  } else if (ncol(am) == 0 & ncol(au) != 0){
    a <- au
  } else if (ncol(am) != 0 & ncol(au) == 0){
    a <- am
  }

  # Set things ----

  if(!(edge == F | edge == T)) {
    stop("The 'edge' parameter should be T or F")
  }

  lim <- as.lim(l = l, r = r)

  d   <- data.frame(j = 1:li, i = labels, l = lim[[1]], r = lim[[2]], x = x)

  ex  <- which(is.na(d$i) | is.na(d$l) | is.na(d$r))

  if(length(ex) != 0) d <- d[-ex,]

  n <- nrow(d)

  # Suppress and divide ----

  if(n != 0){

    d["t"] <- abs(d$r - d$l)

    d <- subset(d, d$t >= ymin)

    d["m"] <- (d$l + d$r)/2

    nd <- nrow(d)

    if(nd != 0)
    {

      if(!edge){

        ca <- a[d$j,,drop = F]

        lc <- merge_list(list(x = d$x, y = d$m, labels = d$i, adj = adj),
                         as.list(ca))

        do.call(text, lc)

      } else if (edge) {

        ds <- subset(d, d$t <  4 * ymin)
        dd <- subset(d, d$t >= 4 * ymin)

        if(nrow(dd) != 0){

          da <- a[dd$j,,drop = F]

          l1 <- merge_list(list(x = dd$x, y = pmax(dd$l,dd$r) - ymin,
                                labels = dd$i, adj = adj), as.list(da))

          l2 <- merge_list(list(x = dd$x, y = pmin(dd$l,dd$r) + ymin,
                                labels = dd$i, adj = adj), as.list(da))

          do.call(text, l1)
          do.call(text, l2)

        }

        if(nrow(ds) != 0){

          sa <- a[ds$j,,drop = F]

          ls <- merge_list(list(x = ds$x, y = ds$m, labels = ds$i, adj = adj),
                           as.list(sa))

          do.call(text, ls)
        }
      }
    }
  }
}
