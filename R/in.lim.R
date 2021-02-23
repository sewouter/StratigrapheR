#' @title Finds the intervals encompassing values
#'
#' @description This function returns the intervals encompassing x values. This
#' works only if the intervals (as lim objects) are non overlapping and
#' non-adjacent (if certain boundaries are neighbouring, the boundary rule
#' should exclude all, or all but one)
#'
#' @param x a vector values
#' @param lim an object convertible into a lim object: either a vector
#' of length 2 or a list of n left (1st element) and n right (2ndt element)
#' interval limits. The intervals should be non-overlapping and non-adjacent.
#' @param l a vector of n left interval limits
#' @param r a vector of n right interval limits
#' @param id a vector of n interval IDs (default is 1 for each interval)
#' @param b a character vector for the interval boundaries rules: "[]"
#' (or "closed") to include both boundaries points, "][" (or "()" and "open") to
#' exclude both boundary points, "[[" (or "[)","right-open" and"left-closed") to
#' include only the left boundary point, and "]]" (or "(]", "left-open",
#' "right-closed") to include only the right boundary point.
#' @param index whether the output should be a list of the initial vector
#' and of the corresponding intervals in which they lay (index = FALSE, is the
#' default), or simply the index of the intervals in the initial lim object
#' (index = TRUE)
#'
#' @return a list of the intervals where the x values lay or a vector of their
#' index
#'
#' @seealso \code{\link{as.lim}}
#' @examples
#' x   <- c(99,1,3,5,2,4,5,6,9,4,8,20,26,52,42,24,25,12,40,10,16,17)
#'
#' lim <- as.lim(l = c(100,10,20,27), r = c(99,12,27,42), b = "]]")
#'
#' in.lim(x, lim = lim)
#'
#' in.lim(x, lim = lim, index = TRUE)
#'
#' # Applications to Stratigraphy
#'
#' proxy <- proxy.example # This is a data.frame with (fake) magnetic
#'                        # susceptibility (ms) and depth (dt)
#'
#' # Each sample was taken in a specific bed (not at the boundary between two,
#' # to make things easier). We will invoke the data of the beds (bed.example)
#' # and identify the lithology of each sample
#'
#' res <- in.lim(proxy.example$dt,  # Position of each sample
#'               l = bed.example$l, # Left boundary of the beds
#'               r = bed.example$r, # Right boundary of the beds
#'               id = bed.example$litho) # Lithology of each bed (if you wanted
#'                                       # to know the name of the bed each
#'                                       # sample is in you would have put
#'                                       # bed.example$id)
#'
#' proxy$litho <- res$id # The result provides the id (here the lithology) of
#'                       # each interval encompassing the measurement (x, here
#'                       # proxy.example$dt)
#'
#' plot(proxy$ms, proxy$dt, type = "l", xlim = c(-2*10^-8, 8*10^-8))
#'
#' shale <- subset(proxy, proxy$litho == "S")
#' points(shale$ms, shale$dt, pch = 4)
#'
#' limestone <- subset(proxy, proxy$litho == "L")
#' points(limestone$ms, limestone$dt, pch = 19)
#'
#' chert <- subset(proxy, proxy$litho == "C")
#' points(chert$ms, chert$dt, pch = 21, bg = "white")
#'
#' legend(6.2*10^-8, 25, legend = c("Shale", "Limestone", "Chert"),
#'        pch = c(4,19,21), bg = c(NA, NA, "white"))
#'
#' @importFrom dplyr arrange left_join
#' @export

in.lim <- function(x, lim = NULL, l = NULL, r = NULL, id = 1L, b = "][",
                   index = FALSE)
{

  if(index!= TRUE &
     index != FALSE){
    stop("The parameter 'index' should be T or F")
  }

  to.order <- as.lim(lim = lim, l = l, r = r, id = id, b = b)
  fii      <- order.lim(lim = to.order)

  if(!are.lim.distinct(fii)) stop("The intervals should be distinct")
  if(!are.lim.nonadjacent(fii)) stop("The intervals should be nonadjacent")

  o  <- order(fii$l)
  ni <- length(fii$l)

  fi <- list(l = fii$l[o], r = fii$r[o], id = fii$id[o], b = fii$b[o])

  # Deal with boundaries

  include.l <- fi$b == "[]" | fi$b == "[["
  include.r <- fi$b == "[]" | fi$b == "]]"

  bsr <- rep(4, ni)

  ill <- lead(include.l)

  bsr[!include.r & ill]  <- 1
  bsr[!include.r & !ill] <- 2

  bsr[!include.r & is.na(ill)] <- 0

  bsl <- rep(2, ni)

  ilr <- lag(include.r)

  bsl[!include.l & ilr]  <- 5
  bsl[!include.l & !ilr] <- 4

  bsl[!include.l & is.na(ilr)] <- 6

  dl <- data.frame(i = seq_len(ni),  x = fi$l, gloVar.bs = bsl,
                   include = include.l)
  dr <- data.frame(i = -seq_len(ni), x = fi$r, gloVar.bs = bsr,
                   include = include.r)
  dx <- data.frame(i = 0, x = x, gloVar.bs = 3, include = NA)

  d <- rbind(dl, dr, dx)

  d1 <- arrange(d, x, gloVar.bs)

  d1$inti <- cumsum(d1$i)

  d2 <- d1[d1$i == 0,c(2,5)]

  dref <- data.frame(inti = seq_len(ni), fi, index = seq_len(ni)[o])
  nref <- data.frame(inti = 0, l = NA, r = NA, id = NA, b = NA, index = NA)

  dref <- rbind(nref, dref)

  d3 <- left_join(d2, dref, by = "inti")

  if(is.unsorted(x)){
    d3 <- d3[order(order(x)),]
  }

  if(!index){

    d3  <- d3[,-c(2,7)]
    res <- as.list(d3)

  } else {

    res <- d3$index

  }

  return(res)

}
