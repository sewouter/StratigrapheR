#' @title Joins and orders adjacent or overlapping lim objects of same ID
#'
#' @description Joins and orders adjacent or overlapping lim objects of same ID
#'
#' @param lim an object convertible into a lim object: either a vector
#' of length 2 or a list of n left (1st element) and n right (2ndt element)
#' interval limits
#' @param l a vector of n left interval limits
#' @param r a vector of n right interval limits
#' @param id a vector of n interval IDs (default is 1 for each interval)
#' @param b a character vector for the interval boundaries rules: "[]"
#' (or "closed") to include both boundaries points, "][" (or "()" and "open") to
#' exclude both boundary points, "[[" (or "[)","right-open" and"left-closed") to
#' include only the left boundary point, and "]]" (or "(]", "left-open",
#' "right-closed") to include only the right boundary point. The notation is
#' simplified to "[]", "[[", "]]" and "][" only.
#' @return a lim object of the joined intervals
#' @seealso \code{\link{as.lim}}
#' @examples
#' l   <- c(50,2,4,6,50,8,50,51,50,80)
#' r   <- c(50,5,6,9,8,2,51,51,50,80)
#' id  <- c("i1", "i1", "i1", "i1", "i2","i2","i2","i2","i2","i2")
#' b   <- c("[]", "][", "][", "]]", "][","[[","][","][","][","][")
#'
#' simp.lim(l = l, r = r, id = id, b = b)
#'
#' @importFrom dplyr arrange left_join lag
#' @export

simp.lim <- function(lim = NULL, l = NULL, r = NULL, id = 1L, b = "[]")
{
  to.order     <- as.lim(lim = lim, l = l, r = r, id = id, b = b)
  lim.function <- order.lim(lim = to.order)

  left  <- lim.function[[1]]
  right <- lim.function[[2]]
  id    <- lim.function[[3]]
  bound <- lim.function[[4]]

  bl <- bound == "[]" | bound == "[["
  br <- bound == "[]" | bound == "]]"

  dl <- data.frame(gloVar.x = left,  id = id, b = bl,
                   gloVar.i = rep(1,  length(left)), stringsAsFactors = F)

  dr <- data.frame(gloVar.x = right, id = id, b = br,
                   gloVar.i = rep(-1, length(right)), stringsAsFactors = F)

  d <- rbind(dl,dr)

  o <- arrange(d,id,gloVar.x,desc(gloVar.i))

  colnames(o)[c(1,4)] <- c("x", "i")

  cr <- cumsum(o$i)
  cl <- lag(cr, default = 0)

  o$cl <- cl
  o$cr <- cr

  o$ni <- cl == 1 | cr == 1

  dr <- which(duplicated(o[,c(1,2)]))
  dl <- which(duplicated(o[,c(1,2)], fromLast = T))

  l <- length(dr)
  d <- c(dl, dr)
  d <- d[kronecker(1:(l), c(0, l), "+")]
  d <- unique(d)

  no <- data.frame(ind = d, o[d, ], stringsAsFactors = F)
  no$comp <- paste(no$x,"V",no$id, sep = "")

  exclude <- (no$comp[no$b])

  if(length(exclude) != 0){
    no <- no[!(no$comp %in% exclude),]
  }

  keep <- (no$comp[no$ni])

  if(length(keep) != 0){
    no <- no[no$comp %in% keep,]
  }

  exclude2 <- no$comp[no$cl == 0 | no$cr == 0]

  if(length(exclude2) != 0){
    no <- no[!(no$comp %in% exclude2),]
  }

  nr <- no$ind[duplicated(no[,c(2,3)])]
  nl <- no$ind[duplicated(no[,c(2,3)], fromLast = T)]

  ln <- length(nr)
  nd <- c(nl, nr)
  nd <- nd[kronecker(1:(ln), c(0, ln), "+")]

  keep2 <- nd[!(duplicated(nd) | duplicated(nd, fromLast = T))]

  keep3 <- 1:nrow(o) %in% keep2 | o$cl == 0 | o$cr == 0

  o <- o[keep3,]

  el <- subset(o, o$i == 1)
  er <- subset(o, o$i == -1)

  nb <- data.frame(xl = el$x, xr = er$x, id = el$id, bl = el$b, br = er$b,
                   stringsAsFactors = F)

  exclude3 <- nb$xl == nb$xr & !nb$bl & !nb$br

  nb <- nb[!exclude3,]

  nb <- left_join(nb, data.frame(bl = c(T,T,F,F),
                                 br = c(T,F,T,F),
                                 b = c("[]", "[[", "]]","]["),
                                 stringsAsFactors = F),
                  by = c("bl", "br"))

  res <- as.lim(l = nb$xl, r = nb$xr, id = nb$id, b = nb$b)

  return(res)
}


