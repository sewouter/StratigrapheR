#' @title Remove instantaneous deposits and add thickness in hiatuses
#'
#' @description Remove instantaneous deposits, or 'fills', (e.g. turbidites) and
#' add thickness estimated to be lost, or gaps' (i.e. hiatuses).
#'
#' @param dt depth or time
#' @param gap list
#' @param fill list
#' @param clean whether to set the points in fills as NA
#' @param left.side l
#' @param left.norm l
#'
#' @examples
#' dt <- as.list(tie.points.example[,2:6])
#'
#' gap  <- list()
#' fill <- list()
#'
#' gap$Charce    <- data.frame(dt = c(370,400), span = c(50,10))
#' gap$El.Porton <- data.frame(dt = -400, span = 30)
#'
#' fill$Charce     <- data.frame(l = 63, r = 65)
#' fill$El.Porton  <- data.frame(l = c(-530), r = c(-630))
#' fill$Frielingen <- data.frame(l = 20, r = 30)
#'
#' strat.repair(dt, gap, fill)
#'
#' @importFrom dplyr lead lag first last
#' @importFrom reshape merge_recurse
#' @export

strat.repair <- function(dt, gap = list(), fill = list(),
                         clean = F, left.side = T, left.norm = T)
{

  if(length(fill) == 0 & length(gap) == 0) {
    blankout <- list(dt = dt, gap = list(), fill = list())
    names(blankout$gap) <- character(0)
    names(blankout$fill) <- character(0)
    return(blankout)
  }

  dupl <- any(duplicated(names(dt))) |
    any(duplicated(names(gap))) |
    any(duplicated(names(fill)))

  if(dupl) stop('The names in the lists should be unique')

  asc.unsorted  <- unlist(lapply(dt, function(x) is.unsorted(x, na.rm = T)))
  desc.unsorted <- unlist(lapply(dt, function(x) is.unsorted(-x, na.rm = T)))

  unsorted <- (asc.unsorted & desc.unsorted)

  if(any(unsorted)){

    which.unsorted <- which(unsorted)

    stop(paste("The following section(s) has (have) its (theirs) events not",
               "ordered:", names(dt)[which.unsorted]))
  }

  if(any(!(names(gap) %in% names(dt)))) {
    stop("The names in 'gap' should match the names in 'dt'.")
  }

  if(any(!(names(fill) %in% names(dt)))) {
    stop("The names in 'fill' should match the names in 'dt'.")
  }

  if(!all(unlist(lapply(gap, inherits, "data.frame")))){
    stop("All the elements provided to 'gap' should be data frames")
  }

  if(!all(unlist(lapply(fill, inherits, "data.frame")))){
    stop("All the elements provided to 'fill' should be data frames")
  }

  if(!all(unlist(lapply(gap, function(x) colnames(x) == c("dt", "span"))))){
    stop("The data frames provided to 'gap' should have two",
         " columns, having 'dt' and 'span' as headers")
  }

  if(!all(unlist(lapply(fill, function(x) colnames(x) == c("l", "r"))))){
    stop("The data frames provided to 'fill' should have two",
         " columns, having 'l' and 'r' as headers")
  }

  if(length(left.side) == 1){

    if(!isFALSE(as.logical(left.side))) left.side <- T else left.side <- F

    left.side <- lapply(lapply(dt, length), function(x) rep(left.side, x))

  } else {
    if(!inherits(left.side, "list")){
      stop("The 'left.side' parameter should be a logical of length 1 or a list.")
    }

    if(any(unlist(lapply(left.side, length)) != unlist(lapply(dt, length)))){
      stop("If 'left.side' is provided as a list, it should have",
           " the same organisation than 'dt'.")
    }

    if(any(names(left.side) != names(dt))){
      stop("If 'left.side' is provided as a list, it should have",
           " the same names than 'dt', in the same order.")
    }

    if(any(!unlist(lapply(left.side, inherits, "logical")))){
      stop("The elements in the 'left.side' list should be logicals")
    }

    if(any(unlist(lapply(left.side, function(x) any(is.na(x)))))){
      stop("There should be no NA value in 'left.side'")
    }
  }

  # ----

  sections <- unique(c(names(fill), names(gap)))

  change <- names(dt) %in% sections

  io <- names(dt)[change]

  ch.dt   <- dt[change]
  ch.left.side <- left.side[change]

  ch.desc <- desc.unsorted[change]
  ch.desc[ch.desc]  <- 1
  ch.desc[!ch.desc] <- -1

  ch.l <- lapply(ch.dt, length)

  df <- data.frame(section = rep(names(ch.dt), unlist(ch.l)),
                   dt = unlist(ch.dt, use.names = F),
                   left.side = unlist(ch.left.side, use.names = F),
                   cor1 = rep(ch.desc, unlist(ch.l)),
                   stringsAsFactors = F)

  df$cdt <- df$dt * df$cor1

  do.the.split <- split(df$cdt, df$section)

  cormax  <- unlist(lapply(do.the.split, max, na.rm = T), use.names = F)
  cormin  <- unlist(lapply(do.the.split, min, na.rm = T), use.names = F)
  cor2    <- lag(cormax) - cormin
  cor2[1] <- 0
  cor2    <- cumsum(cor2) + seq_len(length(cor2)) - 1

  df$cor2 <- rep(cor2, unlist(ch.l))
  df$glovar.wdt  <- df$cdt + df$cor2

  df$id    <- "data"
  df$glovar.order <- 3
  df$sum   <- 0

  df$glovar.order[df$left.side]  <- -2
  df$glovar.order[!df$left.side] <- 2

  sdf <- df[,c(1,2,7,4,8,9,10)]

  keep <- !is.na(sdf$glovar.wdt)

  sdf     <- sdf[keep,]
  sdf$span <- 0

  # ----

  if(length(gap) != 0){

    gsid <- rep(names(gap), unlist(lapply(gap, nrow)))
    gd1  <- merge_recurse(gap)

    gd <- data.frame(section = gsid, dt = gd1$dt, span = gd1$span,
                     stringsAsFactors = F)

    match.gap  <- match(gd$section, io)

    gd$cor1 <- ch.desc[match.gap]

    gd$min <- cormin[match.gap] * gd$cor1
    gd$max <- cormax[match.gap] * gd$cor1

    gless <- !((gd$min * gd$cor1) < (gd$dt * gd$cor1))
    gmore <- !((gd$max * gd$cor1) > (gd$dt * gd$cor1))

    if(any(gless) | any(gmore)){
      stop("One or several gaps are defined outside or at the border of the",
           " range of 'dt' in the following section(s):\n",
           unique(c(gd$section[gless], gd$section[gless])))
    }

    if(any(duplicated(gd$glovar.wdt))){
      stop("There should not be multiple gaps at the same position")
    }

    gd$cor2 <- cor2[match.gap]
    gd$glovar.wdt  <- gd$dt * gd$cor1 + gd$cor2

    dgf       <- gd[rep(seq_len(nrow(gd)),each = 2),]
    dgf$id    <- rep(c("gap_l", "gap_r"), nrow(gd))
    dgf$glovar.order <- rep(c(-1, 1), nrow(gd))
    dgf$sum   <- 0

    dgf$span[seq_len(nrow(gd)) * 2]  <- 0

    sgf <- dgf[,c(1,2,8,4,9,10,11,3)]

  } else {

    sgf <- data.frame(matrix(nrow = 0, ncol = 8))
    colnames(sgf) <- c("section", "dt", "glovar.wdt", "cor1", "id",
                       "glovar.order", "sum", "span")

  }

  # ----

  if(length(fill) != 0){

    fsid <- rep(names(fill), unlist(lapply(fill, nrow)))
    fd1  <- merge_recurse(fill)

    fd2 <- order.lim(l = fd1$l, r = fd1$r, id = fsid)

    fd <- data.frame(section = fd2$id, l  = fd2$l,  r    = fd2$r,
                     stringsAsFactors = F)

    if(any(fd$l == fd$r)) {
      stop("The 'l' and 'r' elements of the 'fill' list should define intervals of",
           " non-null thickness\n       (i.e. 'l' should be different of 'r' for",
           " each row)")
    }

    match.fill <- match(fd$section, io)

    fdcor1 <- ch.desc[match.fill]

    intermediary <- fd

    intermediary$l[fdcor1 < 0] <- fd$r[fdcor1 < 0]
    intermediary$r[fdcor1 < 0] <- fd$l[fdcor1 < 0]

    fd <- intermediary

    fd$cor1 <- fdcor1

    fd$min <- cormin[match.fill] * fd$cor1
    fd$max <- cormax[match.fill] * fd$cor1

    flless <- !((fd$min * fd$cor1) <= (fd$l * fd$cor1))
    frless <- !((fd$min * fd$cor1) <= (fd$r * fd$cor1))

    flmore <- !((fd$max * fd$cor1) >= (fd$l * fd$cor1))
    frmore <- !((fd$max * fd$cor1) >= (fd$r * fd$cor1))

    if(any(flless) | any(frless) | any(flmore) | any(frmore)){

      where.is.it <- unique(c(fd$section[flless], fd$section[frless],
                              fd$section[flless], fd$section[frless]))

      stop("One or several fills are defined outside the",
           " range of 'dt' in the following section(s):\n",
           where.is.it)
    }

    fd$cor2 <- cor2[match.fill]
    fd$wl   <- fd$l * fd$cor1 + fd$cor2
    fd$wr   <- fd$r * fd$cor1 + fd$cor2

    if(!are.lim.distinct(l = fd$wl, r = fd$wr)){
      stop("The intervals defined by 'fills' should not be overlapping one another")
    }

    sfd <- data.frame(section = fd$section[rep(seq_len(nrow(fd)), each = 2)],
                      dt = c(fd$l, fd$r)[seq_mult(2 * nrow(fd), 2, inv = T)],
                      glovar.wdt = c(fd$wl, fd$wr)[seq_mult(2 * nrow(fd), 2, inv = T)],
                      cor1 = c(fd$cor1)[rep(seq_len(nrow(fd)), each = 2)])

    sfd$id    <- rep(c("fill_l", "fill_r"), nrow(fd))
    sfd$glovar.order <- rep(c(-1, 1), nrow(fd))
    sfd$sum   <- rep(c(-1, 1), nrow(fd))
    sfd$span  <- 0


  } else {

    sfd <- data.frame(matrix(nrow = 0, ncol = 8))
    colnames(sgf) <- c("section", "dt", "glovar.wdt", "cor1", "id",
                       "glovar.order", "sum", "span")

  }

  # ----

  s <- rbind(sdf, sgf, sfd)

  sw <- arrange(s, glovar.wdt, glovar.order)

  sw$int  <- lead(sw$glovar.wdt) - sw$glovar.wdt
  sw$nint <- sw$int

  sw$infil <- cumsum(sw$sum)

  sw$nint[0 > sw$infil] <- 0
  sw$nint               <- sw$span + sw$nint

  sw$ldt <- cumsum(c(0, sw$nint[-nrow(sw)])) * sw$cor1

  split2 <- split(sw$ldt, sw$section)
  split0 <- split(sw$dt, sw$section)

  if(left.norm){
    recor <- unlist(lapply(split0, first)) - unlist(lapply(split2, first))
  } else {
    recor <- unlist(lapply(split0, last)) - unlist(lapply(split2, last))
  }

  sw$ndt <- sw$ldt + rep(recor, unlist(lapply(split2, length)))

  if(clean) sw$ndt[sw$infil < 0 & sw$id == "data"] <- NA

  outdata           <- df[,c(1,2)]
  outdata$ndt       <- outdata$dt
  outdata$ndt[keep] <- sw$ndt[sw$id == "data"]

  ofl       <- sw[sw$id == "fill_l" | sw$id == "fill_r",]
  ofl$nspan <- lead(ofl$glovar.wdt) - ofl$glovar.wdt
  outfill   <- ofl[seq_len(nrow(ofl)/2) * 2 - 1,c(1,13,14)]

  ofl1            <- outfill[,c(2,3)]
  colnames(ofl1)  <- c("dt", "span")

  ogp  <- sw[sw$id == "gap_l" | sw$id == "gap_r",]
  extr <- seq_len(nrow(ogp)/2) * 2

  outgap <- data.frame(section = ogp$section[extr],
                       l = ogp$ndt[extr-1], r = ogp$ndt[extr])

  outdata1 <- split(outdata$ndt, outdata$section)
  outfill1 <- split(ofl1, outfill$section)
  outgap1  <- split(outgap[,c(2,3)], outgap$section)

  outfill1 <- lapply(outfill1,
                     function(x) {
                       row.names(x) <- NULL
                       x
                     })

  outgap1 <- lapply(outgap1,
                    function(x) {
                      row.names(x) <- NULL
                      x
                    })

  outdt         <- dt
  outdt[change] <- outdata1

  res <- list(dt = outdt, gap = outgap1, fill = outfill1)

  return(res)

}
