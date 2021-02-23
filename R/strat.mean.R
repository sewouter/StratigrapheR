#' @title Extrapolate and intrapolate tie points
#'
#' @description Extrapolate and intrapolate of stratigraphical tie points or
#' events, based on their position in different sections
#'
#' @param dt a matrix of depth (or time) of the different tie points. Columns
#' are for the sections, rows for each tie point
#' @param events the name of the tie points
#' @param sections the name of the sections
#'
#' @examples
#' dt     <- tie.points.example[,2:6]
#' events <- tie.points.example[,1]
#'
#' strat.mean(dt = dt, events = events)
#'
#' @importFrom dplyr lead lag
#' @export

strat.mean <- function(dt, events = NULL, sections = NULL){

  if(is.null(events)) events   <- row.names(dt)
  if(is.null(sections)) sections <- colnames(dt)

  if(length(events) != nrow(dt)) stop("Incorrect amount of events")
  if(length(sections) != ncol(dt)) stop("Incorrect amount of sections")

  absent.events <- apply(dt, 1, function(x) all(is.na(x)))

  if(any(absent.events)){

    where.absent <- which(absent.events)

    warning(paste("The following event(s) is (are) missing in all sections",
                  "and is (are) removed from the analysis:", events[where.absent]))

    dt     <- dt[!absent.events,]
    events <- events[!absent.events]

  }

  asc.unsorted  <- apply(dt, 2, function(x) is.unsorted(x, na.rm = T))
  desc.unsorted <- apply(dt, 2, function(x) is.unsorted(-x, na.rm = T))

  unsorted <- (asc.unsorted & desc.unsorted)

  l <- nrow(dt)

  if(any(unsorted)){

    which.unsorted <- which(unsorted)

    stop(paste("The following section(s) has (have) its (theirs) events not",
               "ordered:", sections[which.unsorted]))
  }

  dt[,!desc.unsorted] <- -dt[,!desc.unsorted,drop = F]

  find.position <- function(position)
  {

    present <- as.data.frame(!is.na(position))

    colnames(present) <- NULL
    row.names(present) <- NULL

    list.pos.0 <- apply(present, 2, which)

    return(list.pos.0)

  }

  find.commons <- function(position, complete)
  {

    present <- which(complete)
    absent  <- which(!complete)

    list.pos.2 <- find.position(position)

    present.list <- rep(list.pos.2[present], length(absent))
    absent.list  <- rep(list.pos.2[absent], each = length(present))

    comb.list <- mapply(c, present.list, absent.list)

    comb.list.a <- lapply(comb.list, function(x) x[first(which(duplicated(x)))])
    comb.list.b <- lapply(comb.list, function(x) x[last(which(duplicated(x)))])

    commons <- list()

    commons$l <- matrix(unlist(comb.list.a), ncol = length(present), byrow = T)
    commons$r <- matrix(unlist(comb.list.b), ncol = length(present), byrow = T)

    # Removable ----
    # colnames(commons$l) <- sections[present]
    # colnames(commons$r) <- sections[present]
    # row.names(commons$l) <- sections[absent]
    # row.names(commons$r) <- sections[absent]
    # ----

    z.int <- commons$l == commons$r

    commons$l[z.int] <- NA
    commons$r[z.int] <- NA

    return(commons)

  }

  # position <- pos
  # to.first <- F

  extreme.to.most.common <- function(position, to.first = T)
  {

    list.pos.1 <- find.position(position)

    if(isTRUE(to.first)){
      ex.pos   <- sapply(list.pos.1, first)
      complete <- ex.pos == 1
    } else {
      ex.pos   <- sapply(list.pos.1, last)
      complete <- ex.pos == l
    }

    commons <- find.commons(position = position, complete = complete)

    is.match <- !is.na(commons$r)

    num <- rowSums(is.match)
    names(num) <- NULL

    if(all(num == 0)) {
      stop("\n No common interval could be defined to link the section(s) of: ",
           paste(sections[!complete], collapse = ", "),
           "\n to the sections of: ",
           paste(sections[complete], collapse = ", "))
    }

    in.absent     <- which(num == max(num))
    in.absent.pos <- which(!complete)[in.absent]
    in.absent.all <- rep(in.absent.pos,
                         each = max(num))

    in.present.inter <- as.vector(is.match[in.absent,])

    lcom <- as.vector(t(commons$l[in.absent,]))[in.present.inter]
    rcom <- as.vector(t(commons$r[in.absent,]))[in.present.inter]

    in.present.all   <- rep(which(complete),
                            length(in.absent))[in.present.inter]

    df <- data.frame(matrix(ncol = 0, nrow = length(in.absent.all)))

    df$absent.ind  <- in.absent.all
    df$present.ind <- in.present.all

    df$absent <- sections[df$absent.ind]
    df$ref    <- sections[df$present.ind]

    df$lcom <- lcom
    df$rcom <- rcom

    select.l.abs <- as.matrix(df[c(5,1)])
    select.r.abs <- as.matrix(df[c(6,1)])

    select.l.ref <- as.matrix(df[c(5,2)])
    select.r.ref <- as.matrix(df[c(6,2)])

    df$l.abs <- position[select.l.abs]
    df$r.abs <- position[select.r.abs]

    if(isTRUE(to.first)){
      df$e.ref <- unlist(position[1,])[df$present.ind]
    } else {
      df$e.ref <- unlist(position[l,])[df$present.ind]
    }

    df$l.ref <- position[select.l.ref]
    df$r.ref <- position[select.r.ref]

    if(isTRUE(to.first)){
      df$ratio <- (df$r.ref - df$l.ref)/(df$l.ref - df$e.ref)
      df$e.abs <- df$l.abs - ((df$r.abs - df$l.abs)/df$ratio)
    } else {
      df$ratio <- (df$r.ref - df$l.ref)/(df$e.ref - df$r.ref)
      df$e.abs <- df$r.abs + ((df$r.abs - df$l.abs)/df$ratio)
    }

    mat.ratio <- matrix(df$ratio, nrow = length(in.absent), byrow = T)
    mat.est   <- matrix(df$e.abs, nrow = length(in.absent), byrow = T)

    new.end <- rowSums(mat.ratio * mat.est)/rowSums(mat.ratio)

    new.pos <- position

    if(isTRUE(to.first)){
      select.pos <- matrix(c(rep(1, length(in.absent)), in.absent.pos), ncol = 2)
    } else {
      select.pos <- matrix(c(rep(l, length(in.absent)), in.absent.pos), ncol = 2)
    }

    new.pos[select.pos] <- new.end

    # FALSE ----
    # new.pos[2,3] <- 250
    # new.pos[2,1] <- 0
    # new.pos[8,2] <- 50
    # FALSE ----

    if(isTRUE(to.first)){
      control.where <- apply(new.pos[,in.absent.pos,drop = F], 2, which.min)
      control <- control.where != 1
    } else {
      control.where <- apply(new.pos[,in.absent.pos,drop = F], 2, which.max)
      control <- control.where != l
    }

    if(any(control)){

      ori.pos <- new.pos
      ori.pos[,!desc.unsorted] <- -ori.pos[,!desc.unsorted,drop = F]

      ne <- new.end
      ne[!desc.unsorted[in.absent.pos]] <- -ne[!desc.unsorted[in.absent.pos]]

      prob.sections <- sections[in.absent.pos[which(control)]]

      compete <- events[control.where[control]]

      compete.pos <- ori.pos[matrix(c(control.where[control],
                                      in.absent.pos[which(control)]), ncol = 2)]

      if(isTRUE(to.first)){
        stop(paste0("\nThe prediction of missing values failed.",
                    "\nThis happens for the following section(s): ",
                    paste(prob.sections, collapse = ", "),
                    " \nIts (their) predicted position(s) for event ",
                    events[1], " is (are respectively) at ",
                    paste(ne[which(control)], collapse = ", "),
                    "\nwhereas the stratigraphically higher event(s) ",
                    paste(compete, collapse = ", "),
                    " is (are respectively) found lower, at ",
                    paste(compete.pos, collapse = ", ")))
      } else {
        stop(paste0("\nThe prediction of missing values failed.",
                    "\nThis happens for the following section(s): ",
                    paste(prob.sections, collapse = ", "),
                    " \nIts (their) predicted position(s) for event ",
                    events[l], " is (are respectively) at ",
                    paste(ne[which(control)], collapse = ", "),
                    "\nwhereas the stratigraphically lower event(s) ",
                    paste(compete, collapse = ", "),
                    " is (are respectively) found higher, at ",
                    paste(compete.pos, collapse = ", ")))
      }


    }

    return(new.pos)

  }

  posf <- dt

  if(any(is.na(dt[1,]))){

    repeat{

      posf <- extreme.to.most.common(posf)

      if(!any(is.na(posf[1,]))) break

    }
  }

  posl <- dt

  if(any(is.na(dt[l,]))){

    repeat{

      posl <- extreme.to.most.common(posl, F)

      if(!any(is.na(posl[l,]))) break

    }
  }

  pose <- dt
  pose[1,] <- posf[1,]
  pose[l,] <- posl[l,]

  minus <- matrix(unlist(rep(pose[1,], l)), nrow = l, byrow = T)
  maxus <- matrix(unlist(rep(pose[l,], l)), nrow = l, byrow = T)

  normal <- (pose - minus)/(maxus - minus)

  interval <- lead(normal) -  normal
  interval <- interval[-l, ]

  composite        <- cumsum(c(0, apply(interval, 1, mean, na.rm = T)))
  names(composite) <- events

  composite <- composite/composite[l]

  n.mis <- is.na(normal)

  if(any(n.mis)){

    row.names(n.mis) <- NULL

    down <- lead(n.mis) & !n.mis
    up   <- lag(n.mis) & !n.mis

    down.i <- apply(down, 2, which)
    up.i   <- apply(up, 2, which)

    l.l <- unlist(lapply(down.i, length))
    sec <- unlist(mapply(rep, seq_len(length(l.l)), l.l))

    d1 <- data.frame(section = sec, l = unlist(down.i), r = unlist(up.i))

    select.l <- as.matrix(d1[,c(2,1)])
    select.r <- as.matrix(d1[,c(3,1)])

    d1$l.pos <- pose[select.l]
    d1$r.pos <- pose[select.r]

    d1$l.comp <- composite[d1$l]
    d1$r.comp <- composite[d1$r]

    d1$ratio <- (d1$r.pos - d1$l.pos)/(d1$r.comp - d1$l.comp)

    ref      <- matrix(rep(composite, nrow(d1)), nrow = nrow(d1), byrow = T)

    extended <- (ref - d1$l.comp) * d1$ratio + d1$l.pos

    simili <- matrix(rep(1:9, nrow(extended)), nrow = nrow(extended), byrow = T)

    out <- (simili - d1$l - 0.5) < 0 | (simili - d1$r + 0.5) > 0

    wo <- which(!out, arr.ind = T)

    mo <- matrix(c(wo[,2], d1$section[wo[,1]]), ncol = 2)

    pose1 <- pose

    pose1[mo] <- extended[wo]

  } else {

    pose1 <- pose

  }

  pose1[,!desc.unsorted] <- -pose1[,!desc.unsorted,drop = F]

  res <- list()

  res$dt <- cbind(data.frame(Composite = composite), pose1)

  initial <- cbind(data.frame(Composite = rep(F, nrow(pose))), !is.na(dt))
  row.names(initial) <- events

  res$initial <- initial

  return(res)

}






