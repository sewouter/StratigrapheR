#' @title Draws several polygons
#'
#' @description Draws several polygons. This function expands on the polygon()
#' function from base R graphics. The difference is that several polygons can
#' be drawn in one line by providing a polygon id: i. To each polygon you can
#' provide different graphical parameters (i.e. colour, shading, etc). On the
#' contrary of the polygon() function the graphical parameters of the shading
#' lines can be independent of the border lines.
#'
#' @param i a polygon id for each x and y coordinate, i.e. the name of
#' each polygon. If you want to give each polygon a different aspect you should
#' provide a vector of n elements (if you have three polygons "A1", "A2" and
#' "A3" with "A2" that should be blue you should provide the colours of all
#' three: e.g. \code{col = c("white", "blue", "white")})
#' @param x,y numeric vectors of x and y coordinates
#' @param j a list of the ids (names) in the order used for the
#' graphical parameters (e.g. colour, shading, etc...). By default they are in
#' their order of appearance in \code{i}
#' @param forget the polygons that should not be drawn, by their id or
#' index (i.e. name or number of appearance).
#' @param front,back the polygons to be put in front and back position,
#' by their id or index (i.e. name or number of appearance). By default the
#' order is the one defined by \code{j}, and if \code{j} is absent by the order
#' in \code{i}.
#' @param density the density of shading lines, in lines per inch. The
#' default value of NA means that no shading lines are drawn. A zero value of
#' density means no shading nor filling whereas negative values and NA suppress
#' shading.
#' @param angle the slope of shading lines, given as an angle in degrees
#' (counter-clockwise).
#' @param border the colour to draw the border. The default is black. Use
#' border = NA to omit borders.
#' @param col the colour for filling the polygon. The default, NA, is to
#' leave polygons unfilled.
#' @param lty,lwd the border line type and width, see ?par for details.
#' @param scol,slty,slwd the colour, type and width of the shading lines.
#' @param lend,ljoin,lmitre additional graphical parameters, see ?par
#' for details.
#'
#' @details In the case you want shading this function will draw three
#' overlapping polygons: one for the background, one for the shading lines and
#' one for the border. \code{multigons} shares similarities with
#' \code{\link{centresvg}} and \code{\link{framesvg}}, but allows more advanced
#' control of each element.
#'
#' @seealso Similar functions: \code{\link{multilines}}, \code{\link{infobar}}
#'
#' Complementary function: \code{\link{shift}}
#'
#' Uses \code{\link{ignore}} to avoid drawing unnecessary objects
#'
#' @examples
#' # Simple use:
#'
#' i <- c(rep("A1",6), rep("A2",6), rep("A3",6)) # Polygon ids
#' x <- c(1,2,3,3,2,1,2,3,4,4,3,2,3,4,5,5,4,3)   # x coordinates
#' y <- c(1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6)   # y coordinates
#'
#' plot(c(-1,7), c(-2,9), type = "n", xlab = "", ylab = "", main = "Simple use")
#'
#' multigons(i, x, y,
#'           front = "A2", # This gets the polygon A2 in front of all others
#'           density = c(NA, 5, 10),  # Different shading density
#'           scol = "darkred", # Same shading colour
#'           col = c("black", "grey", "white"), # Different background colour
#'           lwd = 2, # Width of border lines for all polygons
#'           slty = 2, # Shading lines type, same for all polygons
#'           slwd = 1) # Shading lines width, same for all polygons
#'
#' # Advanced use:
#' # Lets first create more polygons
#'
#' i2 <- c(i, rep("A4",6), rep("A5",6), rep("A6",6))
#' x2 <- rep(x,2)
#' y2 <- c(y, y - 4)
#'
#' # Then lets attribute a group to each of them: lets say blue and red polygons
#'
#' groups <- data.frame(j = c("A1", "A2", "A3", "A4", "A5","A6"),
#'                       group = c("blue", "red", "blue", "red", "red", "blue"),
#'                       stringsAsFactors = FALSE)
#'
#' # Then lets attribute different graphical parameters for each group
#'
#' legend <- data.frame(group = c("red", "blue"),
#'                      col = c("red", "blue"),
#'                      density = c(10,20),
#'                      scol = c("darkred", "darkblue"),
#'                       stringsAsFactors = FALSE)
#'
#'  # Now that you have a data frame saying which polygon is in which group,
#'  # and one providing distinct graphical parameters for each group, you can
#'  # join the two with help of the dplyr package:
#'
#'  library(dplyr)
#'
#'  parameters <- left_join(groups, legend, by = "group")
#'
#'  # Then simply apply them to multigons:
#'
#'  plot(c(0,12), c(-3,7), type = "n", xlab = "", ylab = "",
#'       main = "Advanced use")
#'
#'  multigons(i2,x2,y2,
#'            forget = c("A1"),     # If you want to avoid drawing one polygon
#'            front = c("A2","A3"), # Puts A2 in front and A3 right behind
#'            col = parameters$col,
#'            density = parameters$density,
#'            scol = parameters$scol,
#'            lwd = 2)
#'
#' # Another way (more advanced, but with interesting programming applications)
#' # to code this:
#'
#' all.parameters <- merge_list(list(i = i2, x = x2 + 6, y = y2),
#'                              as.list(parameters),
#'                              list(lwd = 3, slwd = 2, slty = 2))
#'
#' all.parameters <- all.parameters[!names(all.parameters) == "group"]
#'
#' do.call(multigons, all.parameters)
#'
#' @export

multigons <- function(i, x, y, j = unique(i),
                      forget = NULL, front = NULL, back = NULL,
                      density = NA, angle = 45, border = "black", col = NA,
                      lty = par("lty"), lwd = par("lwd"),
                      scol = border, slty = lty, slwd = lwd,
                      lend = 0, ljoin = 0, lmitre = 10)
{

  i <- as.character(i)

  j <- as.character(j)

  if(inherits(lty, "character")){
    lty[lty == "blank"]    <- 0
    lty[lty == "solid"]    <- 1
    lty[lty == "dashed"]   <- 2
    lty[lty == "dotted"]   <- 3
    lty[lty == "dotdash"]  <- 4
    lty[lty == "longdash"] <- 5
    lty[lty == "twodash"]  <- 6

    lty <- as.integer(lty)
  }

  if(inherits(slty, "character")){
    slty[slty == "blank"]    <- 0
    slty[slty == "solid"]    <- 1
    slty[slty == "dashed"]   <- 2
    slty[slty == "dotted"]   <- 3
    slty[slty == "dotdash"]  <- 4
    slty[slty == "longdash"] <- 5
    slty[slty == "twodash"]  <- 6

    slty <- as.integer(slty)
  }

  argi <- list(density = density, angle = angle, border = border, col = col,
               lty = lty, lwd = lwd, scol = scol, slty = slty, slwd = slwd,
               lend = lend, ljoin = ljoin, lmitre = lmitre)

  draw <- ignore(i = i, x = x, y = y, j = j, arg = argi)

  j  <- draw$j
  lj <- length(j)

  # if(lj != 0){

  d <- data.frame(i = draw$i, x = draw$x, y = draw$y, stringsAsFactors = F)

  arg  <- draw[-c(1:4)]
  larg <- unlist(lapply(arg,length))

  if(any(!(larg == 1 | larg == lj))){
    stop("The arguments beside 'i', 'x' and 'y' should be of length 1 or n")
  }

  am <- data.frame(arg[which(larg == lj)], stringsAsFactors = F)

  au <- data.frame(arg[which(larg == 1)], stringsAsFactors = F)
  au <- au[rep(1,lj),]

  if(lj == 1) {
    a <- am
  } else if(ncol(am) != 0 & ncol(au) != 0){
    a <- cbind(am,au)
  } else if (ncol(am) == 0 & ncol(au) != 0){
    a <- au
  } else if (ncol(am) != 0 & ncol(au) == 0){
    a <- am
  }

  ca <- colnames(a)

  fca <- which(ca == "border" | ca ==  "col" | ca ==  "lty" | ca == "lwd" |
                 ca ==  "lend" |  ca ==  "ljoin" | ca == "lmitre")

  fa <- a[,fca,drop = F]

  shading <- !(is.na(density[[1]]) & length(density) == 1)

  if(shading){

    sca <- which(ca == "density" | ca ==  "angle" | ca ==  "scol" |
                   ca ==  "slty"   | ca ==  "slwd"  | ca ==  "lend" |
                   ca ==  "ljoin" | ca == "lmitre" )

    sa <- a[,sca,drop = F]

    colnames(sa)[colnames(sa) == "scol"]   <- "col"
    colnames(sa)[colnames(sa) == "slty"]   <- "lty"
    colnames(sa)[colnames(sa) == "slwd"]   <- "lwd"

    bca <- which(ca == "border" | ca == "lty" | ca == "lwd" | ca ==  "lend"  |
                   ca ==  "ljoin" | ca == "lmitre" )

    ba <- a[,bca,drop = F]

  } else {

    placebo <- TRUE

  }

  # Order of drawing ----

  transformers <- c(front, back, forget)

  if(any(!is.na(suppressWarnings(as.numeric(transformers))))){

    num.front  <- suppressWarnings(as.numeric(front))
    num.back   <- suppressWarnings(as.numeric(back))
    num.forget <- suppressWarnings(as.numeric(forget))

    front[!is.na(num.front)]   <- j[num.front[!is.na(num.front)]]
    back[!is.na(num.back)]     <- j[num.back[!is.na(num.back)]]
    forget[!is.na(num.forget)] <- j[num.forget[!is.na(num.forget)]]

  }

  transformers <- c(front, back, forget)

  if(any(duplicated(transformers))) {
    stop(paste("There should be no shared elements between ",
               "the 'front', 'back' and 'forget' parameters", sep = ""))
  }

  if(!any(transformers %in% unique(draw$i))){

    o <- seq_len(lj)

  } else {

    unj <- unique(j)

    lose <- match(forget, unj, integer(0))
    fo   <- match(front, unj, integer(0))
    lo   <- match(back, unj, integer(0))

    out <- c(fo,lo,lose)

    if(length(out) != 0) ro <- seq_len(lj)[-out]

    o <- c(lo,ro,rev(fo))

  }

  # Drawing ----

  divi <- split(d, d$i)

  divi <- divi[match(j[o], names(divi))]

  if(shading){

    divi <- rep(divi, each = 3)

    fai <- fa[o,]
    sai <- sa[o,]
    bai <- ba[o,]

    prob.line <- sai$density == 0 | is.na(sai$density)

    fai$density <- NA
    fai$angle   <- 45
    fai$border[!prob.line] <- NA

    bai$density <- NA
    bai$angle   <- 45
    bai$col     <- NA

    sai$border <- NA

    sai <- sai[,c(9,3,4,5,6,7,8,1,2)]
    bai <- bai[,c(1,9,2:8)]

    gp <- rbind(fai, sai, bai)[seq_mult(3*nrow(fai), 3, inv = T),]

    if(any(prob.line)){
      rem1 <- which(prob.line) * 3
      rem2 <- rem1 - 1

      rem <- c(rem2, rem1)[seq_mult(2*length(rem1), 2, inv = T)]

      gp <- gp[-rem,]

      divi <- divi[-rem]
    }

  } else {

    fai <- fa[o,]

    fai$density <- NA
    fai$angle   <- 45

    gp <- fai

  }

  gpl <- split(gp, seq(nrow(gp)))

  poly_fun <- function(coor, leg)
  {
    polygon(x = coor$x, y = coor$y,
            density = leg$density, angle = leg$angle,
            border = leg$border, col = leg$col,
            lty = leg$lty, lwd = leg$lwd,
            lend = leg$lend, ljoin = leg$ljoin,
            lmitre = leg$lmitre)
  }

  mapply(poly_fun, divi, gpl)

  return(invisible())

}
