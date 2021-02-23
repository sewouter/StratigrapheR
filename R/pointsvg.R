#' @title Converts line, rect, polygon and polyline class SVG objects into
#' data frames
#'
#' @description Converts 'line', 'rect', 'polygon' and 'polyline' class SVG
#' objects into data frames. \strong{ONLY THESE CLASSES OF OBJECTS CAN BE
#' IMPORTED.} If you have bezier or spline curves, they will be stored as 'path'
#' class objects that cannot be imported here. The same goes for 'rect' objects
#' that are transformed (rotation, etc...).
#'
#' @param file a .svg file
#' @param standard whether to standardise (centre to (0,0), rescale so
#' that extreme points are at -1 and 1) (T or F)
#' @param keep.ratio if the object is to be  standardised, whether to
#' keep the x/y ratio (T or F)
#' @param round whether to round the coordinates (T or F)
#' @param xdigits the number of digits after the decimal to round to for
#' x values
#' @param ydigits the number of digits after the decimal to round to for
#' y values
#' @param xinverse whether to inverse the plotting for x values (T or F)
#' @param yinverse whether to inverse the plotting for y values (T or F)
#' @param warn whether you want to be annoyed
#' @return A data.frame with x and y coordinates, ids for each object, and a
#' type, either line (L) or polygon (P)
#' @details This function is quite empirical. There is no guarantee it is bug
#' free. If you have .svg files that should work but do not, you can email me:
#' \email{sébastien.wouters@@doct.uliege.be}
#' @seealso Plot the drawing: \code{\link{placesvg}},
#'
#' Plot the drawing and change the coordinates :\code{\link{centresvg}} and
#' \code{\link{framesvg}}
#'
#' Change the drawing: \code{\link{changesvg}} and \code{\link{clipsvg}}
#' @examples
#' # To show you how to import, we first have to have a svg file to import. The
#' #following lines of code will create a svg in a temporary files:
#'
#' svg.file.directory <- tempfile(fileext = ".svg") # Creates temporary file
#' writeLines(example.ammonite.svg, svg.file.directory) # Writes svg in the file
#'
#' print(paste("An example .svg file was created at ", svg.file.directory,
#'             sep = ""))
#'
#' # The pointsvg function allows to import simple svg drawings into R
#'
#' ammonite.drawing <- pointsvg(file = svg.file.directory) # Provide file
#'
#' plot(c(-2,2), c(-2,2), type = "n")
#'
#' placesvg(ammonite.drawing)
#'
#' # If you want to import your own .svg file uncomment the following line:
#'
#' # pointsvg(file.choose())
#'
#' @importFrom stringr str_match str_match_all
#' @importFrom XML xmlParse saveXML
#' @importFrom utils read.table
#' @importFrom dplyr arrange
#' @export

pointsvg <- function(file, standard = TRUE, keep.ratio = FALSE, round = TRUE,
                     xdigits = 4, ydigits = 4, xinverse = FALSE, yinverse = TRUE,
                     warn = T)
{
  if(!isTRUE(warn) | !isTRUE(warn)) stop("Argument 'warn' should be T or F")

  # Read the XML file, and transform it in a text file ----

  svg <- xmlParse(file)

  TEMPFILE     <- tempfile()
  FILENAME <- paste0(TEMPFILE,"_rewrite.svg")

  saveXML(svg, FILENAME)

  # Read text file, select readable objects from it ----

  a <- readLines(FILENAME)

  polygon.l  <- grep(a, pattern = ".*<polygon ")
  polyline.l <- grep(a, pattern = ".*<polyline ")
  line.l     <- grep(a, pattern = ".*<line ")
  rect.l     <- grep(a, pattern = ".*<rect ")
  path.l     <- grep(a, pattern = ".*<path ")

  line.1 <- c(polygon.l, polyline.l, line.l, rect.l)

  type.1 <- c(rep("polygon",  length(polygon.l)),
              rep("polyline", length(polyline.l)),
              rep("line",     length(line.l)),
              rep("rect",     length(rect.l)))

  reorder <- order(line.1)

  line <- line.1[reorder]
  type <- type.1[reorder]

  # Path objects ----

  if(any(path.l)) {

    warning(paste("Elements of the .svg file are of class 'path' at line(s) ",
                  paste(path.l, collapse = ", "), " in the parsed .svg file ",
                  "(run XML::xmlParse(\"", file,
                  "\") to obtain it)",
                  ". These cannot be uploaded in R with this function.",
                  " Use a vector graphics editor to change these elements and",
                  " generate point-based objects only (polygon, polyline, ",
                  "line and rect). You can open your .svg file with a text ",
                  "editor to get familiar with .svg files and classes. To ",
                  "convert curvy objects to ",
                  "polygon or polyline the general idea is to select all the ",
                  "nodes of your object in a vector graphics editor. Then ",
                  "to add nodes between the initial nodes, and to convert the ",
                  "lines between these new nodes to lines. We advise: ",
                  "\n  - for CorelDRAW users to look for the 'shape', 'add ",
                  "nodes' and 'convert to lines' tools. \n  - for Inkscape ",
                  "users to look for the 'node', 'insert new node' and 'make ",
                  "selected segments lines' tools. \n  - for Adobe users to ",
                  "look for the 'convert anchor point' and 'straight lines'",
                  " tools", sep = ""))
  }

  # Transformed objects ----

  tran <- grep(a, pattern = ".*transform=.*")

  if(length(tran) != 0){

    tran.match <- match(tran, line)

    if(any(!is.na(tran.match))){

      tran.type <- unique(type[tran.match])

      warning(paste("Some of the elements of the following type were ",
                    "transformed (rotation, translation, ...): ",
                    paste(tran.type, collapse = ", "), ", at line(s) ",
                    paste(line[tran.match], collapse = ", "),
                    " in the parsed .svg file ",
                    "(run XML::xmlParse(\"", file,
                    "\") to obtain it)",
                    ". They cannot be imported using this function. We advise ",
                    "to convert these objects into polygons using a vector ",
                    "graphics editor.",sep = ""))

      line <- line[-tran.match]
      type <- type[-tran.match]

    }

  }

  # Extract objects ----

  accu           <- data.frame(matrix(ncol = 4,nrow = 0))
  colnames(accu) <- c("x","y","i","type")

  # LINE ----

  if(length(line.l) != 0){

    line.ex <- a[line.l]

    pat <- paste(" x1= *\"-*[0-9.]+\"",
                 " x2= *\"-*[0-9.]+\"",
                 " y1= *\"-*[0-9.]+\"",
                 " y2= *\"-*[0-9.]+\"",
                 sep = "|")

    pd  <- str_match_all(line.ex, pat)

    cl1 <- grepl(pat, line.ex)

    if(!all(cl1)){
      warning(paste("Strange format detected for 'line' class object. ",
                    "If this leads to incorrect output, the best you can do ",
                    "is to identify the 'line' object at line(s) ",
                    paste(line.l[which(!cl1)], collapse = ", "),
                    " in the parsed .svg file ",
                    "(run XML::xmlParse(\"", file,  "\") to obtain it).",
                    " The abnormality is the absence of",
                    " at least one of the arguments 'x1', 'y1', 'x2' or ",
                    "'y2', or that these arguments are not followed by '=' and ",
                    "then by a positive numerical value. You can modify the object in a ",
                    "vector graphics ",
                    "editor to fit a format this function can understand. ",
                    "You can also send the maintainer an email explaining ",
                    "the problem and providing the problematic .svg file to ",
                    "help improve StratigrapheR.", sep = ""))
    }

    lpd   <- sapply(pd, length)
    lpdid <- rep(seq_len(length(lpd)), lpd)

    dpd <- data.frame(gloVar.id = lpdid, text = unlist(pd))

    tinter <- sub("= *\"[0-9.]+\"", "", dpd$text)

    dpd$gloVar.coord <- sub(" ", "", tinter)

    dpd$xy <- as.numeric(gsub("\"", "", str_match(dpd$text, "\"[0-9.]+\"")))

    check.line <- split(dpd$gloVar.coord, dpd$gloVar.id)

    cl2 <- sapply(check.line, function(x) all(c("x1", "y1", "x2", "y2") %in% x))

    if(!all(cl2)){

      warning(paste("Strange format detected for 'line' class object. ",
                    "If this leads to incorrect output, the best you can do ",
                    "is to identify the 'line' object at line ",
                    paste(line.l[which(!cl2)], collapse = ", "),
                    " in the parsed .svg file ",
                    "(run XML::xmlParse(\"", file,  "\") to obtain it).",
                    " The abnormality is the absence of",
                    " at least one of the arguments 'x1', 'y1', 'x2' or ",
                    "'y2'. You can modify the object in a vector graphics  ",
                    "editor to fit a format this function can understand. ",
                    "You can also send the maintainer an email explaining ",
                    "the problem and providing the problematic .svg file to ",
                    "help improve StratigrapheR.", sep = ""))

      dpd <- dpd[!(dpd$gloVar.id %in% seq_len(length(pd))[which(!cl2)]),]

    }

    if(nrow(dpd) != 0){

      clean.line <- arrange(dpd, gloVar.id, gloVar.coord)

      tll <- nrow(clean.line)

      lposx <- c(seq(1,tll,4), seq(2,tll,4))[seq_mult(l = tll/2, 2, inv = T)]
      lposy <- c(seq(3,tll,4), seq(4,tll,4))[seq_mult(l = tll/2, 2, inv = T)]

      data.line <- data.frame(x = clean.line$xy[lposx],
                              y = clean.line$xy[lposy],
                              id = line.l[clean.line$gloVar.id[lposx]])

      data.line$type <- "L"

      accu <- rbind(accu, data.line)

    }

  }

  # POLY -line and -gon ----

  p.id <- c(polyline.l, polygon.l)

  if(length(p.id) != 0){

    poly.ex <- a[p.id]

    p.type    <- c(rep("L", length(polyline.l)), rep("P", length(polygon.l)))
    p.type.ex <- c(rep("polyline", length(polyline.l)),
                   rep("polygon", length(polygon.l)))

    pat2 <- "points= *\"[[0-9.]+,[0-9.]+ ]*\""

    tline <- as.character(str_match(poly.ex, pat2))

    check.poly <- grepl("points= *\"[0-9.]+\\,[0-9.]+", poly.ex)

    if(warn & any(!check.poly)){

      warning(paste("Strange format detected for ",
                    paste(unique(p.type.ex[!check.poly]), collapse = " and "),
                    " class object(s). ",
                    "If this leads to incorrect output, the best you can do ",
                    "is to identify the object at line(s) ",
                    paste(p.id[which(!check.poly)], collapse = ", "),
                    " in the parsed .svg file ",
                    "(run XML::xmlParse(\"", file,  "\") to obtain it).",
                    " The abnormality is the lack of the ",
                    "argument 'points' providing the x and y coordinates in ",
                    "a 'points=\"X1,Y1 X2,Y2 \"' format. You can modify the ",
                    "object in a vector graphics editor to fit a format ",
                    "this function can understand. You can also send the ",
                    "maintainer an email explaining the problem and ",
                    "providing the problematic .svg file to help improve ",
                    "StratigrapheR.", sep = ""))

      tline <- tline[check.poly]

    }

    if(length(tline) != 0){

      pd2 <- str_match_all(tline,"-*[0-9.]+,-*[0-9.]+")

      p.rep <- sapply(pd2, length)

      p.type.2 <- rep(p.type, p.rep)
      p.num    <- rep(p.id, p.rep)

      pd4 <- data.frame(text = unlist(pd2), id = p.num, type = p.type.2)

      pe <- read.table(text=pd4$text, sep=",")

      colnames(pe) <- c("x","y")

      nd <- cbind(pe, pd4[,-1])

      accu <- rbind(accu, nd)

    }

  }

  # RECT ----

  if(length(rect.l) != 0){

    rect.ex <- a[rect.l]

    pat3 <- paste(" x= *\"-*[0-9.]+\"",
                  " y= *\"-*[0-9.]+\"",
                  " width= *\"-*[0-9.]+\"",
                  " height= *\"-*[0-9.]+\"",
                  sep = "|")

    pd3  <- str_match_all(rect.ex, pat3)

    cl3 <- grepl(pat3, rect.ex)

    if(!all(cl3)){
      warning(paste("Strange format detected for 'rect' class object. ",
                    "If this leads to incorrect output, the best you can do ",
                    "is to identify the 'rect' object at line(s) ",
                    paste(rect.l[which(!cl3)], collapse = ", "),
                    " in the parsed .svg file ",
                    "(run XML::xmlParse(\"", file,  "\") to obtain it).",
                    " The abnormality is the absence of",
                    " at least one of the arguments 'x', 'y', 'height' and ",
                    "'width', or that these arguments are not followed by '=' and ",
                    "then by a positive numerical value. You can modify the object in a ",
                    "vector graphics  ",
                    "editor to fit a format this function can understand. ",
                    "You can also send the maintainer an email explaining ",
                    "the problem and providing the problematic .svg file to ",
                    "help improve StratigrapheR.", sep = ""))
    }

    lpd3   <- sapply(pd3, length)
    lpdid3 <- rep(seq_len(length(lpd3)), lpd3)

    dpd3 <- data.frame(gloVar.id = lpdid3, text = unlist(pd3))

    tinter3 <- sub("= *\"[0-9.]+\"", "", dpd3$text)

    dpd3$gloVar.coord <- sub(" ", "", tinter3)

    dpd3$xy <- as.numeric(gsub("\"", "", str_match(dpd3$text, "\"[0-9.]+\"")))

    check.line3 <- split(dpd3$gloVar.coord, dpd3$gloVar.id)

    cl4 <- sapply(check.line3, function(x) all(c("x", "y", "width", "height") %in% x))

    if(!all(cl4)){

      warning(paste("Strange format detected for 'rect' class object. ",
                    "If this leads to incorrect output, the best you can do ",
                    "is to identify the 'rect' object at line(s) ",
                    paste(rect.l[which(!cl4)], collapse = ", "),
                    " in the parsed .svg file ",
                    "(run XML::xmlParse(\"", file,  "\") to obtain it).",
                    " The abnormality is the absence of",
                    " at least one of the arguments 'x', 'y', 'height' and ",
                    "'width', or that these arguments are not followed by",
                    " '=' and then by a positive numerical value. You can modify the ",
                    "object in a vector graphics  ",
                    "editor to fit a format this function can understand. ",
                    "You can also send the maintainer an email explaining ",
                    "the problem and providing the problematic .svg file to ",
                    "help improve StratigrapheR.", sep = ""))

      dpd3 <- dpd3[!(dpd3$gloVar.id %in% seq_len(length(pd3))[which(!cl4)]),]

    }

    if(nrow(dpd3) != 0){

      clean.rect <- arrange(dpd3, gloVar.id, gloVar.coord)

      trl <- nrow(clean.rect)

      rposh <- seq(1,trl,4)
      rposw <- seq(2,trl,4)
      rposx <- seq(3,trl,4)
      rposy <- seq(4,trl,4)

      data.rect <- data.frame(xc = clean.rect$xy[rposx],
                              yc = clean.rect$xy[rposy],
                              h = clean.rect$xy[rposh],
                              w = clean.rect$xy[rposw],
                              id = rect.l[clean.rect$gloVar.id[rposx]])

      data.rect$type <- "P"

      data.rect$x1 <- data.rect$xc
      data.rect$x2 <- data.rect$xc + data.rect$w
      data.rect$y1 <- data.rect$yc
      data.rect$y2 <- data.rect$yc + data.rect$h

      end.x <- c(data.rect$x1,
                 data.rect$x2)[seq_mult(l = nrow(data.rect) * 2, mult = 2, inv = T)]

      endx <- rep(end.x, each = 2)

      end.y <- c(data.rect$y1, data.rect$y2, data.rect$y2, data.rect$y1)

      endy <- end.y[seq_mult(l = nrow(data.rect) * 4, mult = 4, inv = T)]

      endr <- data.frame(x = endx, y = endy,
                         id = rep(data.rect$id, each = 4),
                         type = rep(data.rect$type, each = 4))

      accu <- rbind(accu, endr)

    }

  }

  # Final adjustments ----

  accu$gloVar.id    <- accu$id
  accu$gloVar.coord <- seq_len(nrow(accu))

  accu <- arrange(accu, gloVar.id, gloVar.coord)[,-c(5,6)]

  endl <- sapply(split(accu$id, accu$id), length)

  accu$id <- paste0(accu$type, rep(seq_len(length(endl)), endl))

  # Correct if necessary ----

  res <- changesvg(accu, standard = standard, keep.ratio = keep.ratio,
                   round = round, xdigits = xdigits, ydigits = ydigits,
                   xinverse = xinverse, yinverse = yinverse)

  return(res)

}
