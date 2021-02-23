#' @title Generates PDF and SVG figures
#'
#' @description Takes an ensemble of figures, represented by a function g(), and
#' generates a PDF (or SVG if specified). The PDF can be visualised immediatly
#' on the default PDF reader.
#'
#' @param g the plot function to be exported and looked at
#' @param name the name of the document
#' @param dir the file where the document will be saved (by default a temporary
#'  directory, tempdir())
#' @param ext the extension of the document: ".pdf" by default, but
#' ".svg" works also.
#' @param width the width of the drawing area (in inches)
#' @param height the height of the drawing area (in inches)
#' @param parg list of arguments transmitted to the par() function
#' @param track whether to generate different files for each rerun of pdfDisplay
#' with identical 'name'. The name will be followed by '_(i)' where i is the
#' version number. With this you avoid closing your pdf file at each rerun if
#' your pdf reader is not able to deal with (to my knowledge only SumatraPDF is
#' able)
#' @param openfile should the pdf file be opened (for the moment works
#' only in Windows). Use SumatraPDF as default pdf reader to be able to write
#' over current file
#' @param output whether to output the output of g() or not
#' @param warn useless vestigial parameter, kept for compatibility with
#' StratigrapheR 0.0.1
#'
#' @details The width and height you provide will not exactly be respected.
#' I could not find a pdf printing function that respects dimensions
#' scrupulously for R base graphics.
#'
#' @return the output of the g() function if output = TRUE
#'
#' @examples
#' \dontrun{
#' temp <- tempfile()
#' dir.create(temp)
#'
#' g1   <- function() plot(1,1)
#'
#' pdfDisplay(g1(),"TestGraph", dir = temp,
#'            parg = list(mar = c(6,6,6,6), ps = 24,lwd = 4))
#'
#' g1   <- function() plot(1,1, col = "red")
#'
#' pdfDisplay(g1(), "TestGraph", dir = temp,
#'            parg = list(mar = c(6,6,6,6), ps = 24,lwd = 4))}
#'
#' @importFrom grDevices dev.off
#' @export

pdfDisplay <- function(g, name,  ext =".pdf", dir = tempdir() ,
                       width = 10, height = 10, parg = list(),
                       track = T, openfile = T, output = F, warn= F)
{
  close <- T

  on.exit(if(close) dev.off())

  if(track){

    lf  <- list.files(path = dir, pattern = c(".\\.pdf$|.\\.svg$"))

    fres <- substring(lf, 1, nchar(name))
    lf1  <- lf[fres == name]

    fend <- substring(lf1, nchar(name) + 1)
    fout <- substr(fend, 1, nchar(fend) - 4)

    lfl <- grepl("^_\\([0-9]+\\)$", fout)

    if(any(lfl) == TRUE){

      out1 <- gsub("^_\\(","", fout[lfl])
      out2 <- gsub("\\)","", out1)

      new  <- max(as.numeric(out2)) + 1

      act.name <- paste(name, "_(", new, ")", sep = "")

    } else {

      act.name <- paste(name, "(1)", sep = "_")

    }

  } else {

    act.name <- name

  }

  filename <- paste(dir, "/", act.name, ext, sep="")

  # cairo_pdf(filename = filename, width = width, height = height,
  #           bg = FALSE, onefile = TRUE)

  pdf(file = filename, width = width, height = height,
      bg = "transparent", onefile = T)

  if(length(parg) != 0) {

    opar <- par()[names(par()) %in% names(parg)]

    on.exit(par(opar))

    par(parg)
  }

  res <- g

  dev.off()

  close <- F

  if(openfile) {

    system(paste0('open "', filename, '"'))

  }

  if(length(parg) != 0) par(opar)

  if(!is.null(res) & output) return(res)

}

