#' @title Runs neatPick without user input
#'
#' @description Is the user input free version of neatPick. Runs a function n
#' times, with its arguments n times different. The graphical output is stored
#' into a n pages pdf or a n files folder. The output of the function is
#' accumulated in a list.
#'
#' @param fun the function to be applied n times.
#' @param n number of runs.
#' @param args the arguments to be supplied to fun. Should be a list of
#' each argument to be supplied to fun, having n elements stored
#' indiscriminately in list or in vector form.
#' @param width,height the width and height of the graphics region. In
#' inches by default, can be adapted if onePDFfile = FALSE
#' @param output the kind of output : "function" for the accumulated
#' outputs of the function (list of n elements), "all" to add args, and
#' everything else to output nothing
#' @param name the names of the graphic file(s)
#' @param dir the directory of the file or of the folder of files, by default a
#' temporary file
#' @param gfile whether to create a single pdf with n pages
#' ("onePDF"; default) or a folder of n graphical files ("gfun"). If anything
#' else is given ("none for instance"), it won't produce graphical files. This
#' reduces computation speed by a little more than 15 percents (one try of 1000
#' samples with simple graphs).
#' @param openfile,track parameters for pdfDisplay()
#' @param folder the name of the folder containing the n graphical files
#' @param gfun  a non-empty character string naming the graphical
#' function to be called to create the n graphical files
#' @param ext the extension of the n graphical files
#' @param gargs list of arguments transmitted to the graphical function
#' @param pargs list of arguments transmitted to the par() function
#' @return the accumulated outputs of fun (and arguments if asked) if asked
#' @examples
#' \dontrun{
#' fun <- function(x, y, xlim = c(-1,1),...)
#' {
#'   plot(x, y, xlim = xlim,...)
#'
#'   return(paste(x, y, paste(xlim, collapse = "; "), sep = "; "))
#' }
#'
#' args <- list(x = list(-0.5, 1) , y = c(0.8, 0.8), pch = c(2,4),
#'              xlim = list(c(-1,1), c(-20,20)))
#'
#' temp <- tempfile()
#' dir.create(temp)
#'
#' neatPicked(fun, 2, args = args, width = 5, height = 5, dir = temp)}
#'
#' @export
#' @import shiny
#' @importFrom grDevices pdf

neatPicked <- function(fun, n, args = NA,
                       width = 10, height = 10,
                       output = "all", name = "Fig", dir = tempdir(),

                       gfile = "onePDF", openfile = TRUE, track = TRUE,

                       folder = "My file",
                       gfun = "jpeg", ext = ".jpeg",
                       gargs = list(units = "in", res = 300),

                       pargs = list())
{

  # Verify that each argument is of length n

  names <- names(args)
  len   <- sapply(args, length, simplify = TRUE, USE.NAMES = FALSE)
  bad   <- names[which(!(len %in% c(n)))]

  if(length(bad) != 0) stop(paste("Argument(s) ",
                                  paste(bad, collapse = ", "),
                                  " should have",
                                  " n (", n,") elements",
                                  sep = ""))

  # Graphical loop, in a form able to apply pdfDisplay to assemble all plots in
  # a single pdf or to store separate files in a folder

  g <- function(gfile, ndir)
  {
    accu <- list()

    if(gfile == "gfun" | gfile == "onePDF") par(pargs)

    for(i in seq_len(n))
    {
      print(paste("Treating ", i, " of ", n, sep = ""))

      subarg <- lapply(args,"[",i)
      subarg <- lapply(subarg,unlist)

      if(gfile == "gfun"){

        gargsi <- c(paste(ndir, "/", name, i, ext, sep = ""),
                    height = height, width = width, gargs)
        do.call(gfun, args = gargsi)

      }

      if(gfile != "gfun" & gfile != "onePDF") pdf(file = NULL)

      accu <- c(accu, list(do.call(fun, c(subarg))))

      if(gfile != "onePDF") dev.off()
    }

    return(accu)
  }

  # Apply graphical loop, accumulate output

  if(gfile == "onePDF") {
    accu <- pdfDisplay(g(gfile), name = name, width = width, height = height,
                       dir = dir, openfile = openfile, track = track,
                       output = T)
  } else {

    if (gfile == "gfun") ndir <- folder(dir, folder)

    accu <- g(gfile, ndir)
  }

  # Deal with output

  if(output == "all"){
    res <- c(accu, list(args = args))
  } else if (output == "function") {
    res <- accu
  }

  # Return output

  return(res)

}


