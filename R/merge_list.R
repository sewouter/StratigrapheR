#' @title Method for merging lists by name
#'
#' @description This is a method that merges the contents of lists based on the
#' name of the elements. In the case of identical names, the order of the lists
#' determines which element is kept.
#'
#' @param l1 the first list.
#' @param l2 the list which will supply additional elements to l1 that
#' are not already there by name.
#' @param ... additionnal lists, that bring elements if they are not
#' existing by name in the ones before.
#'
#' @details if a name appears more than once in a list, only the first one will
#' be kept. This is particularly useful if you want to still be able to provide
#' whichever argument you want to a function inside another function. See the
#' advanced use in the examples to see how to do it.
#'
#' @return A merged list of all lists provided, each element (determined by its
#' name) appearing only once.
#'
#' @seealso \code{\link{homogenise}} provides a general way of dealing with
#' function arguments.
#'
#' To get a better understanding of how to deal with function arguements, go see
#' ?do.call and ?list
#'
#' @examples
#' # Simple use
#'
#' a <- list(lty = c(2,4), mar = 4, plot = TRUE)
#' b <- list(mar = "hype",lty = "hype",  pink = TRUE)
#' d <- list(lty = FALSE, pink = "Yikes", mar = "ldkfj", test = "Successful")
#'
#' merge_list(a,b,d)
#'
#' # Advanced use
#'
#' # We will plot points with different parameters for each lithology (see also
#' # the example in ?in.lim)
#'
#' advanced.ex <- function(line.args = list(lty = 3, col = "grey"),
#'                         all = list(pch = 21, cex = 2),
#'                         chert = list(bg = "white"),
#'                         limestone = list(bg = "black"),
#'                         shale = list(bg = "red"),
#'                         main = "")
#' {
#'
#'   # Preparation of plot and necessary data frames
#'
#'   plot(proxy.example.litho$ms, proxy.example.litho$dt, type = "n",
#'        xlim = c(-2*10^-8, 8*10^-8), main = main)
#'
#'   shale.df <- subset(proxy.example.litho, proxy.example.litho$litho == "S")
#'   limestone.df <- subset(proxy.example.litho, proxy.example.litho$litho == "L")
#'   chert.df <- subset(proxy.example.litho, proxy.example.litho$litho == "C")
#'
#'   # Important part:
#'
#'   # We use the do.call function, which calls a given function and provides
#'   # its arguments via a list. It is that list that is created by merge list.
#'   # for the lines function, we provide x and y coordinates, a personalised
#'   # list of arguments (line), and the default parameters. In this order the
#'   # personalised arguments override the default ones, but the latter are used
#'   # in the absence of personalised arguments
#'
#'   line.args <- merge_list(list(x = proxy.example.litho$ms,
#'                                y = proxy.example.litho$dt),
#'                           line.args, # personalised list of arguments
#'                           list(lty = 3, col = "grey") # default parameters
#'   )
#'
#'   do.call(lines, args = line.args)
#'
#'   # Same procedure for the points of each lithology, but we add an 'all'
#'   # argument that applies for each point
#'
#'   chert.args <- merge_list(list(x = chert.df$ms,
#'                                 y = chert.df$dt), # Coordinates
#'                            chert, # Personalised arguments for cherts
#'                            all,   # Personalised arguments for all points
#'                            list(bg = "red"),       # Default arguments
#'                            list(pch = 21, cex = 2) # Default arguments
#'   )
#'
#'   limestone.args <- merge_list(list(x = limestone.df$ms,
#'                                     y = limestone.df$dt),
#'                                limestone, all,
#'                                list(bg = "red"), list(pch = 21, cex = 2))
#'
#'   shale.args <- merge_list(list(x = shale.df$ms, y = shale.df$dt),
#'                            shale, all,
#'                            list(bg = "red"), list(pch = 21, cex = 2))
#'
#'   do.call(points, args = chert.args)
#'   do.call(points, args = limestone.args)
#'   do.call(points, args = shale.args)
#'
#' }
#'
#' omfrow <- par()$mfrow
#'
#' par(mfrow = c(1,3))
#'
#' advanced.ex(main = "Default")
#'
#' advanced.ex(main = "Change line and all",
#'             line.args = list(lty = 1),
#'             all = list(pch = 22))
#'
#' advanced.ex(main = "Personalise more",
#'            line.args = list(lty = 1, col = "black"),
#'             all = list(pch = 22),
#'             shale = list(pch = 4))
#'
#' par(mfrow = omfrow)
#'
#' @export

merge_list <- function (l1, l2,...)
{
  liszt <- c(l1,l2,...)
  namzs <- names(liszt)
  res   <- liszt[!duplicated(namzs)]
  return(res)
}
