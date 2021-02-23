#' Draws circles
#'
#' @param r the radius of the circles (of length 1 or n)
#' @param x the x value of the centre of the circles (of length 1 or n)
#' @param y the y value of the centre of the circles (of length 1 or n)
#' @param ndiv the number of segments making the circles
#' @param plot whether to plot the circles
#' @param output whether to return an output
#' @param add whether to add to an existing plot
#' @param ... graphical parameters to feed to lines
#' @return a list of x and y matrices having n rows, one for each circle
#' @examples
#' plot(0, 0, xlim = c(-1,1), ylim = c(-1,1), asp = 1)
#'
#' encircle(lwd = 2)
#' encircle(r = seq(0.1,0.9,0.1))
#'
#' @export

encircle <- function (r = 1, x = 0, y = 0 , ndiv = 360,
                      plot = TRUE, add = TRUE, output = FALSE,...)
{
  lr <- length(r)
  lx <- length(x)
  ly <- length(y)

  lmax <- max(lr,lx,ly)

  if(!((lr == lmax | lr == 1) &
       (lx == lmax | lx == 1) &
       (ly == lmax | ly == 1))) stop(paste("r, x and y should be of same ",
                                           "length or of length 1", sep = ""))

  phi <- matrix(rep(seq(0, 2 * pi, by = 2 * pi/ndiv),lmax),
                nrow = lmax, byrow = TRUE)

  res <- list()

  res$x  <- r * cos(phi) + x
  res$y  <- r * sin(phi) + y

  if(plot){

    for(i in seq_len(lmax))
    {
      xi <- res$x[i,]
      yi <- res$y[i,]

      lines(xi, yi,...)
    }

  }

  if(output) return(res)

}
