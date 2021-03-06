% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ignore.R
\name{ignore}
\alias{ignore}
\title{Ignores useless objects}
\usage{
ignore(
  i,
  x,
  y = NA,
  d = list(),
  j = unique(i),
  arg = list(),
  xlim = par("usr")[c(1, 2)],
  ylim = par("usr")[c(3, 4)],
  xlog = par("xlog"),
  ylog = par("ylog")
)
}
\arguments{
\item{i}{a polygon id for each x and y coordinate. If n objects are
provided there should be n unique ids describing them, and the
graphical parameters should be of length 1 or n.}

\item{x, y}{numeric vectors of coordinates.}

\item{d}{a list of named vectors going with i, x and y}

\item{j}{a list of the ids in the order used for the \code{arg}
arguments. By default they are in their order of appearance in \code{i}}

\item{arg}{a list of arguments f length 1 or n.}

\item{xlim, ylim}{the limits in x and y; if any object has all his
points past one of these limits, it will be removed.}

\item{xlog, ylog}{whether the axes have logarithmic scale}
}
\value{
a list of i, x, y, d, j and arguments.
}
\description{
Ignores useless objects: this function will discard the polygons
or polylines outside a certain range. This allows to avoid unnecessary work
for multigons(), multilines(), centresvg() and framesvg().
}
\examples{
i <- c(rep("A1",6), rep("A2",6), rep("A3",6))
x <- c(1,2,3,3,2,1,4,5,6,6,5,4,7,8,9,9,8,7)
y <- c(1,2,3,4,5,6,1,2,3,4,5,6,1,2,3,4,5,6)

xlim <- c(2,5)
ylim <- c(0,1.5)

plot(c(0,10),c(0,10),type = "n")
rect(xlim[1], ylim[1], xlim[2], ylim[2])

multilines(i, x, y, lty = 3, col = "grey80")

res <- ignore(i, x, y, arg = list(lty =  1, lwd = 3,
              col = c("orange", "green", "red")),
              xlim = xlim, ylim = ylim)

do.call(multilines, res)

}
\seealso{
Tributary functions: \code{\link{multigons}},
\code{\link{multilines}}, \code{\link{centresvg}} and \code{\link{framesvg}}
}
