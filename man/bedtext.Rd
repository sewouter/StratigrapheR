% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bedtext.R
\name{bedtext}
\alias{bedtext}
\title{Writes the names of the beds in a litholog}
\usage{
bedtext(
  labels,
  l,
  r,
  x = 0.2,
  arg = list(cex = 1),
  adj = c(0.5, 0.5),
  ymin = NA,
  edge = FALSE
)
}
\arguments{
\item{labels}{the name of each bed}

\item{l}{a vector of n left y (or dt, i.e. depth or time) interval
limits for each bed}

\item{r}{a vector of n right y (or dt, i.e. depth or time) interval
limits for each bed}

\item{x}{the position where to write the text (0.2 by default)}

\item{arg}{a list of arguments to feed text(). Go see ?text to know
which arguments can be provided. See ?merge.list for further information.}

\item{adj}{one or two values in [0, 1] which specify the x (and
optionally y) adjustment of the labels. c(0.5,0.5) is the default.}

\item{ymin}{minimum thickness of the bed to write its name (if NA,
a default value is calculated, but user input is best)}

\item{edge}{whether to put the bed name at the edge of the beds (T)
or in the center of the beds (F, is the default)}
}
\description{
Writes the names of the beds in a litholog. You can either place
them at the centre of the beds or in their upper and lower part. You can
also define a thickness below which the name won't be written, to avoid
excessive text crowding the plot.
}
\examples{
l  <- c(0,4,5,8)
r  <- c(4,5,8,16)

x   <- c(4,5,3,4)
i <- c("B1","B2","B3","B4")

test <- litholog(l, r, x, i)

whiteSet(xlim = c(0,6), ylim = c(-10,30))

multigons(test$i, test$xy, test$dt, col = c(NA, "black", "grey","NA"))

bedtext(labels = i, r = r, l = l, edge = TRUE, x = 0.5,
        arg = list(col = c("black", "white", "white", "red")))

}
\seealso{
\code{\link{litholog}} obvisously

if your boundaries have to be recalculated: \code{\link{leftlog}}

other functions complementing litholog: \code{\link{infobar}} and
\code{\link{ylink}}
}
