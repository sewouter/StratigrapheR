% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/restore.R
\name{restore}
\alias{restore}
\title{Plane correction}
\usage{
restore(dec, inc, strike, dip, quadrant = NA, inverted = NA, percent = 100)
}
\arguments{
\item{dec}{declination of the data; it is the angle from the north
taken on an horizontal plane. It is measured clockwise from North and ranges
from 0 to 360° (Tauxe 2010).}

\item{inc}{inclination of the data; it is the angle from the
horizontal, is positive downward, and ranges from +90° for straight down to
-90° for straight up (Tauxe, 2010).}

\item{strike}{strike of the plane used for correction; it is the angle
from the north of the horizontal line of the plane. It is corrected by
dipfix().}

\item{dip}{dip of the plane used for correction; it is the angle from
the horizontal taken on the line of the plane perpendicular to the one of
the strike. It is corrected by dipfix().}

\item{quadrant}{the quadrant were the plane dips downward. Accepted
values are NA, 'N', 'S', 'W' or 'E' (lower- or uppercase alike) for
correction by dipfix().}

\item{inverted}{whether the plane is inverted or not. The default is
NA, it assumes that no bed is inverted.}

\item{percent}{the percentage of correction (can be of length >= 1),
by default it is 100 (\%), bringing the plane to the horizontal.}
}
\description{
Plane correction : declination and inclination are corrected for
planes of given strike, dip, quadrant and inversion
}
\examples{
dec <- c(90,210)
inc <- c(20,60)

strike <- c(0,120)
dip    <- c(20,60)
inverted <- c(FALSE,TRUE)

res <- restore(dec = dec, inc = inc, strike = strike, dip = dip,
               quadrant = NA, inverted = inverted,
               percent = seq(20,100, by = 20))

earnet()
earplanes(strike, dip)
earpoints(dec,inc)
earpoints(round(res$dec,2), round(res$inc,2), a = list(pch = 22))

}
\seealso{
\code{\link{rotate}} and \code{\link{reposition}}
}
