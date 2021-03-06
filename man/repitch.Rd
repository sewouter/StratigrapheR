% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/repitch.R
\name{repitch}
\alias{repitch}
\title{Converts pitch into declination and inclination}
\usage{
repitch(pitch, strike, dip, quadrant = NA)
}
\arguments{
\item{pitch}{pitch (or rake) of the data; it is the angle between the
strike of the plane and a line. It is taken from the left side going downward
along the dip, and is positive downward.}

\item{strike}{strike of the data; it is the angle from the north of
the horizontal line of the plane. It is corrected by the \code{\link{dipfix}}
function.}

\item{dip}{dip of the data; it is the angle from the horizontal taken
on the line of the plane perpendicular to the one of the strike. It is
corrected by the \code{\link{dipfix}} function.}

\item{quadrant}{the quadrant were the plane dips downward. Accepted
values are NA, 'N', 'S', 'W' or 'E' (lower- or uppercase alike) for
correction by the \code{\link{dipfix}} function.}
}
\value{
a list of declination and inclination of the defined lines
}
\description{
Finds the declination and inclination of a line defined by a
pitch on a plane
}
\examples{
strike <- c(90, 135, 135, 135)
dip    <- c(0,  65,  65,  65)
pitch  <- c(40, 40,  140, -40)

earnet()
earplanes(strike,dip,hsphere = "b", a = list(col = "red", lwd = 2))

res <- repitch(pitch = pitch, strike = strike, dip = dip)

earpoints(dec = res$dec, inc = res$inc)

}
\references{
Eric Carlson of the Colorado School of Mines is acknowledged for
 his rake to plunge calculator on which this function is based.
}
\seealso{
\code{\link{dipfix}}, \code{\link{incfix}} and
\code{\link{transphere}}
}
