% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rebound.R
\name{rebound}
\alias{rebound}
\title{Simplifies boundary indicators for lim objects}
\usage{
rebound(b, na.errors = F)
}
\arguments{
\item{b}{a vector of boundary indicators}

\item{na.errors}{whether to replace all other values by NA (rather
than simply stopping the function)}
}
\value{
a simplified vector of boundary indicators (\code{"[]"}, \code{"[["},
\code{"]]"} and \code{"]["} only)
}
\description{
Simplifies boundary indicators for lim objects: from the wide
range supported by R ("[]", "[)", "(]", "()", "[[", "]]", "][", "open",
"closed", "left-open", "right-open", "left-closed", "right-closed") to "[]",
"[[", "]]" and "][" only
}
\examples{
bounds <- c("[]", "[)", "(]", "()",
            "[[", "]]", "][",
            "open", "closed",
            "left-open", "right-open",
            "left-closed", "right-closed")

rebound(bounds)

}
\seealso{
\code{\link{as.lim}}
}
