\name{checkRNG}
\alias{checkRNG}
\title{Checking RNG Differences in Unit Tests}
\usage{
  checkRNG(x, y = getRNG(), ...)
}
\arguments{
  \item{x,y}{objects from which RNG settings are
  extracted.}

  \item{...}{extra arguments passed to
  \code{\link{rng.equal}}.}
}
\description{
  \code{checkRNG} checks if two objects have the same RNG
  settings and should be used in unit tests, e.g., with the
  \pkg{RUnit} package.
}
\examples{
# check for differences in RNG
set.seed(123)
checkRNG(123)
try( checkRNG(123, 123) )
try( checkRNG(123, 1:3) )
}

