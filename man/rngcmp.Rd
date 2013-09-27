\name{rng.equal}
\alias{rng1.equal}
\alias{rng.equal}
\title{Comparing RNG Settings}
\usage{
  rng.equal(x, y)

  rng1.equal(x, y)
}
\arguments{
  \item{x}{objects from which RNG settings are extracted}

  \item{y}{object from which RNG settings are extracted}
}
\value{
  \code{rng.equal} and \code{rng.equal1} return a
  \code{TRUE} or \code{FALSE}.
}
\description{
  \code{rng.equal} compares the RNG settings associated
  with two objects.

  \code{rng1.equal} tests whether two objects have
  identical \strong{initial} RNG settings.
}
\details{
  These functions return \code{TRUE} if the RNG settings
  are identical, and \code{FALSE} otherwise. The comparison
  is made between the hashes returned by \code{RNGdigest}.
}

