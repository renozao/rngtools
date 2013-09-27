\name{RNGseed}
\alias{RNGrecovery}
\alias{RNGseed}
\title{Directly Getting or Setting the RNG Seed}
\usage{
  RNGseed(seed)

  RNGrecovery()
}
\arguments{
  \item{seed}{an RNG seed, i.e. an integer vector. No
  validity check is performed, so it \strong{must} be a
  valid seed.}
}
\value{
  invisibly the current RNG seed when called with no
  arguments, or the -- old -- value of the seed before
  changing it to \code{seed}.
}
\description{
  \code{RNGseed} directly gets/sets the current RNG seed
  \code{.Random.seed}. It can typically be used to backup
  and restore the RNG state on exit of functions, enabling
  local RNG changes.

  \code{RNGrecovery} recovers from a broken state of
  \code{.Random.seed}, and reset the RNG settings to
  defaults.
}
\examples{
# get current seed
RNGseed()
# directly set seed
old <- RNGseed(c(401L, 1L, 1L))
# show old/new seed description
showRNG(old)
showRNG()

# set bad seed
RNGseed(2:3)
try( showRNG() )
# recover from bad state
RNGrecovery()
showRNG()

# example of backup/restore of RNG in functions
f <- function(){
	orng <- RNGseed()
 on.exit(RNGseed(orng))
	RNGkind('Marsaglia')
	runif(10)
}

sample(NA)
s <- .Random.seed
f()
identical(s, .Random.seed)
\dontshow{ stopifnot(identical(s, .Random.seed)) }
}

