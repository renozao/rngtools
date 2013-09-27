\docType{methods}
\name{getRNG}
\alias{getRNG}
\alias{.getRNG}
\alias{getRNG1}
\alias{getRNG1,ANY-method}
\alias{getRNG1-methods}
\alias{.getRNG,ANY-method}
\alias{.getRNG,list-method}
\alias{.getRNG-methods}
\alias{.getRNG,missing-method}
\alias{.getRNG,numeric-method}
\alias{hasRNG}
\alias{nextRNG}
\alias{setRNG}
\alias{.setRNG}
\alias{.setRNG,character-method}
\alias{.setRNG-methods}
\alias{.setRNG,numeric-method}
\title{Getting/Setting RNGs}
\usage{
  getRNG(object, ..., num.ok = FALSE, extract = TRUE,
    recursive = TRUE)

  hasRNG(object)

  .getRNG(object, ...)

  getRNG1(object, ...)

  nextRNG(object, ..., ndraw = 0L)

  setRNG(object, ..., verbose = FALSE, check = TRUE)

  .setRNG(object, ...)
}
\arguments{
  \item{object}{an R object from which RNG settings can be
  extracted, e.g. an integer vector containing a suitable
  value for \code{.Random.seed} or embedded RNG data, e.g.,
  in S3/S4 slot \code{rng} or \code{rng$noise}.}

  \item{...}{extra arguments to allow extension and passed
  to a suitable S4 method \code{.getRNG} or
  \code{.setRNG}.}

  \item{num.ok}{logical that indicates if single numeric
  (not integer) RNG data should be considered as a valid
  RNG seed (\code{TRUE}) or passed to
  \code{\link{set.seed}} into a proper RNG seed
  (\code{FALSE}) (See details and examples).}

  \item{extract}{logical that indicates if embedded RNG
  data should be looked for and extracted (\code{TRUE}) or
  if the object itself should be considered as an RNG
  specification.}

  \item{recursive}{logical that indicates if embedded RNG
  data should be extracted recursively (\code{TRUE}) or
  only once (\code{FASE}).}

  \item{ndraw}{number of draws to perform before returning
  the RNG seed.}

  \item{check}{logical that indicates if only valid RNG
  kinds should be accepted, or if invalid values should
  just throw a warning. Note that this argument is used
  only on R >= 3.0.2.}

  \item{verbose}{a logical that indicates if the new RNG
  settings should be displayed.}
}
\value{
  \code{getRNG}, \code{getRNG1}, \code{nextRNG} and
  \code{setRNG} usually return an integer vector of length
  > 2L, like \code{\link{.Random.seed}}.

  \code{getRNG} and \code{getRNG1} return \code{NULL} if no
  RNG data was found.

  \code{setRNG} invisibly returns the old RNG settings as
  they were before changing them.
}
\description{
  \code{getRNG} returns the Random Number Generator (RNG)
  settings used for computing an object, using a suitable
  \code{.getRNG} S4 method to extract these settings. For
  example, in the case of objects that result from multiple
  model fits, it would return the RNG settings used to
  compute the best fit.

  \code{hasRNG} tells if an object has embedded RNG data.

  \code{.getRNG} is an S4 generic that extract RNG settings
  from a variety of object types. Its methods define the
  workhorse functions that are called by \code{getRNG}.

  \code{getRNG1} is defined to provide separate access to
  the RNG settings as they were at the very beginning of a
  whole computation, which might differ from the RNG
  settings returned by \code{getRNG}, that allows to
  reproduce the result only.

  \code{nextRNG} returns the RNG settings as they would be
  after seeding with \code{object}.

  \code{setRNG} set the current RNG with a seed, using a
  suitable \code{.setRNG} method to set these settings.

  \code{.setRNG} is an S4 generic that sets the current RNG
  settings, from a variety of specifications. Its methods
  define the workhorse functions that are called by
  \code{setRNG}.
}
\details{
  This function handles single number RNG specifications in
  the following way: \describe{ \item{integers}{Return them
  unchanged, considering them as encoded RNG kind
  specification (see \code{\link{RNG}}). No validity check
  is performed.} \item{real numbers}{If \code{num.ok=TRUE}
  return them unchanged. Otherwise, consider them as
  (pre-)seeds and pass them to \code{\link{set.seed}} to
  get a proper RNG seed. Hence calling \code{getRNG(1234)}
  is equivalent to \code{set.seed(1234); getRNG()} (See
  examples). } }

  Think of a sequence of separate computations, from which
  only one result is used for the result (e.g. the one that
  maximises a likelihood): \code{getRNG1} would return the
  RNG settings to reproduce the complete sequence of
  computations, while \code{getRNG} would return the RNG
  settings necessary to reproduce only the computation
  whose result has maximum likelihood.
}
\section{Methods}{
  \describe{

  \item{.getRNG}{\code{signature(object = "ANY")}: Default
  method that tries to extract RNG information from
  \code{object}, by looking sequentially to a slot named
  \code{'rng'}, a slot named \code{'rng.seed'} or an
  attribute names \code{'rng'}.

  It returns \code{NULL} if no RNG data was found. }

  \item{.getRNG}{\code{signature(object = "missing")}:
  Returns the current RNG settings. }

  \item{.getRNG}{\code{signature(object = "list")}: Method
  for S3 objects, that aims at reproducing the behaviour of
  the function \code{getRNG} of the package \code{getRNG}.

  It sequentially looks for RNG data in elements
  \code{'rng'}, \code{noise$rng} if element \code{'noise'}
  exists and is a \code{list}, or in attribute
  \code{'rng'}. }

  \item{.getRNG}{\code{signature(object = "numeric")}:
  Method for numeric vectors, which returns the object
  itself, coerced into an integer vector if necessary, as
  it is assumed to already represent a value for
  \code{\link{.Random.seed}}. }

  \item{getRNG1}{\code{signature(object = "ANY")}: Default
  method that is identical to \code{getRNG(object, ...)}. }

  \item{.setRNG}{\code{signature(object = "character")}:
  Sets the RNG to kind \code{object}, assuming is a valid
  RNG kind: it is equivalent to \code{RNGkind(object, ...}.
  All arguments in \code{...} are passed to
  \code{\link{RNGkind}}. }

  \item{.setRNG}{\code{signature(object = "numeric")}: Sets
  the RNG settings using \code{object} directly the new
  value for \code{.Random.seed} or to initialise it with
  \code{\link{set.seed}}. }

  }
}
\examples{
# get current RNG settings
s <- getRNG()
head(s)
showRNG(s)

# get RNG from a given single numeric seed
s1234 <- getRNG(1234)
head(s1234)
showRNG(s1234)
# this is identical to the RNG seed as after set.seed()
set.seed(1234)
identical(s1234, .Random.seed)
# but if num.ok=TRUE the object is returned unchanged
getRNG(1234, num.ok=TRUE)

# single integer RNG data = encoded kind
head(getRNG(1L))

# embedded RNG data
s <- getRNG(list(1L, rng=1234))
identical(s, s1234)
# test for embedded RNG data
hasRNG(1)
hasRNG( structure(1, rng=1:3) )
hasRNG( list(1, 2, 3) )
hasRNG( list(1, 2, 3, rng=1:3) )
hasRNG( list(1, 2, 3, noise=list(1:3, rng=1)) )
head(nextRNG())
head(nextRNG(1234))
head(nextRNG(1234, ndraw=10))
obj <- list(x=1000, rng=123)
setRNG(obj)
rng <- getRNG()
runif(10)
set.seed(123)
rng.equal(rng)
# set RNG kind
old <- setRNG('Marsaglia')
# restore
setRNG(old)
# directly set .Random.seed
rng <- getRNG()
r <- runif(10)
setRNG(rng)
rng.equal(rng)

# initialise from a single number (<=> set.seed)
setRNG(123)
rng <- getRNG()
runif(10)
set.seed(123)
rng.equal(rng)
}
\seealso{
  \code{\link{.Random.seed}}, \code{\link{showRNG}}
}
\keyword{methods}

