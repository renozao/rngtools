\name{RNGstr}
\alias{RNGdigest}
\alias{RNGinfo}
\alias{RNGstr}
\alias{RNGtype}
\alias{showRNG}
\title{Formatting RNG Information}
\usage{
  RNGstr(object, n = 7L, ...)

  RNGtype(object, ..., provider = FALSE)

  showRNG(object = getRNG(), indent = "#", ...)

  RNGinfo(object = getRNG(), ...)

  RNGdigest(object = getRNG())
}
\arguments{
  \item{object}{RNG seed (i.e. an integer vector), or an
  object that contains embedded RNG data. For
  \code{RNGtype} this must be either a valid RNG seed or a
  single integer that must be a valid encoded RNG kind (see
  \code{\link{RNGkind}}).}

  \item{n}{maximum length for a seed to be showed in full.
  If the seed has length greater than \code{n}, then only
  the first three elements are shown and a digest hash of
  the complete seed is appended to the string.}

  \item{provider}{logical that indicates if the library
  that provides the RNG should also be returned as a third
  element.}

  \item{indent}{character string to use as indentation
  prefix in the output from \code{showRNG}.}

  \item{...}{extra arguments passed to \code{RNGtype}.}
}
\value{
  a single character string

  \code{RNGtype} returns a 2 or 3-long character vector.
}
\description{
  These functions retrieve/prints formated information
  about RNGs.

  \code{RNGtype} returns the same type of values as
  \code{RNGkind()} (character strings), except that it can
  extract the RNG settings from an object. If \code{object}
  is missing it returns the kinds of the current RNG
  settings, i.e. it is identical to \code{RNGkind()}.

  \code{showRNG} displays human readable information about
  RNG settings. If \code{object} is missing it displays
  information about the current RNG.

  \code{RNGinfo} is equivalent to \code{RNGtype} but
  returns a named list instead of an unnamed character
  vector.

  \code{RNGdigest} computes a hash from the RNG settings
  associated with an object.
}
\details{
  All functions can retrieve can be called with objects
  that are -- valid -- RNG seeds or contain embedded RNG
  data, but none of them change the current RNG setting. To
  effectively change the current settings on should use
  \code{\link{setRNG}}.

  \code{RNGstr} returns a description of an RNG seed as a
  single character string.

  \code{RNGstr} formats seeds by collapsing them in a comma
  separated string. By default, seeds that contain more
  than 7L integers, have their 3 first values collapsed
  plus a digest hash of the complete seed.
}
\examples{
# default is a 626-long integer
RNGstr()
# what would be the seed after seeding with set.seed(1234)
RNGstr(1234)
# another RNG (short seed)
RNGstr(c(401L, 1L, 1L))
# no validity check is performed
RNGstr(2:3)
# get RNG type
RNGtype()
RNGtype(provider=TRUE)
RNGtype(1:3)

# type from encoded RNG kind
RNGtype(107L)
# this is different from the following which treats 107 as a seed for set.seed
RNGtype(107)
showRNG()
# as after set.seed(1234)
showRNG(1234)
showRNG()
set.seed(1234)
showRNG()
# direct seeding
showRNG(1:3)
# this does not change the current RNG
showRNG()
showRNG(provider=TRUE)
# get info as a list
RNGinfo()
RNGinfo(provider=TRUE)
# from encoded RNG kind
RNGinfo(107)
# compute digest hash from RNG settings
RNGdigest()
RNGdigest(1234)
# no validity check is performed
RNGdigest(2:3)
}

