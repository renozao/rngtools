# Version 1.5.3

  * Add NEWS file (Issue #9)
  * Fix behaviour when .Random.seed is not set on the first call to `rngtools::setRNG()` (Issue #7)

# Version 1.5.2

  * Fix error reported by CRAN on R-devel (2021-09-20) (Issue #8)
  This fix changes the behaviour of `setRNG()` when setting the random seed to an invalid value
  in call `setRNG(..., check = FALSE)`: this now leaves the RNG unchanged, instead of putting it in a bad state.
  This is for the better and it is highly likely that nobody would have faced this situation anyway (default is `check = TRUE`).
