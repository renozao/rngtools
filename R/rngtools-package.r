# Copyright (C) 2009-2021 Renaud Gaujoux
# 
# This file is part of the rngtools package for R. 
# This program is free software; you can redistribute it and/or
# modify it under the terms of the GNU General Public License
# as published by the Free Software Foundation; either version 3
# of the License, or (at your option) any later version.
# 
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program; if not, write to the Free Software
# Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.

#' Utility functions for working with Random Number Generators
#' 
#' This package contains a set of functions for working with
#' Random Number Generators (RNGs). In particular, it defines a generic
#' S4 framework for getting/setting the current RNG, or RNG data
#' that are embedded into objects for reproducibility.
#' 
#' Notably, convenient default methods greatly facilitate the way current
#' RNG settings can be changed.
#' 
#' @name rngtools
#' @docType package
#' 
#' @import digest
#' @import methods
#' 
#' @examples 
#' 
#' showRNG()
#' s <- getRNG()
#' RNGstr(s)
#' RNGtype(s)
#' 
#' # get what would be the RNG seed after set.seed
#' s <- nextRNG(1234)
#' showRNG(s)
#' showRNG( nextRNG(1234, ndraw=10) )
#' 
#' # change of RNG kind
#' showRNG()
#' k <- RNGkind()
#' k[2L] <- 'Ahrens'
#' try( RNGkind(k) )
#' setRNG(k)
#' showRNG()
#' # set encoded kind 
#' setRNG(501L)
#' showRNG()
#' 
#' # use as set seed
#' setRNG(1234)
#' showRNG()
#' r <- getRNG()
#' 
#' # extract embedded RNG specifications
#' runif(10)
#' setRNG(list(1, rng=1234))
#' rng.equal(r)
#' 
#' # restore default RNG (e.g., after errors)
#' RNGrecovery()
#' showRNG()
#' 
NULL

.onLoad <- function(libname, pkgname){
  # initialize RNG seed if not defined yet
  rs <- get0(".Random.seed", .GlobalEnv)
  if( is.null(rs) ) RNGkind_default()
  
  
}
