# Unit test 
# 
# Author: Renaud Gaujoux (edited by Max Kuhn)
# Created: 01 May 2018
# Copyright: Renaud Gaujoux (2017-2021)
# Copyright: Max Kuhn (2019)
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
###############################################################################

context("Formatting functions")

library(utils)

# RUnit-testthat bridge 
checkIdentical <- function(x, y, msg){
  expect_identical(x, y, info = msg)
  
}
checkTrue <- function(x, y, msg){
  expect_true(x, info = msg)
  
}
##

checkFun <- function(fn, name){
  
  function(x, ...){
    
    oldRNG <- RNGseed()
    if( !missing(x) ){
      d <- fn(x)
      obj <- getRNG(x)
      cl <- class(x)
    }else{
      d <- fn()
      obj <- getRNG()
      cl <- 'MISSING'
    }
    newRNG <- RNGseed()
    msg <- function(x, ...) paste(name, '-', cl, ':', x, '[', ..., ']')
    expect_identical(oldRNG, newRNG, info = msg("does not change RNG", ...))
    
    #
    expect_true( isString(d), info = msg("result is a character string", ...))
    expect_identical(d, fn(obj), info = msg("digest is from the RNG setting", ...))
    
  }
}

test_that('RNGdigest and RNGstr', {
  
  RNGkind_default()
  on.exit( RNGrecovery() )
  
  fn <- c('RNGdigest', 'RNGstr')
  sapply(fn, function(f){
        fn <- getFunction(f, where='package:rngtools')
        checker <- checkFun(fn, f)
        
        checker()
        checker(1234)
        checker(1:3, 'Valid seed')
        checker(2:3, 'Invalid seed')
        x <- list(10, rng=c(401L, 1L, 1L))
        checker(x, 'list with rng slot')
        
      })
  TRUE
  
})

# Note: in R 3.6, RNGkind returns a vector of length 3 (vs 2 in previous versions)
# Here we set the expected default length according to the runtime version 
checkRNGtype <- function(x, ..., expL = .RNGkind_length()){
  
  fn <- RNGtype
  oldRNG <- getRNG()
  if( !missing(x) ){
    d <- fn(x)
    obj <- getRNG(x)
    cl <- paste0(class(x), '(', length(x), ')')
  }else{
    d <- fn()
    obj <- getRNG()
    cl <- 'MISSING'
  }
  newRNG <- getRNG()
  msg <- function(x, ...) paste(cl, ':', x, '[', ..., ']')
  expect_identical(oldRNG, newRNG, info = msg("does not change RNG", ...))
  
  #
  expect_true( is.character(d), msg("result is a character vector", ...) )
  expect_identical( length(d), expL, info = msg("result has correct length (", expL, ")", ...) )
  
}

test_that('RNGtype', {
  
  RNGkind('default', 'default')
  on.exit( RNGrecovery() )
  checker <- checkRNGtype
  
  checker()
  checker(1234, 'Valid single numeric seed')
  checker(1:3, 'Valid seed')
  checker(402L, 'Valid encoded kind')
  expect_true( !identical(RNGtype(402), RNGtype(402L)), "Single integer and real number does not give the same result")
  x <- list(10, rng=c(401L, 1L, 1L))
  checker(x, 'list with rng slot')
  
  # errors
  oldRNG <- getRNG()
  expect_error(RNGtype(2:3), info = "Error with invalid length seed")
  expect_identical(oldRNG, getRNG(), info = "RNG still valid after error")
  #
  
  oldRNG <- getRNG()
  expect_error(RNGtype(123L), info = "Error with invalid RNG kind")
  expect_identical(oldRNG, getRNG(), info = "RNG still valid after error")
  expect_error(RNGtype(1234L), info = "Error with invalid RNG integer")
  expect_identical(oldRNG, getRNG(), info = "RNG still valid after error")
  
})
