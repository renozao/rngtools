# Unit test for getRNG
# 
# Author: Renaud Gaujoux
# Copyright: Renaud Gaujoux (2009-2021)
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
###############################################################################

context("Get/Set RNG")

test_that('getRNG', {

	RNGkind_default()
	on.exit( RNGrecovery() )
	
	checker <- function(x, y, ..., msg=NULL, drawRNG=TRUE){
		
		if( drawRNG ) runif(10)
		fn <- getRNG
		oldRNG <- RNGseed()
		if( !missing(x) ){
			d <- fn(x, ...)
			cl <- paste0(class(x), '(', length(x), ')')
		}else{
			d <- fn()
			cl <- 'MISSING'
		}
		newRNG <- RNGseed()
		.msg <- function(x) paste(cl, ':', x, '[', msg, ']')
		expect_identical(oldRNG, newRNG, .msg("does not change RNG"))
    expect_identical(d, y, .msg("result is correct") )
	}
	
	set.seed(123456)
	seed123456 <- .Random.seed
	checker(, seed123456, msg="No arguments: returns .Random.seed", drawRNG=FALSE)
	checker(123456, seed123456, msg="Single numeric argument: returns .Random.seed as it would be after setting the seed")
	checker(123456, 123456, num.ok=TRUE, msg="Single numeric argument + num.ok: returns argument unchanged")
	checker(.Random.seed, .Random.seed, msg="Integer seed argument: returns its argument unchanged")
	checker(as.numeric(.Random.seed), .Random.seed, msg="Numeric seed argument: returns its argument as an integer vector")
	checker(2:3, 2:3, msg="Integer INVALID seed vector argument: returns its argument unchanged")
	checker(c(2,3), c(2L,3L), msg="Numeric INVALID seed vector argument: returns its argument as an integer vector")
	checker(1L, 1L, msg="Single integer = Encoded RNG kind: returns it unchanged")
	checker(1000L, 1000L, msg="Invalid single integer = Encoded RNG kind: returns it unchanged")
	
})

test_that('setRNG', {
	
	RNGkind_default()
	on.exit( RNGrecovery() )
	
	checker <- function(x, y, tset, drawRNG=TRUE){
	
		on.exit( RNGrecovery() )
		
		if( drawRNG ) runif(10)
		oldRNG <- RNGseed()
		d <- force(x)
		newRNG <- RNGseed()
		
		msg <- function(x, ...) paste(tset, ':', ...)
		expect_true(!identical(oldRNG, newRNG), msg("changes RNG"))
    expect_identical(getRNG(), y, msg("RNG is correctly set") )
    expect_identical(d, oldRNG, msg("returns old RNG") )
	}
	
	set.seed(123456)
	refseed <- .Random.seed
	checker(setRNG(123456), refseed, "Single numeric: sets current RNG with seed")
	
	# setting kind with a character string
	set.seed(123)
	RNGkind('Mar')
	refseed <- .Random.seed
	RNGrecovery()
	set.seed(123)
	checker(setRNG('Mar'), refseed, "Single character: change RNG kind", drawRNG=FALSE)
	
	# setting kind with a character string
	set.seed(123)
	RNGkind('Mar', 'Ahrens')
	refseed <- .Random.seed
	RNGrecovery()
	set.seed(123)
	checker(setRNG('Mar', 'Ahrens'), refseed, "Two character strings: change RNG kind and normal kind", drawRNG=FALSE)
	RNGrecovery()
	set.seed(123)
	checker(setRNG(c('Mar', 'Ahrens')), refseed, "2-long character vector: change RNG kind and normal kind", drawRNG=FALSE)
	
	# setting kind
	set.seed(123456, kind='Mar')
	refseed <- .Random.seed
	checker(setRNG(123456, kind='Mar'), refseed, "Single numeric + kind: change RNG kind + set seed")
	
	# setting Nkind
	set.seed(123456, normal.kind='Ahrens')
	refseed <- .Random.seed
	checker(setRNG(123456, normal.kind='Ahrens'), refseed
				, "Single numeric + normal.kind: change RNG normal kind + set seed")
	
	# setting kind and Nkind
	set.seed(123456, kind='Mar', normal.kind='Ahrens')
	refseed <- .Random.seed
	checker(setRNG(123456, kind='Mar', normal.kind='Ahrens'), refseed
			, "Single numeric + kind + normal.kind: change RNG all kinds + set seed")
	
	# with seed length > 1
	refseed <- as.integer(c(201, 0, 0))
	checker(setRNG(refseed), refseed, "numeric vector: directly set seed")
	
	refseed <- .Random.seed
	expect_error( setRNG(2:3), info = "numeric vector: throws an error if invalid value for .Random.seed")
  expect_identical( .Random.seed, refseed, ".Random.seed is not changed in case of an error in setRNG")
    
    oldRNG <- getRNG()
    expect_error(setRNG(1234L), info = "Error with invalid integer seed")
    expect_identical(oldRNG, getRNG(), "RNG still valid after error")
    expect_error(setRNG(123L), info = "Error with invalid RNG kind")
    expect_identical(oldRNG, getRNG(), "RNG still valid after error")

    # changes in R >= 3.0.2: invalid seeds only throw warning
    if( testRversion('> 3.0.1') ){
        oldRNG <- getRNG()
        expect_warning(setRNG(1234L, check = FALSE), "\\.Random\\.seed.* is not .* valid"
                        , info = "Invalid integer kind: Warning if check = FALSE")
        expect_identical(oldRNG, getRNG(), "RNG keep old value")
        RNGrecovery()
        oldRNG <- getRNG()
        expect_warning(setRNG(123L, check = FALSE), "\\.Random\\.seed.* is not .* valid"
                        , info = "Invalid kind: Warning if check = FALSE")
        expect_identical(oldRNG, getRNG(), "RNG keep old value")
                                                
    }
	
})


test_that('.Random.seed init', {
  cmd <- 'Rscript -e "RNGkind(); rngtools::RNGseq(2, seed=200); RNGkind();"'
  out <- capture.output(system(cmd, intern = TRUE))
  expect_true(!any(grepl("Ecuyer", out)))
  
  tmpf <- tempfile(tmpdir = getwd())
  on.exit( unlink(tmpf) )
  cmd <- sprintf('Rscript -e "runif(1); s <- .Random.seed; RNGkind(); rngtools::RNGseq(2, seed=200); RNGkind(); saveRDS(identical(s, .Random.seed), \\"%s\\")"',
                 tmpf)
  out <- capture.output(system(cmd, intern = TRUE))
  print(out)
  expect_true(readRDS(tmpf))
  
})

