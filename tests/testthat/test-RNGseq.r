# Unit tets for RNGseq
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

context("RNGseq: RNG streams")

test_that('RNGseq_seed', {
	
	# actual testing function
	.test_loc <- function(.msg, ..., .change=FALSE){
		msg <- function(...) paste(.msg, ':', ...)
		os <- RNGseed()
		on.exit(RNGseed(os))
		s <- RNGseq_seed(...)
		expect_true(length(s) == 7L && s[1] %% 100 == 7L, msg("RNGseq_seed returns a value of .Random.seed for L'Ecuyer-CMRG"))
		expect_identical(RNGseed()[1], os[1], msg("RNGseq_seed does not change the type of RNG"))
		
		if( !.change ) expect_identical(RNGseed(), os, msg("RNGseq_seed does not change the value of .Random.seed"))
		else expect_true( !identical(RNGseed(), os), msg("RNGseq_seed changes the value of .Random.seed"))
		s
	}
	
	# test in two RNG settings: default and L'Ecuyer
	.test <- function(.msg, ..., ss=NULL, .change=FALSE, Dchange=.change, Lchange=.change){
		os <- RNGseed()
		on.exit(RNGseed(os))
		
		# default RNG
		RNGkind_default()
		if( !is.null(ss) ) set.seed(ss)
		s1 <- .test_loc(paste(.msg, '- default'), ..., .change=Dchange)
		
		RNGkind("L'Ecuyer")
		if( !is.null(ss) ) set.seed(ss)
		s2 <- .test_loc(paste(.msg, "- CMRG"), ..., .change=Lchange)
		
		list(s1, s2)
	}
	
	os <- RNGseed()
	on.exit(RNGseed(os))
	
	RNGkind_default()
	
	# test different arguments
	s1 <- .test("seed=missing", ss=1, Dchange=TRUE, Lchange=FALSE)
	runif(10)
	s2 <- .test("seed=NULL", NULL, ss=1, Dchange=TRUE, Lchange=FALSE)
	expect_identical(s1, s2, "set.seed(1) + seed=missing and seed=NULL return identical results")
	
	# doRNG seed with single numeric
	runif(10)
	s3 <- .test("seed=single numeric", 1)
	expect_identical(s1[[1]], s3[[1]], "v1.4 - set.seed(1) + seed=missing and seed=1 return identical results when current RNG is NOT CMRG")
	expect_identical(s1[[2]], s3[[2]], "v1.4 - set.seed(1) + seed=missing and seed=1 return identical results when current RNG is CMRG")
	expect_true( !identical(s1[[1]], s1[[2]]), "v1.4 - set.seed(1) + seed=missing return NON identical results in different RNG settings")
	expect_true( !identical(s3[[1]], s3[[2]]), "v1.4 - seed=num return NON identical results in different RNG settings")
	
	# version < 1.4
#	doRNGversion("1.3.9999")
	s1 <- .test("v1.3 - seed=missing", ss=1, Dchange=TRUE, Lchange=TRUE, version=1)
	s3 <- .test("v1.3 - seed=single numeric", 1, version=1)
	expect_identical(s1[[1]], s3[[1]], "v1.3 - set.seed(1) + seed=missing and seed=1 return identical results when current RNG is NOT CMRG")
	expect_true( !identical(s1[[2]], s3[[2]]), "v1.3 - set.seed(1) + seed=missing and seed=1 return NON identical results when current RNG is CMRG")
	expect_true( !identical(s1[[1]], s1[[2]]), "v1.3 - set.seed(1) + seed=missing return NON identical results in different RNG settings")
	expect_true( !identical(s3[[1]], s3[[2]]), "v1.4 - seed=num return NON identical results in different RNG settings")
#	doRNGversion(NULL) 
	##
	
	.test("seed=single integer", 10L)
	# directly set doRNG seed with a 6-length
	.test("seed=6-length integer", 1:6)
	.test("seed=6-length numeric", as.numeric(1:6))
	s <- 1:6
	expect_identical(RNGseq_seed(s)[2:7], s, "RNGseq_seed(6-length) returns stream to the given value")
	# directly set doRNG seed with a full 7-length .Random.seed
	.test("seed=7-length integer", c(407L,1:6))
	.test("seed=7-length numeric", as.numeric(c(107L,1:6)))
	s <- c(407L,1:6)
	expect_identical(RNGseq_seed(s), s, "RNGseq_seed(7-length) returns complete seed with the given value")
	
	# errors
	os <- RNGseed()
	expect_error(RNGseq_seed(NA), info = "seed=NA throws an exception")
	expect_identical(os, RNGseed(), "RNGseq_seed(NA) does not change the value of .Random.seed [error]")
	
	# Current CMRG is L'Ecuyer
	RNGkind("L'Ecuyer")
	set.seed(456)
	s <- RNGseed()
	r <- RNGseq_seed(NULL)
	expect_identical(s, r, "Current is CMRG: seed=NULL return current stream")
	runif(10)
	expect_identical(s, RNGseq_seed(456), "Current is CMRG: seed=numeric return stream seeded with value")
	
})

test_that('RNGseq', {
	
	os <- RNGseed()
	on.exit(RNGseed(os))
	
	# actual testing function
	.test_loc <- function(.msg, n, ..., .list=TRUE, .change=FALSE){
		msg <- function(...) paste(.msg, ':', ...)
		os <- RNGseed()
		on.exit(RNGseed(os))
	
		s <- RNGseq(n, ...)
		
		if( !.change ) expect_identical(RNGseed(), os, msg("the value of .Random.seed is not changed"))
		else expect_true( !identical(RNGseed(), os), msg("the value of .Random.seed does change"))
		
		if( .list )	expect_true(is.list(s), msg("result is a list"))
		else{
			expect_true(is.integer(s), msg("result is an integer vector"))
			s <- list(s)
		}
		
		expect_true(length(s) == n, msg("result has correct length"))
		expect_true(all(sapply(s, length) == 7L), msg("each element has length 7"))
		expect_true(all(sapply(s, function(x) x[1] %% 100) == 7L), msg("each element has correct RNG kind"))
		s
	}
	
	.test <- function(msg, n, ...){
		set.seed(1)
		s1 <- .test_loc(paste(msg, '- no seed'), n, ..., .change=TRUE)
		runif(1)
		s2 <- .test_loc(paste(msg, '- seed=1'), n, 1, ..., .change=FALSE)
		#expect_identical(s1, s2, paste(msg, " - set.seed(1) + no seed is identical to seed=1"))
		.test_loc(paste(msg, '- seed=1:6'), n, 1:6, ...)
	}
	.test("n=1", 1, .list=FALSE)
	.test("n=2", 2)
	.test("n=5", 5)
	
	# with full list
	s <- RNGseq(3)
	expect_identical(RNGseq(length(s), s), s, "If passing a complete list: returns the list itself")
	s3 <- RNGseq(5)
	s <- structure(s, rng=s3)
	expect_identical(RNGseq(length(s3), s), s3, "If passing a complete list in rng S3 slot: returns the complete slot")
	#

	# Current RNG is CMRG
	set.seed(456, "L'Ec")
	s <- .Random.seed
	ref <- list(s, nextRNGStream(s), nextRNGStream(nextRNGStream(s)))
	rs <- RNGseq(3, 456)
	expect_identical(rs, ref, "Current RNG is CMRG: RNGseq(n, num) returns RNG streams that start with stream as set.seed")
	expect_identical(s, .Random.seed, "Current RNG is CMRG: RNGseq(n, num) did not change random seed")
	
	runif(10)
	s <- .Random.seed
	ref <- list(s, nextRNGStream(s), nextRNGStream(nextRNGStream(s)))
	rs2 <- RNGseq(3)
	expect_identical(rs2, ref, "Current RNG is CMRG: RNGseq(n) returns RNG streams that start with current stream")
	expect_identical(.Random.seed, nextRNGStream(tail(rs2,1)[[1]]), "Current RNG is CMRG: RNGseq(n) changes current random seed to next stream of last stream in sequence")
	
})
