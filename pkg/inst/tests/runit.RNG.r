# Unit test for getRNG
# 
# Author: Renaud Gaujoux
###############################################################################

test.getRNG <- function(){

	on.exit( RNGrecovery() )
	
	checker <- function(x, y, ..., drawRNG=TRUE){
		
		if( drawRNG ) runif(10)
		fn <- getRNG
		oldRNG <- RNGseed()
		if( !missing(x) ){
			d <- fn(x)
			obj <- getRNG(x)
			cl <- str_c(class(x), '(', length(x), ')')
		}else{
			d <- fn()
			obj <- getRNG()
			cl <- 'MISSING'
		}
		newRNG <- RNGseed()
		msg <- function(x, ...) paste(cl, ':', x, '[', ..., ']')
		checkIdentical(oldRNG, newRNG, msg("does not change RNG", ...))
		checkIdentical(d, y, msg("result is correct", ...) )
	}
	
	set.seed(123456)
	seed123456 <- .Random.seed
	checker(, seed123456, "No arguments: returns .Random.seed", drawRNG=FALSE)
	checker(123456, seed123456, "Single numeric argument: returns .Random.seed as it would be after setting the seed")
	checker(.Random.seed, .Random.seed, "Numeric seed argument: returns its argument unchanged")
	checker(2:3, 2:3, "Numeric INVALID seed argument: returns its argument unchanged")
	
}

test.setRNG <- function(){
	
	on.exit( RNGrecovery() )
	
	checker <- function(x, y, tset, drawRNG=TRUE){
	
		on.exit( RNGrecovery() )
		
		if( drawRNG ) runif(10)
		oldRNG <- RNGseed()
		d <- force(x)
		newRNG <- RNGseed()
		
		msg <- function(x, ...) paste(tset, ':', ...)
		checkTrue(!identical(oldRNG, newRNG), msg("changes RNG"))
		checkIdentical(getRNG(), y, msg("RNG is correctly set") )
		checkIdentical(d, oldRNG, msg("returns old RNG") )
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
	checkException( setRNG(2:3), "numeric vector: throws an error if invalid value for .Random.seed")
	checkIdentical( .Random.seed, refseed, ".Random.seed is not changed in case of an error in setRNG")
	
}

