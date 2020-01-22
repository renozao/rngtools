# RNG formatting functions
# 
# Author: Renaud Gaujouc
###############################################################################

#' Formatting RNG Information
#' 
#' These functions retrieve/prints formated information about RNGs.
#' 
#' All functions can be called with objects that are -- valid -- 
#' RNG seeds or contain embedded RNG data, but none of them change the current 
#' RNG setting.
#' To effectively change the current settings on should use \code{\link{setRNG}}.
#' 
#' @name RNGstr
NULL

#' @describeIn RNGstr returns a description of an RNG seed as a single character string.
#' 
#' It formats seeds by collapsing them in a comma separated string.
#' By default, seeds that contain more than 7L integers, have their 3 first values 
#' collapsed plus a digest hash of the complete seed.
#' 
#' @param object RNG seed (i.e. an integer vector), or an object that contains
#' embedded RNG data.
#' For \code{RNGtype} this must be either a valid RNG seed or a single integer that 
#' must be a valid encoded RNG kind (see \code{\link{RNGkind}}).
#' @param n maximum length for a seed to be showed in full.
#' If the seed has length greater than \code{n}, then only the first three elements
#' are shown and a digest hash of the complete seed is appended to the string. 
#' 
#' @return a single character string
#' 
#' @export
#' @examples
#' 
#' # default is a 626-long integer
#' RNGstr()
#' # what would be the seed after seeding with set.seed(1234)
#' RNGstr(1234)
#' # another RNG (short seed)
#' RNGstr(c(401L, 1L, 1L))
#' # no validity check is performed 
#' RNGstr(2:3)
#' 
RNGstr <- function(object, n=7L, ...){
	
	if( missing(object) ){
		rp <- RNGprovider()
		rs <- getRNG()
		if( rp == 'base' || length(rs) > 1L )
			object <- rs
		else 
			return( "Unknown" )		
	}
	
	# extract seed from object
	seed <- getRNG(object, ...)
	if( is.null(seed) ) 'NULL'
	else if( is.numeric(seed) ){
		if( length(seed) > n ){
			paste(str_out(seed, 3L),  paste0('[', digest(seed), ']'))
		}else{
			str_out(seed, Inf)
		}
	}
	else
		paste(class(seed), ' [', digest(seed), ']', sep='')
}

#' @describeIn RNGstr extract the kinds of RNG and Normal RNG.
#'  
#' It returns the same type of values as \code{RNGkind()} (character strings), 
#' except that it can extract the RNG settings from an object.
#' If \code{object} is missing it returns the kinds of the current RNG settings, 
#' i.e. it is identical to \code{RNGkind()}.
#' 
#' @param provider logical that indicates if the library that provides the RNG
#' should also be returned as an extra element.
#' 
#' @return \code{RNGtype} returns a named character vector containing the types of the random number generator, which correspond
#' to the arguments accepted by [base::RNGkind].
#' Note that starting with R 3.6, the vector has length 3, while in previous R versions it has length 2 (no sample.kind element).
#' 
#' @export
#' @examples
#' 
#' # get RNG type
#' RNGtype()
#' RNGtype(provider=TRUE)
#' RNGtype(1:3)
#' 
#' # type from encoded RNG kind
#' RNGtype(107L)
#' # this is different from the following which treats 107 as a seed for set.seed
#' RNGtype(107)
#' 
RNGtype <- function(object, ..., provider=FALSE){
	
	res <- 
	if( missing(object) ){
		RNGkind()
	}else{
        old <- RNGseed()
		
		# extract RNG data
		rng <- getRNG(object, ...)
		if( is.null(rng) ){
			warning("Could not find embedded RNG data in ", deparse(substitute(object)), "."
					, " Returned current type.")
		}
		# setup restoration
		on.exit( RNGseed(old) )
		setRNG(rng)
		RNGkind()
	}
	# set RNGkind parameter names each element
	names(res) <- c("kind", "normal.kind", "sample.kind")[1:length(res)]
	
	# determine provider if requested
	if( provider ){
		prov <- RNGprovider(res)
		res <- c(res, provider = prov)
	}
	# return result
	res
}


# Returns the length of RNGkind output
# This is used in a few places to dynamically adapt to the changes in RNGkind output that were introduced in R 3.6
.RNGkind_length <- function(){
  length(RNGkind())
  
}

#' @describeIn RNGstr displays human readable information about RNG settings.
#' If \code{object} is missing it displays information about the current RNG.
#' 
#' @param indent character string to use as indentation prefix in the output 
#' from \code{showRNG}.
#' 
#' @export
#' @examples
#' showRNG()
#' # as after set.seed(1234)
#' showRNG(1234)
#' showRNG()
#' set.seed(1234)
#' showRNG()
#' # direct seeding
#' showRNG(1:3)
#' # this does not change the current RNG
#' showRNG()
#' showRNG(provider=TRUE)
#' 
showRNG <- function(object=getRNG(), indent='#', ...){
	
	# get kind
	tryCatch(suppressMessages(info <- RNGtype(object, ...))
			, error = function(e){
				stop("Could not show RNG due to error: ", conditionMessage(e))
			}
	)
	# show information
  n0 <- .RNGkind_length()
	cat(indent, "RNG kind: ", paste(info[1:n0], collapse=" / ")
			, if( length(info) > n0 ) paste('[', paste0(tail(info, -n0), collapse = ", "), ']', sep='')
			, "\n")
	cat(indent, "RNG state:", RNGstr(object), "\n")
} 

#' @describeIn RNGstr is equivalent to \code{RNGtype} but returns a named 
#' list instead of an unnamed character vector.
#' 
#' @param ... extra arguments passed to \code{RNGtype}.
#'  
#' @importFrom stats setNames
#' @export
#' @examples
#' # get info as a list
#' RNGinfo()
#' RNGinfo(provider=TRUE)
#' # from encoded RNG kind
#' RNGinfo(107)
#' 
RNGinfo <- function(object=getRNG(), ...){
	
	# get type
	kind <- RNGtype(object, ...)
	as.list(kind)
	
}


#' Checking RNG Differences in Unit Tests
#' 
#' \code{checkRNG} checks if two objects have the same RNG
#' settings and should be used in unit tests, e.g., with the \pkg{RUnit} 
#' package.
#' 
#' @param x,y objects from which RNG settings are extracted.
#' @param ... extra arguments passed to \code{\link[RUnit]{checkTrue}}.
#' 
#' @export
#' @rdname uchecks
#' @examples 
#' 
#' # check for differences in RNG
#' set.seed(123)
#' checkRNG(123)
#' try( checkRNG(123, 123) )
#' try( checkRNG(123, 1:3) )
#' 
checkRNG <- function(x, y=getRNG(), ...){
  if( !requireNamespace('RUnit') ){
    stop("Missing Suggests dependency: package 'RUnit' is required to check RNG in unit tests.")
    
  }
  RUnit::checkTrue(rng.equal(x, y), ...)
  
}
