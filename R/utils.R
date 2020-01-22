
# from pkgmaker 0.30
isNumber <- function(x){ 
	is.numeric(x) && length(x) == 1
  
}

# from pkgmaker 0.30
isReal <- function(x){ 
	isNumber(x) && !is.integer(x)
}

# from pkgmaker 0.30
isInteger <- function(x){ 
	is.integer(x) && length(x) == 1
}

# adapted from pkgmaker 0.30
testRversion <- function(x, test=1L){
  # emulate stringr functions
  str_trim <- function(x) sub("^ *(.*[^ ]) *$", "\\1", x)
  str_match <- function(x, pattern, n){
    r <- regexpr(pattern, x, perl = TRUE)
    start <- attr(r, "capture.start")
    len <- attr(r, "capture.length")
    res <- sapply(seq(nrow(start)), FUN = function(i){
      s <- start[i, ]
      if( s[1L] < 0 ) return(rep(NA_character_, length(s) + 1L))
      hit <- sapply(seq_along(s), function(j){
        s <- s[j]
        substr(x[i], s, s + len[i, j] - 1L)
      })
      c(x[i], hit)
      
    })
    t(res)
  }
  ##
  
	rv <- Rversion()
    op <- '=='
    if( grepl("^[=<>]", str_trim(x)) ){
        m <- str_match(x, "^([<>=]=?)(.*)")
        if( is.na(m[, 1]) ) stop('Invalid version specification: ', x)
        op <- m[, 2]
        if( op == '=' ) op <- '=='
        x <- str_trim(m[, 3L])
        if( !missing(test) ) warning("Ignoring argument `test`: comparison operator was passed in argument `x`")
        test <- 0L
    }
	do.call(op, list(utils::compareVersion(rv, x), test))
	
}

# from pkgmaker 0.30
is_NA <- function(x){ 
    is.atomic(x) && length(x) == 1L && is.na(x)
#   x <- unname(x)
#	identical(x, NA) || identical(x, as.character(NA)) || identical(x, as.numeric(NA)) || identical(x, as.integer(NA))
}

# from pkgmaker 0.30
Rversion <- function(){
	paste(R.version$major, R.version$minor, sep='.')
}

# from pkgmaker 0.30
str_out <- function(x, max=3L, quote=is.character(x), use.names=FALSE, sep=", ", total = FALSE){
	if( is_NA(max) ) max <- Inf
	suffix <- NULL
    nTotal <- length(x)
	if( max > 2 && length(x) > max ){
		suffix <- "..."
		x <- c(head(x, max-1), tail(x, 1))
	}
	x <- head(x, max)
	
	# add quotes if necessary
	quote <- 
			if( isTRUE(quote) ) "'"
			else if( is.character(quote) ) quote
	if( !is.null(quote) ) x <- unlist(lapply(x, function(v) paste(quote,v,quote, sep='')))
	else if( all(sapply(x, isInteger)) ) x <- unlist(lapply(x, function(v) paste0(v,'L')))
	# add names if necessary
	if( use.names && !is.null(names(x)) ){
		nm <- paste0(names(x),'=')
		x <- paste(ifelse(nm=='=','',nm), x, sep='')
	}
	# insert suffix
	if( !is.null(suffix) ){
		x <- c(head(x, length(x)-1L), suffix, tail(x, 1L))
	}
	s <- paste(paste(x, collapse=sep), sep='')
	
	if( total ) s <- paste0(s, ' (', format(nTotal, big.mark=",", scientific=F), ' total)')
	
	# return formatted string 
	s
}

# from pkgmaker 0.30
isString <- function (x, y, ignore.case = FALSE) 
{
    if (res <- is.character(x) && length(x) == 1L) {
        if (!missing(y)) {
            if (!isString(y)) 
                stop("Invalid argument 'y': must be a string itself.")
            if (ignore.case) {
                x <- toupper(x)
                y <- toupper(y)
            }
            res <- x == y
        }
    }
    res
}
