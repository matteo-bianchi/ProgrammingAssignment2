## The first function, 'makeCacheMatrix', creates
## an object that contains a matrix and its inverse;
## the second, 'cacheSolve', computes the inverse
## of a matrix created with the previous function.

## Creates an object that contains a matrix 'x' and its inverse;
## both of them can be obtained and set via suitable functions.
makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(y) {
		x <<- y
		i <<- NULL
	}
	get <- function() x
	setInv <- function(inv) i <<- inv
	getInv <- function() i
	list(set = set,
	     get = get,
	     setInv = setInv,
	     getInv = getInv)
}

## Computes the inverse of matrix 'x': if already computed it is loaded
## from the cache, otherwise it is computed and stored in the cache.
cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	i <- x$getInv()
	if (!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- x$get()
	i <- solve(data, ...)
	x$setInv(i)
	i
}
