## Put comments here that give an overall description of what your
## functions do

## Write a short comment describing this function

makeCacheMatrix <- function(x = matrix()) {
	i <- NULL
	set <- function(matrix) {
		m <<- matrix
		i <<- NULL
	}
	get <- function() m
	setInv <- function(inv) i <<- inv
	getInv <- function() i
	list(set = set,
	     get = get,
	     setInv = setInv,
	     getInv = getInv)
}


## Write a short comment describing this function

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
	i <- m$getInv()
	if(!is.null(i)) {
		message("getting cached data")
		return(i)
	}
	data <- m$get()
	i <- solve(data, ...)
	m$setInv(i)
	i
}
