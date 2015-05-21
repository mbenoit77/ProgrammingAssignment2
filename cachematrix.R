## This file contains two functions: makeCacheMatrix and cacheSolve.
## Function makeCacheMatrix creates a special "matrix" object that 
##		can cache its inverse
## Function cacheSolve computes the inverse of the special matrix, 
##		returning the cached version if it already exists


## makeCacheMatrix creates a special "matrix", which is really a list containing a function to
##		1. set the value of the matrix
##		2. get the value of the matrix
##		3. set the value of the inverse
##		4. get the value of the inverse

makeCacheMatrix <- function(x = matrix()) {
	# additional data member for the inverse
	inv <- NULL

	# four methods: set, get, setInverse, getInverse
	set <- function(y) {
		x <<- y
		inv <<- NULL
	}
	get <- function() x
	setInverse <- function(inverse) inv <<- inverse
	getInverse <- function() inv

	# pass back the methods as a list
	list(set = set, get = get,
		 setInverse = setInverse,
		 getInverse = getInverse)
}


## cacheSolve calculates the inverse of the special "vector" created with the above function. 
## Returns the inverse from the cache, if it exists; otherwise perfoms calculation and caches result.

cacheSolve <- function(x, ...) {
	## Return a matrix that is the inverse of 'x'
	
	# try to grab the inverse from the cache
	inv <- x$getInverse()
	# if the inverse is not null, return that
	if(!is.null(inv)) {
		message("getting cached data")
		return(inv)
	}
	# otherwise, get the data and the call solve() to get the inverse
	data <- x$get()
	inv <- solve(data, ...)
	# set the inverse in the cache
	x$setInverse(inv)
	# return the newly obtained inverse
	inv
}
