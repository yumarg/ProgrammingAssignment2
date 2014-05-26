## This file contains code that calculates and caches the inverse (xinverse) of a matrix (x) instead of computing it repeatedly
## functions included in this file are: makeCacheMatrix() and cacheSolve() 

## The makeCacheMatrix() function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    xinverse <- NULL

    set <- function(y) {
        x <<- y
        xinverse <<- NULL
    }
    

    get <- function() x
    setInverse <- function(solve) xinverse <<- solve
    getInverse <- function() xinverse
    
    # List of functions in makeCacheMatrix() function
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The cacheSolve() function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    xinverse <- x$getInverse()

    if(!is.null(xinverse)) {
        message("getting cached data")
        return(xinverse)
    }

    data <- x$get()
    xinverse <- solve(data, ...)
    x$setInverse(xinverse)
    xinverse
}