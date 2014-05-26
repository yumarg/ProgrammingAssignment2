## This file contains code that calculates and caches the inverse (xinverse) of a matrix (x) instead of computing it repeatedly
## functions included in this file are: makeCacheMatrix() and cacheSolve() 

## The makeCacheMatrix() function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    # Assign xinverse with NULL
    xinverse <- NULL
    
    # set() makeCacheMatrix "x" value to be like "y" and set "xinverse" value to NULL
    set <- function(y) {
        x <<- y
        xinverse <<- NULL
    }
    
    # get() will return "x" value when called
    get <- function() x
    # setInverse() will set the inverse value of the matrix provided by "solve" argument
    setInverse <- function(solve) xinverse <<- solve
    # getInverse() will return the inverse value stored in "xinverse"
    getInverse <- function() xinverse
    
    # List of functions in makeCacheMatrix() function
    list(set = set, get = get,
         setInverse = setInverse,
         getInverse = getInverse)
}


## The cacheSolve() function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    # call getInverse Function and assign the value to "xinverse"
    xinverse <- x$getInverse()
    # condition checks if inverse is already set
    if(!is.null(xinverse)) {
        # if yes, write a message and return "xinverse"
        message("getting cached data")
        return(xinverse)
    }
    # get the value and assign it to data
    data <- x$get()
    # use solve to obtain the inverse of the matrix 'x'
    xinverse <- solve(data, ...)
    # call the function setInverse() to set the inverse value
    x$setInverse(xinverse)
    # return the inverse value 
    xinverse
}