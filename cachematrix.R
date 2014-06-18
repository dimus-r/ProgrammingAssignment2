## This module contains functions to inverse a matrix:
## - makeCacheMatrix accepts a square matrix as an input and prepares the object 
##   that can be passed to cacheSolve function
## - cacheSolve returns inverted matrix from the object prepared by makeCacheMatrix


## makeCacheMatrix creates a list containing functions to
## - set the original matrix ($set)
## - get the original matrix ($get)
## - set the inverted matrix ($setTransform)
## - get the inverted matrix ($getTransform)
##
## Note that we are using word "Transform" 
## and not for example "Inverse" to emphasize that the object can be used 
## to perform and cache different kinds of calculation

makeCacheMatrix <- function(x = matrix()) {
    m <- NULL ## transformed matrix
    
    ## pay attention to "<<-", we are assigning values in the parent environmnent
    set <- function(y) {
        x <<- y
        m <<- NULL ## result is no longer valid
    }
    
    get <- function() x
    
    setTransform <- function(inverse) m <<- inverse
    
    getTransform <- function() m
    
    list(set = set, get = get,
         setTransform = setTransform,
         getTransform = getTransform)
}


## Return a matrix that is the inverse of 'x'
## Reads from cache if the result was previously calculated,
## otherwise solves the matrix and caches the result
cacheSolve <- function(x, ...) {
    inverse <- x$getTransform()
    if(!is.null(inverse)) { ## result is cached
        return(inverse)
    }
    
    inverse <- solve(x$get(), ...) ## finally we are getting the result
    x$setTransform(inverse) ## cache it
    inverse
}
