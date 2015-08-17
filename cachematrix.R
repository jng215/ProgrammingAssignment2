##
## Coursera - R Programming (rprog-031) - Assignment 2
##
## Overview: Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it 
## repeatedly. In this file are two functions that will provide the matrix "cache"
## features.
##


## Function: makeCacheMatrix
## Purpose:  This function creates a special "matrix" object that can 
##           cache its inverse.
## Created:  jng215, 8/17/2015
##
makeCacheMatrix <- function(x = matrix()) {
    mt <- NULL
    set <- function(y) {
        x <<- y
        mt <<- NULL
    } 
    get <- function() x
    setinverse <- function(inverse) mt <<- inverse
    getinverse <- function() mt
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Function: cacheSolve
## Purpose:  This function computes the inverse of the special "matrix" returned 
##           by makeCacheMatrix above. If the inverse has already been calculated  
##           (and the matrix has not changed), then the cachesolve should retrieve  
##           the inverse from the cache.
## Created:  jng215, 8/17/2015
##
cacheSolve <- function(x, ...) {
    mt <- x$getinverse()
    if(!is.null(mt)) {
        message("getting cached data")
        return(mt)
    }
    data <- x$get()
    mt <- solve(data, ...)
    x$setinverse(mt)
    mt
}



# Sample tests using sample Matrix described in the URL below:
# http://www.mathwords.com/i/inverse_of_a_matrix.htm
#
# b <- makeCacheMatrix(matrix(c(4,3,3,2),nrow=2,ncol=2))
# b$get()
# cacheSolve(b)
# cacheSolve(b)  # get cached data
# #
# c <- makeCacheMatrix(matrix(c(1,3,2,4),nrow=2,ncol=2))
# c$get()
# cacheSolve(c)
# cacheSolve(c)  # get cached data
# #
# d <- makeCacheMatrix(matrix(c(1,0,1,2,4,0,3,5,6),nrow=3,ncol=3))
# d$get()
# cacheSolve(d)
# cacheSolve(d)  # get cached data
# #
