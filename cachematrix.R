
# Functions to increase calculations involving matrix inverses.
# A "cache matrix" is defined, that can cache its inverse.

# 'makeCacheMatrix' returns an extended matrix object
# with differents methods to interact with its content.

makeCacheMatrix <- function(x = matrix()) {
    # ensures that 'x' is a matrix
    stopifnot(inherits(x, "matrix")) 
    # at this environment in which 'x' exists
    # the inverse matrix (xInv) and some methods are defined
    xInv <- NULL
    set <- function(m) {
        x <<- m
        xInv <<- NULL # deletes a previous cache
    }
    get <- function() x
    setInverse <- function(mInv) xInv <<- mInv
    getInverse <- function() xInv
    # a list with the methods is returned
    # x and xInv are accessible via lexical scoping
    cacheMatrix <- list(set = set, 
         get = get,
         setInverse = setInverse,
         getInverse = getInverse)
    # the output is defined as class "cacheMatrix"
    class(cacheMatrix) <- "cacheMatrix"
    cacheMatrix
}

# 'cacheSolve' returns the inverse of a "cache matrix"
# from its cache and builds the cache if it not exist.

cacheSolve <- function(x, ...) {
    # ensures that 'x' is a cacheMatrix
    stopifnot(inherits(x, "cacheMatrix")) 
    cInv <- x$getInverse()
    if(!is.null(cInv)) {
        message("using the cached inverse")
        return(cInv)
    }
    # as the cache does not exist
    # the inverse is calculated and cached
    c <- x$get()
    cInv <- solve(c, ...)
    x$setInverse(cInv)
    cInv
}

# Example:
#   mt <- makeCacheMatrix(matrix(rnorm(4), nrow=2, ncol=2))
#   cacheSolve(mt) # first time
#   cacheSolve(mt) # second time (use cache)
