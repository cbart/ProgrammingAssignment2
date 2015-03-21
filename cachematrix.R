## Put comments here that give an overall description of what your
## functions do

## Produces an object wrapping given x matrix with additional inverse
## caching capabilities.
##
## Parameter x should be a square, invertible matrix.
##
## Use $get() and $set(x) to retrieve and store the underlying matrix.
## Use cacheSolve(x, ...) to get inverse of the underlying matrix
## (x in this case is the object returned by makeCacheMatrix).

makeCacheMatrix <- function(x = matrix()) {
    solved <- NULL
    set <- function(newX) {
        x <<- newX
        solved <<- NULL
    }
    get <- function() x
    inverse <- function(...) {
        if (is.null(solved)) {
            solved <<- solve(x, ...)
        }
        solved
    }
    list(set=set, get=get, inverse=inverse)
}


## Returns the inverse of matrix boxed in x.
##
## The computations are cached within every call of $set(x) on the given
## object x.
##
## Parameter x is the object caching matrix inverse produced
## by makeCacheMatrix.

cacheSolve <- function(x, ...) {
    x$inverse(...)
}
