## Matrix inversion is usually a costly computation.
## their may be some benefit to caching the inverse of a matrix rather than compute it repeatedly.
## the functions below implement caching for inverse of a matrix.
## created by seongju.hong
## created on 2014/05/25

## makeCacheMatrix : This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {

    s <- NULL

    ## assign four functions to four variables(set, get, setsolve, getsolve).
    set <- function(y) {
        x <<- y
        s <<- NULL
    }
    get <- function() x
    setsolve <- function(solve) s <<- solve
    getsolve <- function() s

    ## make a list containing four variables.
    list(set = set, get = get,
         setsolve = setsolve,
         getsolve = getsolve)
}

## cacheSolve : This function computes the inverse of the special "matrix" returned by makeCacheMatrix above. 
## If the inverse has already been calculated (and the matrix has not changed), then the cachesolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {

    s <- x$getsolve()

    ## if the cached inverse exists, return it.
    if(!is.null(s)) {
        message("getting cached data")
        return(s)
    }

    ## if the cached inverse not exists, compute, cache, and return it.
    data <- x$get()
    s <- solve(data, ...)
    x$setsolve(s)
    s
}
