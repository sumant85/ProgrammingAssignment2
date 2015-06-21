## This set of functions provide a utility to cache the inverse of a matrix.
## Generally, matrix inversion is an expensive operation and these methods
## can be used to save computation time.

## This function returns a matrix whose inverse is cacheable
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(i) inv <<- i
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## Returns the inverse of a matrix. Caches the value, and returns the cached
## value in all subsequent calls.
## @param x: Should be a matrix returned by makeCacheMatrix function.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    data <- x$get()
    m <- solve(data, ...)
    x$setinverse(m)
    m
}
