## This pair of functions caches the inverse of a matrix

## Creates a special "matrix" object that can cache its inverse
makeCacheMatrix <- function(x = matrix()) {
        s  <- NULL
        set  <- function(y) {
              x <<- y
              s <<- NULL 
        }
        get  <- function() x
        setsolve  <- function(solve) s  <<- solve
        getsolve  <- function() s
        list(set = set, get = get, 
              setsolve = setsolve, 
              getsolve = getsolve)
}

## Computes the inverse. If inverse has already been calculated (and matrix has not changed),
## then cachesolve should retrieve inverse from cache
cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        s  <- x$getsolve()
        if (!is.null(s)) {
              message("getting cached data")
              return(s)
        }
        data  <- x$get()
        s  <- solve(data, ...)
        x$setsolve(s)
        s
}
