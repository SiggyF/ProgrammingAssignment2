## This module exposes functions that cache the inverse of a matrix.

## creates a special "matrix" object that can cache its inverse.
makeCacheMatrix <- function(x = matrix()) {
    ## We use I to denote the inverse, as is common in some math notations
    I <- NULL
    ## Create a setter
    set <- function(y) {
        x <<- y
        I <<- NULL
    }
    ## Create a getter
    get <- function() x
    ## similar to the mean example
    setinverse <- function(inverse) I <<- inverse
    getinverse <- function() I
    ## this is returned
    list(
        set = set,
        get = get,
        setinverse = setinverse,
        getinverse = getinverse
        )
}


## computes the inverse of the special "matrix" returned by makeCacheMatrix
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    I <- x$getinverse()
    if(!is.null(I)) {
        message("getting cached data")
        return(I)
    }
    data <- x$get()
    I <- solve(data, ...)
    x$setinverse(I)
    I
}
