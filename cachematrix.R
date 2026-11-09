
## Programming Assignment 2: Lexical Scoping
## Cache the inverse of a matrix to avoid repeated calculations.

## Create a matrix object that stores its inverse.
makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL

    set <- function(y) {
        x <<- y
        inv <<- NULL
    }

    get <- function() x

    setinverse <- function(inverse) {
        inv <<- inverse
    }

    getinverse <- function() inv

    list(set = set,
         get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}

## Return the cached inverse, or compute it if necessary.
cacheSolve <- function(x, ...) {
    inv <- x$getinverse()

    if (!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }

    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}
