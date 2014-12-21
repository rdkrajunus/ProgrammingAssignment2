## Together, these functions create a special matrix, whose inverse is cached
## and only computed once.  Subsequent calls to the inverse return the same
## inverse, unless the underlying matrix changes.

## makeCacheMatrix creates a special "matrix" which is a list containing
## functions to
## 1) set the value of the matrix
## 2) get the value of the matrix
## 3) set the value of the inverse of the matrix
## 4) get the value of the inverse of the matrix
makeCacheMatrix <- function(x = matrix()) {
    inverse <- NULL

    set <- function(y = matrix()) {
        x <<- y
        inverse <<- NULL
    }

    get <- function() { x }

    setinverse <- function(solve) { inverse <<- solve }

    getinverse <- function() { inverse }

    list(get = get,
         set = set,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of the supplied cache-matrix if it hasn't
## been computed already for this cache matrix.  If the inverse has been
## computer, cacheSolve returns the cached inverse. generated with the
## makeCacheMatrix function
cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
    inverse <- x$getinverse()

    if (!is.null(inverse)) {
        message("getting cached matrix inverse")
        return(inverse)
    }
    else {
        matrix <- x$get()
        inverse <- solve(matrix, ...)
        x$setinverse(inverse)
        inverse
    }
}
