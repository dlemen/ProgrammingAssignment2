## Helper functions for solving and caching the inverse
## of a matrix.

## makeCacheMatrix: Creates a wrapper object for a matrix that 
## can cache its inverse.
## 
## Example use:
## > m <- makeCacheMatrix(matrix(runif(4), nrow=2, ncol=2))
## > m$get()
## [,1]      [,2]
## [1,] 0.4849760 0.6534898
## [2,] 0.5952678 0.9303847

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve: Checks the object created by makeCacheMatrix() 
## to see if the inverse of its matrix has already been 
## solved and cached. If so, return the cached inverse.
## If not, solve and cache the result before returning it.
## 
## Example use:
##
## > m <- makeCacheMatrix(matrix(runif(4), nrow=2, ncol=2))
## > m$getinverse()
## NULL
## > cacheSolve(m)
## [,1]      [,2]
## [1,]  1.860741 -1.265745
## [2,] -2.426461  3.856918

cacheSolve <- function(x, ...) {
    ## Return a matrix that is the inverse of 'x'
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
