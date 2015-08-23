## Matrix inversion is usually a costly computation and there may be some 
## benefit to caching the inverse of a matrix rather than compute it repeatedly 
## This script consists f a pair of functions that cache the inverse of a matrix.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
    inv <- NULL
    
    ## set the inverse
    set <- function(y) {
        x <<- y
        inv <<- NULL
    }
    
    ## anonymous function to get the inverse
    get <- function() x
    
    ## store the inverse in cache
    setinv <- function(inverse) inv <<- inverse
    
    ## get the inverse from cache
    getinv <- function() inv
    
    ## return matrix of functions
    list(set = set, get = get,
         setinv = setinv,
         getinv = getinv)
}


## This function computes the inverse of the special "matrix" returned by 
## makeCacheMatrix above. If the inverse has already been calculated 
## (and the matrix has not changed), then the cachesolve retrieves the 
## inverse from the cache.

cacheSolve <- function(x, ...) {
    inv <- x$getinv()
    ## if the cache contains the already computed inverse then get & return it
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    ## cache does not have inverse, so get the data and calculate inverse
    data <- x$get()
    inv <- solve(data, ...)
    
    ## set the inverse in cache
    x$setinv(inv)
    
    ## return inverse
    inv
}
