## The functions in this file generate a 'matrix' object that
## caches its own inverse, and provide a means of calculating and
## extracting that cached value

## makeCacheMatrix generates a matrix object which caches its own
##inverse

makeCacheMatrix <- function(x = matrix()) {
    i <- NULL
    set <- function(y) {
        #Change the stored value and invalidate the current cache
        x <<- y
        i <<- NULL
    }
    get <- function () x
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    list(set = set, get = get,
         setinverse = setinverse,
         getinverse = getinverse)
}


## cacheSolve computes the inverse of a matrix object returned
## by the makeCacheMatrix function

cacheSolve <- function(x, ...) {
    inverse = x$getinverse()
    if(!is.null(inverse)) {
        message("getting cached inverse")
        return(inverse)
    }
    mat <- x$get()
    inverse <- solve(mat) #actually calculate inverse
    x$setinverse(inverse)
    ## Return a matrix that is the inverse of 'x'
    inverse
}
