## The following pair of functions can be used to calculate and cache
## the inverse of a matrix

## The makeCacheMatrix function creates a special matrix object that can
## cache its inverse, using internal functions to set and get the matrix
## or to set and get the inverse itself

makeCacheMatrix <- function(x = matrix()) {
        i <- NULL                               
        set <- function(y) {                    
                x <<- y
                i <<- NULL              
        }
        get <- function() x
        setinverse <- function(solve) i <<- solve
        getinverse <- function() i
        list (set = set, get = get,
              setinverse = setinverse,
              getinverse = getinverse)
}


## The cacheSolve function computes the inverse of the matrix returned by
## the makeCacheMatrix function above. If the matrix has not changed and the
## inverse has already been computed, then cacheSolve will pull the cached
## inverse value rather than compute it again, thus saving time

cacheSolve <- function(x, ...) {
        ## Return a matrix that is the inverse of 'x'
        i <- x$getinverse()
        if(!is.null(i)) {
                message("getting cached data")
                return(i)
        }
        data <- x$get()
        i <- solve(data, ...)
        x$setinverse(i)
        i
}
