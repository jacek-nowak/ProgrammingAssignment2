## Function makeCacheMatrix creates a matrix object with functions capable of 
## setting and getting the matrix data. Other functions will inverse the matrix and set
## the inverse data. All operations have enabled caching to prevent unnecessary inverse 
## calculations

## This function creates the matrix object

makeCacheMatrix <- function(x = matrix()) {
   cache <- NULL
   ## set the matrix data and clear the cache (as data just changed)
   set <- function(y) {
      x <<- y
      cache <<- NULL
   }
   ## return original matrix
   get <- function() x
   ## cache the inversion
   setinverse <- function(inv) cache <<- inv
   ## return the cache value
   getinverse <- function() cache
   ## return the matrix object
   list(set = set, get = get,
        setinverse = setinverse,
        getinverse = getinverse)
}


## This function returns cached matrix inverse. If the chache is empty it calculates the
## inverse, populates the cache and returns the calculated data.

cacheSolve <- function(x, ...) {
   inv <- x$getinverse()
   if(!is.null(inv)) {
      ## Cache is available, return it and end the function
      return(inv)
   }
   ## cache empty, calculate the inverse and store it in the cache
   data <- x$get()
   inv <- solve(data, ...)
   x$setinverse(inv)
   inv
}
