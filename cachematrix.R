# Implementation of wrapper for holding an inversable matrix cache and the object that holds it
#A wrapper holding a matrix and it's inverse's matrix
makeCacheMatrix <- function(x = matrix()) {
            
            cachedData <- NULL
            
            set <- function(y) {
              x <<- y
              cachedData <<- NULL
            }
            
            get <- function() x
            setinverse <- function(inverseMatrix) cachedData <<- inverseMatrix
            getinverse <- function() cachedData
            list(set = set, get = get,
                 setinverse = setinverse,
                 getinverse = getinverse)

}


## Getting a matrix that was generated through makeCacheMatrix and returns inverse if it exists from the cache
## or otherwise calculates the value on fly and updates the cache
cacheSolve <- function(x, ...) {
  
        ## Return a matrix that is the inverse of 'x'
        cachedData <- x$getinverse()
        if(!is.null(cachedData)) {
          #getting cached data
          return(cachedData)
        }
        ## the inverse matrix isn't in the cahce and is now being calculated
        data <- x$get()
        cachedData <- solve(data,...)
        x$setinverse(cachedData)
        cachedData
}
