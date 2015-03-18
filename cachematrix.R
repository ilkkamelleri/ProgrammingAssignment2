## This code contains two functions (makeCacheMatrix and cacheSolve).
## The functions can be used to get the inverse of a given matrix.
## Whenever the inverse is calculated and returned it is also stored in cache, 
## for possible reuse later without having to recalculate the inverse.


## This function creates a matrix that can be used to cache the value for the inverse 
## of the given matrix.

makeCacheMatrix <- function(x = matrix()) {

    i <- NULL
    
    set <- function(y) {
        x <<- y
        i <<- NULL
    }
    get <- function() x
    
    setinverse <- function(inverse) i <<- inverse
    getinverse <- function() i
    
    matrix(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## This function returns the inverse of the given matrix x.
## If the inverse is already available in the cache, the function returns the cached value. 
## Otherwise the inverse is calculated, stored in cache and returned.

cacheSolve <- function(x, ...) {
          
    i <- x$getinverse()
    
    if(!is.null(i)) {
        message("getting cached data")
    return(i)
    }
    
    data <- x$get()
    i <- solve(data, ...)
    x$setinverse(i)
  
    ## Return a matrix that is the inverse of 'x'
    i
}
