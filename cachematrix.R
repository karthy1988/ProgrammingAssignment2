## "makeCacheMatrix" function creates a special matrix which takes a matrix as input
## and returns a list of functions contaning "set", "get", "setinverse" and "getinverse".
## The purpose of makeCacheMatrix is to cache the inverse of a give matrix "x".

makeCacheMatrix <- function(x = matrix()) {
     
      inv <- NULL             
      set <- function(y) {
          x <<- y
          inv <<- NULL
      }
    
    get <- function() x
    setinverse <- function(inverse) inv <<- inverse
    getinverse <- function() inv
    list(set = set, get = get, setinverse = setinverse, getinverse = getinverse)
}


## "cacheSolve" function simply calculates the inverse of the matrix using the "solve()"
## function. While it calculates the inverse, it first checks if the inverse value
## already exits, if yes, then it returns the value from cache. Else, it computes the value. 

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'

    inv <- x$getinverse()
    if(!is.null(inv)) {
        message("getting cached data")
        return(inv)
    }
    
    data <- x$get()
    inv <- solve(data, ...)
    x$setinverse(inv)
    inv
}