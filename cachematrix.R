## These functions create the ability to cache the inverse of a matrix
## to save computation time when computing repeatedly.  

## makeCacheMatrix creates a list containing a function to set, get,
## setinverse, and getinverse of a matrix

makeCacheMatrix <- function(x = matrix()) {
  inv = NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setinverse <- function(inverse) inv <<- inverse
  getinverse <- function() inv
  list(set=set, get=get, setinverse=setinverse, getinverse=getinverse)
}


## Here, the cacheSolve function retrieves the inverse of the matrix
## if it finds it in the cache.  If it is not in the cache, it then computes
## the value.  It assumes the matrix is always invertible.

cacheSolve <- function(x, ...) {
  inv <- x$getinverse()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data)
  x$setinverse(inv)
  inv      
}
