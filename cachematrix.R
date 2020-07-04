## In this assignment a set of functions is designed to cache the inverse of a matrix,
## thereby preventing the need of repeated computation

## makeCacheMatrix: function creates a matrix object that can cache its inverse

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y){
    x <<- y
    inv <<- NULL
  }
  get <- function(){x}
  setInverse <- function(Inverse){inv <<- Inverse}
  getInverse <- function() {inv}
  list(set = set, get=get, setInverse = setInverse, getInverse= getInverse)
}

## cacheSolve: function computes the inverse of the matrix returned by makeCacheMatrix()
## In case the inverse has already been calculated it is retrieved from the cache. 

cacheSolve <- function(x, ...) {
  inv <- x$getInverse()
  if(!is.null(inv)){
    message("getting cached data")
    return(inv)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(inv)
  inv
}
