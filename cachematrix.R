## Put comments here that give an overall description of what your
## functions do
## These functions collectively set/get the inverse of a matrix using
## cached values when possible

## Write a short comment describing this function
## Description: setter/getter function for caching a matrix and its inverse
## using mutable variables x ( the matrix) and inverse
makeCacheMatrix <- function(x = matrix()) {
  inverse <- NULL
  set <- function(y) {
    x <<- y
    inverse <<- NULL
  }
  
  get <- function() x
  
  setInverse <- function(newInverse) inverse <<- newInverse
  
  getInverse <- function() inverse
  
  # the return value is actually a list of function name/function pairs
  # corresponding to the set/get operations of interest
  # see sections 10.5 and 10.7 of the "Intro to R" guide 
  list(set = set, get = get,
       setInverse = setInverse,
       getInverse = getInverse)
  
}


## Write a short comment describing this function
## Description: used to find the inverse of a matrix as quickly as possible
cacheSolve <- function(x, ...) {
  inverse <- x$getInverse()
  if(!is.null(inverse)) {
    message("getting cached data")
    return(inverse)
  }
  
  # we need to recompute the inverse
  data <- x$get()
  inverse <- solve(data, ...)
  x$setInverse(inverse)
  inverse
  
}
