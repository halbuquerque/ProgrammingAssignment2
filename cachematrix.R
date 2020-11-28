## This files contains 2 functions: makeCacheMatrix and cacheSolve.
## These functions can be used to find the inverse of square matrices and to store the 
## result of the inversion in cache.

## The makeCacheMatrix function prepares a matrix to be stored in cache and offers
## methods to get and set the matrix that will have its inverse cached.

makeCacheMatrix <- function(x = matrix()) {
  inv <- NULL
  set <- function(y) {
    x <<- y
    inv <<- NULL
  }
  get <- function() x
  setInv <- function(solve) inv <<- solve
  getInv <- function() inv
  list(set = set, get = get,
       setInv = setInv,
       getInv = getInv)
}

## The cacheSolve function checks if the parameter matrix already has its inverse calculation stored
## in cache and, if it does, it returns the cached matrix. Otherwise, it solves for the inverse of 
## the matrix and stores the result in cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInv()
  if(!is.null(inv)) {
    message("getting cached data")
    return(inv)
  }
  data <- x$get()
  inv <- solve(data, ...)
  x$setInv(inv)
  inv
}
