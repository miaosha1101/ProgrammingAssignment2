## R Programming Assignment 2: 
## Caching the Inverse of a Function

## Generate a special matrix object that can cache its inverse
## assuming the supplied matrix is invertible

makeCacheMatrix <- function(x = matrix()) {
  x_inverse <- NULL
  reset <- function(y) {
    x <<- y
    x_inverse <<- NULL
  }
  getMatrix <- function() x
  getInverse <- function() x_inverse
  setInverse <- function(z) x_inverse <<- z 
  list(reset = reset, 
       getMatrix = getMatrix, 
       getInverse = getInverse,
       setInverse = setInverse )
}

## Computes the inverse of the special matrix returned by makeCacheMatrix. 
## If the inverse has already been calculated and the matrix has not changed,
## then cacheSolve should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  x_inverse <- x$getInverse()
  if(!is.null(x_inverse)){
    message("getting cached data")
    return(x_inverse)
  }
  data <- x$getMatrix()
  x_inverse <- solve(data)
  x$setInverse(x_inverse)
  x_inverse
}
