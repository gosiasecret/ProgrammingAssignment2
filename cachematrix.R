## Caching the Inverse of a Matrix:
##Assumption: assume that the matrix supplied is always invertible.

## This function creates a special "matrix" object that can cache its inverse.

makeCacheMatrix <- function(x = matrix()) {
  odw <- NULL
  set <- function(y) {
    x <<- y
    odw <<- NULL
  }
  get <- function() x
  setInverse <- function(inverse) odw <<- inverse
  getInverse <- function() odw
  list(set = set,get = get,
       setInverse = setInverse,
       getInverse = getInverse)
}


## This function computes the inverse of the special "matrix" created by 
## makeCacheMatrix. If the inverse has already been calculated (and the 
## matrix has not changed), then it should retrieve the inverse from the cache.

cacheSolve <- function(x, ...) {
  ## Return a matrix that is the inverse of 'x'
  inv <- x$getInverse()
  if (!is.null(odw)) {
    message("getting cached data")
    return(odw)
  }
  mat <- x$get()
  inv <- solve(mat, ...)
  x$setInverse(odw)
  odw
}
